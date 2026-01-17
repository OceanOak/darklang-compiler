#!/usr/bin/env python3
"""
Validate E2E test expected outputs against the Darklang interpreter.

This script parses .e2e test files, converts Ralph2 syntax to Darklang syntax,
runs expressions through the darklang-interpreter, and compares results.

Usage:
    python scripts/validate-darklang.py                    # Validate all e2e tests
    python scripts/validate-darklang.py path/to/test.e2e   # Validate specific file
    python scripts/validate-darklang.py --verbose          # Show all results
    python scripts/validate-darklang.py --show-conversions # Show syntax conversions
"""

import argparse
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Optional


class TestResult(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    SKIP = "SKIP"
    ERROR = "ERROR"


@dataclass
class TestCase:
    """Represents a single test case from an .e2e file."""
    line_number: int
    expression: str
    expected: str
    preamble: Optional[str] = None  # e.g., "def foo(...) = ..."


@dataclass
class ValidationResult:
    """Result of validating a single test case."""
    test: TestCase
    result: TestResult
    actual: Optional[str] = None
    skip_reason: Optional[str] = None
    error_message: Optional[str] = None
    converted_expr: Optional[str] = None
    converted_expected: Optional[str] = None


class E2EParser:
    """Parses .e2e test files."""

    # Pattern for test line: expression = expected
    # Handle inline `def` definitions that precede the test expression
    TEST_PATTERN = re.compile(
        r'^(?P<preamble>(?:def\s+\w+.*?=\s+.*?\s+)+)?'  # Optional def preamble
        r'(?P<expr>.+?)\s*=\s*(?P<expected>.+)$'
    )

    def parse_file(self, filepath: Path) -> list[TestCase]:
        """Parse an .e2e file and return list of test cases."""
        tests = []

        with open(filepath, 'r') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()

                # Skip empty lines and comments
                if not line or line.startswith('//'):
                    continue

                # Try to parse as a test line
                test = self._parse_test_line(line, line_num)
                if test:
                    tests.append(test)

        return tests

    def _parse_test_line(self, line: str, line_num: int) -> Optional[TestCase]:
        """Parse a single test line."""
        # Handle lines with inline def definitions
        # Format: def foo(x: T): T = body expr = expected

        # First, check if line contains a def
        if line.startswith('def '):
            # Find the last '=' that's followed by expected value
            # We need to handle nested def statements
            return self._parse_line_with_def(line, line_num)

        # Simple case: expr = expected
        # Need to find the last top-level '=' that separates expr from expected
        equals_pos = self._find_test_equals(line)
        if equals_pos is None:
            return None

        expr = line[:equals_pos].strip()
        expected = line[equals_pos + 1:].strip()

        # Strip trailing comments from expected value
        expected = self._strip_comment(expected)

        if not expr or not expected:
            return None

        return TestCase(
            line_number=line_num,
            expression=expr,
            expected=expected
        )

    def _strip_comment(self, value: str) -> str:
        """Strip trailing // comment from a value."""
        # Find // that's not inside a string
        in_string = False
        string_char = None
        i = 0
        while i < len(value):
            char = value[i]

            if char in '"\'`' and (i == 0 or value[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
            elif not in_string and char == '/' and i + 1 < len(value) and value[i + 1] == '/':
                return value[:i].strip()

            i += 1

        return value

    def _parse_line_with_def(self, line: str, line_num: int) -> Optional[TestCase]:
        """Parse a line that starts with def."""
        # Pattern: def name(params): RetType = body <actual_expr> = <expected>
        # The challenge is finding where the def body ends and the test expr begins

        # Strategy: Find the pattern where we have the function body followed by
        # a function call or expression, then = expected

        # Look for common patterns like "def foo(...) = ... foo(...) = expected"
        # We need to match the function name and find its call

        # Extract function name
        match = re.match(r'def\s+(\w+)', line)
        if not match:
            return None
        func_name = match.group(1)

        # Find where the function definition ends
        # Look for the function call pattern after the def
        # Pattern: after "def name(...) = body" we should see "name(" or an expression

        # Find all occurrences of the function name
        # The last meaningful '=' should be our test separator

        # Simple heuristic: find the rightmost ' = ' where right side looks like a value
        equals_pos = self._find_test_equals(line)
        if equals_pos is None:
            return None

        preamble_and_expr = line[:equals_pos].strip()
        expected = line[equals_pos + 1:].strip()

        # Now separate preamble (def) from the test expression
        # Look for the function call or expression after the def body

        # Find where the function body ends - usually right before function is called
        # or where a standalone expression begins

        # Use a pattern to find "def...= body func_name(" or similar
        def_pattern = rf'(def\s+{func_name}\s*\([^)]*\)\s*(?::\s*\w+)?\s*=\s*.+?)\s+({func_name}\s*\(.+)$'
        def_match = re.match(def_pattern, preamble_and_expr)

        if def_match:
            preamble = def_match.group(1).strip()
            expr = def_match.group(2).strip()
        else:
            # Try alternate pattern for multiple defs or different structures
            # For now, treat the whole left side as the expression with embedded def
            preamble = None
            expr = preamble_and_expr

        return TestCase(
            line_number=line_num,
            expression=expr,
            expected=expected,
            preamble=preamble
        )

    def _find_test_equals(self, line: str) -> Optional[int]:
        """Find the position of the '=' that separates expression from expected value."""
        # We need to find the '=' that's at the top level (not inside parens, brackets, etc.)
        # and separates the test expression from the expected value
        # Also, we stop at // comment markers

        depth = 0
        in_string = False
        string_char = None
        last_equals = None

        i = 0
        while i < len(line):
            char = line[i]

            # Handle string literals
            if char in '"\'`' and (i == 0 or line[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
                i += 1
                continue

            if in_string:
                i += 1
                continue

            # Stop at // comment (only at top level)
            if char == '/' and i + 1 < len(line) and line[i + 1] == '/' and depth == 0:
                break

            # Track nesting depth
            if char in '([{':
                depth += 1
            elif char in ')]}':
                depth -= 1
            # Handle == and != operators
            elif char == '=' and depth == 0:
                if i + 1 < len(line) and line[i + 1] == '=':
                    i += 2  # Skip ==
                    continue
                if i > 0 and line[i - 1] in '!<>':
                    i += 1  # Skip part of !=, <=, >=
                    continue
                # This could be our test separator or an assignment in def
                last_equals = i

            i += 1

        return last_equals


class SyntaxConverter:
    """Converts Ralph2 syntax to Darklang syntax."""

    # Mapping of Ralph2 stdlib modules to Darklang
    STDLIB_MODULES = {
        'Int64': 'Stdlib.Int64',
        'Int32': 'Stdlib.Int32',
        'Int16': 'Stdlib.Int16',
        'Int8': 'Stdlib.Int8',
        'UInt64': 'Stdlib.UInt64',
        'UInt32': 'Stdlib.UInt32',
        'UInt16': 'Stdlib.UInt16',
        'UInt8': 'Stdlib.UInt8',
        'Float': 'Stdlib.Float',
        'String': 'Stdlib.String',
        'List': 'Stdlib.List',
        'Dict': 'Stdlib.Dict',
        'Option': 'Stdlib.Option',
        'Result': 'Stdlib.Result',
        'Bool': 'Stdlib.Bool',
        'Char': 'Stdlib.Char',
        'Tuple2': 'Stdlib.Tuple2',
        'Tuple3': 'Stdlib.Tuple3',
        'Math': 'Stdlib.Math',
        'Bytes': 'Stdlib.Bytes',
        'Base64': 'Stdlib.Base64',
        'Uuid': 'Stdlib.Uuid',
    }

    def convert(self, expr: str) -> str:
        """Convert Ralph2 expression to Darklang syntax."""
        result = expr

        # Convert stdlib function calls: Module.func(args) -> Stdlib.Module.func args
        result = self._convert_stdlib_calls(result)

        # Convert lambdas: (x: Int64) => body -> fun x -> body
        result = self._convert_lambdas(result)

        # Convert list syntax: [1, 2, 3] -> [1L; 2L; 3L]
        result = self._convert_lists(result)

        # Convert integer literals: add L suffix
        result = self._convert_int_literals(result)

        return result

    def _convert_stdlib_calls(self, expr: str) -> str:
        """Convert Ralph2 stdlib calls to Darklang syntax."""
        result = expr

        # Pattern: Module.func(arg1, arg2, ...)
        # Convert to: Stdlib.Module.func arg1 arg2 ...

        for ralph_mod, dark_mod in self.STDLIB_MODULES.items():
            # Match Module.function(args) but NOT if already preceded by Stdlib.
            # Negative lookbehind for 'Stdlib.'
            pattern = rf'(?<!Stdlib\.)\b{ralph_mod}\.(\w+)\s*\(([^)]*)\)'

            def replace_call(m):
                func_name = m.group(1)
                args_str = m.group(2).strip()

                if not args_str:
                    return f'{dark_mod}.{func_name}'

                # Parse arguments - need to handle nested parens
                args = self._split_args(args_str)
                args_converted = ' '.join(args)
                return f'{dark_mod}.{func_name} {args_converted}'

            result = re.sub(pattern, replace_call, result)

        # Also convert existing Stdlib.Module.func(args) to Stdlib.Module.func args
        pattern = r'Stdlib\.(\w+)\.(\w+)\s*\(([^)]*)\)'

        def convert_stdlib_parens(m):
            module = m.group(1)
            func_name = m.group(2)
            args_str = m.group(3).strip()

            if not args_str:
                return f'Stdlib.{module}.{func_name}'

            args = self._split_args(args_str)
            args_converted = ' '.join(args)
            return f'Stdlib.{module}.{func_name} {args_converted}'

        result = re.sub(pattern, convert_stdlib_parens, result)

        return result

    def _split_args(self, args_str: str) -> list[str]:
        """Split function arguments, respecting nested parentheses."""
        args = []
        current = []
        depth = 0
        in_string = False
        string_char = None

        for i, char in enumerate(args_str):
            if char in '"\'`' and (i == 0 or args_str[i-1] != '\\'):
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                    string_char = None
                current.append(char)
            elif in_string:
                current.append(char)
            elif char in '([{':
                depth += 1
                current.append(char)
            elif char in ')]}':
                depth -= 1
                current.append(char)
            elif char == ',' and depth == 0:
                args.append(''.join(current).strip())
                current = []
            else:
                current.append(char)

        if current:
            args.append(''.join(current).strip())

        return args

    def _convert_lambdas(self, expr: str) -> str:
        """Convert Ralph2 lambda syntax to Darklang."""
        result = expr

        # Pattern: (x: Type) => body
        # Convert to: fun x -> body

        # Single parameter lambda: (x: Type) => body
        single_param = r'\((\w+)\s*:\s*[\w\[\](),<>\s]+\)\s*=>'
        result = re.sub(single_param, r'fun \1 ->', result)

        # Multi-parameter lambda: (x: T1, y: T2) => body
        # Convert to: fun x y -> body
        multi_param = r'\((\w+)\s*:\s*[\w\[\](),<>\s]+(?:\s*,\s*(\w+)\s*:\s*[\w\[\](),<>\s]+)+\)\s*=>'

        def replace_multi_lambda(m):
            full_match = m.group(0)
            # Extract all parameter names
            param_pattern = r'(\w+)\s*:\s*[\w\[\](),<>\s]+'
            params = re.findall(param_pattern, full_match.rstrip('=>').strip('() '))
            return f'fun {" ".join(params)} ->'

        result = re.sub(multi_param, replace_multi_lambda, result)

        return result

    def _convert_lists(self, expr: str) -> str:
        """Convert list syntax: [1, 2, 3] -> [1L; 2L; 3L]."""
        result = expr

        # Find list literals and convert comma to semicolon
        # This is tricky because we need to handle nested structures

        def convert_list_content(match):
            content = match.group(1)
            if not content.strip():
                return '[]'
            # Split by comma (respecting nesting) and join with semicolon
            items = self._split_args(content)
            return '[' + '; '.join(items) + ']'

        # Simple pattern for list literals
        # This won't handle deeply nested cases perfectly
        result = re.sub(r'\[([^\[\]]*)\]', convert_list_content, result)

        return result

    def _convert_int_literals(self, expr: str) -> str:
        """Add L suffix to integer literals."""
        result = expr

        # Match integers that don't already have a suffix
        # Avoid matching:
        # - Floats (have decimal point)
        # - Already suffixed (L, l, s, y)
        # - Part of identifiers

        # Pattern: word boundary, optional minus, digits, word boundary
        # But not followed by L/l/s/y/. and not preceded by letter

        def add_l_suffix(m):
            # Check what comes before and after
            num = m.group(0)
            return num + 'L'

        # Match standalone integers
        # Negative lookahead for decimal point or existing suffix
        # Negative lookbehind for letters, underscore, or decimal point (to avoid matching in identifiers or floats)
        result = re.sub(
            r'(?<![\w\.])(-?\d+)(?![\w\.])',
            add_l_suffix,
            result
        )

        return result

    def convert_expected(self, expected: str) -> str:
        """Convert expected value to Darklang format."""
        result = expected

        # Convert lists
        result = self._convert_lists(result)

        # Convert integers in expected values
        result = self._convert_int_literals(result)

        return result


class Validator:
    """Validates test cases against the Darklang interpreter."""

    def __init__(self, verbose: bool = False, show_conversions: bool = False):
        self.converter = SyntaxConverter()
        self.verbose = verbose
        self.show_conversions = show_conversions

    def validate_test(self, test: TestCase) -> ValidationResult:
        """Validate a single test case."""
        # Check for skip conditions
        skip_reason = self._should_skip(test)
        if skip_reason:
            return ValidationResult(
                test=test,
                result=TestResult.SKIP,
                skip_reason=skip_reason
            )

        # Convert syntax
        try:
            converted_expr = self.converter.convert(test.expression)
            converted_expected = self.converter.convert_expected(test.expected)
        except Exception as e:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=f"Conversion error: {e}"
            )

        # Run through interpreter
        try:
            actual = self._run_interpreter(converted_expr)
        except Exception as e:
            return ValidationResult(
                test=test,
                result=TestResult.ERROR,
                error_message=str(e),
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )

        # Compare results
        if self._compare_results(actual, converted_expected):
            return ValidationResult(
                test=test,
                result=TestResult.PASS,
                actual=actual,
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )
        else:
            return ValidationResult(
                test=test,
                result=TestResult.FAIL,
                actual=actual,
                converted_expr=converted_expr,
                converted_expected=converted_expected
            )

    def _should_skip(self, test: TestCase) -> Optional[str]:
        """Check if test should be skipped."""
        expr = test.expression
        expected = test.expected

        # Skip tests with def (function definitions)
        if test.preamble or 'def ' in expr:
            return "Contains function definition"

        # Skip tests expecting compile errors
        if 'expect_compile_error' in expected:
            return "Expects compile error"

        # Skip tests expecting runtime errors (error messages, etc.)
        if (expected.strip() == 'error' or
            expected.startswith('error=') or
            (expected.startswith('"') and 'error' in expected.lower()) or
            '= error' in expr):  # Expression ends with '= error'
            return "Expects error result"

        # Skip tests with stdout expectations
        if 'stdout=' in expected:
            return "Expects stdout output"

        # Skip tests with stderr expectations
        if 'stderr=' in expected:
            return "Expects stderr output"

        # Skip tests with exit code expectations
        if 'exit=' in expected:
            return "Expects exit code"

        # Skip tests with Builtin.test* functions
        if 'Builtin.test' in expr or 'Builtin.test' in expected:
            return "Uses Builtin.test functions"

        # Skip tests with match expressions (complex conversion)
        if 'match ' in expr:
            return "Contains match expression"

        # Skip tests with let bindings (not supported in darklang-interpreter eval)
        if 'let ' in expr:
            return "Contains let binding (not supported in eval)"

        # Skip tests with lambda expressions (not supported in darklang-interpreter eval)
        if '=>' in expr or 'fun ' in expr:
            return "Contains lambda (not supported in eval)"

        # Skip tests with type annotations in let bindings (complex)
        if re.search(r'let \w+\s*:\s*\w+', expr):
            return "Contains typed let binding"

        # Skip tests with sized integer suffixes (y, s, l for int8/16/32, uy, us, ul for unsigned)
        if re.search(r'\d+[yslYSL]\b', expr) and not re.search(r'\d+L\b', expr):
            return "Contains sized integer literal"
        if re.search(r'\d+u[yslYSL]\b', expr):
            return "Contains unsigned integer literal"

        # Skip tests with Stdlib functions that may not exist in Darklang or have different semantics
        unsupported_stdlib = [
            'Stdlib.FingerTree', 'Random.',
            '.digitToString', '.getByteAt', '.getByteLength',
            '.setByteAt', '.appendByte', '.fromBytes', '.toBytes',
            '.head', '.tail', '.init', '.last',  # Some list funcs may differ
            '.indexOf',  # Returns Option in Darklang vs Int64 in Ralph2
            '.take', '.drop', '.substring',  # May not exist
            '.slice',  # Different argument interpretation (start, end vs start, length)
            'Int64.sub',
            'Int64.mul',
            'Int64.div',
            'Int64.isEven',
            'Int64.isOdd',
        ]
        for unsup in unsupported_stdlib:
            if unsup in expr:
                return f"Uses unsupported stdlib: {unsup}"

        # Skip tests with internal functions (double underscore prefix)
        if '.__' in expr:
            return "Contains internal function"

        # Skip tests with explicit type parameters (Func<Type>)
        if re.search(r'\w+<\w+>', expr):
            return "Contains explicit type parameter"

        # Skip tests with string interpolation ($"...")
        if '$"' in expr:
            return "Contains string interpolation"

        # Skip tests with float literals that need >2 decimal places (different precision)
        if re.search(r'-?\d+\.\d{3,}', expr):
            return "Contains high-precision float literal"

        # Skip float arithmetic (eval uses int operators for +, -, *)
        if re.search(r'\d+\.\d+', expr) and re.search(r'\s[+*]\s|\s-\s', expr):
            return "Contains float arithmetic (not supported in eval)"

        # Skip tests with custom type definitions/usage (not supported in eval)
        if expr.startswith('type '):
            return "Contains type definition"

        # Skip tests with record construction (Name { ... }) or enum variants
        if re.search(r'\b[A-Z]\w*\s*\{', expr):
            return "Contains record/type construction"

        # Skip tests with custom enum/type usage (PascalCase.Variant or Type.method)
        # Standard stdlib modules are allowed (Int64, List, String, etc.)
        allowed_modules = {'Int64', 'Int32', 'Int16', 'Int8', 'UInt64', 'UInt32', 'UInt16', 'UInt8',
                          'Float', 'String', 'List', 'Dict', 'Option', 'Result', 'Bool', 'Char',
                          'Tuple2', 'Tuple3', 'Math', 'Bytes', 'Base64', 'Uuid', 'Stdlib', 'Some', 'None', 'Ok', 'Error'}
        pascal_matches = re.findall(r'\b([A-Z][a-z]+[A-Za-z]*)\b', expr)
        for pascal_name in pascal_matches:
            if pascal_name not in allowed_modules:
                return f"Contains custom type/enum: {pascal_name}"

        # Skip tests with user-defined function calls (camelCase identifiers not in stdlib)
        # These are functions defined earlier in the test file that aren't available in eval
        # Look for camelCase function calls that aren't stdlib functions
        func_call_pattern = r'(?<!\.)\b([a-z][a-zA-Z0-9]*)\s*\('
        func_matches = re.findall(func_call_pattern, expr)
        allowed_funcs = {'if', 'match', 'fun', 'let', 'in', 'true', 'false'}  # Keywords
        for func in func_matches:
            if func not in allowed_funcs:
                return f"Contains user-defined function: {func}"

        # Skip tests with division operator (different semantics in Darklang)
        # Ralph2 uses / for integer division, Darklang uses / for float division
        if re.search(r'\s/\s', expr) or re.search(r'\d+\s*/\s*\d+', expr):
            return "Contains division operator (different semantics)"

        # Skip tests with modulo operator (may have different semantics)
        if re.search(r'\s%\s', expr) or re.search(r'\d+\s*%\s*\d+', expr):
            return "Contains modulo operator"

        # Skip tests with bitwise operators (different semantics in Darklang)
        if '<<' in expr or '>>' in expr or '&' in expr or '|' in expr or '^' in expr or '~' in expr:
            return "Contains bitwise operator"

        # Skip tests with unary negation applied to parenthetical (darklang eval limitation)
        if re.search(r'-\s*\(', expr):
            return "Contains negation of parenthetical (eval limitation)"

        # Skip tests with boolean not operator (! has different semantics in darklang eval)
        if '!' in expr:
            return "Contains boolean not operator"

        return None

    def _run_interpreter(self, expr: str) -> str:
        """Run expression through darklang-interpreter."""
        try:
            result = subprocess.run(
                ['darklang-interpreter', 'eval', expr],
                capture_output=True,
                text=True,
                timeout=10
            )

            if result.returncode != 0:
                raise RuntimeError(f"Interpreter error: {result.stderr.strip()}")

            return result.stdout.strip()
        except subprocess.TimeoutExpired:
            raise RuntimeError("Interpreter timeout")
        except FileNotFoundError:
            raise RuntimeError("darklang-interpreter not found")

    def _compare_results(self, actual: str, expected: str) -> bool:
        """Compare actual output with expected value."""
        # Normalize both for comparison
        actual_norm = self._normalize_value(actual)
        expected_norm = self._normalize_value(expected)

        return actual_norm == expected_norm

    def _normalize_value(self, value: str) -> str:
        """Normalize a value for comparison."""
        result = value.strip()

        # Remove L suffix from integers for comparison
        result = re.sub(r'(\d+)L\b', r'\1', result)

        # Strip surrounding double quotes for string comparison
        # Darklang interpreter outputs strings without quotes
        if result.startswith('"') and result.endswith('"'):
            result = result[1:-1]

        # Normalize whitespace in lists/tuples
        result = re.sub(r'\s*,\s*', ', ', result)
        result = re.sub(r'\s*;\s*', '; ', result)

        # Normalize list output format (Darklang uses multiline for lists)
        if result.startswith('[') and '\n' in result:
            # Flatten multiline list output
            result = re.sub(r'\[\s*\n\s*', '[', result)
            result = re.sub(r'\s*\n\s*\]', ']', result)
            result = re.sub(r',\s*\n\s*', ', ', result)

        return result


def find_e2e_files(base_path: Path) -> list[Path]:
    """Find all .e2e files in the given path."""
    if base_path.is_file():
        return [base_path] if base_path.suffix == '.e2e' else []

    return sorted(base_path.rglob('*.e2e'))


def print_summary(results: dict[Path, list[ValidationResult]]):
    """Print summary of validation results."""
    total_pass = 0
    total_fail = 0
    total_skip = 0
    total_error = 0

    print("\n" + "=" * 60)
    print("VALIDATION SUMMARY")
    print("=" * 60)

    for filepath, file_results in sorted(results.items()):
        passed = sum(1 for r in file_results if r.result == TestResult.PASS)
        failed = sum(1 for r in file_results if r.result == TestResult.FAIL)
        skipped = sum(1 for r in file_results if r.result == TestResult.SKIP)
        errored = sum(1 for r in file_results if r.result == TestResult.ERROR)

        total_pass += passed
        total_fail += failed
        total_skip += skipped
        total_error += errored

        status = "OK" if failed == 0 and errored == 0 else "FAIL"
        rel_path = filepath.name
        print(f"  {rel_path}: {status} (pass={passed}, fail={failed}, skip={skipped}, error={errored})")

    print("-" * 60)
    print(f"TOTAL: pass={total_pass}, fail={total_fail}, skip={total_skip}, error={total_error}")

    if total_fail > 0 or total_error > 0:
        print("\nValidation FAILED")
        return 1
    else:
        print("\nValidation PASSED")
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Validate E2E test expected outputs against Darklang interpreter"
    )
    parser.add_argument(
        'paths',
        nargs='*',
        default=['src/Tests/e2e'],
        help='Files or directories to validate'
    )
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Show all test results'
    )
    parser.add_argument(
        '--show-conversions', '-c',
        action='store_true',
        help='Show syntax conversions'
    )
    parser.add_argument(
        '--show-failures', '-f',
        action='store_true',
        help='Show only failures'
    )

    args = parser.parse_args()

    # Find all e2e files
    all_files = []
    for path_str in args.paths:
        path = Path(path_str)
        if not path.exists():
            print(f"Warning: Path does not exist: {path}")
            continue
        all_files.extend(find_e2e_files(path))

    if not all_files:
        print("No .e2e files found")
        return 1

    # Parse and validate
    e2e_parser = E2EParser()
    validator = Validator(
        verbose=args.verbose,
        show_conversions=args.show_conversions
    )

    all_results: dict[Path, list[ValidationResult]] = {}

    for filepath in all_files:
        print(f"\nValidating: {filepath.name}")

        try:
            tests = e2e_parser.parse_file(filepath)
        except Exception as e:
            print(f"  Error parsing file: {e}")
            continue

        file_results = []
        for test in tests:
            result = validator.validate_test(test)
            file_results.append(result)

            # Print result based on verbosity
            if args.verbose or (args.show_failures and result.result in (TestResult.FAIL, TestResult.ERROR)):
                status_str = result.result.value
                print(f"  Line {test.line_number}: {status_str}")

                if args.show_conversions and result.converted_expr:
                    print(f"    Ralph2:   {test.expression}")
                    print(f"    Darklang: {result.converted_expr}")

                if result.result == TestResult.FAIL:
                    print(f"    Expected: {test.expected}")
                    print(f"    Actual:   {result.actual}")
                elif result.result == TestResult.ERROR:
                    print(f"    Error: {result.error_message}")
                elif result.result == TestResult.SKIP:
                    print(f"    Reason: {result.skip_reason}")

        all_results[filepath] = file_results

        # Quick summary for this file
        if not args.verbose:
            passed = sum(1 for r in file_results if r.result == TestResult.PASS)
            failed = sum(1 for r in file_results if r.result == TestResult.FAIL)
            skipped = sum(1 for r in file_results if r.result == TestResult.SKIP)
            errored = sum(1 for r in file_results if r.result == TestResult.ERROR)
            print(f"  Results: pass={passed}, fail={failed}, skip={skipped}, error={errored}")

    return print_summary(all_results)


if __name__ == '__main__':
    sys.exit(main())
