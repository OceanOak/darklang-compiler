// SyntaxDSLTests.fs - Unit tests for the syntax fixture parser and runner.
//
// Keeps the DSL implementation honest without expressing its own behavior in the DSL.

module SyntaxDSLTests

open TestDSL.SyntaxFormat
open TestDSL.SyntaxTestRunner

type TestResult = Result<unit, string>

let testParsesMultipleSyntaxCases () : TestResult =
    let content =
        """---NAME---
canonical formatting
---SOURCE---
let x = 5 in x
---EXPECTED---
let x = 5I in x
---ROUNDTRIP---

---NAME---
reject fat-arrow lambda
---SOURCE---
let inc = (x: Int64) => x + 1
---EXPECT-ERROR---
does not use
"""

    match parseSyntaxFileContent "syntax.syntax" content with
    | Ok [ first; second ]
        when first.Name = "canonical formatting"
             && first.ExpectedFormat = Some "\nlet x = 5I in x\n"
             && first.Roundtrip
             && second.ExpectedError = Some "does not use" ->
        Ok ()
    | Ok cases -> Error $"Expected two fully parsed syntax cases, got {cases}"
    | Error msg -> Error $"Expected syntax cases to parse, got: {msg}"

let testRejectsLegacySyntaxSelector () : TestResult =
    let content =
        """---NAME---
legacy selector
---PARSE-AS---
compiler
---SOURCE---
1
"""

    match parseSyntaxFileContent "invalid.syntax" content with
    | Error msg when msg.Contains "Unknown syntax section: PARSE-AS" -> Ok ()
    | Error msg -> Error $"Expected legacy selector validation error, got: {msg}"
    | Ok _ -> Error "Expected the legacy parser selector to be rejected"

let testRunsFormattingAndRoundtripChecks () : TestResult =
    let testCase =
        { Name = "canonical formatting"
          Source = "let x = 5 in Stdlib.Int64.add(x, 1)"
          ExpectedError = None
          ExpectedFormat = Some "let x = 5I in Stdlib.Int64.add x 1I"
          Roundtrip = true
          SourceFile = "syntax.syntax" }

    let result = runSyntaxTest testCase
    if result.Success then Ok ()
    else Error $"Expected syntax runner success, got: {result.Message}"

let tests = [
    ("syntax DSL parses multiple cases", testParsesMultipleSyntaxCases)
    ("syntax DSL rejects legacy parser selectors", testRejectsLegacySyntaxSelector)
    ("syntax DSL runs format and roundtrip checks", testRunsFormattingAndRoundtripChecks)
]
