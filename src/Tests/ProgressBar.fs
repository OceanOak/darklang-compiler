// ProgressBar.fs - Progress bar utilities for the test runner
//
// Provides a progress bar for long-running test suites.

module ProgressBar

open Output

let private barWidth = 20
type State = {
    mutable Total: int
    mutable Completed: int
    mutable Failed: int
    Label: string
}

let create label total = { Total = total; Completed = 0; Failed = 0; Label = label }

let update (state: State) =
    let rawPct =
        if state.Total = 0 then 0.0
        else float state.Completed / float state.Total
    let pct = max 0.0 (min 1.0 rawPct)
    let filled = int (pct * float barWidth)
    let bar = String.replicate filled "=" + String.replicate (barWidth - filled) " "
    let failStr = if state.Failed > 0 then $" ({Colors.red}{state.Failed} failed{Colors.reset})" else ""
    // Use \r to return to start of line, \x1b[K to clear to end of line
    eprint $"\r\x1b[K  {state.Label}: [{bar}] {state.Completed}/{state.Total}{failStr}"

let increment (state: State) (success: bool) =
    state.Completed <- state.Completed + 1
    if not success then state.Failed <- state.Failed + 1
    update state

let finish (state: State) =
    // Clear the progress line and print final summary
    eprint "\r\x1b[K"
