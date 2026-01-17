#!/bin/bash
# Show worktree status with merge info relative to main
# In interactive mode, shows a real-time updating UI

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
DIM='\033[0;90m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Refresh interval in seconds for interactive mode
REFRESH_INTERVAL=2

worktree_status_flags() {
    local path="$1"
    local flags=""
    local porcelain=""

    porcelain=$(git -C "$path" status --porcelain=1 2>/dev/null || true)

    if [ -n "$porcelain" ]; then
        if echo "$porcelain" | grep -q '^[MADRCU]'; then
            flags="${flags} ${YELLOW}(staged)${NC}"
        fi

        if echo "$porcelain" | grep -q '^.[MADRCU]'; then
            flags="${flags} ${YELLOW}(unstaged)${NC}"
        fi

        if echo "$porcelain" | grep -q '^\?\?'; then
            flags="${flags} ${YELLOW}(untracked)${NC}"
        fi
    fi

    echo "$flags"
}

worktree_unpushed_count() {
    local path="$1"
    local upstream=""
    local count=""

    upstream=$(git -C "$path" rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null || true)
    if [ -z "$upstream" ]; then
        echo ""
        return
    fi

    count=$(git -C "$path" rev-list --count "${upstream}..HEAD" 2>/dev/null || true)
    if [ -z "$count" ] || [ "$count" = "0" ]; then
        echo ""
        return
    fi

    echo " ${RED}↑ ${count} unpushed${NC}"
}

render_status() {
    # Collect output, then sort by branch name
    {
    git worktree list | while read -r line; do
        path=$(echo "$line" | awk '{print $1}')

        # Extract branch, handling prunable entries
        if echo "$line" | grep -q "prunable"; then
            branch=$(echo "$line" | sed 's/.*\[\(.*\)\].*/\1/' | sed 's/ prunable//')
            prunable=" ${DIM}(prunable)${NC}"
        else
            branch=$(echo "$line" | sed 's/.*\[\(.*\)\].*/\1/')
            prunable=""
        fi

        # Check if worktree path exists and is accessible
        if [ -d "$path" ] && git -C "$path" rev-parse HEAD &>/dev/null; then
            status_flags=$(worktree_status_flags "$path")

            if [ "$branch" = "main" ]; then
                unpushed=$(worktree_unpushed_count "$path")
                status="${GREEN}✓${NC}${unpushed}${status_flags}${prunable}"
                echo -e "0\tmain\t${CYAN}main${NC}\t${status}"
                continue
            fi

            ahead=$(git -C "$path" rev-list --count main..HEAD 2>/dev/null || echo "?")
            behind=$(git -C "$path" rev-list --count HEAD..main 2>/dev/null || echo "?")

            if [ "$ahead" = "0" ] && [ "$behind" = "0" ]; then
                status="${GREEN}✓ merged${NC}${status_flags}${prunable}"
            elif [ "$ahead" = "0" ]; then
                status="${YELLOW}↓ ${behind} behind${NC}${status_flags}${prunable}"
            elif [ "$behind" = "0" ]; then
                status="${RED}↑ ${ahead} ahead${NC}${status_flags}${prunable}"
            else
                status="${RED}↑ ${ahead} ahead, ↓ ${behind} behind${NC}${status_flags}${prunable}"
            fi
        else
            status="${DIM}(inaccessible)${NC}${prunable}"
        fi

        echo -e "1\t${branch}\t${branch}\t${status}"
    done
    } | sort -t$'\t' -k1,1 -k2,2 | awk -F '\t' '
    function vislen(s, t) {
        t = s
        gsub(/\x1B\[[0-9;]*m/, "", t)
        return length(t)
    }
    {
        branches[NR] = $2
        labels[NR] = $3
        statuses[NR] = $4
        if (vislen($2) > max) max = vislen($2)
    }
    END {
        printf "%-" max "s  %s\n", "BRANCH", "STATUS"
        printf "%-" max "s  %s\n", "------", "------"
        for (i = 1; i <= NR; i++) {
            pad = max - vislen(labels[i])
            if (pad < 0) pad = 0
            printf "%s%*s  %s\n", labels[i], pad, "", statuses[i]
        }
    }
    '
}

# Check if running interactively (stdout is a terminal and stdin is a terminal)
is_interactive() {
    [ -t 1 ] && [ -t 0 ]
}

run_interactive() {
    local lines_printed=0

    # Hide cursor
    tput civis 2>/dev/null || true

    # Cleanup on exit
    cleanup() {
        tput cnorm 2>/dev/null || true  # Show cursor
        echo ""  # Newline after status
        exit 0
    }
    trap cleanup EXIT INT TERM

    while true; do
        # Move cursor up to overwrite previous output
        if [ "$lines_printed" -gt 0 ]; then
            tput cuu "$lines_printed" 2>/dev/null || true
            tput cr 2>/dev/null || true
        fi

        # Clear from cursor to end of screen
        tput ed 2>/dev/null || true

        # Print header with timestamp
        echo -e "${BOLD}Worktree Status${NC} ${DIM}(updated $(date '+%H:%M:%S'), refresh: ${REFRESH_INTERVAL}s, Ctrl+C to exit)${NC}"
        echo ""

        # Render status and count lines
        local output
        output=$(render_status)
        echo "$output"

        # Count lines printed (header + blank + status output)
        lines_printed=$((2 + $(echo "$output" | wc -l)))

        # Wait for refresh interval
        sleep "$REFRESH_INTERVAL"
    done
}

run_oneshot() {
    render_status
}

# Main
if is_interactive; then
    run_interactive
else
    run_oneshot
fi
