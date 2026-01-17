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

get_recent_commits() {
    local path="$1"
    local count="$2"
    git -C "$path" log --oneline -n "$count" 2>/dev/null | while read -r line; do
        echo "$line"
    done
}

render_status() {
    local term_width
    term_width=$(tput cols 2>/dev/null) || term_width=120

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

        # Get recent commits (up to 8)
        local commits=""
        if [ -d "$path" ] && git -C "$path" rev-parse HEAD &>/dev/null; then
            commits=$(git -C "$path" log --oneline -n 8 --format="%h %s" 2>/dev/null | tr '\n' '|' | sed 's/|$//')
        fi

        # Check if worktree path exists and is accessible
        if [ -d "$path" ] && git -C "$path" rev-parse HEAD &>/dev/null; then
            status_flags=$(worktree_status_flags "$path")

            if [ "$branch" = "main" ]; then
                unpushed=$(worktree_unpushed_count "$path")
                status="${GREEN}✓${NC}${unpushed}${status_flags}${prunable}"
                echo -e "0\tmain\t${CYAN}main${NC}\t${status}\t${commits}"
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

        echo -e "1\t${branch}\t${branch}\t${status}\t${commits}"
    done
    } | sort -t$'\t' -k1,1 -k2,2 | awk -F '\t' -v term_width="$term_width" '
    BEGIN {
        dim = "\033[0;90m"
        nc = "\033[0m"
    }
    function vislen(s, t) {
        t = s
        gsub(/\x1B\[[0-9;]*m/, "", t)
        return length(t)
    }
    function truncate_to_width(s, max_width, vl, result, i, c, in_escape, visible) {
        vl = 0
        result = ""
        in_escape = 0
        for (i = 1; i <= length(s); i++) {
            c = substr(s, i, 1)
            if (c == "\033") {
                in_escape = 1
                result = result c
            } else if (in_escape) {
                result = result c
                if (c == "m") in_escape = 0
            } else {
                if (vl >= max_width) break
                result = result c
                vl++
            }
        }
        return result
    }
    {
        branches[NR] = $2
        labels[NR] = $3
        statuses[NR] = $4
        commits[NR] = $5
        if (vislen($2) > max_branch) max_branch = vislen($2)
        if (vislen($4) > max_status) max_status = vislen($4)
    }
    END {
        for (i = 1; i <= NR; i++) {
            pad = max_branch - vislen(labels[i])
            if (pad < 0) pad = 0

            # Build left side: branch + status
            left = sprintf("%s%*s  %s", labels[i], pad, "", statuses[i])
            left_vislen = vislen(left)

            # Calculate space for commits (leave 3 chars for separator)
            commits_width = term_width - left_vislen - 3
            if (commits_width < 20) commits_width = 0

            if (commits_width > 0 && commits[i] != "") {
                # Format commits: replace | with dimmed separator
                split(commits[i], commit_arr, "|")
                commit_str = ""
                for (j = 1; j <= length(commit_arr); j++) {
                    if (j > 1) commit_str = commit_str " · "
                    commit_str = commit_str commit_arr[j]
                    if (vislen(commit_str) > commits_width - 3) break
                }
                commit_str = truncate_to_width(commit_str, commits_width)
                printf "%s   %s%s%s\n", left, dim, commit_str, nc
            } else {
                printf "%s\n", left
            }
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
    local clear_to_eol
    local clear_to_eos
    clear_to_eol=$(tput el 2>/dev/null) || clear_to_eol=""
    clear_to_eos=$(tput ed 2>/dev/null) || clear_to_eos=""

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
        # Build complete output first (double-buffering to prevent flicker)
        local footer
        local status_output
        footer="${DIM}$(date '+%H:%M:%S') · ${REFRESH_INTERVAL}s · Ctrl+C${NC}"
        status_output=$(render_status)

        # Move cursor up to overwrite previous output
        if [ "$lines_printed" -gt 0 ]; then
            tput cuu "$lines_printed" 2>/dev/null || true
            tput cr 2>/dev/null || true
        fi

        # Print status lines with clear to end of line
        while IFS= read -r line; do
            echo -e "${line}${clear_to_eol}"
        done <<< "$status_output"
        # Footer line
        echo -e "${footer}${clear_to_eol}"
        # Clear any remaining lines from previous output
        echo -ne "${clear_to_eos}"

        # Count lines printed (status output + footer)
        lines_printed=$((1 + $(echo "$status_output" | wc -l)))

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
