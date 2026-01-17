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
    local staged=" "
    local unstaged=" "
    local untracked=" "
    local porcelain=""

    porcelain=$(git -C "$path" status --porcelain=1 2>/dev/null || true)

    if [ -n "$porcelain" ]; then
        if echo "$porcelain" | grep -q '^[MADRCU]'; then
            staged="${YELLOW}s${NC}"
        fi

        if echo "$porcelain" | grep -q '^.[MADRCU]'; then
            unstaged="${YELLOW}u${NC}"
        fi

        if echo "$porcelain" | grep -q '^\?\?'; then
            untracked="${YELLOW}?${NC}"
        fi
    fi

    echo "${staged}${unstaged}${untracked}"
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
    local repo_root=""
    local log_lines=""

    repo_root=$(git rev-parse --show-toplevel 2>/dev/null || true)
    if [ -n "$repo_root" ]; then
        log_lines=$(git -C "$repo_root" log -8 --pretty=format:'%h %s' 2>/dev/null | awk '{
            hash = $1
            $1 = ""
            sub(/^ /, "")
            subject = $0
            if (length(subject) > 62) subject = substr(subject, 1, 62)
            if (subject != "") {
                print hash " " subject
            } else {
                print hash
            }
        }')
    fi

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
                status="${GREEN}✓${NC}${unpushed}${prunable}"
                echo -e "0\tmain\t${CYAN}main${NC}\t${status_flags}\t${status}"
                continue
            fi

        ahead=$(git -C "$path" rev-list --count main..HEAD 2>/dev/null || echo "?")
        behind=$(git -C "$path" rev-list --count HEAD..main 2>/dev/null || echo "?")

        if [ "$ahead" = "0" ] && [ "$behind" = "0" ]; then
            status="${GREEN}✓ merged${NC}${prunable}"
        elif [ "$ahead" = "0" ]; then
            status="${YELLOW}↓ ${behind} behind${NC}${prunable}"
        elif [ "$behind" = "0" ]; then
            status="${RED}↑ ${ahead} ahead${NC}${prunable}"
        else
            status="${RED}↑ ${ahead} ahead, ↓ ${behind} behind${NC}${prunable}"
        fi
        else
            status="${DIM}(inaccessible)${NC}${prunable}"
            status_flags="   "
        fi

        echo -e "1\t${branch}\t${branch}\t${status_flags}\t${status}"
    done
    } | sort -t$'\t' -k1,1 -k2,2 | awk -F '\t' -v dim="$DIM" -v nc="$NC" -v context="$context" -v log_lines="$log_lines" '
function vislen(s, t) {
    t = s
    gsub(/\x1B\[[0-9;]*m/, "", t)
    gsub(/↑/, "^", t)
    gsub(/↓/, "v", t)
    gsub(/✓/, "v", t)
    return length(t)
}
BEGIN {
    log_count = split(log_lines, log_array, "\n")
    if (log_count > 0 && log_array[log_count] == "") {
        log_count--
    }
}
{
    branches[NR] = $2
    labels[NR] = $3
    bits[NR] = $4
    statuses[NR] = $5
    if (vislen($2) > max_branch) max_branch = vislen($2)
    if (vislen($5) > max_status) max_status = vislen($5)
}
END {
    max_rows = 8
    rows = (NR > max_rows) ? NR : max_rows
    for (i = 1; i <= rows; i++) {
        label = (i <= NR) ? labels[i] : ""
        bit = (i <= NR) ? bits[i] : "   "
        status = (i <= NR) ? statuses[i] : ""
        log_line = (i <= log_count) ? log_array[i] : ""
        if (log_line != "") {
            log_display = dim log_line nc
        } else {
            log_display = ""
        }
        pad_branch = max_branch - vislen(label)
        if (pad_branch < 0) pad_branch = 0
        pad_status = max_status - vislen(status)
        if (pad_status < 0) pad_status = 0
        printf "%s%*s  %s  %s%*s  %s\n", label, pad_branch, "", bit, status, pad_status, "", log_display
    }
    context_trim = context
    if (vislen(context_trim) > max_branch) {
        context_trim = substr(context_trim, 1, max_branch)
    }
    printf "%s%-*s%s\n", dim, max_branch, context_trim, nc
}
'
}

cleanup() {
    tput rmcup 2>/dev/null || true
    tput cnorm 2>/dev/null || true
}

trap cleanup EXIT
tput smcup 2>/dev/null || true
tput civis 2>/dev/null || true

while true; do
    context="updated $(date +'%H:%M:%S')"
    output="$(render_status)"
    printf "\033[H\033[J%s\n" "$output"
    if read -rs -n 1 -t 2 key; then
        if [ "$key" = "q" ] || [ "$key" = $'\e' ]; then
            break
        fi
    fi
done
