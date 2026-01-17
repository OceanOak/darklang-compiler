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
            staged="${CYAN}s${NC}"
        fi

        if echo "$porcelain" | grep -q '^.[MADRCU]'; then
            unstaged="${CYAN}u${NC}"
        fi

        if echo "$porcelain" | grep -q '^\?\?'; then
            untracked="${CYAN}?${NC}"
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

format_count() {
    local count="$1"

    if [[ "$count" =~ ^[0-9]+$ ]]; then
        printf "%02d" "$count"
    else
        printf "%s" "$count"
    fi
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
            print hash "\t" subject
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

        display_branch="$branch"
        if [[ "$display_branch" == compiler-for-dark-* ]]; then
            display_branch="${display_branch#compiler-for-dark-}"
        elif [[ "$display_branch" == compiler-for-dark* ]]; then
            display_branch="${display_branch#compiler-for-dark}"
            display_branch="${display_branch#-}"
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
        ahead_fmt=$(format_count "$ahead")
        behind_fmt=$(format_count "$behind")

        if [ "$ahead" = "0" ] && [ "$behind" = "0" ]; then
            status="${GREEN}✓${NC}${prunable}"
        elif [ "$ahead" = "0" ]; then
            status="${YELLOW}↓${behind_fmt}${NC}${prunable}"
        elif [ "$behind" = "0" ]; then
            status="${RED}↑${ahead_fmt}${NC}${prunable}"
        else
            status="${RED}↑${ahead_fmt} ↓${behind_fmt}${NC}${prunable}"
        fi
        else
            status="${DIM}(inaccessible)${NC}${prunable}"
            status_flags="   "
        fi

        echo -e "1\t${branch}\t${display_branch}\t${status_flags}\t${status}"
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
    max_visible = (NR > (max_rows - 1)) ? (max_rows - 1) : NR
    rows = max_rows
    max_branch_limit = 8
    if (max_branch > max_branch_limit) max_branch = max_branch_limit
    log_index = 0
    for (i = 1; i <= rows; i++) {
        if (i == max_visible + 1) {
            log_index++
            log_line = (log_index <= log_count) ? log_array[log_index] : ""
            if (log_line != "") {
                split(log_line, log_parts, "\t")
                log_hash = log_parts[1]
                log_subject = log_parts[2]
                if (log_subject != "") {
                    log_display = dim log_hash nc " " log_subject
                } else {
                    log_display = dim log_hash nc
                }
            } else {
                log_display = ""
            }
            pad_context = max_branch - vislen(context)
            if (pad_context < 0) pad_context = 0
            printf "%s%s%s%*s   %s  %*s  %s\n", dim, context, nc, pad_context, "", "   ", max_status, "", log_display
            continue
        }

        data_index = (i > max_visible + 1) ? i - 1 : i
        label = (data_index <= max_visible) ? labels[data_index] : ""
        if (label !~ /\x1B/ && vislen(label) > max_branch) {
            label = substr(label, 1, max_branch)
        }
        bit = (data_index <= max_visible) ? bits[data_index] : "   "
        status = (data_index <= max_visible) ? statuses[data_index] : ""
        log_index++
        log_line = (log_index <= log_count) ? log_array[log_index] : ""
        if (log_line != "") {
            split(log_line, log_parts, "\t")
            log_hash = log_parts[1]
            log_subject = log_parts[2]
            if (log_subject != "") {
                log_display = dim log_hash nc " " log_subject
            } else {
                log_display = dim log_hash nc
            }
        } else {
            log_display = ""
        }
        pad_branch = max_branch - vislen(label)
        if (pad_branch < 0) pad_branch = 0
        pad_status = max_status - vislen(status)
        if (pad_status < 0) pad_status = 0
        printf "%s%*s   %s  %s%*s  %s\n", label, pad_branch, "", bit, status, pad_status, "", log_display
    }
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
    context="$(date +'%H:%M:%S')"
    output="$(render_status)"
    printf "\033[H\033[J%s\n" "$output"
    if read -rs -n 1 -t 2 key; then
        if [ "$key" = "q" ] || [ "$key" = $'\e' ]; then
            break
        fi
    fi
done
