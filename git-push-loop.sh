#!/bin/bash

# Git push loop script
# Tries to push every 30 seconds

while true; do
    # Check if there are commits to push
    LOCAL=$(git rev-parse @)
    REMOTE=$(git rev-parse @{u} 2>/dev/null)

    if [ "$LOCAL" != "$REMOTE" ]; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Commits to push detected, pushing..."
        if git push 2>&1; then
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Push succeeded"
        else
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Push FAILED"
        fi
    fi

    sleep 30
done
