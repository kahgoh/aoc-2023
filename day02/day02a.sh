#!/bin/bash
FILENAME="input.data"

function filter-entry {
    IFS=";" ; read -ra entry <<< "$1"
    for pick in "${entry[@]}"; do
        echo "  -- $pick"
        IFS="," ; read -ra chosen <<< "$pick"
        for selection in "${chosen[@]}"; do
            count=$(echo "$selection" | awk 'BEGIN { FS = " " } ; { print $1 }' -)
            colour=$(echo "$selection" | awk 'BEGIN { FS = " " } ; { print $2 }' -)
            # echo "Color is [$colour], count is [$count]"
            case $colour in
                "red")
                    max=12
                    ;;
                "green")
                    max=13
                    ;;
                "blue")
                    max=14
                    ;;
                *)
                    max=-1
            esac
            
            if [[ "$max" -gt 0 ]] && [[ "$count" -gt "$max" ]]; then
                return 1
            fi
        done
    done
    return 0
}

sum=0
while IFS= read -r line; do
    gameNum=$(echo "$line" | awk 'BEGIN { FS = ":" } ; { print $1 }' -)
    games=$(echo "$line" | awk 'BEGIN { FS = ":" } ; { print $2 }' -)

    gameNum=$(echo "$gameNum" | awk '{ print $2 }' -)

    echo "Game $gameNum:"

    if filter-entry "$games"; then
        echo "Accept! [$gameNum]"
        sum=$(( $sum + $gameNum ))
    else
        echo "Reject!"
    fi
done < "$FILENAME"

echo "Total sum: $sum"