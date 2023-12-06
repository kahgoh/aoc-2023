#!/bin/bash
FILENAME="input.data"

function get-power {
    IFS=";" ; read -ra entry <<< "$1"
    min_red=0
    min_green=0
    min_blue=0

    for pick in "${entry[@]}"; do
        # echo "  -- $pick"
        IFS="," ; read -ra chosen <<< "$pick"

        for selection in "${chosen[@]}"; do
            count=$(echo "$selection" | awk 'BEGIN { FS = " " } ; { print $1 }' -)
            colour=$(echo "$selection" | awk 'BEGIN { FS = " " } ; { print $2 }' -)
            # echo "Color is [$colour], count is [$count]"
            case $colour in
                "red")
                    if [[ $count -gt $min_red ]]; then
                        min_red=$count
                    fi
                    ;;
                "green")
                    if [[ $count -gt $min_green ]]; then
                        min_green=$count
                    fi
                    ;;
                "blue")
                    if [[ $count -gt $min_blue ]]; then
                        min_blue=$count
                    fi
                    ;;
                *)
                    # Nothing to do
            esac
        done
    done

    # Max return value from a Bash function is 255, so we have to echo it out
    # instead of using return
    echo $(( $min_red * $min_green * $min_blue ))
}

sum=0
while IFS= read -r line; do
    gameNum=$(echo "$line" | awk 'BEGIN { FS = ":" } ; { print $1 }' -)
    games=$(echo "$line" | awk 'BEGIN { FS = ":" } ; { print $2 }' -)

    gameNum=$(echo "$gameNum" | awk '{ print $2 }' -)

    echo "Game $gameNum:"
    # get-power "$games"
    power=$(get-power "$games")
    echo "  Power: $power"
    sum=$(( $sum + $power ))
done < "$FILENAME"

echo "Total sum: $sum"