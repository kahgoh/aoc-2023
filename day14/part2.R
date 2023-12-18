data=scan("input.data", what=character())
width=nchar(data[1])
height=length(data)
map=matrix(nrow=height, ncol=width, byrow=TRUE)

row = 1
for (line in data) {
    for (col in 1:width) {
        map[row,col]<-substring(line, col, col)
    }
    row = row + 1
}

move_north<-function(map) {
    for (col in 1:dim(map)[2]) {
        next_row = 1
        for (row in 1:dim(map)[1]) {
            if (map[row, col] == "#") {
                next_row = row + 1
            } else if (map[row, col] == "O") {
                if (next_row < row) {
                    map[next_row, col] ="O"
                    map[row, col] ="."
                }
                next_row = next_row + 1
            }
        }
    }
    return(map)
}

move_west<-function(map) {
    for (row in 1:dim(map)[1]) {
        next_col = 1

        for (col in 1:dim(map)[2]) {
            if (map[row, col] == "#") {
                next_col = col + 1
            } else if (map[row, col] == "O") {
                if (next_col < col) {
                    map[row, next_col] ="O"
                    map[row, col] ="."
                }
                next_col = next_col + 1
            }
        }
    }
    return(map)
}

move_south<-function(map) {
    for (col in 1:dim(map)[2]) {
        next_row = dim(map)[1]
        for (row in dim(map)[1]:1) {
            if (map[row, col] == "#") {
                next_row = row - 1
            } else if (map[row, col] == "O") {
                if (next_row > row) {
                    map[next_row, col] ="O"
                    map[row, col] ="."
                }
                next_row = next_row - 1
            }
        }
    }
    return(map)
}

move_east<-function(map) {
    for (row in 1:dim(map)[1]) {
        next_col = dim(map)[2]

        for (col in dim(map)[2]:1) {
            if (map[row, col] == "#") {
                next_col = col - 1
            } else if (map[row, col] == "O") {
                if (next_col > col) {
                    map[row, next_col] ="O"
                    map[row, col] ="."
                }
                next_col = next_col - 1
            }
        }
    }
    return(map)
}

cycle<-function(map) {
    return(move_east(move_south(move_west(move_north(map)))))
}

score<-function(map) {
    scores=c(1:width)
    scores[1:width]=0

    for (row in 1:height) {
        for (col in 1:width) {
            if (map[row, col] == "O") {
                scores[col] = scores[col] + height - row + 1
            }
        }
    }
    return(sum(scores))
}

key<-function(map) {
    key=""

    for (row in 1:dim(map)[1]) {
        for (col in 1:dim(map)[2]) {
            if (map[row, col] == "O") {
                key=paste(key, sprintf("%d,%d", row, col))
            }
        }
    }
    return(key)
}

seen<-list()
scores<-list()
start_key<-key(map)
seen[[start_key]]<-0

for (cycle_count in 1:1000000000) {
    map<-cycle(map)
    current_key<-key(map)
    scores[[cycle_count]]<-score(map)
    print(sprintf("Cycle %d: %d", cycle_count, scores[[cycle_count]]))
    if (!is.null(seen[[current_key]])) {
        start_cycle=seen[[current_key]]
        period=cycle_count - seen[[current_key]]
        cat("Repeat from ", start_cycle, " period is ", period, "\n")
        match=start_cycle + ((1000000000 - start_cycle) %% period)
        cat("Corresponding index, ", match, " score is ", scores[[match]], "\n")
        break;
    } else {
        seen[[current_key]] <- cycle_count
    }
}
