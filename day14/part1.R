map=scan("input.data", what=character())
width=nchar(map[1])
height=length(map)

scores=c(1:width)
potential=c(1:width)

scores[1:width]=0
potential[1:width]=height

row = height
for (line in map) {
    for (index in 1:width) {
        char=substring(line, index, index)
        if (char == "O") {
            scores[index] = scores[index] + potential[index]
            potential[index] = potential[index] - 1
        } else if (char == "#") {
            potential[index] = row - 1
        }
    }
    row = row - 1
}

cat("Scores:     ", scores, "\n")
cat("Total : ", sum(scores), "\n")