require("./queue")
require("./work")

local map = {}
local row = 1
for line in io.lines("input.data") do
    map[row] = {}

    for col = 1, string.len(line) do
        map[row][col] = string.sub(line, col, col)
    end
    row = row + 1
end

-- Print copy of map
print("Map")
for row = 1, #map do
    for col = 1, #(map[row]) do
        io.write(map[row][col])
    end
    print("")
end

-- Walking functions
function Walk(map, queue)
    local coverage = {}

    for row = 1, #map do
        coverage[row] = {}
        for col = 1, #map[row] do
            coverage[row][col] = 0
        end
    end

    while not Queue.is_empty(queue) do
        local step = Queue.dequeue(queue)
        if (step.Row >= 1 and step.Column >= 1 and
            step.Row <= #map and step.Column <= #map[step.Row] and
            step.Direction & coverage[step.Row][step.Column] == 0) then
            coverage[step.Row][step.Column] = coverage[step.Row][step.Column] | step.Direction

            local symbol = map[step.Row][step.Column]

            if (step.Direction == NORTH) then
                if (symbol == "/") then
                    Queue.enqueue(queue, Work.turn(step, EAST))
                elseif (symbol == "\\") then
                    Queue.enqueue(queue, Work.turn(step, WEST))
                elseif (symbol == "-") then
                    Queue.enqueue(queue, Work.turn(step, WEST))
                    Queue.enqueue(queue, Work.turn(step, EAST))
                else
                    Queue.enqueue(queue, Work.forward(step))
                end
            elseif (step.Direction == WEST) then
                if (symbol == "/") then
                    Queue.enqueue(queue, Work.turn(step, SOUTH))
                elseif (symbol == "\\") then
                    Queue.enqueue(queue, Work.turn(step, NORTH))
                elseif (symbol == "|") then
                    Queue.enqueue(queue, Work.turn(step, SOUTH))
                    Queue.enqueue(queue, Work.turn(step, NORTH))
                else
                    Queue.enqueue(queue, Work.forward(step))
                end
            elseif (step.Direction == SOUTH) then
                if (symbol == "/") then
                    Queue.enqueue(queue, Work.turn(step, WEST))
                elseif (symbol == "\\") then
                    Queue.enqueue(queue, Work.turn(step, EAST))
                elseif (symbol == "-") then
                    Queue.enqueue(queue, Work.turn(step, WEST))
                    Queue.enqueue(queue, Work.turn(step, EAST))
                else
                    Queue.enqueue(queue, Work.forward(step))
                end
            elseif (step.Direction == EAST) then
                if (symbol == "/") then
                    Queue.enqueue(queue, Work.turn(step, NORTH))
                elseif (symbol == "\\") then
                    Queue.enqueue(queue, Work.turn(step, SOUTH))
                elseif (symbol == "|") then
                    Queue.enqueue(queue, Work.turn(step, SOUTH))
                    Queue.enqueue(queue, Work.turn(step, NORTH))
                else
                    Queue.enqueue(queue, Work.forward(step))
                end
            end
        end
    end
    return coverage
end

-- Score the map
function Score(map, start_row, start_column, direction)
    local queue = Queue.new()
    Queue.enqueue(queue, Work.new(start_row, start_column, direction))

    local coverage = Walk(map, queue)
    local count = 0
    for row = 1, #coverage do
        for col = 1, #(coverage[row]) do
            if (coverage[row][col] > 0) then
                count = count + 1
            end
        end
    end
    return count
end

local max = 0
for row = 1, #map do
    local left_score = Score(map, row, 1, EAST)
    local right_score = Score(map, row, #map[row], WEST)
    if (left_score > max) then
        max = left_score
    end
    if (right_score > max) then
        max = right_score
    end
end
for col = 1, #map[1] do
    local top_score = Score(map, 1, col, SOUTH)
    local bottom_score = Score(map, #map, col, NORTH)
    if (top_score > max) then
        max = top_score
    end
    if (bottom_score > max) then
        max = bottom_score
    end
end
io.write("Count: ", max, "\n")