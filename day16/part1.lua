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

-- Work queue
local queue = Queue.new()
Queue.enqueue(queue, Work.new(1, 1, EAST))
local coverage = Walk(map, queue)
io.write("Coverage ", #coverage, ", ", #coverage[1], "\n")
local count = 0
for row = 1, #coverage do
    for col = 1, #(coverage[row]) do
        io.write(coverage[row][col])
        if (coverage[row][col] > 0) then
            count = count + 1
        end
    end
    print("")
end
io.write("Count: ", count, "\n")