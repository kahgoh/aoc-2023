Work={}

NORTH = 1
WEST = 2
SOUTH = 4
EAST = 8

function Work.new(row, column, direction) 
    return {
        Row=row,
        Column=column,
        Direction=direction
    }
end

function Work.forward(from)
    -- print("Forward from ", from.Row, ",", from.Column, " direction ", from.Direction)
    return Work.turn(from, from.Direction)
end

function Work.turn(from, direction) 
    if (direction == NORTH) then
        return {
            Row=from.Row - 1,
            Column=from.Column,
            Direction=direction
        }
    elseif (direction == WEST) then
        return {
            Row=from.Row,
            Column=from.Column - 1,
            Direction=direction
        }
    elseif (direction == SOUTH) then
        return {
            Row=from.Row + 1,
            Column=from.Column,
            Direction=direction
        }
    elseif (direction == EAST) then
        return {
            Row=from.Row,
            Column=from.Column + 1,
            Direction=direction
        }
    end
end