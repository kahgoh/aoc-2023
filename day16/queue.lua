Queue = {}
function Queue.new() 
    return {first = 0, last = 0}
end

function Queue.enqueue(queue, value)
    local next = queue.last + 1
    queue[next] = value
    if (queue.first == 0) then
        queue.first = next
    end
    queue.last = next
    return queue
end

function Queue.dequeue(queue)
    local value = queue[queue.first]
    local next = queue.first + 1
    queue[queue.first] = nil
    if (next > queue.last) then
        queue.first = 0
        queue.last = 0
    else
        queue.first = next
    end
    return value
end

function Queue.is_empty(queue)
    return queue.first == 0
end