-module(part2).
-export([solve/1]).

solve(File_name)->
    Points = puzzle:read(File_name),
    io:format("Points : ~w~n", [Points]),
    X_scale = x_points(Points),
    Y_scale = y_points(Points),
    sum_distance(Points, X_scale, Y_scale, 0).

x_points(Points)->
    lists:sort(lists:uniq(lists:map(fun ({X, _Y}) -> X end, Points))).

y_points(Points)->
    lists:sort(lists:uniq(lists:map(fun ({_X, Y}) -> Y end, Points))).

sum_distance([_Last], _X_scale, _Y_scale, Accumulator) -> Accumulator;
sum_distance([Next | Remaining], X_scale, Y_scale, Accumulator) ->
    sum_distance(Remaining, X_scale, Y_scale, add_distance(Next, Remaining, X_scale, Y_scale, Accumulator)).

add_distance(_From, [], _X_scale, _Y_scale, Accumulator) -> Accumulator;
add_distance({X1, Y1}, [{X2, Y2} | Remaining], X_scale, Y_scale, Accumulator) ->
    add_distance({X1, Y1}, Remaining, X_scale, Y_scale, Accumulator + measure({X1, Y1}, {X2, Y2}, X_scale, Y_scale)).

measure({X1, Y1}, {X2, Y2}, X_scale, Y_scale) ->
    measure_axis(min(X2, X1), max(X2, X1), X_scale) + measure_axis(min(Y2, Y1), max(Y2, Y1), Y_scale).

measure_axis(From, To, _Scale) when From == To -> 0;
measure_axis(From, To, Scale) -> 
    do_measure_axis(From + 1, To, Scale, 0).

do_measure_axis(From, To, _Scale, Accumulator) when From == To -> Accumulator + 1;
do_measure_axis(From, To, Scale, Accumulator) ->
    case lists:member(From, Scale) of
        true -> do_measure_axis(From + 1, To, Scale, 1 + Accumulator);
        _Else -> do_measure_axis(From + 1, To, Scale, 1000000 + Accumulator)
    end.

