-module(part1).
-export([solve/1]).

solve(File_name)->
    Points = puzzle:read(File_name),
    io:format("Points : ~w~n", [Points]),
    Expanded = expand(Points),
    sum_distance(Expanded, 0).

expand(Points)->
    New_points = double_rows(Points, []),
    io:format("Row * 2: ~w~n", [New_points]),
    double_cols(lists:sort(New_points), []).

sum_distance([_Last], Accumulator) -> Accumulator;
sum_distance([Next | Remaining], Accumulator) ->
    sum_distance(Remaining, add_distance(Next, Remaining, Accumulator)).

add_distance(_From, [], Accumulator) -> Accumulator;
add_distance({X1, Y1}, [{X2, Y2} | Remaining], Accumulator) ->
    add_distance({X1, Y1}, Remaining, Accumulator + abs(X2 - X1) + abs(Y2 - Y1)).

% Go through the points and double the distance in between the two
double_rows([Last], Accumulator) -> lists:reverse([Last] ++ Accumulator);
double_rows([{X1, Y1}, {X2, Y2} | Remaining], Accumulator) when Y1 == Y2 ->
    double_rows([{X2, Y2}] ++ Remaining, [{X1, Y1}] ++ Accumulator);
double_rows([{X1, Y1}, {X2, Y2} | Remaining], Accumulator) ->
    Spacing = Y2 - Y1 - 1,
    New_Remaining = inc_y([{X2, Y2}] ++ Remaining, Spacing, []),
    double_rows(New_Remaining, [{X1, Y1}] ++ Accumulator).

inc_y([], _Amount, Accumulator) ->
    lists:reverse(Accumulator);
inc_y([{X, Y} | Remaining], Amount, Accumulator) ->
    inc_y(Remaining, Amount, [{X, Y + Amount}] ++ Accumulator).

double_cols([Last], Accumulator) -> lists:reverse([Last] ++ Accumulator);
double_cols([{X1, Y1}, {X2, Y2} | Remaining], Accumulator) when X1 == X2 ->
    double_cols([{X2, Y2}] ++ Remaining, [{X1, Y1}] ++ Accumulator);
double_cols([{X1, Y1}, {X2, Y2} | Remaining], Accumulator) ->
    Spacing = X2 - X1 - 1,
    New_Remaining = inc_x([{X2, Y2}] ++ Remaining, Spacing, []),
    double_cols(New_Remaining, [{X1, Y1}] ++ Accumulator).

inc_x([], _Amount, Accumulator) ->
    lists:reverse(Accumulator);
inc_x([{X, Y} | Remaining], Amount, Accumulator) ->
    inc_x(Remaining, Amount, [{X + Amount, Y}] ++ Accumulator).