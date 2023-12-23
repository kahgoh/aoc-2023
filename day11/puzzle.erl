-module(puzzle).
-export([read/1]).

% Reads the data from the file.
read(File_name)->
    {ok, Handle} = file:open(File_name, [read]),
    collect(Handle).

% Collect points from file
collect(File_handle)->
    Result = do_collect(File_handle, file:read_line(File_handle), 0, []),
    file:close(File_handle),
    Result.

do_collect(_File_handle, eof, _Row, Accumulator)->
    lists:reverse(Accumulator);
do_collect(File_handle, {ok, Line}, Row, Accumulator)->
    Next_acc = collect_row(Line, {0, Row}, Accumulator),
    do_collect(File_handle, file:read_line(File_handle), Row + 1, Next_acc).

% Collect points from row
collect_row([], {_Col, _Row}, Accumulator)->
    Accumulator;
collect_row([Next | Remaining], {Col, Row}, Accumulator)->
    if Next == $# ->
        collect_row(Remaining, {Col + 1, Row},  [{Col, Row}] ++ Accumulator);
    true ->
        collect_row(Remaining, {Col + 1, Row},  Accumulator)
    end.
