-module(line).
-export([start/1, init/2, send/2, stop/1]).

start(N) -> spawn(?MODULE, init, [0, N]).

init(N, Tot) ->
    if
        N == Tot - 1 ->
            loop(ultimo, N);
        N =/= Tot - 1 ->
            Next_pid = spawn(?MODULE, init, [N + 1, Tot]),
            loop(Next_pid, N)
    end.

loop(Next_pid, N) ->
    receive
        {send, Msg} ->
            io:format("~p received message ~p~n", [N, Msg]),
            case Next_pid of
                ultimo -> ok;
                _ -> Next_pid ! {send, Msg}
            end,
            loop(Next_pid, N);
        stop ->
            case Next_pid of
                ultimo -> ok;
                _ -> Next_pid ! stop
            end
    end.

send(Pid, Msg) ->
    Pid ! {send, Msg},
    ok.

stop(Pid) ->
    Pid ! stop.
