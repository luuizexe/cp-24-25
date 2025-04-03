-module(ring).
-export([start/1, init/2, send/3, stop/1]).

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
        {send, From, Rings, Msg} ->
            if
                Rings =/= 0 ->
                    io:format("~p received message ~p with ~p left~n", [N, Msg, Rings - 1]),
                    case Next_pid of
                        ultimo -> From ! {send, From, Rings - 1, Msg};
                        _ -> Next_pid ! {send, From, Rings - 1, Msg}
                    end,
                    loop(Next_pid, N);
                Rings == 0 ->
                    loop(Next_pid, N),
                    ok
            end;
        stop ->
            case Next_pid of
                ultimo -> ok;
                _ -> Next_pid ! stop
            end
    end.

send(Pid, N, Msg) ->
    Pid ! {send, Pid, N, Msg},
    ok.

stop(Pid) ->
    Pid ! stop.
