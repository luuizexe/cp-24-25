-module(stack).

-export([empty/0, push/2, pop/1, peek/1]).

empty() ->
    [].

push(Stack, Elem) ->
    [Elem | Stack].

pop([]) -> [];
pop([_ | T]) -> T.

peek([]) -> empty;
peek([H | _]) -> {ok, H}.
