-module(ex4).
-export([start/0, print/1, stop/0]).

start() -> register(echo, spawn(fun() -> loop() end)).

print(Term) -> echo ! {print, Term}, ok.

stop() -> echo ! stop, ok.

loop() ->
    receive
        {print, M} -> io:format("~p~n", [M]), loop();
        stop -> void
    end.