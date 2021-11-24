-module(ex4).
-export([start/0, print/1, stop/0]).

% starts the server registering the name of the process with the atom 'echo'
start() -> register(echo, spawn(fun() -> loop() end)).

% prints the submitted argument on the server
print(Term) -> echo ! {print, Term}, ok.

% stops the server
stop() -> echo ! stop, ok.

% server behaviour
loop() ->
  receive
    {print, M} -> io:format("~p~n", [M]), loop();
    stop -> void
  end.