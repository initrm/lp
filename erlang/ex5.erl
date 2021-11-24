-module(ex5).
-export([start/0, dummy1/0, dummy2/0, dummy3/0, tot/0]).

% starts the server registering the server process with the atom 'service_counter'
start() -> register('service_counter', spawn(fun() -> loop() end)).

% dummy functions
dummy1() -> rpc(dummy1).
dummy2() -> rpc(dummy2).
dummy3() -> rpc(dummy3).

% returns a list containing records about each service and the times it has been called
tot() -> rpc(tot).

% rpc
rpc(Q) ->
  'service_counter' ! {self(), Q},
  receive
    {'service_counter', Reply} -> Reply
  end.

% register a call to a service incrementing the count of times it has been called
register_service_call(Service) ->
  case get(Service) of
    undefined -> put(Service, 1);
    N -> put(Service, N+1)
  end.

% handle server behaviour
loop() ->
  receive
    {From, dummy1} -> register_service_call('dummy1'), io:format("Yo here it is dummy service one!~n", []), From ! {'service_counter', ok}, loop();
    {From, dummy2} -> register_service_call('dummy2'), io:format("Yo here it is dummy service two!~n", []), From ! {'service_counter', ok}, loop();
    {From, dummy3} -> register_service_call('dummy3'), io:format("Yo here it is dummy service three!~n", []), From ! {'service_counter', ok}, loop();
    {From, tot} -> From ! {'service_counter', get()}, loop()
  end.