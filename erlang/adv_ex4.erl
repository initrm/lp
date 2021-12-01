-module(adv_ex4).
-export([start/0, long_reversed_string/2]).

% starts a process to which has a service to reverse long strings
start() -> register(master, spawn(fun() -> master_behaviour() end)).

% reverses a long string using master-slaves pattern
long_reversed_string(S, N) -> rpc({reverse, S, N}).

% creates N slave processes which wait for a string and an index and returns the index with the string reversed
init_slave_processes(N) when N > 0 -> [spawn(fun() -> receive {Index, String} -> master ! {done, Index, string:reverse(String)} end end)|init_slave_processes(N-1)];
init_slave_processes(_) -> [].

% reverses the string thorugh slave processes
reverse(_, _, []) -> void;
reverse(S, D, [P|TL]) -> 
  P ! {length(TL), string:slice(S, length(TL) * trunc(ceil(string:length(S)/D)), trunc(ceil(string:length(S)/D)))},
  reverse(S, D, TL).

% merges the reversed strings from the processes
merge_results([{_, S}|[]]) -> S;
merge_results([{_, S}|TL]) -> S++merge_results(TL).

% rpc
rpc(Q) ->
  master ! {self(), Q},
  receive
    {master, Reply} -> Reply
  end.

% behaviour of the master process
master_behaviour() ->
  receive 
    {From, {reverse, String, Decompositions}} -> 
      % saves some data to be used in the recomposition
      put(decompositions, Decompositions), % number of decompositions
      put(results, []), % results array (reversed strings)
      put(from, From), % pid of the process wich needs the response
      % starts the reversing process
      reverse(String, Decompositions, init_slave_processes(Decompositions)),
      % loops
      master_behaviour();
    {done, Index, Content} ->
      put(results, [{Index, Content}|get(results)]),
      case get(decompositions) == length(get(results)) of
        % in case the decompositions are ended it merges the results and returns them
        true -> {get(from) ! {
          master, 
          merge_results(lists:sort(fun({IdxA, _}, {IdxB, _}) -> IdxA > IdxB end, get(results)))
        }};
        % loops
        false -> master_behaviour()
      end
  end.