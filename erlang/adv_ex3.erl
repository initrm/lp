-module(adv_ex3).
-export([start/1, to_slave/2]).

% starts the master process and its N slaves
start(N) -> init_master_process(N).

% initializes the master process registering its pid with the atom master
init_master_process(Slaves) -> register(master, spawn(fun() -> master_behaviour(Slaves) end)).

% basic master behaviour which is executed once
master_behaviour(N) -> 
  process_flag(trap_exit, true), % system process, do not exit on exit signals
  init_slave_processes(N), % initializes slave processes
  io:format("(Master Process) Initialization success, slave processes: ~n~p.~n", [get(slaves)]),
  master_loop(). % starts the master loop

% master loop behaviour which receives a message and relays it to the slaves
% it also handles exit signals from the slaves and restarts them
master_loop() -> 
  receive
    % a slave process has exited
    {'EXIT', Pid, Why} -> 
      io:format("(Master Process) Process ~p has died (reason: ~p), restarting it...~n", [Pid, Why]), 
      % save a new list of slave processes pid removing the pid of the process which exited
      % and adding a new slave pid
      put(slaves, [spawn_link(fun() -> slave_loop() end)|[X || X <- get(slaves), not(X == Pid)]]),
      io:format("(Master Process) Process has been restarted (new pid: ~p).~n", [lists:nth(1, get(slaves))]),
      master_loop();
    % relay the message to the slave N
    {M, N} ->
      io:format("(Master Process) Received message: ~p, sending it to slave n°~p.~n", [M, N]),
      lists:nth(N, get(slaves)) ! M,
      master_loop()
  end.

% spawns and link to the caller process a number of slave processes and saves their pids
% in the process dictionary under the key 'slaves'
init_slave_processes(N) when N > 0 ->
  S = get(slaves),
  case S of
    undefined -> put(slaves, [spawn_link(fun() -> slave_loop() end)]), init_slave_processes(N-1);
    _ -> put(slaves, [spawn_link(fun() -> slave_loop() end)|S]), init_slave_processes(N-1)
  end;
init_slave_processes(_) -> ok.

% slave behaviour
slave_loop() -> 
  receive
    % on die atom received it exits sending an exit signal to the master process
    % in reality, it broadcast a message to each process linked to him, in this case, only master
    die -> exit(self(), "Received 'die' message.");
    % on any message received different from die atom it prints it
    M -> io:format("(Slave Process: ~p) ~p~n", [self(), M]), slave_loop()
  end.

% sends a message to the master and tells it to relay the message to slave N 
to_slave(M, N) -> master ! {M, N}.