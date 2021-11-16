-module(ex3).
-export([start/3]).

% start: int → int → string → void
% starts N processes, sends the Message to each process M times and gracefully shutdown every
% created process
start(M, N, Message) -> PL = init_processes(N), send_messages(Message, M, PL), gracefully_shutdown(PL).

% gracefully_shutdown: pid list -> void
% gracefully shutdown every process in the pid list (sending the 'quit' atom)
gracefully_shutdown([]) -> void;
gracefully_shutdown([HD|TL]) -> HD ! quit, gracefully_shutdown(TL).

% send_messages: string → int → pid list → void
% sends a message M times to all the processes into the pid list
send_messages(Message, M, PL) when M > 0 -> send_message(Message, PL), send_messages(Message, M-1, PL);
send_messages(_, _, _) -> void.

% send_message: string → pid list → atom 
% sends the message to all the processes in the list
send_message(_, []) -> void;
send_message(Message, [HD|TL]) -> HD ! Message, send_message(Message, TL).

% init_processes: int → pid list 
% initializes N processes and returns the list of the pids;
init_processes(N) when N > 0 -> [spawn(fun() -> process_behaviour() end)|init_processes(N-1)];
init_processes(_) -> [].

% process_behaviour
% behaviour of the spawned processes, prints the received message in loop
% gracefully exit when receive the atom 'quit'
process_behaviour() ->
    receive
        quit -> io:format("(PID: ~p) Gracefully shutting down.~n", [self()]);
        Message -> io:format("(PID: ~p) Received message: ~p.~n", [self(), Message]), process_behaviour()
    end.