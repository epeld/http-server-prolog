
:- module(main, [main/0]).
:- use_module(library(socket)).

:- use_module(meta, [ignore_safe_print/1]).
:- use_module(http, [handle_connection/1]).

main :-
  tcp_socket(SocketId),
  tcp_bind(SocketId, localhost:8080),
  tcp_listen(SocketId, 5),
  thread_create(acceptor_main(SocketId), ThreadId, [alias(http_acceptor)]),
  format("Spawned thread ~w for handling incoming requests~n", [ThreadId]).

acceptor_main(SocketId) :-
  call_cleanup(
    accept_loop(SocketId),
    (
      ignore(tcp_close_socket(SocketId)),
      format("Server down~n")
    )
  ).

accept_loop(SocketId) :-
  tcp_accept(SocketId, Slave, Peer),
  format("Accepted ~w~n", [Peer]),
  ignore_safe_print(handle_connection(Slave)),
  accept_loop(SocketId).
