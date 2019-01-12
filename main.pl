
main :-
  tcp_socket(Socket),
  tcp_bind(Socket, localhost:8080),
  tcp_listen(Socket, 5),
  call_cleanup(
    accept_loop(Socket),
    tcp_close_socket(Socket)
  ).


accept_loop(Socket) :-
  tcp_accept(Socket, Slave, Peer),
  format("Accepted ~w~n", [Peer]),
  tcp_close_socket(Slave),
  accept_loop(Socket).
