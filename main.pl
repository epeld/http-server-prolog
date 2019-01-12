
main :-
  tcp_socket(SocketId),
  tcp_bind(SocketId, localhost:8080),
  tcp_listen(SocketId, 5),
  call_cleanup(
    accept_loop(SocketId),
    tcp_close_socket(SocketId)
  ).


accept_loop(SocketId) :-
  tcp_accept(SocketId, Slave, Peer),
  format("Accepted ~w~n", [Peer]),
  ignore(
    catch(
      handle_connection(Slave),
      _Err,
      true
    )
  ),
  accept_loop(SocketId)
.
