
:- module(http, [handle_connection/1]).

handle_connection(Socket) :-
  setup_call_cleanup(
    tcp_open_socket(Socket, StreamPair),
    handle_http_request(StreamPair),
    (
      ignore(close(StreamPair)),
      ignore(tcp_close_socket(Socket))
    )
  ).


handle_http_request(StreamPair) :-
  format(StreamPair, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).
