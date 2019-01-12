
:- module(http, [handle_connection/1]).
:- use_module(meta, [ignore_safe/1]).

:- set_prolog_flag(double_quotes, codes).

handle_connection(Socket) :-
  setup_call_cleanup(
    tcp_open_socket(Socket, StreamPair),
    handle_http_request(StreamPair),
    (
      ignore_safe(close(StreamPair)),
      ignore_safe(tcp_close_socket(Socket))
    )
  ).


handle_http_request(StreamPair) :-
  format("Parsing..~n"),
  phrase_from_stream(http_request(Method, URL), StreamPair), !,
  format("Done..~n"),
  format("~s ~s~n", [Method, URL]),
  format(StreamPair, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).


http_request(Method, URL) -->
  anything(Method), " ", anything(URL), " ", "HTTP/1.1", newline, rest.

newline --> [13, 10] ; [13] ; [10].

anything([R]) --> [R].
anything([R | Anything]) --> [R], anything(Anything).

rest(_,[]).
