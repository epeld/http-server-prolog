
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
  stream_pair(StreamPair, In, _Out), tcp_fcntl(In, setfl, nonblock),
  phrase_from_stream(http_request(Anything), StreamPair), !,
  format("Done..~n"),
  string_codes(String, Anything),
  format("~w~n", [String]),
  format(StreamPair, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).


http_request(Text) -->
  anything(Text), [13], rest.

anything([R]) --> [R].
anything([R | Anything]) --> [R], anything(Anything).

rest(A,A).
