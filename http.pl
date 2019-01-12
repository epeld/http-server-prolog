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
  parse_http_request(StreamPair, Method, URL, Version, _Headers),
  format("~s ~s ~s~n", [Method, URL, Version]),
  format(StreamPair, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).


parse_http_request(StreamPair, Method, URL, Version, Headers) :-
  format("Parsing..~n"),
  phrase_from_stream(http_request(Method, URL, Version, Headers), StreamPair), !,
  format("Done..~n").


http_request(Method, URL, Version, []) -->
  anything(Method), " ", anything(URL), " ", http_version(Version), cr, lf, rest.


cr --> [13].
lf --> [10].


http_version(Version) -->
  "HTTP/", version_number(Version).


version_number([N | Rest]) -->
  number_or_dot(N),
  version_number1(Rest).

version_number1([N | Rest]) -->
  number_or_dot(N),
  (version_number1(Rest) ; {Rest = []}).

number_or_dot(N) -->
  digit(N).

number_or_dot(N) -->
  ".",
  {
    [N] = "."
  }.

digit(N) -->
  [N],
  {
    member(N, "123456789.")
  }.

anything([R]) --> [R].
anything([R | Anything]) --> [R], anything(Anything).


rest(_,[]).
