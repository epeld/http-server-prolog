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
  parse_http_request(StreamPair, Method, URL, Version, Headers),
  format("~s ~s ~s ~w~n", [Method, URL, Version, Headers]),
  format(StreamPair, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).


parse_http_request(StreamPair, Method, URL, Version, Headers) :-
  phrase_from_stream(http_request(Method, URL, Version, Headers), StreamPair),
  !.


http_request(Method, URL, Version, Headers) -->
  anything(Method), " ", anything(URL), " ", http_version(Version), cr, lf, headers(Headers), rest.


cr --> [13].
lf --> [10].
sp --> [32].

crlf --> cr, lf.


headers([H | Headers]) -->
  header(H), crlf,
  headers(Headers).

headers([]) --> crlf.


header(Name : Value) -->
  header_name(CName), ":", sp, header_value(CValue),
  {
    %% string_codes(Name, CName),
    %% string_codes(Value, CValue)
    Name = CName,
    Value = CValue
  }.

header_name([A | Rest]) -->
  letter_or_dash(A),
  header_name1(Rest).

header_name1([A | Rest]) -->
  letter_or_dash(A),
  header_name1(Rest).

header_name1([A]) -->
  letter_or_dash(A).


header_value([H | Rest]) -->
  non_whitespace(H),
  header_value1(Rest).

header_value1([H | Rest]) -->
  non_whitespace(H),
  header_value1(Rest).

header_value1([]) --> [].

non_whitespace(C) -->
  [C],
  {
    between(33, 126, C)
  }.


http_version(Version) -->
  "HTTP/", version_number(Version).


version_number([N | Rest]) -->
  number_or_dot(N),
  version_number1(Rest).

version_number1([N | Rest]) -->
  number_or_dot(N),
  (version_number1(Rest) ; {Rest = []}).


letter_or_dash(A) --> letter(A) ; dash(A).

dash(A) --> "-", {[A] = "-"}.

letter(A) -->
  [A],
  {
    member(A, "abcdefghijklmnopqrstuvwxyz") ;
    member(A, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }.


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
