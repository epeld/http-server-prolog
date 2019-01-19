:- module(http, [handle_connection/1]).
:- use_module(meta, [ignore_safe/1]).
:- use_module(request_handler, [handle_request/6]).

:- use_module(library(http/json), [json_read/2]).
:- use_module(library(memfile), [new_memory_file/1, open_memory_file/3]).

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
  parse_http_request(StreamPair, Method, URL, Version, Headers, Body),
  stream_pair(StreamPair, _In, Out),
  format("Dispatching ~s ~s~n", [Method, URL]),
  handle_request(Method, URL, Version, Headers, Body, Out).


parse_http_request(StreamPair, Method, URL, Version, Headers, Body) :-
  parse_http_url_line(StreamPair, Method, URL, Version),
  parse_headers(StreamPair, Headers),
  parse_body(StreamPair, Headers, Body).


parse_http_url_line(Stream, Method, URL, Version) :-
  read_line_to_codes(Stream, Line),
  phrase(
    (
      anything(Method), " ", anything(URL), " ", http_version(Version)
    ),
    Line
  ).

parse_headers(Stream, Headers) :-
  read_line_to_codes(Stream, Line),
  headers_line(Stream, Line, Headers).

headers_line(_Stream, [], []).
headers_line(Stream, Line, [H | Rest]) :-
  phrase(header(H), Line),
  parse_headers(Stream, Rest).

%
% Body-Parsing
%

parse_body(Stream, Headers, Body) :-
  message_length(Headers, Length),
  !,
  parse_body_length(Stream, Length, Headers, Body).

parse_body(_, _Headers, nothing) :-
  /* \+ message_length(Headers, _) */ true.

parse_body_length(Stream, Length, Headers, Body) :-
  % We now set up a buffer and read the full body into it before continuing
  format("Body contains ~w bytes~n", [Length]),
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      setup_call_cleanup(
        open_memory_file(Handle, write, BufferOut),
        copy_stream_data(Stream, BufferOut, Length),
        close(BufferOut)
      ),
      setup_call_cleanup(
        open_memory_file(Handle, read, BufferIn),
        parse_contents(BufferIn, Headers, Body),
        close(BufferIn)
      )
    ),
    free_memory_file(Handle)
  ).

print_memory_file_info(Handle) :-
  size_memory_file(Handle, Size),
  format("Buffered ~w bytes~n", [Size]),
  memory_file_to_codes(Handle, Codes),
  format("Body buffer now contains: '~w'~n", [Codes]).

parse_contents(Stream, Headers, Body) :-
  member("content-type" : "application/json", Headers),
  format("Parsing JSON..~n"),
  json_read(Stream, Body).

message_length(Headers, Length) :-
  member("message-length" : Len, Headers),
  !,
  number_codes(Length, Len).

message_length(Headers, Length) :-
  member("content-length" : Len, Headers),
  number_codes(Length, Len).

%
% DCG
%

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
    downcase_codes(CName, Name),
    downcase_codes(CValue, Value)
    %% Name = CName,
    %% Value = CValue
  }.

downcase_codes(In, Out) :-
  atom_codes(Atom, In),
  downcase_atom(Atom, OutAtom),
  atom_codes(OutAtom, Out).

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
