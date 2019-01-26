:- module(request_handler, [handle_request/3]).
:- set_prolog_flag(double_quotes, string).
:- multifile
    handle_request/6.


handle_request(get, "/", _Context) :-
  format("HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).



handle_request(get, "/debug", Context) :-
  member(version(Version), Context),
  member(body(Body), Context),
  member(headers(Headers), Context),
  with_output_to(
    codes(ResponseBody),
    output_debug_response(Version, Headers, Body)
  ),
  length(ResponseBody, Length),
  format("HTTP/1.1 200 OK~nContent-Length: ~w~n~n~s", [Length, ResponseBody]).


output_debug_response(Version, Headers, Body) :-
  format("Debug info:~n--------------------~n"),
  format("HTTP Version ~s~n", [Version]),
  forall(
    member(Name : Value, Headers),
    format("~s: ~s~n", [Name, Value])
  ),
  format("Body: ~s~n", [Body]).
