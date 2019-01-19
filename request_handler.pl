:- module(request_handler, [handle_request/6]).

:- multifile
    handle_request/6.


handle_request(Method, URL, Version, Headers, Body, Stream) :-
  format("200 OK~n"),
  format(Stream, "HTTP/1.1 200 OK~nContent-Length: 0~n~n", []).
