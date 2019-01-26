:- module(urls, []).

:- set_prolog_flag(double_quotes, string).

match_parts(Pattern, URL, Matching) :-
  split_string(URL, "/", "", Parts),
  match_parts_rec(Pattern, Parts, Matching).

match_parts_rec([], [], []).

match_parts_rec([S | RestPattern], [S | RestParts], Matching) :-
  match_parts_rec(RestPattern, RestParts, Matching).

match_parts_rec([P | RestPattern], [S | RestParts], [ (P-S) | Matching]) :-
  atom(P),
  match_parts_rec(RestPattern, RestParts, Matching).
