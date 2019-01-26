:- module(urls, [match_parts/3]).

:- set_prolog_flag(double_quotes, string).

% Expects a "pattern", i.e something like:
%   ["users", user_id, "profile"]
% matches that against an URL, e.g
%   users/foo/profile
% produces a list of matched parts, i.e
%   [user_id - "foo"]
% in this case
match_parts(Pattern, URL, Matching) :-
  split_string(URL, "/", "", Parts),
  match_parts_rec(Pattern, Parts, Matching).

match_parts_rec([], [], []).

match_parts_rec([S | RestPattern], [S | RestParts], Matching) :-
  match_parts_rec(RestPattern, RestParts, Matching).

match_parts_rec([P | RestPattern], [S | RestParts], [ (P-S) | Matching]) :-
  atom(P),
  match_parts_rec(RestPattern, RestParts, Matching).
