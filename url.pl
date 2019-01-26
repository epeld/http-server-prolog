:- module(urls, [match_parts/3, pattern_string/2]).

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



% Convenience util for converting a string like
%   foo/:bar/baz
% into something that can be passed to match_parts_rec/3, i.e
%   ["foo", bar, "baz"]
% in this case
pattern_string(String, Pattern) :-
  split_string(String, "/", "", Parts),
  maplist(pattern_part, Parts, Pattern).

colon_codes(String, Rest) :-
  string_codes(String, [58 | Rest]).

pattern_part(InString, Atom) :-
  colon_codes(InString, Codes),
  atom_string(Atom, Codes).

pattern_part(InString, OutString) :-
  \+ colon_codes(InString, _),
  format(string(OutString), "~s", [InString]).
