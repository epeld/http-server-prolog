
:- module(meta, [ignore_safe/1, ignore_safe_print/1]).

ignore_safe(Goal) :-
  ignore(catch(Goal, _Err, true)).


ignore_safe_print(Goal) :-
  ignore(
    catch(
      Goal,
      Err,
      format("Error was caught and ignored: ~w~n", [Err])
    )
  ).
