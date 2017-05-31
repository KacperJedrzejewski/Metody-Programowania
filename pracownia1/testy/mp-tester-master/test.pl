% zmień na nazwę pliku zawierający główny predykat solve/2
:- use_module(solution).

:- current_prolog_flag(argv, Argv), use_module(Argv).

:- op(200, fx, ~).
:- op(500, xfy, v).

test_all :-
  run_test(_), fail;
  format("Done!~n", []).

% odpal pojedynczy test
run_test(Name) :-
  tests(Name, Type, Input, Timeout, Ans),
  format("~w~t~32+ ", [Name]),
  % sprawdz czy test jest prawidło zoapisany
  ( validate_test(Name, Type, Input, Timeout, Ans) ->
      % odpal test
      catch(run_test(Input, Timeout, Ans, Status),
        time_limit_exceeded,
        (Status = tle)),
      print_status(Status)
  ; format("invalid test~n")
  ).

% =============================================================================
% Predykaty pomocnicze

print_status(tle)       :- format("tle~n").
print_status(ok(Time))  :- format("~3fs ok~n", [Time]).
print_status(wa(Time))  :- format("~3fs wrong answer~n", [Time]).
print_status(inv(Time)) :- format("~3fs invalid answer~n", [Time]).

% get all solutions for Input within time limit constains
% validate solutions from Solutions
%  a) check if all are general valuations
%
run_test(Input, Timeout, Ans, Status) :-
  statistics(cputime, T0),
  TimeLimit is Timeout / 1000,
  call_with_time_limit(TimeLimit,
    findall(X, solve(Input, X), Solutions)),
  statistics(cputime, T1),
  Time is T1 - T0,
  % validate_solution - checks if solution is general valuation
  ( maplist(validate_solution, Solutions) ->
	  % mamy zbiór wszystkich uogólnionych wartościowań
	  % i chcemy sprawdzić czy zgadzają się z tesem
      check_solutions(Ans, Solutions, Time, Status)
  ; Status = inv(Time)
  ).

% Gdzie sprawdzam, czy Sol nie jest przypadkiem uogólnionym rozwiazaniem?
% z tego co widze to Sol moze być ugolnione
% 
% Sol jest odpowiedzią z testu, czyli jest wartościowaniem
% uogólnionym czy zwyklym?
check_solutions(solution(Sol), Solutions, Time, Status) :-
  ((member(GSol, Solutions), instance_of(Sol, GSol)) ->
    Status = ok(Time)
  ; Status = wa(Time)
  ).

check_solutions(count(N), Solutions, Time, Status) :-
  ( check_count_solution(Solutions, N) ->
    Status = ok(Time)
  ; Status = wa(Time)
  ).

% pusta lista rozwiązań solve/2 daje 0 liczbę count
check_count_solution([], 0).

% dla każdego uogólnionego wartościowania:
% 	
check_count_solution([Sol|Solutions], N) :-
  % Sol2 contains only paris whose value is x
  include(is_any, Sol, Sol2),
  length(Sol2, M),
  N2 is N - (2 ** M),
  N2 >= 0,
  check_count_solution(Solutions, N2).

is_any((_, x)).

% sprawdz czy Sol z testu jest instancją uogólnionego wartościowania
% wygenerowanego przez solve/2
instance_of(Sol, GSol) :-
  % posortuj krotki z wartosciowania testowego
  sort(Sol,  SSol),
  % posortuj krotki z wartościowania ogólnego
  sort(GSol, SGSol),

  % maplist(variable_match, [(p,t), (q,f)], [(p,x), (q,x)])
  maplist(variable_match, SSol, SGSol).

variable_match((X, _), (X, x)) :- !.
variable_match((X, V), (X, V)).

validate_test(Name, Type, Input, Timeout, Ans) :-
  atom(Name),
  ground(Type), member(Type, [validity, performance]),
  acyclic_term(Input), ground(Input), validate_input(Input),
  number(Timeout), Timeout >= 500, Timeout =< 10000,
  acyclic_term(Ans), ground(Ans), validate_ans(Ans).

validate_input(Input) :-
  maplist(is_clause, Input).

validate_ans(solution(Sol)) :-
  maplist(is_valuation, Sol).

validate_ans(count(N)) :-
  number(N), N >= 0.

validate_solution(Solution) :-
  % True if Term does not contain cycles
  acyclic_term(Solution), 

  % True if Term holds no free variables
  ground(Solution), 

  % True if Goal can successfully be applied on all elements of List
  % checks if all valuations in Solution are general valuations
  maplist(is_gvaluation, Solution). 

% checks whether valuation is 'simple'
is_valuation((X, V)) :-
  is_variable(X), member(V, [t, f]).

% checks wheter valuation is general
is_gvaluation((X, V)) :-
  is_variable(X), member(V, [t, f, x]).

is_clause([]) :- !.
is_clause(C) :- is_non_empty_clause(C).

is_non_empty_clause(L) :- is_literal(L), !.
is_non_empty_clause(L v C) :-
  is_literal(L), is_non_empty_clause(C).

is_literal(X) :- is_variable(X), !.
is_literal(~X) :- is_variable(X).

is_variable([]) :- !, fail.
is_variable(X) :- atom(X).

