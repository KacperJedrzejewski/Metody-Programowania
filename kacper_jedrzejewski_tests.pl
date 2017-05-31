% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(kacper_jedrzejewski_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy
tests(excluded_middle_v1, validity, [p v ~p], 500, solution([(p,t)])).
tests(excluded_middle_v2, validity, [p v ~p], 500, solution([(p,f)])).
tests(excluded_middle_count, validity, [p v ~p], 500, count(2)).

tests(only_one_variable,validity,[p],500,solution([(p,t)])).
tests(only_one_variable_count,validity,[p],500,count(1)).

tests(two_equal_variable,validity,[p v p],500,solution([(p,t)])).
tests(two_equal_variable_count,validity,[p v p],500,count(1)).

tests(negation, validity, [~p], 500, solution([(p,f)])).
tests(negation_count, validity, [~p], 500, count(1)).

tests(empty_clause, validity, [[]],500,count(0)).

tests(three_variable_v1,validity,[p v q v r],500 ,solution([(p, t),(q, f),(r, f)])).
tests(three_variable_v2,validity,[p v q v r],500 ,solution([(p, f),(q, f),(r, t)])).
tests(three_variable_v3,validity,[p v q v r],500 ,solution([(p, f),(q, t),(r, f)])).
tests(three_variable_v4,validity,[p v q v r],500 ,solution([(p, f),(q, t),(r, t)])).
tests(three_variable_v5,validity,[p v q v r],500 ,solution([(p, t),(q, f),(r, f)])).
tests(three_variable_v6,validity,[p v q v r],500 ,solution([(p, t),(q, f),(r, t)])).
tests(three_variable_v8,validity,[p v q v r],500 ,solution([(p, t),(q, t),(r, t)])).
tests(three_variable_count,validity,[p v q v r],500 ,count(7)).

tests(simple_valuation_v1, validity, [p v q], 500, solution([(p,t),(q,t)])).
tests(simple_valuation_v2, validity, [p v q], 500, solution([(p,t),(q,f)])).
tests(simple_valuation_v3, validity, [p v q], 500, solution([(p,f),(q,t)])).
tests(simple_valuation_count, validity, [p v q], 500, count(3)).

tests(prawo_pochlaniania_v1, validity, [p,p v q], 500, solution([(p,t),(q,t)])).
tests(prawo_pochlaniania_v2, validity, [p,p v q], 500, solution([(p,t),(q,f)])).
tests(prawo_pochlaniania_count, validity, [p,p v q], 500, count(2)).

tests(excluded_middle2_v1, validity, [p v ~p, q] , 500, solution([(p,t),(q,t)])).
tests(excluded_middle2_v2, validity, [p v ~p, q] , 500, solution([(p,t),(q,t)])).
tests(excluded_middle2_count, validity, [p v ~p, q] , 500, count(2)).

tests(tautology_v1,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, f),(q, f),(r, f)])).
tests(tautology_v2,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, f),(q, f),(r, t)])).
tests(tautology_v3,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, f),(q, t),(r, f)])).
tests(tautology_v4,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, f),(q, t),(r, t)])).
tests(tautology_v5,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, t),(q, f),(r, f)])).
tests(tautology_v6,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, t),(q, f),(r, t)])).
tests(tautology_v7,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, t),(q, t),(r, f)])).
tests(tautology_v8,validity,[p v q v r v ~p, q v p v ~q],500 ,solution([(p, t),(q, t),(r, t)])).
tests(tautology_count,validity,[p v q v r v ~p, q v p v ~q],500 ,count(8)).

tests(inconsistent_clause_count,validity,[p v q,~p v q,p v ~q, ~p v ~q],500,count(0)).

tests(empty_clause_with_variable, validity, [p v q,p1 v q, p2 v p,[]],500,count(0)).

tests(three_variable_negation_v1,validity,[p v ~q v ~r, p v q v r],500,solution([(p,t),(q,t),(r,t)])).
tests(three_variable_negation_v2,validity,[p v ~q v ~r, p v q v r],500,solution([(p,t),(q,t),(r,f)])).
tests(three_variable_negation_v3,validity,[p v ~q v ~r, p v q v r],500,solution([(p,t),(q,f),(r,t)])).
tests(three_variable_negation_v4,validity,[p v ~q v ~r, p v q v r],500,solution([(p,t),(q,f),(r,f)])).
tests(three_variable_negation_count,validity,[p v ~q v ~r, p v q v r],500,count(4)).