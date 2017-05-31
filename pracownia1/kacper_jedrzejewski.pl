% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
% :- module(kacper_jedrzejewski, [solve/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

solve([],[]). 
solve(Clauses, Solution) :- do_lista(Clauses,Lista), wynik(Lista,[],Solution).
 
do_lista([H|T],X) :- do_lista([H|T],[],X).
do_lista([],X,Y) :- reverse(X,Y).
do_lista([H|T],Y,X) :- lista([H],Posrednia),do_lista(T,[Posrednia|Y],X).

lista(X,Y) :- lista(X,[],Y).
lista([],Acc,Lista) :- reverse(Acc,Lista).
lista([H],Acc,Y) :- atom(H),lista([],[H|Acc],Y),!.
lista([H v X],Acc,Y) :- lista([X],[H|Acc],Y),!.
lista([~H],Acc,Y) :- lista([],[~H|Acc],Y),!.
lista([~H v X],Acc,Y) :- lista([X],[~H|Acc],Y).

wynik([[]],[],false) :- !.
wynik([],Solution,Solution).
wynik([H|T],Acc,Solution) :- pojedyncza_klazula(H,Acc,ZwartosciowaneZmienne), poprawne(H,ZwartosciowaneZmienne), wynik(T,ZwartosciowaneZmienne,Solution).

	
pojedyncza_klazula([],Y,Y) :- !.
pojedyncza_klazula([H|T],Acc,Y):-atom(H),nowy_atom(H,Acc),!,wartosciowanie(H,HzWartoscia),pojedyncza_klazula(T,[HzWartoscia|Acc],Y).
pojedyncza_klazula([~H|T],Acc,Y):-nowy_atom(H,Acc),!,wartosciowanie(H,HzWartoscia),pojedyncza_klazula(T,[HzWartoscia|Acc],Y).
pojedyncza_klazula([_|T],Acc,Y):-pojedyncza_klazula(T,Acc,Y).

nowy_atom(H,X):-nowy_atom(H,X,X).
nowy_atom(_,[],_) :- !.
nowy_atom(H,[(H1,_)|T],X) :- H \= H1 , nowy_atom(H,T,X).

wartosciowanie(X,Y) :- Y = (X,t); Y = (X,f).

poprawne([H|_],[(H,t)|_]) :- !.
poprawne([~H|_],[(H,f)|_]) :- !.
poprawne([~H|T],[_|T1]) :- poprawne([~H|T],T1),!.
poprawne([_|T],[H1|T1]) :- poprawne(T,[H1|T1]),!.
poprawne([H|T],[_|T1]) :- poprawne([H|T],T1).


