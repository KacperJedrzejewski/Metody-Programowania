% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(kacper_jedrzejewski_tests, [tests/3]).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy
tests(empty_program, input(""), program([])).
tests(invalid, input("def main()"), no).
tests(adder, file('adder.hdml'), yes).
tests(srcpos, input("def main(_) = 1"),program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).
tests(zagniezdzone_nawiasy, input("def main((((A)))) = []"), yes).
tests(unarnySpelnialny, input("def main(A,B,C,D,E) = A & B & C / D + # ~ E"), yes).
tests(unarnyNieSpelnialny, input("def main(A,B,C,D,E) = A & B & C / D # ~ E"), no).
tests(wielkoscLiter, input("Def main(_) = []"), no).
tests(if, input("def main(A,B) = if A < B then A else B"), yes).
tests(let, input("def main(A,B) = let A = 2 in B"), yes).
tests(unarny, input("def main(A) = #A"), yes).
tests(unarnyZly, input("def main(A) = +A"), no).
tests(unarnychkilka, input("def main(A) = ##~-#~A"), yes).
tests(unarnychkilkaV1, input("def main(A) = ##~-#+A"), no).
tests(defDwaRazy, input("def main(_) = [] def main2(_) = 2 "), yes).
tests(defDwaRazyV1, input("def main(_) = def main2(_) = 2 "), no).
tests(zagniezdzoneWyrazenie, input("def main(A,B) = if let A = 2 in B then A else B"), yes).


tests(good_nesting_8, input("def test2(A, B, C) = 
	if A > 0 then zero(A, B, C)
	else 
		let A, B, C = 1 in ( [ ( [] ) ] )"), yes).

