% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(kacper_jedrzejewski, [parse/3]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

%komentarze wprowadzic maja byc zagniezdzone
%zmienne moga byc pod _ poprawic to

parse(_Path, Codes, Program) :-
  phrase(lexer(ListToken),Codes),
  phrase(program(Program),ListToken),!.

lexer(Tokens) -->
   white_space,comment(0),white_space,
   (  (  "[",       !, { Token = tokLKParen }
      ;  "]",       !, { Token = tokRKParen }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "..",      !, { Token = tokDots}
      ;  ",",       !, { Token = tokComma}
      ;  "<=",      !, { Token = tokLeq }
      ;  ">=",      !, { Token = tokGeq }
      ;  "=",       !, { Token = tokEq }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">",       !, { Token = tokGt }
      ;  "^",       !, { Token = tokExp}
      ;  ";",       !, { Token = tokOr}
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "&",       !, { Token = tokAnd}
      ;  "*",       !, { Token = tokTimes }
      ;  "/",       !, { Token = tokDiv}
      ;  "%",       !, { Token = tokMod}
      ;  "@",       !, { Token = tokAt}
      ;  "#",       !, { Token = tokHash}
      ;  "~",       !, { Token = tokTylda}
      ;  "|",       !, { Token = tokOr}
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (def, tokDef),
                                     (else, tokElse),
                                     (if, tokIf),
                                     (in, tokIn),
                                     (let, tokLet),
                                     (then, tokThen),
                                     ('_', tokSpace)]),
               !
            ;  Token = tokVar(Id)
            }
      
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
    [].
   

comment(N) --> "(*",!,{N1 is N + 1 },comment1(N1).
comment(0) --> [].

comment1(1) --> 
    "*)",!.
comment1(N) --> 
    "*)",!,{N1 is N - 1},comment1(N1).
comment1(N) -->
    "(*",!,{N1 is N + 1},comment1(N1)
    ;[_],comment1(N).

digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha)}
   ; "_", {L=95}.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T)
    ;"_", {A=95},alphanum(T)
    ;"'", {A=39},alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As), { atom_codes(Id, [L|As]) }.

%_------------------------------------------------


program(X)--> definicje(X).

definicje(X) --> puste(X) 
                 ;definicja(Y) ,definicje(Z),{X=[Y|Z]}.
 
definicja(X) --> [tokDef],[tokVar(Y)],[tokLParen],wzorzec(Z) ,[tokRParen], [tokEq] , wyrazenie(Q),{X=def(Y,Z,Q)}.

wzorzec(X) -->  [tokSpace],  [tokComma],wzorzec(Z), {X=pair(no,wildcard(no),Z)}
                ;[tokSpace],!, {X=wildcard(no)} 
                ;[tokVar(Y)],[tokComma],wzorzec(Z),{X=pair(no,var(no,Y),Z)}
                ;[tokVar(Y)],!,{X=var(no,Y)}
                ;[tokLParen],wzorzec(X) ,[tokRParen].
                
                

wyrazenie(Y) --> [tokIf],!,wyrazenie(Z) ,[tokThen],wyrazenie(Q),[tokElse],wyrazenie(X) ,{Y=if(no,Z,Q,X)}
                ;[tokLet],!,wzorzec(X) ,[tokEq],wyrazenie(Z) ,[tokIn],wyrazenie(Q),{Y =let(no,X,Z,Q)} 
                ;wyrazenie_op(Y).

wyrazenie_op(X) --> 
                     operator_unarny(Y),!,wyrazenie_op(Z),{X=op(no,Y,Z)}
                    ;wyrazenie_op2(Y) ,[tokComma] ,wyrazenie_op(Q),{X=pair(no,Y,Q)} 
                    ;wyrazenie_proste(X) 
                    ;wyrazenie_op2(X).

wyrazenie_op2(X) --> operator_unarny(Y),!,wyrazenie_op2(Z),{X=op(no,Y,Z)}
                    ;wyrazenie_op3(Y) ,operator_binarny2(Z),! ,wyrazenie_op3(Q),{X=op(no,Z,Y,Q)}
                    ;wyrazenie_proste(X)
                    ;wyrazenie_op3(X).

wyrazenie_op3(X) --> operator_unarny(Y),!,wyrazenie_op3(Z),{X=op(no,Y,Z)}
                    ;wyrazenie_op4(Y) ,operator_binarny3(Z),! ,wyrazenie_op3(Q),{X=op(no,Z,Y,Q)}
                    ;wyrazenie_op4(X).




wyrazenie_op4(X) --> operator_unarny(Y),!,wyrazenie_op4(Z),{X=op(no,Y,Z)}
                    ;wyrazenie_op5(Y),wyrazenie_op4(Y,X),!
                    ;wyrazenie_op5(X).

wyrazenie_op4(A,X)-->operator_binarny4(Y),!,wyrazenie_op5(Z),{A1=op(no,Y,A,Z)},wyrazenie_op4(A1,X).
wyrazenie_op4(X,X)-->[].



wyrazenie_op5(X) --> operator_unarny(Y),!,wyrazenie_op5(Z),{X=op(no,Y,Z)}
                    ;wyrazenie_proste(Y),wyrazenie_op5(Y,X).
                    

wyrazenie_op5(A,X)-->operator_binarny5(Y),!,wyrazenie_proste(Z),{A1=op(no,Y,A,Z)},wyrazenie_op5(A1,X).
wyrazenie_op5(X,X)-->[].

                   
operator_binarny2('=') --> [tokEq],!.
operator_binarny2('<') --> [tokLt],!.
operator_binarny2('>') --> [tokGt],!.
operator_binarny2('<>') --> [tokNeq],!.
operator_binarny2('<=') --> [tokLeq],!.
operator_binarny2('>=') --> [tokGeq],!.

operator_binarny3('@') --> [tokAt].

operator_binarny4('^') --> [tokExp],!.
operator_binarny4('|') --> [tokOr],!.
operator_binarny4('+') --> [tokPlus],!.
operator_binarny4('-') --> [tokMinus],!.

operator_binarny5('&') --> [tokAnd],!.
operator_binarny5('*') --> [tokTimes],!.
operator_binarny5('/') --> [tokDiv],!.
operator_binarny5('%') --> [tokMod],!.

operator_unarny('-') --> [tokMinus],!.
operator_unarny('#') --> [tokHash],!.
operator_unarny('~') --> [tokTylda],!.

wyrazenie_proste(X) --> [tokLParen],wyrazenie(X), [tokRParen]
                        ;wyrazenie_atomowe(X) 
                        ;wybor_bitu(X) 
                        ;wybor_bitow(X) .

wybor_bitu(X) --> ([tokLKParen],wyrazenie(A), [tokRKParen];wyrazenie_atomowe(A)),wybor_bitu(A,X).

wybor_bitu(A,X) --> [tokLKParen],wyrazenie(Z) ,[tokRKParen],{A1=bitsel(no,A,Z)},wybor_bitu(A1,X).
wybor_bitu(X,X) --> [].

wybor_bitow(X) --> ([tokLKParen],wyrazenie(A), [tokRKParen];wyrazenie_atomowe(A)),wybor_bitow(A,X).

wybor_bitow(A,X) --> [tokLKParen],wyrazenie(Z) ,[tokDots],wyrazenie(Q),[tokRKParen],{A1=bitsel(no,A,Z,Q)},wybor_bitow(A1,X).
wybor_bitow(X,X) --> [].


wyrazenie_atomowe(X) -->   wywolanie_funkcji(X) 
                         ; zmienna(X)
                         ; literal_calkowitoliczbowy(X)  
                         ; pusty_wektor(X) 
                         ; pojedynczy_bit(X) .

wywolanie_funkcji(X) --> [tokVar(Y)] ,[tokLParen],!,wyrazenie(Z),[tokRParen],{X=call(no,Y,Z)}.

zmienna(X) --> [tokVar(Y)], { X= var(no,Y)} .

literal_calkowitoliczbowy(X) --> [tokNumber(Y)],{X=num(no,Y)}.

pojedynczy_bit(X) --> [tokLKParen],wyrazenie(Y) ,[tokRKParen],{X=bit(no,Y)}.

pusty_wektor(X) --> [tokLKParen],!,[tokRKParen], { X=empty(no) }.

puste([]) --> []. 


















