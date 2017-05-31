# Automatyczne testowanie 1 pracowni

## Jak testować?
W pierwszej kolejności umieść swoje rozwiązanie w pliku _solution.pl_.  
Następnie uruchom testy poleceniem make:

```
make test           % uruchom wszystkie testy z ./tests
make human          % uruchom wszstkie testy h-*.test
make generator      % uruchom wszystkie testy g-*.test
make g-huge.test    % uruchom pojedynczy test g-huge.test
```

Przykładowe rozwiązanie dla make test [http://ix.io/pcF](http://ix.io/pcF)

## Jak dodać nowe testy?
Skopiuj plik template.test znajdujący się w katalogu tests i zmień jego nazwę na odpowiednio:
```
h-imie_nazwisko.test
```
jeśli jest to plik zawierający ręcznie pisane testy lub:

```
g-imie_nazwisko.test
```
jeśli jest to plik zawierający testy wygenerowane automatycznie.

Następnie umieść ten plik w katalogu tests, oraz wykonaj pull-requesta aby udostępnić
test innym.

Dobrze, żeby skomentować każdy test i go krótko uzasadnić, aby osoba, której test nie
przechodzi wiedziała gdzie jest błąd.

## Jak dzielić się wynikami?
```
make test | curl -F 'f:1=<-' ix.io
```

## Uwagi
Wiem, że niektóre testy u niektórych osób są błędne.
Postaram się je poprawić.
