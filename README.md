# ada_threads
Projekt w języku ADA mający na celu pokazanie jego możliwości w zakresie wielowątkowości i współbieżności.

WERSJA 1.0 PROGRAMU WYDANA 24.01.2018

Tematem projektu jest 'aplikacja taksówkarska'.
Program w formie tekstowej, uruchamiany z konsoli.

Zarys funkcjonalności oraz wyświetlanych informacji:
 - wyświetlanie liczby wolnych i zajętych kierowców
 - prezentowanie w formie listy statusu każdego z kierowców
 - prosta prezentacja statusu zajętego kierowcy w formie np. procentowej lub jako pasek postępu
 - jakieś wybieranie dlugosci trasy w formie wpisania cyfry

Kwestie techniczne
 - każdy kierowca będzie osobnym wątkiem
 - dodatkowy wątek będzie monitorował zmiany statusu kierowców, nowe zlecenia oraz będzie drukował interfejs
 - kwestię konfiliktów i dzielenia zasobów może zrealizować np. licznik zajętych wątków, który będzie zwiększany przez nadzorcę a zmniejszany przez kończący pracę wątek
 - dodatkowo wątki mogą też na koniec dodawać koszt transportu do łącznej zarobionej sumy
 
ELEMENTY ZREALIZOWANE
 - procedura główna wywołująca wątek - nadzorcę
 - wypisanie statusu kierowców
 - nadzorca pracuje w pętli
 - monitoruje zmiany statusu kierowców, przyjmuje nowe zlecenia
 - odświeżanie informacji ręczne po każdorazowym wciśnięciu enter (nie częściej niż co 0.5s)
 - odświeżanie informacji automatycznie co 3.0s
 - współdzielone między wątkami: licznik aktywnych kierowców, tablica aktywności, tablica postępów
 
