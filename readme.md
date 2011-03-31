2. �bungsaufgabe
================

Aufgabe:
--------
Entwickeln Sie in einer stark typisierten funktionalen Sprache (ML oder Haskell) ein Programm zur Verwaltung von Platzreservierungen in Z�gen. Zwei Arten von Reservierungen sollen unterst�tzt werden: Einzelplatzreservierungen mit der genauen Angabe einer Sitznummer in einem bestimmten Waggon und Gruppenreservierungen, bei denen nur der Waggon angegeben wird und ein beliebiger, nicht reservierter Platz im Waggon verwendet werden kann. Jede Reservierung gilt nur f�r einen bestimmten Zug zwischen zwei Stationen (die zur Vereinfachung durch fortlaufende Nummern benannt sein k�nnen). Die Anzahl der reservierbaren Sitzpl�tze ist nicht nur durch die Anzahl der vorhandenen Pl�tze pro Waggon und die Anzahl der Waggons pro Zug begrenzt, sondern auch durch eine Mindestzahl von Pl�tzen pro Zug, die f�r Fahrg�ste ohne Reservierung frei bleiben m�ssen. Nat�rlich darf kein Platz mehrfach belegt sein, und es muss ausreichend viele freie Pl�tze in Waggons mit Gruppenreservierungen geben.

Es werden verschiedene Abfragem�glichkeiten ben�tigt:

* Mindestanzahl der freien und maximale Anzahl der durch Einzelreservierung bzw. Gruppenreservierung belegten Pl�tze pro Zug und Waggon zwischen je zwei Stationen (wobei sich Minimum und Maximum darauf beziehen, dass Reservierungen m�glicherweise nur auf Teilen der abgefragten Strecke existieren);

* Einzelreservierungen f�r einen bestimmten Platz in einem Zug, wobei das Ergebnis die Stationen angibt, zwischen denen eine Reservierung besteht;

* Gruppenreservierungen f�r einen bestimmten Waggon in einem Zug, wobei das Ergebnis die Anzahl der Personen und die Stationen angibt, f�r die bzw. zwischen denen eine Reservierung besteht.

Daten sollen beim absichtlichen Beenden des Programms nicht verloren gehen, sondern persistent (in einer Datei) gespeichert werden. Die Datenmengen werden klein bleiben. Es ist daher problemlos m�glich, alle Daten beim Programmstart einzulesen und vor Beendigung neu zu schreiben.

Bei dieser Aufgabe kommt es darauf an, Daten zu verwalten. Dies scheint auf den ersten Blick in Widerspruch zu modernen funktionalen Sprachen zu stehen. Bei genauerer Betrachtung ergeben sich aber viele L�sungsm�glichkeiten. Ein Blick in die mit dem Programmiersystem mitgelieferten Bibliotheken kann vielleicht bei der Auswahl des L�sungsansatzes helfen.

ML:
---
Zur Programmierung in ML verwenden Sie am besten Objective Caml. Dieses System erweitert ML unter anderem um objektorientierte Konzepte, die einen Programmierstil �hnlich dem von Java erlauben w�rden. Falls Sie Ihr Programm in Objective Caml schreiben, schr�nken Sie sich bitte auf die funktionalen Konzepte der Sprache ein, und verzichten Sie auf die objektorientierten Erweiterungen.

Haskell:
--------
Allgemeine Informationen zu Haskell (beispielsweise die Sprachdefinition) finden Sie auf der Haskell-Seite. Sie haben im Wesentlichen die Wahl zwischen GHC und Hugs. GHC is ein eher umfangreiches und effizientes System mit zahlreichen Sprach-Erweiterungen, das sich auch f�r gr��ere Anwendungen eignet, aber vielleicht einen etwas h�heren Einlernaufwand mit sich bringt. Sie k�nnen gleich eine einigerma�en vollst�ndige Entwicklungsumgebung mit mehreren Werkzeugen installieren. Hugs ist ein einfacheres, nur auf einem Interpreter basierendes System, das Sie m�glicherweise schon aus Funktionale Programmierung kennen. F�r die L�sung dieser Aufgabe reicht Hugs vollkommen aus.
