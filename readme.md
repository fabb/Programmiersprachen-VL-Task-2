What's this?
============

This is one of three tasks to be completed in the proceedings of the course "Programming Languages" at the University of Technology Vienna. I solved these together with lewurm.

In this task, I did the implementation and lewurm reviewed it.

Below you can find the original task description in German.


2\. Übungsaufgabe
================

Aufgabe:
--------
Entwickeln Sie in einer stark typisierten funktionalen Sprache (ML oder Haskell) ein Programm zur Verwaltung von Platzreservierungen in Zügen. Zwei Arten von Reservierungen sollen unterstützt werden: Einzelplatzreservierungen mit der genauen Angabe einer Sitznummer in einem bestimmten Waggon und Gruppenreservierungen, bei denen nur der Waggon angegeben wird und ein beliebiger, nicht reservierter Platz im Waggon verwendet werden kann. Jede Reservierung gilt nur für einen bestimmten Zug zwischen zwei Stationen (die zur Vereinfachung durch fortlaufende Nummern benannt sein können). Die Anzahl der reservierbaren Sitzplätze ist nicht nur durch die Anzahl der vorhandenen Plätze pro Waggon und die Anzahl der Waggons pro Zug begrenzt, sondern auch durch eine Mindestzahl von Plätzen pro Zug, die für Fahrgäste ohne Reservierung frei bleiben müssen. Natürlich darf kein Platz mehrfach belegt sein, und es muss ausreichend viele freie Plätze in Waggons mit Gruppenreservierungen geben.

Es werden verschiedene Abfragemöglichkeiten benötigt:

* Mindestanzahl der freien und maximale Anzahl der durch Einzelreservierung bzw. Gruppenreservierung belegten Plätze pro Zug und Waggon zwischen je zwei Stationen (wobei sich Minimum und Maximum darauf beziehen, dass Reservierungen möglicherweise nur auf Teilen der abgefragten Strecke existieren);

* Einzelreservierungen für einen bestimmten Platz in einem Zug, wobei das Ergebnis die Stationen angibt, zwischen denen eine Reservierung besteht;

* Gruppenreservierungen für einen bestimmten Waggon in einem Zug, wobei das Ergebnis die Anzahl der Personen und die Stationen angibt, für die bzw. zwischen denen eine Reservierung besteht.

Daten sollen beim absichtlichen Beenden des Programms nicht verloren gehen, sondern persistent (in einer Datei) gespeichert werden. Die Datenmengen werden klein bleiben. Es ist daher problemlos möglich, alle Daten beim Programmstart einzulesen und vor Beendigung neu zu schreiben.

Bei dieser Aufgabe kommt es darauf an, Daten zu verwalten. Dies scheint auf den ersten Blick in Widerspruch zu modernen funktionalen Sprachen zu stehen. Bei genauerer Betrachtung ergeben sich aber viele Lösungsmöglichkeiten. Ein Blick in die mit dem Programmiersystem mitgelieferten Bibliotheken kann vielleicht bei der Auswahl des Lösungsansatzes helfen.

ML:
---
Zur Programmierung in ML verwenden Sie am besten Objective Caml. Dieses System erweitert ML unter anderem um objektorientierte Konzepte, die einen Programmierstil ähnlich dem von Java erlauben würden. Falls Sie Ihr Programm in Objective Caml schreiben, schränken Sie sich bitte auf die funktionalen Konzepte der Sprache ein, und verzichten Sie auf die objektorientierten Erweiterungen.

Haskell:
--------
Allgemeine Informationen zu Haskell (beispielsweise die Sprachdefinition) finden Sie auf der Haskell-Seite. Sie haben im Wesentlichen die Wahl zwischen GHC und Hugs. GHC is ein eher umfangreiches und effizientes System mit zahlreichen Sprach-Erweiterungen, das sich auch für größere Anwendungen eignet, aber vielleicht einen etwas höheren Einlernaufwand mit sich bringt. Sie können gleich eine einigermaßen vollständige Entwicklungsumgebung mit mehreren Werkzeugen installieren. Hugs ist ein einfacheres, nur auf einem Interpreter basierendes System, das Sie möglicherweise schon aus Funktionale Programmierung kennen. Für die Lösung dieser Aufgabe reicht Hugs vollkommen aus.
