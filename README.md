CrossEntropy
============

Assignment #2 for my Computational Linguistics Class.

Problem
-------

Given a collection of pairs of letters and their number of occurrences for four languages, determine what language a fourth "mystery" language is.

### Sample Data

* `input/English.csv`
* `input/French.csv`
* `input/German.csv`
* `input/Swahili.csv`

Answer
------

For two languages `A` and `B` with cross entropy `H(A,B)` and entropy `H(A)`, `H(A,B)-H(A)=0` suggests `A` and `B` are the same language.

The mystery language is French.

### Graph

<!--
![English](https://raw.github.com/markandrus/CrossEntropy/master/graphs/English.png)
![French](https://raw.github.com/markandrus/CrossEntropy/master/graphs/French.png)
![German](https://raw.github.com/markandrus/CrossEntropy/master/graphs/German.png)
-->
![Mystery](https://raw.github.com/markandrus/CrossEntropy/master/graphs/Mystery.png)
<!--
![Swahili](https://raw.github.com/markandrus/CrossEntropy/master/graphs/Swahili.png)
-->
