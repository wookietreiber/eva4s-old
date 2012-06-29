# Evolutionary Algorithms

This project was started as an exercise to implement the [Evolutionary Algorithms][ea] (EA) learned
during an eponymous computer science lecture using the programming language [Scala][scala].

### Selectors

Different `Selector` implementations (which model environmental selection) may be used dynamically
via the [strategy pattern][strategy]:

-   child selection (selects the offspring, no parents)
-   comma selection (selects the fittest offspring, no parents)
-   plus selection aka survival of the fittest (selects the fittest individuals, including parents)
-   random selection (selects a fixed amount of individuals, including parents)

### Matchmakers

Different `Matchmaker` implementations (which model parental selection) may be used dynamically via
the [strategy pattern][strategy]:

-   random forced matchmaking (pairs up a fixed amount of individuals randomly)
-   random acceptance matchmaking (pairs up individuals randomly and gives them a chance to accept
    the match)


## Examples

Currently implemented example algorithms include:

### Minimization Problems

-   [Traveling Salesman Problem (TSP)][tsp]

### Maximization Problems

-   currently not supported


[ea]: http://en.wikipedia.org/wiki/Evolutionary_algorithm
[scala]: http://www.scala-lang.org/
[strategy]: http://en.wikipedia.org/wiki/Strategy_pattern
[tsp]: http://en.wikipedia.org/wiki/Travelling_salesman_problem

