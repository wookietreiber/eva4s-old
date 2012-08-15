# Evolutionary Algorithms for Scala

This project was started as an exercise to implement the [Evolutionary Algorithms][ea] learned
during an eponymous computer science lecture using the programming language [Scala][scala].


## Modularity

Different implementations of various models may be used dynamically via the [strategy
pattern][strategy].

### Selectors

A `Selector` determines how the individuals for the next generation are chosen, i.e. it models
environmental selection.

-   child selection (selects the offspring, no parents)
-   comma selection (selects the fittest offspring, no parents)
-   plus selection aka survival of the fittest (selects the fittest individuals, including parents)
-   random selection (selects a fixed amount of individuals, including parents)

### Matchmakers

A `Matchmaker` pairs individuals up with each other, i.e. models parental selection.

-   rank based matchmaking
-   tournament based matchmaking
-   random forced matchmaking (pairs up a fixed amount of individuals randomly)
-   random acceptance matchmaking (pairs up individuals randomly and gives them a chance to accept
    the match)

### Mutagens

A `Mutagen` determines the probability with which individuals mutate, depending on the current
generation.

-   constant
-   polynomial: monotonic function based on `f(x) = a + b * pow(x,degree)`
-   exponential: monotonic function based on `f(x) = a * exp(b*x)`


## Examples

Currently implemented example algorithms include:

### Minimization Problems

-   [Traveling Salesman Problem (TSP)][tsp]
-   Equation Solver (WIP)

### Maximization Problems

-   currently not directly supported (you can turn every maximization problem into a minimization
    problem)


[ea]: http://en.wikipedia.org/wiki/Evolutionary_algorithm
[scala]: http://www.scala-lang.org/
[strategy]: http://en.wikipedia.org/wiki/Strategy_pattern
[tsp]: http://en.wikipedia.org/wiki/Travelling_salesman_problem

