# Harmony Search

*Implementation of a great search algorithm written in R*

## Usage

There are two main files:

- **metaheuristic.r** - Metaheuristic's core
- **utils.r** - Utility functions used inside core

In order to use the algorithm you have to define config list, and apply it to `metaheuristicRun` function. There are two example usages of the algorithm inside repository:

- **squared.r** - Minimizaiton of simple squared two-dimensional function [WolframAplha](http://www.wolframalpha.com/input/?i=x%5E2+%2B+y%5E2)
- **rastrigin.r** - Minimizaiton of more complicated Rastrigin's function with many local minima [Wiki](http://en.wikipedia.org/wiki/Rastrigin_function)

If you want to create your own example, just use **template.r** file.

## Enjoy!