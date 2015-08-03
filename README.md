# lucy

*lucy* is a wrapper for the popular [igraph](http://igraph.org/r/) package.  Particular attention is paid toward making igraph play nice with [magrittr's](https://github.com/smbache/magrittr) pipes.

*lucy* also includes functions for graph propagation and simulation, so igraph objects are easier use in the building of probabilistic graphical models (eg. Bayesian networks and artificial neural networks).  

However, *lucy* is generally useful to anyone who likes using *igraph* as a workhorse for manipulating graph data structures.  See the [vignettes](https://github.com/robertness/lucy/tree/master/vignettes) for details.

## Installation
This package is currently in development and not yet released on CRAN.

To get the current development version from github:

    # install.packages("devtools")
    devtools::install_github("robertness/lucy")
