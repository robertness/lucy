# igraphr

*igraphr* creates higher level graph operations build upon the basic graph data structure and graph operations found in igraph.  A simple example is the function *iparents*, which returns the parents of a node in a graph, as opposed to having to write: 

    V(g)[nei(v, mode="out")]

*igraphr* is useful to anyone who likes using *igraph* as their primary tool for working with graph data structures.

## Installation
This package is currently in development and not yet released on CRAN.

To get the current development version from github:

    # install.packages("devtools")
    devtools::install_github("osazuwa223/igraphr")
