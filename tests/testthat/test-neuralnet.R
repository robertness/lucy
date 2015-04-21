context("Neural network implementation")
set.seed(21)
g <- generateMultiConnectedDAG(5)
inputs <- c("2", "3")
outputs <- c("4")
input.table <- as.data.frame(matrix(runif(1000 * 2), ncol = 2,
                                    dimnames = list(NULL, inputs)))
names(input.table) <- inputs
output.table <- input.table %>%
  as.matrix %>% 
  {. %*% matrix(c(1.7, .8), ncol = 1)} %>%
  {(function(x) x / (1 + x))(.)} %>%
  data.frame %>%
  `names<-`("4")
g <- fitNetwork(g, input.table, output.table, 
                activation = logistic,
                activation.prime = logistic.prime,
                min.max.constraints = c(min = -Inf, max = Inf),
                verbose=T)
test_that("initializeGraph names the edges.", {})
test_that("fitNetwork returns a graph structure", {
  expect_true(class(g) == "igraph")
})

"a graph imported from a model from a neural network package package should 
provide the same prediction."

"model should perform a reasonable prediction on a toy problem."

"a MLP structure should have the same error rate as a model with the same structure
from a neural network package."

"for a simple muli-node input, single-node output graph, calculateVals should reproduce
simple arithmetic."

"no unexpected input to resetUpdateAttributes, specifically one where all nodes or
edges are updated == TRUE or all are FALSE"

"adding one bias node per non-input node should have same parameter count as a comparable
multi-layer perceptron."

"values and updated status for input nodes and bias nodes should never change"

"works when no derivitive of activation function is provided."



test_that("fitNetwork returns a MSPR on the infert dataset that is close to 
          that of a network of the same shape fit with the neuralnet package", {})