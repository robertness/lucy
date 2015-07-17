#load("inst/extdata/graph.rda")
#load("inst/extdata/observationData.rda")
library(signalgraph)
# g_enriched <- initializeGraph(g, obs_data)
#g <- induced.subgraph(g_enriched, V(g_enriched)[is.observed])
save(g, file = "inst/extdata/g_enriched.rda")
library(glmnet)
load("inst/extdata/g_enriched.rda")
V(g)$fit <- list(NA)
calculateVals <- function(g, v){ 
  parents <- iparents(g, v)
  if(length(parents) == 0) return(g)
  fit <- do.call("cbind", V(g)[parents]$observed) %>% # Make a matrix from the parent values
    `colnames<-`(V(g)[parents]$name) %>%
    log %>% # log the values
    cv.glmnet(y = unlist(v$observed), family = "gaussian", type.measure="deviance", 
              nfolds = 10) # fit gaussian lasso with 10-fold cross validation
  V(g)$fit <- list(fit) # Add the fit to the graph as an attribute of the vertex
  E(g)[to(v)]$weight <- fit %>%
    coef(s = "lambda.min") %>% # grab the coefficients with the lambda that 
    #minimizes deviance in cross-validation
    {structure(as.numeric(.), names = rownames(.))} %>% 
    .[-1]
  g
}
g_fit <- update_vertices(g = g, function(g, v) NULL, calculateValues)
save(g_fit, file="inst/extdata/g_fit.rda")

