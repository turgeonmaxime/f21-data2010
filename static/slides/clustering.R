## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

knitr::opts_chunk$set(cache = FALSE, message = FALSE,
                      linewidth = 50)


## -----------------------------------------------------------------------------
library(dslabs)
dataset <- brca$x

results <- kmeans(dataset, centers = 5)
# How many observations per cluster?
results$size


## -----------------------------------------------------------------------------
library(factoextra)

fviz_cluster(results, data = dataset,
             geom = "point")


## -----------------------------------------------------------------------------
# This function computes the numbers for a range
# of values and then plots the results
fviz_nbclust(dataset, kmeans, 
             method = "silhouette")


## -----------------------------------------------------------------------------
results <- kmeans(dataset, centers = 2)
fviz_cluster(results, data = dataset,
             geom = "point")


## -----------------------------------------------------------------------------
data_gene <- tissue_gene_expression$x
fviz_nbclust(data_gene, kmeans, 
             method = "silhouette")


## -----------------------------------------------------------------------------
# Since the optimal value is on the boundary
# it's a good idea to repeat with a larger k.max
fviz_nbclust(data_gene, kmeans, 
             method = "silhouette",
             k.max = 20)


## -----------------------------------------------------------------------------
# Visualization
results <- kmeans(data_gene, centers = 10)
fviz_cluster(results, data = data_gene,
             geom = "point")


## -----------------------------------------------------------------------------
# Bonus
table(results$cluster, 
      tissue_gene_expression$y)


## ----dend_brca, warning = FALSE, cache = TRUE---------------------------------
# By default, use Ward's criterion for linkage
results <- hcut(dataset, k = 2)
fviz_dend(results, rect = TRUE)


## -----------------------------------------------------------------------------
fviz_nbclust(dataset, hcut, 
             method = "silhouette")


## -----------------------------------------------------------------------------
fviz_nbclust(data_gene, hcut, 
             method = "silhouette", 
             k.max = 20)


## ----dend_gene, warning = FALSE, cache = TRUE---------------------------------
results <- hcut(data_gene, k = 11)
fviz_dend(results, rect = TRUE)


## -----------------------------------------------------------------------------
# Gene expression data is often represented 
# using a heatmap
heatmap(data_gene)

