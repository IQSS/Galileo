batchS <- 
function (data, grid = somgrid(), radii, init) 
{
    data <- as.matrix(data)
    nd <- nrow(data)
    ng <- nrow(grid$pts)
    if (missing(init)) 
        init <- data[sample(1L:nd, ng, replace = FALSE), , drop = FALSE]
    nhbrdist <- as.matrix(dist(grid$pts))
    diag(nhbrdist) <- 0
    for (r in radii) {
        cl <- as.numeric(knn1(init, data, 1:ng))
        A <- (nhbrdist <= r)[, cl]
        ind <- rowSums(A) > 0
        init[ind, ] <- A[ind, ] %*% data/rowSums(A)[ind]
    }
    structure(list(grid = grid, codes = init), class = "SOM")
}
