lotr <- read_graph("Networks/Lotr.graphml", format="graphml")
L.bm <- as_incidence_matrix(lotr)
L.cm <- crossprod(L.bm)
diag(L.cm) <- 0
L.overlap <- graph_from_adjacency_matrix(L.cm, mode="undirected", weighted = TRUE)
L.e <- as_edgelist(L.overlap)
write.csv(L.e, file="Le.csv", row.names = FALSE, col.names = FALSE)
L.bp <- bipartite.projection(lotr)
L.bp
summary(L.bp$proj1)
L.last <- L.bp$proj1
L.ci <- cluster_infomap(L.last)
V(L.last)$com_inf <- membership(L.ci)
write_graph(L.last, file = "b.graphml", format = "graphml")
L.cl <- cluster_louvain(L.last)
V(L.last)$com_lou <- membership(L.cl)
L.max <- max_cliques(L.last)
table(sapply(L.max, length))
write_graph(L.last, file = "b.graphml", format = "graphml")
