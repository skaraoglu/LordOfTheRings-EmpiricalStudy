setwd("C:/Users/User/Desktop/Lord of the Rings Network/Last")
library(igraph)
library(linkcomm)
library(poweRlaw)
source("PoweRlaw-Utility.R")
source("reify_link_communities.R")
source("new_window.R")
topnv <- function(graph, values, n=10) {
  return(V(graph)[sort.list(values, decreasing=TRUE)[1:n]])
}
############ Graphs
lotr <- read_graph("Lotr-raw.graphml", format="graphml")
V(lotr)$type <- V(lotr)$entry_type == "CHARACTER"
L.bp <- bipartite_projection(lotr)
L.cc <- L.bp$proj2
#Deletion of the characters with no scenes
L.cc <- delete_vertices(L.cc, which(V(L.cc)$Degree == 0))
#Powerlaw and scale free networks
L.cc.degs <- nonzero_degrees(L.cc)
L.cc.disexp <- initialize_disexp(L.cc.degs)
L.cc.dislnorm <- initialize_dislnorm(L.cc.degs)
L.cc.displ <- initialize_displ(L.cc.degs)
L.cc.dispois <- initialize_dispois(L.cc.degs)
plot_discrete_distributions("Lotr one-mode", L.cc.disexp, L.cc.dislnorm, L.cc.displ, L.cc.dispois)
lotr.degs <- nonzero_degrees(lotr)
lotr.disexp <- initialize_disexp(lotr.degs)
lotr.dislnorm <- initialize_dislnorm(lotr.degs)
lotr.displ <- initialize_displ(lotr.degs)
lotr.dispois <- initialize_dispois(lotr.degs)
plot_discrete_distributions("Lotr one-mode", lotr.disexp, lotr.dislnorm, lotr.displ, lotr.dispois)
#Global and Local transitivity
transitivity(L.cc, type = "global")
transitivity(L.cc, type = "localaverage")
#Mean distance
mean_distance(L.cc)
#Degree
L.cc.dd <- degree_distribution(L.cc)
plot(L.cc.dd, main="Lord of the Rings characters log-log degree dist", log="xy", xlab="k", ylab="p(k)")
lotr.dd <- degree_distribution(lotr)
plot(lotr.dd, main="Lord of the Rings bi-partite log-log degree dist", log="xy", xlab="k", ylab="p(k)")
V(L.cc)$i_degree <- degree(L.cc)
V(L.cc)$i_wdegree <- strength(L.cc)
V(L.cc)$eigen_centrality <- eigen_centrality(L.cc)$vector
V(L.cc)$page_rank <- page_rank(L.cc)$vector
V(L.cc)$authority <- authority_score(L.cc)$vector
V(L.cc)$hub <- hub_score(L.cc)$vector
V(L.cc)$betweenness <- betweenness(L.cc, normalized=TRUE)
V(L.cc)$closeness <- closeness(L.cc, normalized=TRUE)
V(lotr)$i_degree <- degree(lotr)
V(lotr)$i_indegree <- degree(lotr, mode="in")
V(lotr)$i_outdegree <- degree(lotr, mode="out")
V(lotr)$i_wdegree <- strength(lotr)
V(lotr)$i_windegree <- strength(lotr, mode="in")
V(lotr)$i_woutdegree <- strength(lotr, mode="out")
V(lotr)$eigen_centrality <- eigen_centrality(lotr)$vector
V(lotr)$page_rank <- page_rank(lotr)$vector
V(lotr)$authority <- authority_score(lotr)$vector
V(lotr)$hub <- hub_score(lotr)$vector
V(lotr)$betweenness <- betweenness(lotr, normalized=TRUE)
V(lotr)$closeness <- closeness(lotr, normalized=TRUE)
reciprocity(L.cc)
reciprocity(lotr)
lotr.rewired <- rewire(lotr, with = keeping_degseq(niter = ecount(lotr) * 100))
L.cc.rewired <- rewire(L.cc, with = keeping_degseq(niter = ecount(L.cc) * 100))
assortativity_degree(lotr)
assortativity_degree(lotr.rewired)
assortativity_degree(L.cc)
assortativity_degree(L.cc.rewired)
#max cliques
L.cc.mc <- max_cliques(L.cc)
lotr.mc <- max_cliques(as.undirected(lotr, mode="collapse"))
for (i in 1:length(L.cc.mc)) {for (v in L.cc.mc[[i]]) {V(L.cc)[[v]]$max_clique <- i}}
for (i in 1:length(lotr.mc)) {for (v in lotr.mc[[i]]) {V(lotr)[[v]]$max_clique <- i}}
L.cc.cl <- cluster_louvain(L.cc)
attributes(L.cc.cl)
modularity(L.cc.cl)
length(L.cc.cl)
V(L.cc)$com_louvain <- membership(L.cc.cl)
L.cc.ci <- cluster_louvain(L.cc)
modularity(L.cc.ci)
length(L.cc.ci)
V(L.cc)$com_infomap <- membership(L.cc.ci)
L.cc.edges <- as_edgelist(L.cc)
L.cc.lc <- getLinkCommunities(L.cc.edges, hcmethod = "average", directed =FALSE, plot=TRUE)
lotr.edges <- as_edgelist(lotr)
lotr.lc <- getLinkCommunities(lotr.edges, hcmethod = "average", directed =FALSE, plot=TRUE)
write_graph(L.cc.communities, "Lotr-cc-With-Link-Communities.graphml", format="graphml")
write_graph(lotr.communities, "Lotr-bp-With-Link-Communities.graphml", format="graphml")
write_graph(L.cc, "Lotr-cc.graphml", format="graphml")
write_graph(lotr, "Lotr-bp.graphml", format="graphml")
