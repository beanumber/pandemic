library(tidyverse)
library(igraph)

url <- "https://files.indicatrix.org/pandemic.graphml"

g <- igraph::read_graph(url, format = "graphml")

V(g)$color <- rgb(V(g)$r, V(g)$g, V(g)$b, max = 255)
V(g)$color[V(g)$color == "#A9B74D"] <- "#F2FF00"

g_pandemic <- g

save(g_pandemic, file = "data/g_pandemic.rda", compress = "xz")

V(g)$pr <- page_rank(g)$vector
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$eccen <- eccentricity(g)

vg <- igraph::as_data_frame(g, "vertices") 

vg %>%
  arrange(desc(eccen))

ggplot(vg, aes(x = eccen, y = pr, color = color)) + 
  geom_point() +
  ggrepel::geom_label_repel(aes(label = label))

graph_group <- function(g, color) {
  induced_subgraph(g, V(g)$color == color)
}

regions <- lapply(unique(V(g)$color), graph_group, g = g) 
lapply(regions, diameter)

lapply(regions, edge_density)
