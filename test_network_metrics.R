library(dplyr)
library(igraph)
library(here)
library(stringr)
library(ggplot2)
library(scales)
library(networkD3)

graph_object <- readRDS(paste0(here("app", "leg_app"), "/graph_object.rds"))

title <- vertex_attr(graph_object)$name
year <- str_sub(title,-4,-1)
node_degree_all <- unname(degree(graph_object))
node_degree_in <- unname(degree(graph_object, mode = "in"))
node_degree_out <- unname(degree(graph_object, mode = "out"))
node_betweenness <- unname(betweenness(graph_object))

metrics <- data.frame(title, year, node_degree_all, node_degree_in, node_degree_out, node_betweenness) %>%
  mutate(label_degree_all = ifelse(node_degree_all >= 15, as.character(title), "")) %>%
  mutate(label_degree_in = ifelse(node_degree_in >= 12, as.character(title), "")) %>%
  mutate(label_degree_out = ifelse(node_degree_out >= 15, as.character(title), "")) %>%
  mutate(label_betweeness = ifelse(node_betweenness >= 2000, as.character(title), "")) %>%
  mutate(year = as.numeric(as.character(year)))

year_degree_all <- ggplot(data = metrics, aes(x = year, y = node_degree_all)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks = pretty_breaks(n = length(year)/10))

year_degree_all + 
  geom_label_repel(aes(label = label_degree_all),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') 

year_degree_in <- ggplot(data = metrics, aes(x = year, y = node_degree_in)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks = pretty_breaks(n = length(year)/10))

year_degree_in + 
  geom_label_repel(aes(label = label_degree_in),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') 

year_degree_out <- ggplot(data = metrics, aes(x = year, y = node_degree_out)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks = pretty_breaks(n = length(year)/10))

year_degree_out + 
  geom_label_repel(aes(label = label_degree_out),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') 

year_betweenness <- ggplot(data = metrics, aes(x = year, y = node_betweenness)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(breaks = pretty_breaks(n = length(year)/10))

year_betweenness + 
  geom_label_repel(aes(label = label_betweeness),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')

d3_object <- igraph_to_networkD3(graph_object)

links <- d3_object$links
nodes <- d3_object$nodes

d3_object$nodes <- d3_object$nodes %>%
  left_join(metrics, by = (c("name" = "title"))) %>%
  mutate(group = 1)

d3_object$links <- d3_object$links %>%
  mutate(value = 1)

forceNetwork(Links = d3_object$links, Nodes = d3_object$nodes, 
             Source = "source", Target = "target", Value = "value",
             NodeID = "name", Group = "group", fontSize = 30,
             arrows = TRUE, zoom = TRUE, opacityNoHover = 0, 
             Nodesize = "node_betweenness", opacity = 1, 
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), 
             charge = -3, legend = TRUE)
