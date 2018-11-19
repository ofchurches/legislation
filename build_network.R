library(here)
library(tidyverse)
library(stringr)
library(igraph)
library(networkD3)

#Start here if you want to avoid creating the data frame. Load the data frame.
acts_df <- readRDS(paste0(here("derived_data"), "/acts_df.rds"))

#There are a few different approaches the could be taken to pulling the act titles from the act text.
#1) Find the longest possible act title then regex the pattern: "the", longest title length, "four digits". This picked up a lot of non-cases (not act titles)
#
#check the longest title we'll have to budget for
# acts_df <- acts_df %>% 
#   mutate(length_title = str_length(acts_list)) %>%
#   mutate(word_length_title = str_count(acts_list, "\\S+"))
# 
# longest_title_plus <- max(acts_df$length_title) + 10
# longest_title_words <- max(acts_df$word_length_title)

#This is the super serious bit of regex:    (?i)(?<=the)(.{0,100})(\d{4})   put it into https://regex101.com/ to see how it works

# acts_df <- acts_df %>%
#   mutate(mentions = str_extract_all(acts_content, paste0("(?i)(the)(.{0,", longest_title_plus, "})(\\d{4})"))) %>% #all matches to, any case, "the", between 0 and longest_title_plus characters, four digits 
#   unnest(mentions)

#2) Find the pattern "capital letter start of word", repeated any number of times, "four digits". This doesn't work, picks up dates eg, "July 2017".
#   mutate(mentions_2 = str_extract_all(mentions, "(\\b[A-Z][a-z]*\\b\\s*)+\\d{4}")) %>% #all matches to start of a word a capital letter, any number of lower case letters end of a word, any amount of white space, all one or more times, four digits
#   unnest(mentions_2) %>%
#   mutate(mentions_3 = str_replace_all(mentions_2, "\\s", " ")) %>% #all spaces are just one space
#   mutate(mentions_4 = str_replace_all(mentions_3, "^[ \t]+", "")) %>%
#   mutate(mentions_5 = if_else(str_detect()))

#3) Make all white space one space. Then find instances of the titles of act in each of the other acts.

acts_mentions <- acts_df %>%
  mutate(acts_content_2 = str_replace_all(acts_content, "\\s+", " ")) %>% #all spaces are just one space (otherwise they won't match the titles)
  mutate(mentions = str_extract_all(acts_content_2, paste(as.character(acts_list), collapse="|"))) %>%
  unnest(mentions)

#Start here if you want to avoid creating the data frame. Load the data frame.
acts_mentions <- readRDS(paste0(here("derived_data"), "/acts_mentions.rds"))

nodes <- acts_mentions %>%
  select(acts_list) %>%
  unique() %>%
  mutate(id = row_number()) %>%
  rename(label = acts_list)

edges <- acts_mentions %>%
  select(c(acts_list, mentions)) %>%
  rename(label = acts_list) %>%
  left_join(nodes, by = "label") %>% #bring in the source id
  rename(source_label = label) %>% 
  rename(label = mentions) %>%
  left_join(nodes, by = "label") %>% #bring in the target id
  rename(target_label = label) %>%
  rename(from = id.x) %>%
  rename(to = id.y) %>%
  filter(from != to) %>% #remove self loops
  group_by(from) %>%
  mutate(value = n()) %>% #value is the degree
  ungroup()

#use igraph to build the object
edges_graph <- edges %>%
  select(source_label, target_label) %>%
  rename(from = source_label) %>%
  rename(to = target_label)

graph_object <- graph_from_data_frame(edges_graph, directed=TRUE)

#with networkD3
d3_object <- igraph_to_networkD3(graph_object)

saveRDS(d3_object, paste0(here("app", "leg_app"), "/d3_object.rds"))
