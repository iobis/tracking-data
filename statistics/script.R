# requires robis version 1.0.0-9003!

library(robis)
library(ggplot2)
library(dplyr)

# load data

datasets <- dataset(q = "tracking")

if (!file.exists("occurrence.dat")) {
  occ <- occurrence(resourceid = datasets$id, fields = c("decimalLongitude", "decimalLatitude", "species", "resourceID", "datasetName", "yearcollected"))
  save(occ, file = "occurrence.dat")
} else {
  load("occurrence.dat")
}

# add node details

occ <- occ %>% left_join(datasets %>% select(resourceID = id, node_id, node_name), by = "resourceID")

# map

ggplot() +
  borders("world", colour = "gray80", fill = "gray80") +
  geom_point(data = occ, aes(x = decimalLongitude, y = decimalLatitude, fill = node_name), stroke = 0, alpha = 1, shape = 21, colour = "white", size = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  coord_quickmap() +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 3)))

ggsave("map.png", width = 20, height = 10, dpi = 300)

# records over time

ggplot(occ) + geom_bar(aes(x = yearcollected, fill = node_name), stat = "bin", binwidth = 1) + scale_fill_brewer(palette = "Set1")
ggsave("barplot.png", width = 12, height = 8)

# species stats

species <- occ %>% group_by(species) %>% summarize(records = n(), datasets = length(unique(resourceID))) %>% arrange(desc(records))
write.csv(species, file = "species.csv", row.names = FALSE)
