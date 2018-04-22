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

# map

ggplot() +
  borders("world", colour = "gray80", fill = "gray80") +
  geom_point(data = occ, aes(x = decimalLongitude, y = decimalLatitude), stroke = 0, alpha = 1, shape = 21, fill = "#cc3300", colour = "white", size = 0.5) +
  coord_quickmap() +
  theme(
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("map.png", width = 20, height = 10, dpi = 300)

# records over time

ggplot(occ) + geom_bar(aes(x = yearcollected), stat = "bin", binwidth = 1)
ggsave("barplot.png", width = 12, height = 8)

# species stats

species <- occ %>% group_by(species) %>% summarize(records = n(), datasets = length(unique(resourceID))) %>% arrange(desc(records))
write.csv(species, file = "species.csv", row.names = FALSE)
