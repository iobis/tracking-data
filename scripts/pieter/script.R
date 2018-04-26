unloadNamespace("ziptrack");library(ziptrack)
library(readr)
library(dplyr)
library(leaflet)
library(ggplot2)

occ <- read_csv("../../data/OBIS_Leopold-Prik-Reubens.csv") %>%
  select(eventDate = datetime, organismID = animal_id_pk, decimalLongitude = deployment_long, decimalLatitude = deployment_lat) %>%
  arrange(organismID, eventDate)

binned <- bin_tracking_data(occ)

leaflet(binned %>% distinct(decimalLongitude, decimalLatitude)) %>% addTiles() %>%
  addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude)

binned %>% group_by(organismID) %>% summarize(detections = sum(detections)) %>% arrange(desc(detections))

ggplot(binned %>% filter(organismID %in% c(404, 459, 409, 392, 476, 410))) +
  geom_segment(aes(x = eventDateArrival, xend = eventDateDeparture, y = decimalLongitude, yend = decimalLongitude, colour = factor(organismID)), size = 2) +
  scale_colour_brewer(palette = "Paired")
ggsave("time_binned.png", width = 12, height = 8)

ggplot(occ %>% filter(organismID %in% c(404, 459, 409, 392, 476, 410))) +
  geom_segment(aes(x = eventDateArrival, xend = eventDateDeparture, y = decimalLongitude, yend = decimalLongitude, colour = factor(organismID)), size = 2) +
  scale_colour_brewer(palette = "Paired")
ggsave("time_original.png", width = 12, height = 8)
