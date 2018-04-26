library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(geosphere)

time_res <- 24 # hours
space_res <- 1000 # m

# fetch data

if (file.exists("data/occurrence.dat")) {
  load("data/occurrence.dat")  
} else {
  occurrence <- read_csv("data/OBIS_Leopold-Prik-Reubens.csv") %>%
    select(receiver, transmitter, time = datetime, id = id_pk, deployment = deployment_fk, scientificname = scientific_name, animal = animal_id_pk, lat = deployment_lat, lon = deployment_long) %>%
    filter(!is.na(lon) & !is.na(lat) & !is.na(time)) %>%
    arrange(animal, time)
  save(occurrence, file = "data/occurrence.dat")  
}

# data exploration

leaflet(occurrence %>% distinct(lon, lat)) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat)

ggplot(occurrence) + geom_line(aes(time, lat, group = animal, colour = factor(animal)))
ggsave("lines.png", width = 12, height = 8)

table(occurrence$deployment, occurrence$animal)

# process

groupings <- rep(NA, nrow(occurrence))
groupings[1] <- 1

assignGroups <- function() {
  current_grouping <- 1
  current_animal <- occurrence$animal[1]
  current_lat <- occurrence$lat[1]
  current_lon <- occurrence$lon[1]
  current_time <- occurrence$time[1]
  
  for (i in 2:nrow(occurrence)) {
    if (current_lon == occurrence$lon[i] & current_lat == occurrence$lat[i]) {
      distance <- 0      
    } else {
      distance <- distHaversine(c(current_lon, current_lat), c(occurrence$lon[i], occurrence$lat[i]))
    }
    if (current_time == occurrence$time[i]) {
      time <- 0
    } else {
      time <- difftime(current_time, occurrence$time[i], units = "hours")
    }
    if (occurrence$animal[i] != current_animal | distance > space_res | time > time_res) {
      message(i)
      current_grouping <- current_grouping + 1
      current_animal <- occurrence$animal[i]
      current_lat <- occurrence$lat[i]
      current_lon <- occurrence$lon[i]
      current_time <- occurrence$time[i]
    }
    groupings[i] <<- current_grouping
  }
  occurrence$grouping <<- groupings
}

assignGroups()

grouped <- occurrence %>%
  group_by(grouping) %>%
  summarize(
    animal = first(animal),
    time_arr = min(time),
    time_dep = max(time),
    lon = mean(lon),
    lat = mean(lat)
  )
save(grouped, file = "grouped.dat")

leaflet(grouped %>% distinct(lon, lat)) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat)

ggplot(grouped) + geom_line(aes(time_arr, lat, group = animal, colour = factor(animal)))
ggsave("lines_after.png", width = 12, height = 8)

