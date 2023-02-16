# Code for world map with country origin of the articles studied 
# within the quantitative part of the paper 


# Acknowledgment to Dr. Selina Baldauf for 
# help with this code for map construction

# Required packages:

library(tidyverse)
library(rworldmap)
library(rgeos)
library(readxl)
library(janitor)
library(rgdal)
library(ggrepel)
library(paletteer)
library(ggplot2)

# Step 1: Get world map data and calculate centroids of each country -----------
# This is then used to plot the points in the map in the middle of each country

# 1.1 get simple world map
wmap <- rworldmap::getMap(resolution = "high")

# 1.2 calculate centroids and transform to a tibble
centroids <- rgeos::gCentroid(wmap, byid = TRUE) 

# 1.3 centroids as tibble
df_centers <- as.data.frame(centroids) |>
  rownames_to_column(var = "country") |>
  as_tibble()

# Step 2: Get the study data -------------------------------------------------
# This is used to summarize the number of studies by country

# 2.1 read the excel file
data <- read_csv("Data_Ecol_Intensf_Multitroph_BEF.csv")

# Filter papers that are relevant for our search criteria
site_info <- data%>% 
  filter(Relevant=="Yes")
str(site_info)
names(site_info)

site_info

# 2.2 clean column headers (only necessary if they are not nice)
site_info <- janitor::clean_names(site_info)

# 2.3 select relevant columns (mainly country column and paper ID is needed here)
site_info <- site_info |> select(paper_number, country_broad)

# 2.4 select unique papers (only needed if there are duplicate paper IDs)
site_info_unique <- site_info |>
  distinct() |>
  rename(country = country_broad) # rename country column

# 2.5 check the unique countries (just to see if it makes sense)
site_info_unique |>
  pull(country) |>
  unique()


# Step 3: Prepare study data for plotting -----------------------------------

# 3.1 make separate tables for studies with known and unknown countries
site_info_unknown <- site_info_unique |> filter(
  is.na(country) | country == "International"
)
site_info_known <- site_info_unique |> filter(
  !(
    is.na(country) | country == "International"
  )
)

# 3.2 rename some countries to find them in the centroids data frame
site_info_known <- site_info_known |>
  mutate(country = case_when(
    country == "England" ~ "United Kingdom",
    country == "USA" ~ "United States of America",
    TRUE ~ country
  ))

# 3.3 count number of studies by country
study_no_known <- site_info_known |>
  count(country)

study_no_unknown <- site_info_unknown |>
  count(country)

# 3.4 Merge centroids and study data
study_no_known <- merge(study_no_known, df_centers, by = "country", all.x = TRUE)

# 3.5 Check if there are values missing (in our case La Reunion)
study_no_known

# 3.6 add Reunion coordinates by hand
study_no_known[study_no_known$country == "Reunion Island", ]$y <- -21.230135
study_no_known[study_no_known$country == "Reunion Island", ]$x <- 55.315722


# 3.7 Create column with study number in brackets (for the plot labels)
study_no_known <- study_no_known |> 
  mutate(
    label_detail = paste0(country, " (", n, ")")
  )

# Step 4: Make the plot ------------------------------------------------------
# 4.0 get the continent data (only needs to be done once to save json on machine)
# You don't need to do this because the file is already in the project
# url <- "https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/f3fde312c9b816dff3994f39f2bcda03209eff8f/continents.json"
# httr::stop_for_status(httr::GET(url, httr::write_disk("continents.json")))

# 4.1 read the continent shapes
continents <- rgdal::readOGR("continents.json")
continents_map <- fortify(continents, region = "CONTINENT")

# 4.2 remove Antarctica
continents_map <- continents_map |> filter(id != "Antarctica")

# 4.3 Make the plot
map <- ggplot() +
  # make the basic map
  geom_map(
    data = continents_map,
    map = continents_map,
    aes(x = long, y = lat, map_id = id),
    color = "black", fill = "white",size=0.1
  ) +
  # add the points with the study number as color
  geom_point(data = study_no_known, aes(x = x, y = y, color = n), size = 2) +
  # add the labels with country and number of study in brackets
  ggrepel::geom_label_repel(
    data = study_no_known, aes(x = x, y = y, label = label_detail),
    nudge_x = .15,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf,
    size = 2.5
  ) +
  paletteer::scale_color_paletteer_c(palette = "ggthemes::Classic Orange-Blue", direction = -1,
                                     name = "Number of studies",
                                     guide = guide_colorbar(
                                       title.position = "top",
                                       direction = "horizontal"
                                     )) +
  theme_void() +
  theme(
    legend.position = c(0.5,0.1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )#+coord_fixed()

map

# 4.4 Save the plot
ggsave(
  paste0("map.png"),
  map,
  width = 16,
  height = 9,
  units = "cm",
  scale = 1.3
)
