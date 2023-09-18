# creates a SQLite database from within R

library(forceR)
library(RSQLite)
library(DBI)
library(readr)
library(tidyverse)

# create a new SQLite database, supply the filename to dbConnect():
db <- dbConnect(SQLite(), "BFDB")

# load overview
iBite <- read_csv("./tables/iBite_table.csv",
                  show_col_types = FALSE)


# load file with all data, select and rename columns
BF_measurements_raw <- read_csv("./tables/iBite_all_force_measurements_long.csv",
                                show_col_types = FALSE)

BF_measurements <- BF_measurements_raw %>% 
  select(-c(ID, specimen)) %>% 
  rename(measurement_ID = iBite) %>% 
  mutate(measurement_ID = paste0("iBite_", measurement_ID))

# test plot
BF_measurements %>% 
  filter(measurement_ID == "iBite_0007" |
           measurement_ID == "iBite_0008") %>% 
  ggplot(., aes(x = t, 
                y = force,
                col = as.factor(measurement_ID))) +
  geom_line()


# filter and ammend overview table
overview <- iBite %>%
  select(iBite, specimen, 
         order, family, genus, species,
         amplification,
         latitude, longitude, country,
         head.w, head.h, head.l, th.w, body.l, wing.l,
         max.bf.iBite) %>% 
  mutate(lever = 0.538,
         publication = 'Rühr et al. (2023): Scientific Data',
         publication_doi = "tba",
         project = "Insect Bite Force Database v1.0.0",
         uploaded_by = "P.T. Rühr",
         unit_x = "ms",
         unit_y = "N",
         setup = "forceX;forceR",
         repository = "Zenodo",
         repository_doi = "10.5281/zenodo.8183211") %>% 
  rename(y_max = max.bf.iBite,
         measurement_ID = iBite,
         specimen_ID = specimen,
         collection_lat = latitude,
         collection_long = longitude,
         collection_country = country,
         tax_order = order,
         tax_family = family,
         tax_genus = genus,
         tax_species = species,
         head_width = head.w, head_height = head.h, head_length = head.l, 
         thorax_width = th.w, body_length = body.l, wing_length = wing.l) %>% 
  group_by(measurement_ID) %>% 
  mutate(measurement_ID = paste0("iBite_", measurement_ID),
         specimen_ID = paste0("iBite_", specimen_ID),
         collection_lat = round(collection_lat, 6),
         collection_long = round(collection_long, 6),
         y_max = round(y_max, 3),
         tax_class = "Insecta",
         tax_phylum = "Arthropoda",
         method = "in_vivo")

overview$x_count <- NA
overview$step_size <- NA
overview$x_max <- NA
overview$measurement_date <- "NULL"
overview$collected_by <- "NULL"
overview$measured_by <- "NULL"
overview$identified_by <- "NULL"
overview$museum_ID <- "NULL"
overview$permit_ID <- "NULL"
overview$tax_cf <- "NULL"
overview$tax_authority <- "NULL"
overview$notes <- "NULL"

overview$collection_lat[is.na(overview$collection_lat)] <- "\\N"
overview$collection_long[is.na(overview$collection_long)] <- "\\N"


# # here! filter measurements by first 10 measurements in measurements
# BF_measurements <- BF_measurements %>% 
#   filter(measurement_ID %in% unique(BF_measurements$measurement_ID)[1:10])
# # here! filter overview by first 10 measurements in measurements
# overview <- overview %>% 
#   filter(measurement_ID %in% unique(BF_measurements$measurement_ID))


# amend overview with data from measurements
i=1
for (i in 1:nrow(overview)) { # 1:nrow(overview)
  curr_measurement_ID = overview$measurement_ID[i]
  curr_measurement <- BF_measurements %>%
    filter(measurement_ID == curr_measurement_ID)
  overview$x_count[i] <- curr_measurement %>%
    nrow()
  overview$step_size[i] <- curr_measurement$t[2] - curr_measurement$t[1]
  overview$x_max[i] <- max(curr_measurement$t)
  if(overview$collection_country[overview$measurement_ID == curr_measurement_ID] == "Slovenia"){
    overview$collected_by[i] <- "M. Frenzel"
    overview$measured_by[i] <- "M. Frenzel"
  } else if(overview$collection_country[overview$measurement_ID == curr_measurement_ID] == "Panama"){
    overview$collected_by[i] <- "C. Edel"
    overview$measured_by[i] <- "C. Edel"
  } else if(overview$collection_country[overview$measurement_ID == curr_measurement_ID] == "Greece:Crete" |
            overview$collection_country[overview$measurement_ID == curr_measurement_ID] == "France:Corsica"){
    overview$collected_by[i] <- "A. Blanke"
    overview$measured_by[i] <- "A. Blanke"
  } else{
    overview$collected_by[i] <- "P.T. Rühr"
    overview$measured_by[i] <- "P.T. Rühr"
  }
  print_progress(i,nrow(overview))
}

# # for now: choose columns that contain data - fill further columns in the future
# colnames(overview)

# replace _ with . in species epithets
overview$tax_species <- gsub("_", ".", overview$tax_species)

# add genus_species to overview
overview$tax_genus_species <- paste(overview$tax_genus, overview$tax_species, sep = "_")

# copy an R data frame into a SQLite database with dbWriteTable():
# dbRemoveTable(db, "overview")
# dbRemoveTable(db, "measurements")
dbWriteTable(db, "overview", overview, overwrite = TRUE)
dbWriteTable(db, "measurements", BF_measurements, overwrite = TRUE)
dbListTables(db)

# Issue a query with dbGetQuery(): (escape with "")

dbGetQuery(db, 'SELECT * FROM overview LIMIT 5')
dbGetQuery(db, 'SELECT * FROM measurements LIMIT 5')

dbDisconnect(db)
