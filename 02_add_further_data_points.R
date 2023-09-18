# creates a SQLite database from within R

# Read excel files
library(readxl)
library(forceR)
library(RSQLite)
library(DBI)
library(readr)
library(tidyverse)

# multiple string split multiple str_split
# strings between pattern, e.g. undersocres 
# synonymous of get.sting.between.pattern
str_split_mult <- function(x, pattern, position = 1){
  sapply(strsplit(x, pattern), `[`, position)
}

# create a new SQLite database, supply the filename to dbConnect():
db <- dbConnect(SQLite(), "BFDB")

# load Isip 2022
BF_Isip <- read_csv("./tables/Isip 2022 - max_bf.csv",
                    show_col_types = FALSE)

# load Sakamoto 2019
BF_Sakamoto <- read_excel("./tables/Sakamoto 2019 - rspb20181932supp2.xlsx",
                          sheet = 2)

# load external insect bite force data
BF_data_external <- read_csv("./tables/external_data.csv",
                             show_col_types = FALSE)


# load BFDB
db <- dbConnect(SQLite(), "BFDB")

overview <<- dbReadTable(db, "overview") %>% 
  as_tibble()

# remove all non-Rühr-entries
overview <- overview %>% 
  filter(!is.na(collected_by))
# dbDisconnect(db)

# combine external data tables to fit overview
colnames(overview)

colnames(overview)[colnames(overview) %in% colnames(BF_Isip) == FALSE]

# ISIP
BF_Isip_to_join <- BF_Isip %>% 
  select(BinomialReptileDatabase, Family, max_bf, 
         max_svl, max_hl, max_hw, max_hh) %>%  # Sex
  mutate(tax_genus = str_split_mult(BinomialReptileDatabase, 
                                    pattern = " ",
                                    position = 1),
         tax_species = str_split_mult(BinomialReptileDatabase, 
                                      pattern = " ",
                                      position = 2),
         method = "in_vivo",
         tax_class = "Reptilia",
         publication = 'Isip et al. (2022): Proceedings of the Royal Society B',
         publication_doi = "https://doi.org/10.1098/rspb.2021.2493",) %>% 
  rename(tax_genus_species = BinomialReptileDatabase,
         # sex = Sex,
         tax_family = Family,
         head_width = max_hw,
         head_length = max_hl,
         head_height = max_hh,
         body_length = max_svl,
         y_max = max_bf) %>% 
  mutate(tax_genus_species = gsub(" ", "_", tax_genus_species),
         head_width = head_width*10,
         head_length = head_length*10,
         head_height = head_height*10,
         body_length = body_length*10)

missing_columns <- colnames(overview)[colnames(overview) %in% colnames(BF_Isip_to_join) == FALSE] %>% print()


# SAKAMOTO
BF_Sakamoto_to_join <- BF_Sakamoto %>% 
  rename(tax_genus_species = Taxon,
         specimen = Individual,
         y_max = F_Bite2,
         head_width = W_Sk,
         head_length = L_Sk,
         head_height = H_Sk,
         body_length = SVL,
         group = Group5,
         method = BiteType) %>% 
  mutate(y_max = as.numeric(y_max),
         head_width = as.numeric(head_width)*10,
         head_length = as.numeric(head_length)*10,
         head_height = as.numeric(head_height)*10,
         body_length = as.numeric(body_length)*10,
         group = gsub("Mammals", "Mammalia", group),
         group = gsub("Reptiles", "Reptilia", group),
         group = gsub("Finches", "Aves", group),
         group = gsub("Bats", "Mammals", group),
         group = gsub("Dinosaurs", "Dinosauria", group),
         publication = 'Sakamoto et al. (2019): Proceedings of the Royal Society B',
         publication_doi = "https://doi.org/10.1098/rspb.2021.2493",
         tax_genus = str_split_mult(tax_genus_species, pattern = "_", position = 1),
         tax_species = str_split_mult(tax_genus_species, pattern = "_", position = 2)) %>% 
  filter(tax_genus_species != "FAM_62192") %>% 
  rename(tax_class = group) %>% 
  mutate(tax_class = gsub("Mammals", "Mammalia", tax_class))

BF_Sakamoto_to_join_red <- BF_Sakamoto_to_join[, colnames(BF_Sakamoto_to_join) %in% colnames(overview)]


# external insects
BF_data_external_to_join <- BF_data_external %>% 
  rename(tax_genus = genus_new,
         tax_species = species_new,
         y_max = bite_force,
         head_width = head.w,
         head_length = head.l,
         head_height = head.h,
         body_length = body.l,
         publication = source) %>% 
  mutate(y_max = as.numeric(y_max),
         head_width = as.numeric(head_width),
         head_length = as.numeric(head_length),
         head_height = as.numeric(head_height),
         body_length = as.numeric(body_length),
         publication_doi = "tba",
         tax_genus_species = paste(tax_genus, tax_species, collapse = "_"),
         tax_class = "Insecta",
         method = "in_vivo",)

BF_data_external_to_join_red <- BF_data_external_to_join[, colnames(BF_data_external_to_join) %in% colnames(overview)]


# add new data to overview
overview <- overview %>% 
  add_row(BF_Isip_to_join)
overview <- overview %>% 
  add_row(BF_Sakamoto_to_join_red)
overview <- overview %>% 
  add_row(BF_data_external_to_join_red)

# replace _ with . in species epithets
overview$tax_species <- gsub("_", ".", overview$tax_species)

# add genus_species to overview
overview$tax_genus_species <- paste(overview$tax_genus, overview$tax_species, sep = "_")

# copy an R data frame into a SQLite database with dbWriteTable():
# dbRemoveTable(db, "overview")
# dbRemoveTable(db, "measurements")
dbWriteTable(db, "overview", overview, overwrite = TRUE)
dbListTables(db)

# Issue a query with dbGetQuery(): (escape with "")

dbGetQuery(db, 'SELECT * FROM overview LIMIT 5')
dbGetQuery(db, 'SELECT * FROM measurements LIMIT 5')

library(scales)
p1 <- ggplot(data = overview, aes(x = body_length,
                                         y = y_max, 
                                         col = as.factor(tax_class))) +
  geom_point(cex = 1.0, pch = 16) +
  stat_smooth(method = "lm", alpha = 0.25) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("body length [mm]")) +
  theme_bw()  +
  annotation_logticks() 

# p1
p1.body.l.hist.regular <- ggExtra::ggMarginal(p1, type = "histogram")
p1.body.l.hist.regular

p1 <- ggplot(data = overview, aes(x = head_width,
                                  y = y_max, 
                                  col = as.factor(tax_class))) +
  geom_point(cex = 1.0, pch = 16) +
  stat_smooth(method = "lm", alpha = 0.25) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("head width  [mm]")) +
  theme_bw()  +
  annotation_logticks() 

# p1
p1.body.l.hist.regular <- ggExtra::ggMarginal(p1, type = "histogram")
p1.body.l.hist.regular

dbDisconnect(db)
