# http://shanghai.hosting.nyu.edu/data/r/case-3-sql-shiny.html
# https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html
# https://www.sqltutorial.org/sql-sample-database/

library(DBI)
library(RSQLite)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(forecast)
library(ggplot2)
library(plotly)
library(maps)
library(leaflet)

db <- dbConnect(SQLite(), dbname = "BFDF")
dbListTables(db)

# get overview for each table
rs_overview <- dbGetQuery(db, 'SELECT * FROM overview LIMIT 5')
rs_measurements <- dbGetQuery(db, 'SELECT * FROM measurements LIMIT 5')

# # tests
# colnames(rs_overview)
# colnames(rs_measurements)
# 
dbGetQuery(
    conn = db,
    statement =
      'SELECT *
              FROM overview
              ORDER BY y_max DESC' )

# test
dbGetQuery(
  conn = db,
  statement =
    'SELECT measurement_ID, tax_phylum, tax_order, tax_genus, tax_species,
                  y_max, unit_y collection_country
            FROM overview
            WHERE collected_by = "P.T. Rühr"
            ORDER BY y_max DESC
            LIMIT 50' )


classes <- dbGetQuery(db, 'SELECT distinct tax_class from overview')
classes

orders <- dbGetQuery(db, 'SELECT distinct tax_order from overview')
orders

genera <- dbGetQuery(db, 'SELECT distinct tax_genus from overview')
genera

species <- dbGetQuery(db, 'SELECT distinct tax_species from overview')
species

genera_species <- dbGetQuery(db, 'SELECT distinct tax_genus_species from overview')
genera_species

countries <- dbGetQuery(db, 'SELECT distinct collection_country from overview')
countries

dbDisconnect(db)
