library(DBI)
library(RSQLite)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(viridisLite)
library(scales)
library(dplyr)
# setwd("C:/tmp/BFDB/BFDB_app/BFDB")

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "data", directoryPath = "./data/")
addResourcePath(prefix = "images", directoryPath = "./images/")

overiew <<- NULL
measurements <<-NULL
map.data <<- NULL
iso_codes <<- NULL

readData <<- function(session) {
  progress <<- Progress$new(session)
  progress$set(value = 0.1, message = 'Loading BF databank...')
  
  # databank loading
  db <- dbConnect(SQLite(), dbname = "./data/BFDB_sql")
  
  overview <<- dbReadTable(db, "overview") %>% 
    as_tibble() %>% 
    rename(max_F_specimen = y_max) %>% 
    mutate(tax_class = factor(tax_class, levels = c("Insecta", "Reptilia", "Dinosauria", "Aves", "Mammalia")))
  
  measurements <<- dbReadTable(db, "measurements") %>% 
    as_tibble()
  
  dbDisconnect(db)
  
  # regression table reduction
  progress$set(value = 0.33, message = 'Creating Regression Data...')
  regression.data <<- overview %>% 
    group_by(tax_genus_species) %>% 
    arrange(desc(max_F_specimen)) %>% 
    slice(1) %>% 
    rename(max_F_species = max_F_specimen)
  
  # Map creation
  progress$set(value = 0.66, message = 'Creating Map...')
  
  iso_codes <- read.csv("./data/iso_codes.csv")
  
  map.data <<- overview %>%
    distinct(tax_genus_species, .keep_all = TRUE) %>% 
    select(collection_country, collection_lat, collection_long) %>%
    group_by(collection_country) %>%
    summarise(n = n()) %>%
    rename(country = collection_country)%>%
    mutate(log_n = log(n),
           country = gsub(":Corsica", "", country),
           country = gsub(":Crete", "", country)) %>% 
    left_join(iso_codes %>%
                select(country, ISO3))
  
  progress$close()
}



# User interface ----
ui<<- navbarPage(windowTitle = "BFDB: Bite Force Database",
                 tags$script(HTML("var header = $('.navbar > .container-fluid');
  header.append('<div style=\"float:left\"><img src=\"images/logo_header.svg\" alt=\"alt\" style=\"float:left;height:75px;padding-top:1px;\"></div>');
      console.log(header)")),
                 
                 tags$head(tags$link(rel = "icon", type = "image/svg", sizes = "16x16", href = "images/logo_header.svg")#,
                           # tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                           #  padding-top:4px !important;
                           #  padding-bottom:0 !important;
                           #  height: 25px;
                           #  }
                           # .navbar {min-height:25px !important;}'))
                 ),
                 
                 tabPanel(span("Home", style="color:#749716ff"),
                          helpText(
                            h3("BFDB - The Bite Force Database."),
                            p(),
                            p("This databank is intended to keep track of the bite force data published in scientific journals. We also plan to add other closing force data, such as pinching forces of scorpion or crab pincers."),
                            p("Also, choosing data and grouping coloration in regression plots is not yet interactive, but under develpment."),
                            p(),
                            p("If you want your data to be added to this database or have other inquiries, please get in contact: peter.ruehr[at]gmaicom."),
                            p(),
                            p("Data from the following publications is already added to the database:"),
                            HTML("<ul>
                            <li>Rühr et al. (<strong>accepted</strong>): A bite force database of 654 insect species. <em>Scientific Data</em></li>
                            <li>Isip et al. (<strong>2022</strong>): Clade-wide variation in bite-force performance is determined primarily by size, not ecology. <em>Proceedings of the Royal Society B</em></li>
                            <li>David et al. (<strong>2016</strong>): Musculoskeletal modelling of the dragonfly mandible system as an aid to understanding the role of single muscles in an evolutionary context. <em>Journal of Experimental Biology</em></li>
                            <li>David et al. (<strong>2016</strong>): Musculoskeletal modelling under an evolutionary perspective: deciphering the role of single muscle regions in closely related insects. <em>Journal of The Royal Society Interface</em></li>
                            <li>Weihmann et al. (<strong>2015</strong>): Fast and Powerful: Biomechanics and Bite Forces of the Mandibles in the American Cockroach <em>Periplaneta americana</em>. <em>PLoS ONE</em></li>
                            <li>Goyens et al. (<strong>2014</strong>): Biomechanical determinants of bite force dimorphism in <i>Cyclommatus metallifer</i> stag beetles. <em>The Journal of Experimental Biology</em></li>
                            <li>Wheater & Evans (<strong>1989</strong>): The mandibular forces and pressures of some predacious Coleoptera. <em>Journal of Insect Physiology</em></li>
                          </ul>"),
                            HTML("Note that Sakamoto et al. (<strong>2019</strong>) and Isip et al. (<strong>2022</strong>) compiled data from many previos studies, which have not yet been separated in this database and thus do not appear as list entries for the Map."),
                          )
                 ),
                 
                 # BFDB data -----------------------------------------------------------------
                 tabPanel(span("Data", style="color:#749716ff"),
                          p("Bite Force Data base entries. Download buttons are provided below (clipboard, csv, or Excel)."),
                          p(),
                          div(dataTableOutput("table_print_BFDB"), style = "font-size:80%"),
                 ),
                 
                 
                 # Regressions -----------------------------------------------------------------
                 tabPanel(span("Regressions", style="color:#749716ff"),
                          p("The following plots contain regressions of the max. bite force per species over their body length and head width, coloured by taxonomic orders."),
                          p("Note that we chose body length and head width as predictors here since they are widely available throughout many publications."),
                          plotOutput("regression_BL", click = "plot_click", ),
                          p(),
                          plotOutput("regression_HW", click = "plot_click", )
                          
                          
                 ),
                 
                 # Map -----------------------------------------------------------------
                 tabPanel(span("Map", style="color:#749716ff"),
                          p("The following map indicated where the species measured were collected."),
                          p("Note that all specimens gathered from breeding cultures are not represented in the map, and that many publications do not specify where the specimens were collected. Also, Sakamoto et al. (<strong>2019</strong>) and Isip et al. (<strong>2022</strong>) compiled data from many previos studies, which have not yet been separated in this database and thus do not appear as list entries for the Map"),
                          plotlyOutput("map_plot", height = "800px")), #  width = "1600px", height = "800px"
                 
                 tabPanel(span("Imprint/Contact", style="color:#749716ff"),
                          helpText(
                            p(),
                            h4("Site Notice"),
                            HTML("<small>Information provided according to Sec. 5 German Telemedia Act (TMG)<br>
Peter Rühr<br>
Mainzer Strasse 16<br>
D-50678 Cologne</small>"),
                            
                            h4("Contact"),
                            HTML("<small>Telephone: +49 172 707 38 57<br>
Email: peter.ruehr@gmail.com</small>"),
                            
                            h4("Preferred mention for citations: "),
                            HTML("<small>Rühr et al. (<strong>accepted</strong>): A bite force database of 654 insect species. <em>Scientific Data</em>.</small>"),
                            p(),
                            HTML("<small><em>BFDB. The Bite Force Database</em> (<strong>2023)</strong>. Retrieved &lt;yyyy&#92;mm&#92;dd&gt; from https://peter-t-ruehr.shinyapps.io/BFDB/.</small>"),
                            
                            h4("Dispute Resolution"),
                            HTML("<small>The European Commission provides a platform for online dispute resolution (OS): https://ec.europa.eu/consumers/odr. Please find our email in the impressum/legal notice.
We do not take part in online dispute resolutions at consumer arbitration boards.</small>"),
                            
                            h4("Liability for Contents"),
                            HTML("<small>As service providers, we are liable for own contents of these websites according to Sec. 7, paragraph 1 German Telemedia Act (TMG). However, according to Sec. 8 to 10 German Telemedia Act (TMG), service providers are not obligated to permanently monitor submitted or stored information or to search for evidences that indicate illegal activities.<br>
Legal obligations to removing information or to blocking the use of information remain unchallenged. In this case, liability is only possible at the time of knowledge about a specific violation of law. Illegal contents will be removed immediately at the time we get knowledge of them.</small>"),
                            
                            h4("Liability for Links"),
                            HTML("<small>Our offer includes links to external third party websites. We have no influence on the contents of those websites, therefore we cannot guarantee for those contents. Providers or administrators of linked websites are always responsible for their own contents.<br>
The linked websites had been checked for possible violations of law at the time of the establishment of the link. Illegal contents were not detected at the time of the linking. A permanent monitoring of the contents of linked websites cannot be imposed without reasonable indications that there has been a violation of law. Illegal links will be removed immediately at the time we get knowledge of them.</small>"),
                            
                            h4("Copyright"),
                            HTML("<small>Contents and compilations published on these websites by the providers are subject to German copyright laws. Reproduction, editing, distribution as well as the use of any kind outside the scope of the copyright law require a written permission of the author or originator. Downloads and copies of these websites are permitted for private use only. The commercial use of our contents without permission of the originator is prohibited.<br>
Copyright laws of third parties are respected as long as the contents on these websites do not originate from the provider. Contributions of third parties on this site are indicated as such. However, if you notice any violations of copyright law, please inform us. Such contents will be removed immediately.</small>")
                          )
                 )
)

# Server logic ------------------------------------------------------------
server <-  function(input, output, session) {
  if(is.null(measurements)){
    readData(session)
  }
  
  # all BFDB data -------------------------------------------------------
  output$table_print_BFDB <-  renderDataTable({df <-  reactiveVal(overview)
  overview},
  
  extensions = 'Buttons',
  
  options = list(pageLength = 10000,
                 searching = FALSE,
                 lengthChange = FALSE,
                 dom = 'tB',
                 autoWidth = TRUE,
                 buttons = c('copy', 'csv', 'excel')),
  rownames= FALSE)
  
  
  # REGRESSIONS ---------------------------------------------------------------------
  output$regression_BL <-  renderPlot({
    p1 <- ggplot(data = regression.data, aes(x = body_length,
                                             y = max_F_species, 
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
  })
  
  output$regression_HW <-  renderPlot({
    p1 <- ggplot(data = regression.data, aes(x = head_width,
                                             y = max_F_species, 
                                             col = as.factor(tax_class))) +
      geom_point(cex = 1.0, pch = 16) +
      stat_smooth(method = "lm", alpha = 0.25) +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(y = bquote("bite force [N]"), x = bquote("head width [mm]")) +
      theme_bw()  +
      annotation_logticks() 
    
    # p1
    p1.body.l.hist.regular <- ggExtra::ggMarginal(p1, type = "histogram")
    p1.body.l.hist.regular
  })
  
  
  # MAP ---------------------------------------------------------------------
  output$map_plot <-  renderPlotly({ # renderPlot
    fig_map <-  plot_ly(map.data, type='choropleth', 
                        locations=map.data$ISO3, 
                        z=map.data$log_n, 
                        colorscale="Viridis",
                        hovertemplate = paste0(map.data$country, ": ", map.data$n, " species."))
    
    
    fig_map %>% 
      hide_colorbar() # %>%
    # layout(title = 'Map of Publications per country') 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)