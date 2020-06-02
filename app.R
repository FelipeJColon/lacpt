##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
##
## ############################################################################
##
## DISCLAIMER: 
## This script has been developed for research purposes only. 
## The script is provided without any warranty of any kind, either express or 
## implied. The entire risk arising out of the use or performance of the sample
## script and documentation remains with you. 
## In no event shall LSHTM, its author, or anyone else involved in the 
## creation, production, or delivery of the script be liable for any damages 
## whatsoever (including, without limitation, damages for loss of business 
## profits, business interruption, loss of business information, or other 
## pecuniary loss) arising out of the use of or inability to use the sample
## scripts or documentation, even if LSHTM has been advised of the
## possibility of such damages. 
##
## ############################################################################
##
## DESCRIPTION
## Shiny App for the visualisation of local area COVID19 data
##
## Version control: GitHub
## Initially created on 22 April 2020
##
##
## Written by: Oliver Brady, Paul Mee, Felipe J Colon-Gonzalez and
## Neal Alexander
## For any problems with this code, please contact: Oliver.Brady@lshtm.ac.uk
## 
## ############################################################################


# Required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(mapview)) install.packages("mapview", repos = "http://cran.us.r-project.org")
if(!require(shiny.i18n)) install.packages("shiny.i18n", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gsubfn)) install.packages("gsubfn", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(scico)) install.packages("scico", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")

# Load global variables
app_title <- "COVID-19: Local Area Comparison and Projection Tool (LACPT)"

source(file.path("input_data", "OB_standardisation_functions.R"))

# Load in pre-computed BigWrap dataset
load(fetch_latest(fileDir = "input_data/", Format = ".RData"))

Brazil_cases <- BigStandard$standardised_incidence

# Load map
myMap <- rgdal::readOGR("input_data", "Brazil_AD2_shape")

Measure   <- "Age standardised incidence"
DateUntil <- Sys.Date()

Brazil_cases_sp <- Brazil_cases
rm(Brazil_cases)

# date trim
Brazil_cases_sp <- Brazil_cases_sp[Brazil_cases_sp$date_end <= DateUntil, ]

# trim to just latest number of cumulative cases / incidence
Brazil_cases_cum_cases <- aggregate(cum_cases ~ Area + X + Y, data = Brazil_cases_sp, FUN = max)
Brazil_cases_cum_incid <- aggregate(standardised_cases ~ Area + X + Y, data = Brazil_cases_sp, FUN = max)

# make sf object
Brazil_cases_cum_cases <- st_as_sf(Brazil_cases_cum_cases, coords = c("X", "Y"))
Brazil_cases_cum_incid <- st_as_sf(Brazil_cases_cum_incid, coords = c("X", "Y"))

# extract dates from cv data
min_date <- as.Date(min(Brazil_cases_sp$date_end),"%Y-%m-%d")
max_date <- as.Date(max(Brazil_cases_sp$date_end),"%Y-%m-%d")

Brazil_cases_time <- aggregate(cum_cases ~ date_end, 
                               data = Brazil_cases_sp, FUN = sum)

Brazil_cases_sp2   <- Brazil_cases_sp[Brazil_cases_sp$cum_cases > 50, ]
Brazil_cases_areas <- aggregate(Area ~ date_end, data = Brazil_cases_sp2, 
                                FUN = length)

LocalArea <- "São Paulo_SP"

d_dat        <- Brazil_cases_sp
# d_dat$region <- str_sub(d_dat$Area, start= -2)

regions <- sort(unique(str_sub(Brazil_cases_sp$Area, start= -2)))

# c_dat        <- BigStandard
# names(c_dat) <- substr(names(c_dat), 1, nchar(names(c_dat))-3)


ui <- navbarPage(
    theme       = shinytheme("flatly"),
    position    = "static-top",
    collapsible = TRUE,
    id          = "nav", 
    title       = div(
        a(img(src="cmmid.svg",
              height="35px"),
          href="https://cmmid.lshtm.ac.uk/"),
        span(app_title, style="line-height:50px")
    ),
    windowTitle = app_title,
    
    tabPanel(title="National overview",
             
             div(
                 class="outer",
                 tags$head(includeCSS("styles.css")),
                 mapviewOutput("map",
                               width="100%",
                               height=700),
                 
                 absolutePanel(
                     top = 40, 
                     left = 90,
                     width = 90,
                     draggable = FALSE,
                     selectInput("language", "Language", c("EN", "PR"),
                                 width = '70px')
                 ),
                 
                 absolutePanel(id = "controls", 
                               top = 150, 
                               left = 20,
                               width = 250,
                               draggable = FALSE,
                               prettyRadioButtons('outcome_select',
                                                  label=em(h4(uiOutput("outcome_label"),
                                                              align="center")),
                                                  choices = c('Incidence per 1000 people',
                                                              'Cases'),
                                                  selected = 'Incidence per 1000 people',
                                                  shape = "round",
                                                  animation = "jelly",
                                                  plain = TRUE,
                                                  bigger = FALSE,
                                                  inline = FALSE)),
                 
                 absolutePanel(
                     top = 20, 
                     right = 20,
                     width = 320,
                     draggable = FALSE,
                     wellPanel(
                         HTML(
                             markdownToHTML(
                                 fragment.only=TRUE, 
                                 knit("summary.Rmd", 
                                      quiet = TRUE)
                             ))),
                     style = "opacity: 0.92"),
                 
                 absolutePanel(id = "controls", 
                               class = "panel panel-default",
                               top = 320, left = 20, 
                               width = 380, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               plotOutput("cumulative_plot", height="300px", 
                                          width="100%"),
                               
                               
                               sliderInput("plot_date",
                                           label = h5("date", align="center"),
                                           min = as.Date(min_date,"%Y-%m-%d"),
                                           max = as.Date(max_date,"%Y-%m-%d"),
                                           value = as.Date(max_date),
                                           step=days(1),
                                           timeFormat = "%d %b", 
                                           animate=FALSE)),
                 
                 absolutePanel(id = "logo", class = "card", bottom = 20, 
                               left = 150, width = 120, fixed=TRUE, 
                               draggable = FALSE, height = "auto",
                               tags$a(href='https://www.lshtm.ac.uk', 
                                      tags$img(src='lshtm_logo.png',
                                               height='40',width='80'))),
                 absolutePanel(id = "logo", class = "card", 
                               bottom = 20, left = 20, width = 60, 
                               fixed=TRUE, draggable = FALSE,
                               height = "auto",
                               tags$a(href='http://cmmid.lshtm.ac.uk/', 
                                      tags$img(src='cmmid_black.png',
                                               height='40',width='100')))
             )
    ),
    
    tabPanel(title="Local area comparison",
             
             sidebarLayout(
                 sidebarPanel(
                     width=3,
                     
                     pickerInput("region_select",
                                 h5("Select your State"),
                                 choices = regions,
                                 selected = "SP",
                                 multiple = FALSE),
                     
                     pickerInput("area_select",
                                 h5("Select your Municipality"),
                                 choices = sort(as.character(unique(d_dat$Area))),
                                 selected = "São Paulo_SP",
                                 multiple = FALSE),
                     
                     pickerInput("outcome_select2",
                                 h5("Select a measure"),
                                 choices = c("Cumulative case incidence", 
                                             "Cumulative death incidence",
                                             "Hospital bed occupancy",
                                             "ITU bed occupancy"),
                                 selected = c("Cumulative case incidence"),
                                 multiple = FALSE),
                     
                     uiOutput("outcome_slider"),
                     
                     br(),
                     
                     
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("",
                                  plotlyOutput("p1",
                                               height="400px"),
                                  br(),
                                  htmlOutput("selected_text"),
                                  br(), br(),
                                  # verbatimTextOutput("rawtable2"),
                                  plotOutput("p2",
                                             height="400px"),
                                  br(),
                                  htmlOutput("intervention_text"),
                                  br())
                         
                     )
                 )
             )
    ),
    
    tabPanel(title="Trends",
             
             sidebarLayout(
                 sidebarPanel(
                     width=3,
                     
                     htmlOutput("state_selector2"),
                     # uiOutput("highlight_slider"),
                     
                     # htmlOutput("area_selector2"),
                     
                     # htmlOutput("outcome_selector2"),
                     
                     # htmlOutput("filter_selector2"),
                     
                     # uiOutput("area_slider2"),
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("",
                                  plotOutput("p3",
                                             height="900px"),
                                  br(),
                                  htmlOutput("quantile_text"),
                                  br(),
                                  htmlOutput("quantile_text2"),
                                  br(),
                                  htmlOutput("quantile_text3"),
                                  br(),
                                  htmlOutput("quantile_text4"),
                                  br(), br()
                                  
                         )
                     )
                 )
             )
    ),
    
    tabPanel(title="Download data",
             tags$h4("Download local area data"),
             # pickerInput("area_select3",
             #             h5("Select your local area"),
             #             choices  = sort(
             #                 as.character(names(BigStandard))),
             #             options  = list(`actions-box` = TRUE),
             #             selected = "São Paulo_SP",
             #             multiple = FALSE),
             numericInput("maxrows",
                          uiOutput("rows_label"), 15),
             downloadButton("downloadCsv",
                            uiOutput("download_label")),
             tags$br(), tags$br(),
             verbatimTextOutput("rawtable"),
             tags$br(),tags$br()
    ),
    
    
    tabPanel(title="About this site",
             tags$div(
                 tags$h4("Data Sources"),
                 tags$h5("(1) COVID-19 Cases"),
                 "The data on COVID-19 cases aggregated by municipality is",
                 "obtained from the Brazil.IO COVID-19 project repository which",
                 "is updated on a daily basis. This dataset contains confirmed",
                 "COVID-19 cases and deaths obtained from the bulletins of the",
                 "State Health Secretariats across the country. For more",
                 "information see:",  
                 tags$a(href=" https://brasil.io/dataset/covid19/caso_full/",
                        "brasil.io/dataset"),
                 tags$br(),
                 tags$h5("(2) Age distribution of COVID cases"),
                 "This was derived from data on notified COVID-19 cases",
                 "reported throughout Brazil between 2nd Feb and 25th March",
                 "2020 collected by the Brazilian Ministry of Health and is",
                 "used with their permission.",
                 tags$br(),
                 tags$h5("(3) Age distribution data"),
                 "Data on the distribution of the population by age for each",
                 "municipality was obtained from the Instituto Brasileiro de",
                 "Geografia e Estatisitica (IBGE) national demographic census",
                 "for 2010. The data was downloaded from",
                 tags$a(href="https://sidra.ibge.gov.br/tabela/3107",
                        "IBGE"),
                 tags$br(),
                 tags$h5("(4) COVID-19 Intervention data"),
                 "Data on the types of interventions implemented and the",
                 "dates of their introduction were derived by manual",
                 "extraction of information from two primary data sources; ",
                 "the ACAPS #COVID 19 Government Measures Dataset ",
                 tags$a(href="https://www.acaps.org/covid19-government-measures-dataset",
                        "ACAPS"), "and the ",
                 tags$a(href="https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker",
                        "Coronavirus Government Response Tracker"), "collated by a",
                 "team based at the Blavatnik School of Government and the",
                 "University of Oxford, UK.",
                 tags$br(),tags$br(),
                 
                 tags$h4("Methods"),
                 tags$h5("Age and population standardisation process"),
                 "The aim of our standardisation process is to make data",
                 "comparable between areas that have different population",
                 "sizes and age profiles. We first assign the number of cases",
                 "reported in each area (1) a hypothetical age distribution",
                 "based on the Brazilian national age distribution of COVID-19",
                 "cases (2). We then use local area age-stratified population",
                 "data (3) to calculate age-specific incidence (standardised",
                 "incidence) which is a measure that is comparable between",
                 "areas. This analysis was last updated on 13th May 2020.",
                 
                 tags$br(),tags$br(),
                 
                 tags$h5("Covariates"),
                 "The Socio Demographic Index (SDI) is a composite relative ",
                 "measure of development ranging from 0 (lowest) to 1 (highest).",
                 "It is calculated using the average rankings of income per", 
                 "capita, fertility rate and mean years of education among all",
                 "municipalities as measured by the 2010 IBGE census. Variables",
                 "for population density, access to piped water and access to",
                 "sewage system or septic tank were also optained from the 2010",
                 "IBGE census.Travel time to biggest city in the state was ",
                 "calculated using",
                 tags$a(href="https://www.worldpop.org", "WorldPop"),  
                 "population data and the ",
                 tags$a(href="https://malariaatlas.org", "Malaria Atlas Project"),
                 "travel time friction surface. Travel time represents the",
                 "land-based travel time between the most densely populated",
                 "area in the municipality that is experiencing the Covid-19",
                 "outbreak and the most densely populated area in the",
                 "corresponding State.",
                 
                 tags$br(),tags$br(),
                 "Thi analysis was last updated on 28th May 2020.",
                 
                 tags$br(),tags$br(),
                 
                 tags$h4("Authors"),
                 "Dr Oliver Brady",
                 tags$br(),
                 "Dr Paul Mee",
                 tags$br(),
                 "Dr Felipe J Colón-González",
                 tags$br(),
                 "Dr Neal Alexander",
                 tags$br(),
                 "Centre for the Mathematical Modelling of Infectious",
                 "Diseases", tags$a(href="http://cmmid.lshtm.ac.uk/",
                                    "(CMMID)."),
                 tags$br(),tags$br(),
                 
                 tags$h4("Disclaimer"), 
                 "The aim of this site is to complement the resources provided",
                 "by the Brazilian ministry of health. This app has been",
                 "developed for research purposes only and is not suitable to",
                 "use as medical advice or to assess your personal level of",
                 "risk. Data are provided without any warranty of any kind,", 
                 "either express or implied. The entire risk arising out of",
                 "the use or performance of the app and associated data remains",
                 "with you. In no event shall the London School of Hygiene and",
                 "Tropical Medicine (LSHTM), the authors, or anyone else",
                 "involved in the creation, production, or delivery of the",
                 "app be liable for any damages whatsoever including, without",
                 "limitation, damages for loss of business profits, business",
                 "interruption, loss of business information, or other",
                 "pecuniary loss arising out of the use of or inability to use",
                 "this app even if LSHTM has been advised of the possibility of 
                 such damages.",
                 
                 tags$br(),tags$br(),
                 tags$h4("Contact"),
                 tags$a(href="oliver.brady@lshtm.ac.uk",
                        "Oliver.Brady@lshtm.ac.uk"),
                 "London School of Hygiene and Tropical Medicine",
                 "Keppel Street",
                 "London",
                 "WC1E 7HT",
                 tags$br(), tags$br(), tags$br()
             )
    )
    
)

server <- function(input, output, session) {
    
    output$outcome_label = renderText({
        switch(input$language, "EN"="Select Variable",
               "PR"="Variable a escoger") 
    })
    
    
    spatial_reactive_db <- reactive({
        if (input$outcome_select=="Cases") { 
            
            # trim to just latest number of cumulative cases / incidence
            db <- aggregate(cum_cases ~ Area + X + Y, data = Brazil_cases_sp, 
                            FUN = max)
            
            # Rename outcome
            names(db)[4] <- "outcome"
            
            # Create size for circle markers
            db$size <- log1p(db$outcome*3)
            
            # make sf object
            db <- st_as_sf(db, coords = c("X", "Y"))
            
            # set crs
            st_crs(db)   <- 4326
            
            # Convert to spatial points data frame
            spatial      <- as(db, "Spatial")
            return(spatial)
        }
        if (input$outcome_select=="Incidence per 1000 people") { 
            db            <- aggregate(cum_incid ~ Area + X + Y, 
                                       data = Brazil_cases_sp, FUN = max)
            names(db)[4]  <- "outcome"
            db$size       <- sqrt(db$outcome*4)
            db            <- st_as_sf(db, coords = c("X", "Y"))
            st_crs(db)    <- 4326
            spatial       <- as(db, "Spatial")
            return(spatial)
        }
    })
    
    output$map <- renderLeaflet({
        
        pal <- colorNumeric(palette=rev(scico::scico(5, palette="roma")), 
                            domain = spatial_reactive_db()$outcome)
        
        leaflet(data = spatial_reactive_db(),
                options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
            addCircleMarkers(col = ~pal(outcome), opacity = 0.9,
                             radius = ~size, weight=2,
                             popup= ~paste(Area, round(outcome, 1), 
                                           sep=" - ")) %>%
            addTiles() %>%
            addLegend("bottomright",
                      pal = pal, values = ~outcome,
                      opacity = 0.7, title = "Scale")
        
    })
    
    cumulative_reactive_db <- reactive({
        cumsum_cases = dplyr::filter(Brazil_cases_time, 
                                     date_end <= input$plot_date)
    })
    
    munic_reactive_db <- reactive({
        cumsum_cases = dplyr::filter(Brazil_cases_areas, 
                                     date_end <= input$plot_date)
    })
    
    output$cumulative_plot <- renderPlot({
        p1 <- ggplot(cumulative_reactive_db(), aes(x = date_end, 
                                                   y = cum_cases)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d") +
            labs(y= "Cumulative cases", x = "Date")
        
        
        p2 <- ggplot(munic_reactive_db(), aes(x = date_end, y = Area)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d") +
            labs(y="Municipalities with > 50 cases", x="Date")
        
        ggarrange(p1, p2, nrow=2)
    })
    
    area_db1 <- reactive({
        
        # preprocessing to re-route beginign of the epidemic depending on chosen area
        x_dat <- re.route.origin(BigStandard$standardised_incidence)
        
        # and add intervention timign data
        x_dat <- district.start.date.find(x_dat, BigStandard$Intervention)
        
        x_dat$Area   <- as.character(x_dat$Area)
        x_dat$Region <- str_sub(x_dat$Area, start= -2)
        
        maxDaysSince <- max(x_dat$Days_since_start[x_dat$Area == input$area_select])
        x_dat_com    <- x_dat[x_dat$Days_since_start <= maxDaysSince, ]
        
        timeSUM    <- cbind(aggregate(standardised_cases ~ Days_since_start,
                                      data=x_dat, FUN=quantile, probs = 0.33)[, 2],
                            aggregate(standardised_cases ~ Days_since_start,
                                      data=x_dat, FUN=quantile, probs = 0.5)[, 2],
                            aggregate(standardised_cases ~ Days_since_start,
                                      data=x_dat, FUN=quantile, probs = 0.66)[, 2])
        # compariosn matrix
        comp_mat <- cbind(x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 1],
                          x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 2],
                          x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 3])
        comp_mat_sum = as.logical(apply(comp_mat, 2, median))
        
        if(all(comp_mat_sum)){OB_sum = "above average"}
        if(sum(comp_mat_sum) < 3){OB_sum = "average"}
        if(sum(comp_mat_sum) < 2){OB_sum = "below average"}
        
        if(input$outcome_select2=="Hospital bed occupancy") {
            x_dat %<>%
                dplyr::rename(outcome=Bed_occ_50) %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        } else if(input$outcome_select2=="Cumulative death incidence") {
            x_dat %<>%
                dplyr::rename(outcome=Deaths_50)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        } else if(input$outcome_select2=="ITU bed occupancy"){
            x_dat %<>%
                dplyr::rename(outcome=ITU_Bed_occ_50)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        } else if(input$outcome_select2=="Cumulative case incidence"){
            x_dat %<>%
                dplyr::rename(outcome=standardised_cases)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        }
        
        if(input$outcome_select2=="Hospital bed occupancy") {
            y_label <- "Hospital bed occupancy per 1,000 residents (log scale)"
        } else if(input$outcome_select2=="ITU bed occupancy") {
            y_label <- "ITU bed occupancy per 1,000 residents (log scale)"
        } else if(input$outcome_select2=="Cumulative death incidence") {
            y_label <- "Standardised death incidence per 1,000 residents (log scale)"
        } else if(input$outcome_select2=="Cumulative case incidence"){
            y_label <- "Standardised case incidence per 1,000 residents (log scale)"
        }
        
        
        outdata <- list(of1=x_dat,
                        of2=y_label,
                        of3=OB_sum)

})
    
    output$rawtable2 <- renderPrint({
        orig <- options(width = 1000)
        print(head(area_db1()$of1$Area), row.names = FALSE)
        options(orig)
    })
    
    # y_label <- reactive({
    #     if(input$outcome_select2=="Hospital bed occupancy") {
    #         y_label <- "Standardised hospital beds required per 1,000 people"
    #     } else if(input$outcome_select2=="ITU bed occupancy") {
    #         y_label <- "Standardised ITU beds required per 1,000 people"
    #     } else if(input$outcome_select2=="Cumulative death incidence") {
    #         y_label <- "Standardised deaths per 1,000,000 people"
    #     } else {
    #         y_label <- "Standardised cases per 1,000 people"
    #     }
    # })
    
    # output$state_selector <- renderUI({ #creates State select box object called in ui
    #     pickerInput(inputId = "region_select", #name of input
    #                 label = h5("Select your State"), #label displayed in ui
    #                 choices = sort(unique(as.character(regions))),
    #                 selected = "SP") #default choice (not required)
    # })
    # 
    # output$area_selector <- renderUI({#creates County select box object called in ui
    #     
    #     areas <- as.character(d_dat$Area[as.character(d_dat$Region) ==
    #                                          input$region_select])
    #     data_available <- sort(areas[!is.na(areas)])
    #     pickerInput(inputId = "area_select", #name of input
    #                 label = h5("Select your local area"), #label displayed in ui
    #                 choices = unique(data_available), #calls list of available counties
    #                 selected = "São Paulo_SP")
    # })
    # 
    # output$filter_selector <- renderUI({#creates County select box object called in ui
    #     
    #     pickerInput(inputId = "filter_select", #name of input
    #                 label = h5("Select your Region"), #label displayed in ui
    #                 choices = c("None",
    #                             sort(unique(as.character(myMap@data$Region)))),
    #                 selected = "None")
    # })
    
    output$state_selector2 <- renderUI({ #creates State select box object called in ui
        pickerInput(inputId = "region_select2", #name of input
                    label = h5("Select your State"), #label displayed in ui
                    choices = sort(unique(as.character(myMap@data$Region))),
                    selected = "SP") #default choice (not required)
    })
    
    
    output$p1 <- renderPlotly({
        
        g1 <-  ggplot(area_db1()$of1, aes(x = Days_since_start,
                                          y = outcome,
                                          group = Area))  +
            theme_minimal() +
            xlab("Days since start of the outbreak (incidence above 1 case per 10,000 residents)") +
            theme(legend.text=element_text(size=16),
                  legend.title=element_text(size=14)) +
            theme(axis.text.x = element_text(size=9),
                  axis.text.y = element_text(size=9),
                  axis.title.x = element_text(size=9),
                  axis.title.y = element_text(size=9)) +
            theme(strip.text.x = element_text(size=9)) +
            theme(plot.title = element_text(size=16)) +
            geom_line(data = area_db1()$of1[area_db1()$of1$Area != input$area_select,],
                      color = "grey") +
            geom_line(data = area_db1()$of1[area_db1()$of1$Region == input$region_select, ],
                      color = "orange") +
            geom_line(data = area_db1()$of1[area_db1()$of1$Area == input$area_select, ],
                      color = "#ef6f6a", size=0.9) +
            # geom_text(data = area_db1()$of1[area_db1()$of1$Area==input$area_select, ],
            #           aes(label = substr(input$area_select, 1,
            #                              nchar(input$area_select)-3),
            #               x = max(Days_since_start)+2,
            #               y = max(outcome) * 1.25),
            #           color = "#ef6f6a") +
            # geom_text(data = area_db1()$of1[area_db1()$of1$Region==input$region_select, ],
            #           aes(label = input$region_select,
            #               x = max(Days_since_start),
            #               y = max(outcome) * 0.7),
            #           color = "orange") +
            # geom_text(data = area_db1()$of1[area_db1()$of1$Area!=input$area_select, ],
            #           aes(label = "Other areas",
            #               x = max(Days_since_start),
            #               y = max(outcome) * 0.9),
            #           color = "#5c6068") +
            ylab(area_db1()$of2) +
            xlim(0,70) +
            ggtitle(input$outcome_select2) +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(trans='log10')
        g1
        
       
    })
    
    
    
    output$selected_text <- renderText({
        paste("After accounting for different population sizes",
              "and age structures of COVID-19 affected municipalities,",
              "the outbreak in",
              substr(input$area_select, 1, nchar(input$area_select)-3),
              "is currently tracking", "<b>", area_db1()$of3, "</b>",
              "compared to other areas in Brazil")
        
    })
    
    
    output$p2 <- renderPlot({
        
        z_dat <- area_db1()$of1
        
        int_opts <- colnames(z_dat)[grepl("start", colnames(z_dat))]
        int_opts <- int_opts[!int_opts == "Days_since_start"]
        
        # loop through interventions aggregating at the area level
        int_first <- matrix(NA, nrow = length(unique(z_dat$Area)),
                            ncol = length(int_opts))
        for(i in 1:length(int_opts)){
            int_first[, i] = aggregate(as.formula(paste0(int_opts[i], " ~ Area")), 
                                       data = z_dat, FUN = min)[, 2]
        }
        
        # remove columsn with all 0s
        colnames(int_first) <- gsub("_start", "", int_opts)
        int_first           <- int_first[, colSums(int_first) > 0]
        
        # reformat into a data frame
        int_first = data.frame(Area = sort(unique(z_dat$Area)),
                               int_first)
        
        # now reshape into long format
        Int_long <- reshape(int_first,
                            times = colnames(int_first)[-1],
                            varying = list(2:ncol(int_first)),
                            direction = "long")
        
        # formatting
        Int_long$time = as.character(Int_long$time)
        Int_long$time = gsub("_", " ", Int_long$time)
        
        # reordering to maintain alphabetical order
        Int_long$time <- factor(Int_long$time,
                                levels = sort(unique(Int_long$time)),
                                ordered = TRUE)
        
        # standardise intervention column name
        colnames(Int_long)[3] = "Intervention_type"
        
        # precompute a variable that states whether interventions in the local area were later or earlier than the mean
        E_L <- Int_long[Int_long$Area == input$area_select, 
                        "Intervention_type"] >=
            aggregate(Intervention_type ~ time, Int_long,
                      FUN = mean)$Intervention_type
        Int_long$PlotCol = "black"
        
        Int_long$PlotCol[Int_long$Area == input$area_select] = "red"
        
        g2 <- ggplot(Int_long, aes(x=time, y=Intervention_type)) +
            geom_boxplot(aes(x=time, y=Intervention_type, color = "black"),
                         outlier.shape = NA) +
            geom_jitter(size = 2, alpha = 0.5) +
            geom_point(aes(x=time, y=Intervention_type, color = "black"),
                       data = Int_long[Int_long$Area == input$area_select, ],
                       color = rgb(1,0,0,0.5), size=3) +
            scale_color_manual(labels = c("Other areas", input$area_select), 
                               values= c("black", "red")) +
            guides(color = guide_legend(override.aes = list(linetype = 0, 
                                                            size=5))) +
            labs(color='Timing of interventions') +
            xlab("") +
            ylab("Days since start of the outbreak (>10 cases)") +
            ylim(0, 60) +
            theme_minimal() +
            ggtitle("Timing of interventions") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=20)) +
            theme(legend.text=element_text(size=10),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size =9)) +
            coord_flip()
        
        
        g2
    })
    
    output$intervention_text <- renderText({
        
        z_dat <- re.route.origin(BigStandard$standardised_incidence)
        
        # and add intervention timign data
        z_dat <- district.start.date.find(z_dat, BigStandard$Intervention)
        
        z_dat$Area   <- as.character(z_dat$Area)
        z_dat$Region <- str_sub(z_dat$Area, start= -2)
        
        int_opts <- colnames(z_dat)[grepl("start", colnames(z_dat))]
        int_opts <- int_opts[!int_opts == "Days_since_start"]
        
        
        # loop through interventions aggregating at the area level
        int_first <- matrix(NA, nrow = length(unique(z_dat$Area)),
                            ncol = length(int_opts))
        for(i in 1:length(int_opts)){
            int_first[, i] = aggregate(as.formula(paste0(int_opts[i], " ~ Area")), data = z_dat, FUN = min)[, 2]
        }
        
        # remove columsn with all 0s
        colnames(int_first) <- gsub("_start", "", int_opts)
        int_first           <- int_first[, colSums(int_first) > 0]
        
        # reformat into a data frame
        int_first = data.frame(Area = sort(unique(z_dat$Area)),
                               int_first)
        
        # now reshape into long format
        Int_long <- reshape(int_first,
                            times = colnames(int_first)[-1],
                            varying = list(2:ncol(int_first)),
                            direction = "long")
        
        # formatting
        Int_long$time = as.character(Int_long$time)
        Int_long$time = gsub("_", " ", Int_long$time)
        
        # reordering to maintain alphabetical order
        Int_long$time <- factor(Int_long$time,
                                levels = sort(unique(Int_long$time)),
                                ordered = TRUE)
        
        # standardise intervention column name
        colnames(Int_long)[3] = "Intervention_type"
        
        # precompute a variable that states whether interventions in the
        # local area were later or earlier than the mean
        E_L <- Int_long[Int_long$Area == LocalArea, "Intervention_type"] >=
            aggregate(Intervention_type ~ time, Int_long,
                      FUN = mean)$Intervention_type
        
        paste("On average, interventions were put in place ",
              "<b>", c("earlier", "later")[median(E_L) + 1], "</b>",
              " in the outbreak in",
              substr(input$area_select, 1, nchar(input$area_select)-3),
              "compared to other municipalities in Brazil")
    })
    
    area_db2 <- reactive({
        
        s_dat <- re.route.origin(BigStandard$standardised_incidence)
        
        # and add intervention timign data
        s_dat <- district.start.date.find(s_dat, BigStandard$Intervention)
        
        s_dat$Area   <- as.character(s_dat$Area)
        s_dat$Region <- str_sub(s_dat$Area, start= -2)
        
        if(input$filter_select2 != "National"){
            trend_s_dat <- s_dat[s_dat$Region == input$filter_select2, ]
        }else{
            trend_s_dat <- s_dat
        }
        
        if(input$outcome_selectx=="Incidence"){
            trend_s_dat$outcome <- trend_s_dat$standardised_cases
        } else{
            trend_s_dat$outcome <- trend_s_dat$Deaths_50
        }
        
        popdenDF       <- aggregate(popden ~ Area, max, data=trend_s_dat)
        
        popdenQuartile <- quantile(popdenDF$popden, probs=(0:4)/4)
        
        popdenDF$popdenQuartile <- cut(popdenDF$popden, popdenQuartile,
                                       include.lowest=TRUE)
        
        Q_labels    <- paste0("Q", 1:4)
        Q_labels[1] <- paste0(Q_labels[1], " (Lowest density)")
        Q_labels[4] <- paste0(Q_labels[4], " (Highest density)")
        
        levels(popdenDF$popdenQuartile) <- Q_labels
        popdenDF$popdenQuartile         <-as.character(popdenDF$popdenQuartile)
        
        SDIDF <- aggregate(SDI ~ Area, max, data=trend_s_dat)
        
        SDIQuartile <- quantile(SDIDF$SDI, probs=(0:4)/4)
        
        SDIDF$SDIQuartile <- cut(SDIDF$SDI, SDIQuartile, include.lowest=TRUE)
        
        Q_labels    <- paste0("Q", 1:4)
        Q_labels[1] <- paste0(Q_labels[1], " (Least developed)")
        Q_labels[4] <- paste0(Q_labels[4], " (Most developed)")
        
        levels(SDIDF$SDIQuartile) <- Q_labels
        SDIDF$SDIQuartile <- as.character(SDIDF$SDIQuartile)
        
        PipedDF<-aggregate(Piped_water ~ Area, max, data=trend_s_dat)
        
        PipedQuartile<-quantile(PipedDF$Piped_water, probs=(0:4)/4)
        
        PipedDF$PipedQuartile <- cut(PipedDF$Piped_water, PipedQuartile,
                                     include.lowest=TRUE)
        Q_labels <- paste0("Q", 1:4)
        levels(PipedDF$PipedQuartile) <- Q_labels
        PipedDF$PipedQuartile<-as.character(PipedDF$PipedQuartile)
        
        SewDF<-aggregate(Sewage_or_septic ~ Area, max, data=trend_s_dat)
        
        SewQuartile<-quantile(SewDF$Sewage_or_septic, probs=(0:4)/4)
        SewDF$SewQuartile <- cut(SewDF$Sewage_or_septic, SewQuartile,
                                 include.lowest=TRUE)
        Q_labels <- paste0("Q", 1:4)
        levels(SewDF$SewQuartile) <- Q_labels
        SewDF$SewQuartile<-as.character(SewDF$SewQuartile)
        
        TravDF       <- aggregate(Travel_time ~ Area, max, data=trend_s_dat)
        TravQuartile <- quantile(TravDF$Travel_time, probs=(0:4)/4)
        
        TravDF$TravQuartile <- cut(TravDF$Travel_time, TravQuartile, include.lowest=TRUE)
        
        Q_labels <- paste0("Q", 1:4)
        
        levels(TravDF$TravQuartile) <- Q_labels
        TravDF$TravQuartile <- as.character(TravDF$TravQuartile)
        
        AreaProfilesDF<-merge(x=trend_s_dat, y=popdenDF, by="Area", all.x=T, all.y=F)
        AreaProfilesDF<-merge(x=AreaProfilesDF, y=SDIDF, by="Area", all.x=T, all.y=F)
        AreaProfilesDF<-merge(x=AreaProfilesDF, y=PipedDF, by="Area", all.x=T, all.y=F)
        AreaProfilesDF<-merge(x=AreaProfilesDF, y=SewDF, by="Area", all.x=T, all.y=F)
        AreaProfilesDF<-merge(x=AreaProfilesDF, y=TravDF, by="Area", all.x=T, all.y=F)
        
        TimePointsVector<-as.numeric(names(table(trend_s_dat$Days_since_start %/% 10)))
        TimePointsVector<-10*TimePointsVector[TimePointsVector>0]
        
        QuartileTimeDF<-AreaProfilesDF[AreaProfilesDF$Days_since_start %in% 
                                           TimePointsVector,
                                       c("Area", 
                                         "Days_since_start",
                                         "standardised_cases", 
                                         "popdenQuartile", 
                                         "SDIQuartile", 
                                         "PipedQuartile",
                                         "SewQuartile", 
                                         "TravQuartile", 
                                         "outcome")]
        
        bp_ofile <- list(of1=QuartileTimeDF,
                         of2=SDIQuartile,
                         of3=PipedQuartile,
                         of4=SewQuartile,
                         of5=TravQuartile)
    })
    
    output$p3 <- renderPlot({
        
        g3 <- ggplot(area_db2()$of1,
                     aes(x=factor(Days_since_start),
                         y=outcome,
                         group=interaction(factor(Days_since_start),
                                           factor(popdenQuartile)))) +
            geom_boxplot(aes(fill=factor(popdenQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Incidence per 1,000 people \n(log scale, outliers omitted") +
            scale_y_log10() +
            theme_minimal() +
            scale_fill_tableau(name="Quartile of\npopulation \nper km^2 \n(one definition \nof urban is \n400 or more).",
                               palette="Superfishel Stone") +
            ggtitle("Population density quantiles") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size = 14))
        
        g4 <- ggplot(area_db2()$of1,
                     aes(x=factor(Days_since_start),
                         y=outcome,
                         group=interaction(factor(Days_since_start),
                                           factor(SDIQuartile)))) +
            geom_boxplot(aes(fill=factor(SDIQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Incidence per 1,000 people \n(log scale, outliers omitted") +
            scale_fill_tableau(
                name="Quartile of SDI: Socio \nDemographic Index,\na composite relative \nmeasure of development \nranging from 0 (lowest) \nto 1 (highest).",
                labels=paste0(round(area_db2()$of2[1:4],2), "-",
                              round(area_db2()$of2[2:5],2)),
                palette="Superfishel Stone") +
            theme_minimal() +
            ggtitle("Socio Demographic Index quantiles") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size = 14)) +
            scale_y_log10()
        
        g5 <- ggplot(area_db2()$of1,
                     aes(x=factor(Days_since_start),
                         y=outcome,
                         group=interaction(factor(Days_since_start),
                                           factor(PipedQuartile)))) +
            geom_boxplot(aes(fill=factor(PipedQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Incidence per 1,000 people \n(log scale, outliers omitted") +
            scale_fill_tableau(
                name="Quartile of proportion \nof households with \npiped water ",
                labels=paste0(round(area_db2()$of3[1:4],2), "-",
                              round(area_db2()$of3[2:5],2)),
                palette="Superfishel Stone") +
            scale_y_log10() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size = 14))
        
        g6 <- ggplot(area_db2()$of1,
                     aes(x=factor(Days_since_start),
                         y=outcome,
                         group=interaction(factor(Days_since_start),
                                           factor(SewQuartile)))) +
            geom_boxplot(aes(fill=factor(SewQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Incidence per 1,000 people \n(log scale, outliers omitted") +
            scale_fill_tableau(
                name="Quartile of proportion of \nhouseholds connected \nto the sewage network \nor with a septic tank ",
                labels=paste0(round(area_db2()$of4[1:4],2), "-",
                              round(area_db2()$of4[2:5],2)),
                palette="Superfishel Stone") +
            scale_y_log10() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size =14))
        
        
        g7 <- ggplot(area_db2()$of1,
                     aes(x=factor(Days_since_start), 
                         y=outcome, 
                         group=interaction(factor(Days_since_start),
                                           factor(TravQuartile)))) + 
            geom_boxplot(aes(fill=factor(TravQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Incidence per 1,000 people \n(log scale, outliers omitted") +
            scale_fill_tableau(
                name="Quartile of \nTravel time (in minutes)\n to largest city in the State", 
                labels=paste0(round(area_db2()$of5[1:4],2), "-", 
                              round(area_db2()$of5[2:5],2)),
                palette="Superfishel Stone") +
            scale_y_log10() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size =14))
        
        
        ggarrange(g3, g4, g5, g6, g7,
                  labels=NULL,
                  ncol=2, nrow=3, label.x=-0.03,
                  font.label=list(size=18, face="bold"),
                  common.legend=FALSE, legend="right")
        
    })
    
    output$quantile_text <- renderText({
        paste( "Currently there is no strong trend between population",
               "density and incidence of COVID-19 with comparable incidence",
               "rates between urban and rural areas.")
        
    })
    
    
    
    output$quantile_text2 <- renderText({
        paste( "Ccurrently there is a slight non-significant trend",
               "towards lower incidence rates in higher Socio-Demographic",
               "Index areas, however these differences appear to reduce",
               "as the outbreak progresses.")
        
    })
    
    
    
    output$quantile_text3 <- renderText({
        paste("Currently there is a slight non-significant trend",
              "towards lower incidence rates in areas that have higher",
              "access to piped waters.")
        
    })
    
    
    
    output$quantile_text4 <- renderText({
        
        if(input$filter_select2 == "National"){
            paste("Ccurrently there is a slight non-significant trend towards",
                  "lower incidence rates in areas that have higher access connection",
                  "to the sewage network (or septic tank), particularly in the early",
                  "stages of the epidemic.")
        }
    })
    
    output$rows_label = renderText({
        switch(input$language, "EN"="Number of rows to show",
               "ES"="Número de filas a mostrar")
    })
    
    out_dat <- reactive({
        out_data <- re.route.origin(BigStandard$standardised_incidence)
        
        # and add intervention timign data
        out_data <- district.start.date.find(out_data, BigStandard$Intervention)
        
        names(out_data) <- tolower(names(out_data))
        out_data
    })
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(head(out_dat(), n=input$maxrows), row.names = FALSE)
        options(orig)
    })
    
    area_db3 <- reactive({
        unwanted_array <- list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A',
                               'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A',
                               'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E',
                               'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I',
                               'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O',
                               'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U',
                               'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a',
                               'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a',
                               'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e',
                               'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i',
                               'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o',
                               'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u',
                               'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
        
        area <- gsub(" ", "_", tolower(gsubfn(paste(names(unwanted_array),collapse='|'),
                                              unwanted_array, input$area_select3)))
        
    })
    
    output$downloadCsv <- downloadHandler(
        
        filename = function() {
            paste0("data_covid19_lacpt_", DateUntil, ".csv")
        },
        content = function(file) {
            write.csv(out_dat(), file)
        }
    )
    
    output$download_label = renderText({
        switch(input$language, "EN"="Download data as CSV",
               "PR"="Descargar datos en formato CSV")
    })
    }

shinyApp(ui, server)
