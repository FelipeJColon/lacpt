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
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")

# Load global variables
app_title <- "COVID-19 Local Information Comparison (CLIC Brazil)"

options(shiny.sanitize.errors = TRUE)

source(file.path("input_data", "OB_standardisation_functions.R"))

Measure   <- "Age standardised incidence"
DateUntil <- Sys.Date()

# Search and download latest version 
target <- gsub("-", "_", DateUntil)
sear   <- drop_search(target)
try({
    drop_download(sear$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

# Load in pre-computed BigWrap dataset
load(fetch_latest(fileDir = "input_data/", type = "BigStandard"))

# Load State names and abbreviations
states <- readRDS(file.path("input_data", "statesBR.RDS")) %>%
    rename(Region="UF")

Brazil_cases  <- BigStandard$standardised_incidence
# Brazil_deaths <- BigStandard$standardised_deaths

# Load map
myMap <- rgdal::readOGR("input_data", "Brazil_AD2_shape")

# Measure   <- "Age standardised incidence"
DateUntil <- Sys.Date()

Brazil_cases_sp <- Brazil_cases
rm(Brazil_cases)

# Add full State names
Brazil_cases_sp %<>% inner_join(states)

# date trim
Brazil_cases_sp <- Brazil_cases_sp[Brazil_cases_sp$date_end <= DateUntil, ]
Brazil_cases_sp <- Brazil_cases_sp[!is.na(Brazil_cases_sp$X), ]

# trim to just latest number of cumulative cases / incidence
# Brazil_cases_cum_cases <- aggregate(cum_cases ~ Area + X + Y, data = Brazil_cases_sp, FUN = max)
# Brazil_cases_cum_incid <- aggregate(standardised_cases ~ Area + X + Y, data = Brazil_cases_sp, FUN = max)

# make sf object
# Brazil_cases_cum_cases <- st_as_sf(Brazil_cases_cum_cases, coords = c("X", "Y"))
# Brazil_cases_cum_incid <- st_as_sf(Brazil_cases_cum_incid, coords = c("X", "Y"))

# extract dates from cv data
min_date <- as.Date(min(Brazil_cases_sp$date_end),"%Y-%m-%d")
max_date <- as.Date(max(Brazil_cases_sp$date_end),"%Y-%m-%d")

Brazil_cases_time <- aggregate(cum_cases ~ date_end, 
                               data = Brazil_cases_sp, FUN = sum)

Brazil_cases_sp2   <- Brazil_cases_sp[Brazil_cases_sp$cum_cases > 50, ]
Brazil_cases_areas <- aggregate(Area ~ date_end, data = Brazil_cases_sp2,
                                FUN = length)
rm(Brazil_cases_sp2)

# LocalArea <- "São Paulo_SP"

# d_dat        <- Brazil_cases_sp
# d_dat$region <- str_sub(d_dat$Area, start= -2)

# regions <- sort(unique(str_sub(Brazil_cases_sp$Area, start= -2)))

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
                     right = 380,
                     width = 90,
                     draggable = FALSE,
                     selectInput("language", "Language", c("EN", "PR"),
                                 width = '70px')
                 ),
                 
                 absolutePanel(id = "controls", 
                               top = 50, 
                               left = 20,
                               width = 250,
                               draggable = FALSE,
                               prettyRadioButtons('outcome_select',
                                                  label=em(h4(uiOutput("outcome_label"),
                                                              align="center")),
                                                  choices = c('Case incidence',
                                                              'Cases',
                                                              "Death incidence",
                                                              "Deaths"),
                                                  selected = 'Case incidence',
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
                               top = 270, left = 20, 
                               width = 380, fixed=TRUE,
                               draggable = TRUE, height = "auto",
                               plotOutput("cumulative_plot", height="300px", 
                                          width="100%"),
                               
                               
                               sliderInput("plot_date",
                                           label = h5("Show cumulative cases before", 
                                                      align="center"),
                                           min = as.Date(min_date,"%Y-%m-%d"),
                                           max = as.Date(max_date,"%Y-%m-%d"),
                                           value = as.Date(max_date),
                                           step=days(1),
                                           timeFormat = "%d %b", 
                                           animate=TRUE)),
                 
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
                     
                     # htmlOutput("state_selector"),
                     pickerInput("region_select",
                                 h5("Select a State"),
                                 choices = sort(unique(as.character(Brazil_cases_sp$Name))),
                                 selected = "Sao Paulo",
                                 multiple = FALSE),
                     
                     htmlOutput("area_selector"),
                     
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
                     # tabsetPanel(
                         tabPanel("",
                                  plotlyOutput("p1", height="400px"),
                                  imageOutput("legend", height="50px"),
                                  htmlOutput("selected_text"),
                                  br(), br(),
                                  # verbatimTextOutput("rawtable2"),
                                  plotOutput("p2",
                                             height="400px"),
                                  br(),
                                  htmlOutput("intervention_text"),
                                  br())
                         
                     # )
                 )
             )
    ),
    
    tabPanel(title="Trends",
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
             sidebarLayout(
                 sidebarPanel(
                     width=3,
                     
                     pickerInput("region_select2",
                                 h5("Select National or a State"),
                                 choices = c("National",
                                             sort(
                                                 unique(
                                                     as.character(
                                                         Brazil_cases_sp$Name)))),
                                 selected = "National",
                                 multiple = FALSE),
                     
                     # htmlOutput("state_selector2"),
                     # uiOutput("highlight_slider"),
                     
                     # htmlOutput("area_selector2"),
                     
                     # htmlOutput("outcome_selector2"),
                     
                     # htmlOutput("filter_selector2"),
                     
                     # uiOutput("area_slider2"),
                 ),
                 
                 mainPanel(
                     # tabsetPanel(
                         tabPanel("",
                                  plotOutput("p3",
                                             height="900px"),
                                  br(),
                                  tags$li("Currently there is a
                                          trend towards higher incidence 
                                          rates in lower density areas."),
                                  tags$li("Currently there are no clear 
                                          differences between in incidence 
                                          with higher or lower development."),
                                  tags$li("CCurrently there is a trend towards
                                          higher incidence rates in areas with 
                                          less access to piped water."),
                                  tags$li("Currently there is a trend towards
                                          higher incidence rates in areas with 
                                          less access to the sewerage network."),
                                  # tags$li("Currently more isolated regions 
                                  #         (longer travel time) have higher 
                                  #         incidence outbreaks, particularly 
                                  #         later on in the epidemic."),
                                  br(), br()
                                  
                         )
                     # )
                 )
             )
    ),
    
    tabPanel(title="Model-based forecast",
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$h5("Available soon"),
    ),
    
    tabPanel(title="Download data",
             tags$h4("Download local area data"),
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
                 "obtained from the Brasil.IO COVID-19 project repository",
                 "which is updated on a daily basis. Municipality is the",
                 "second administrative level in Brazil (below state).",
                 "There are 5,570 municipalities in Brazil with an average",
                 "of 37,728 inhabitants per municipality. This dataset",
                 "contains confirmed COVID-19 cases and deaths obtained from",
                 "the bulletins of the State Health Secretariats across the",
                 "country. Data is obtained from this source and our",
                 "application is updated daily at 09:00 GMT. For more",
                 "information see:",  
                 tags$a(href=" https://brasil.io/dataset/covid19/caso_full/",
                        "brasil.io/dataset"),
                 tags$br(),
                 tags$h5("(2) Age distribution of COVID cases"),
                 "This was derived from data on notified COVID-19 cases",
                 "reported throughout Brazil between 2nd Feb and 8th June",
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
                 "University of Oxford, UK. Data sources were extracted on",
                 "14th May 2020.",
                 tags$br(),tags$br(),
                 
                 tags$h4("Methods"),
                 tags$h5("Age and population standardisation process"),
                 "The aim of our standardisation process is to make data",
                 "comparable between areas that have different populatio",
                 "sizes and age profiles. We first assign the number of cases",
                 "reported in each area (1) a hypothetical age distribution",
                 "based on the Brazilian national age distribution of COVID-19",
                 "cases (2). We then use local area age-stratified population",
                 "data (3) to calculate age-specific incidence (standardised",
                 "incidence) which is a measure that is comparable between",
                 "areas. A measure of standardised incidence of Covid-19 ",
                 "cases per 1,000 inhabitants is then calculated based on",
                 "the national age profile of Brazil for visualisation in",
                 "this applications",
                 
                 tags$br(),
                 "Days since the start of the outbreak is calculated from",
                 "whenever each area passes a cumulative incidence threshold",
                 "equivalent to 1 case per 10,000 inhabitants.",
                 tags$br(),tags$br(),
                 
                 
                 tags$h5("Covariates"),
                 "The Socio Demographic Index (SDI) is a composite relative",
                 "measure of development ranging from 0 (lowest) to 1",
                 "(highest). It is calculated using the average rankings of",
                 "income per capita, fertility rate and mean years of ",
                 "education among all municipalities as measured by the 2010",
                 "IBGE census. Variables for population density, access to",
                 "piped water and access to sewage system or septic tank were",
                 "also obtained from the 2010 IBGE census. Travel time to",
                 "biggest city in the state was calculated using ",
                 tags$a(href="https://www.worldpop.org", "WorldPop"),  
                 "population data and the ",
                 tags$a(href="https://developers.google.com/earth-engine/datasets/catalog/Oxford_MAP_friction_surface_2015_v1_0", "Malaria Atlas Project"),
                 "travel time friction surface using the",
                 tags$a(href="https://malariaatlas.org/application-project/malariaatlas_package/", "malariaAtlas"),
                 "R package accumulated cost route finding algorithm. Travel",
                 "time represents the land-based travel time between the most",
                 "densely populated area in the municipality that is",
                 "experiencing the Covid-19 outbreak and the most densely",
                 "populated area in the corresponding state. We may expect",
                 "higher Covid-19 incidence rates in highly accessible areas",
                 "due to frequent re-introduction of the virus, or conversely",
                 "we may expect higher incidence rates in remote areas due to",
                 "the increased challenge of adhering to movement restrictions.",
                 
                 # tags$br(),tags$br(),
                 # "Thi analysis was last updated on 28th May 2020.",
                 
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
                 "CMMID nCov working group",
                 # "Centre for the Mathematical Modelling of Infectious",
                 # "Diseases", tags$a(href="http://cmmid.lshtm.ac.uk/",
                 #                    "(CMMID)."),
                 tags$br(),tags$br(),
                 
                 tags$h4("Disclaimer"), 
                 "The aim of this site is to complement the resources provided",
                 "by the Brazilian ministry of health. This app has been",
                 "developed for research purposes only and is not suitable to",
                 "use as medical advice or to assess your personal level of",
                 "risk. Data are provided without any warranty of any kind,",
                 "either express or implied. The entire risk arising out of",
                 "the use or performance of the app and associated data",
                 "remains with you. In no event shall the London School of",
                 "Hygiene and Tropical Medicine (LSHTM), the authors, or",
                 "anyone else involved in the creation, production, or",
                 "delivery of the app be liable for any damages whatsoever",
                 "including, without limitation, damages for loss of business",
                 "profits, business interruption, loss of business information,",
                 "or other pecuniary loss arising out of the use of or ",
                 "inability to use this app even if LSHTM has been advised",
                 "of the possibility of such damages.",
                 
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
    ),
    tabPanel(title="How to use",
             tags$div(
                 tags$h3("COVID-19 Local Information Comparison"),
                 tags$h4("(CLIC Brazil)"),
                 tags$br(), 
                 tags$h4("Introduction"),
                 "The COVID-19 Local Information Comparison (CLIC Brazil)",
                 "web application allows local public health decision makers",
                 "and researchers to compare the current COVID-19 epidemic",
                 "in different local areas (municipalities) across Brazil.",
                 "It aims to:",
                 tags$br(), 
                 tags$li("Identify hotspots of COVID-19disease."),
                 tags$li("Compare the outbreak trajectories between",
                 "municipalities to understand where the epidemic is",
                 "growing fastest."),
                 tags$li("Assess the socioeconomic drivers of COVID-19 risk."),
                 tags$br(),
                 tags$h4("National Overview tab"),
                 "This tab shows a map of Brazil in which each municipality",
                 "with a COVID-19 outbreak is indicated by a circle. By ",
                 "selecting the options under ‘Select Variable’, the map ",
                 "can be configured to display either the total number of ",
                 "COVID-19 cases or the incidence per 1000 people in the",
                 "population. Cirlces with a dartker shading indicate higher",
                 "values. By sliding the ‘date’ button you can get a snapshot",
                 "of the epidemic on previous days and explore how the",
                 "epidemic has evolved. The plots and text box provide summary",
                 "information on the growth of the national epidemic.",
                 tags$br(),
                 tags$h4("Local area comparison tab"),
                 "The main graph on this tab shows the growth of the epidemic",
                 "over time in each muncipality. Case numbers have been",
                 "'age-standardised' to allow comparisons between",
                 "municipalities with different population sizes and age",
                 "structures. The timing of the start of the outbreak in", 
                 "each muncipality has also been standardised to the first",
                 "day on which a case incidence of greater than 1 case per",
                 "1000 residents was reported. - Using the top pull-down",
                 "list on the left you select the state in which you",
                 "municipality of interest is located the trajectories for",
                 "all municpalities in the state will be highlighted in",
                 "orange on the plot. - The central pull-down list allows",
                 "you to select the municipality of interest, these are",
                 "labelled with the Municipality name and the",
                 "State name. - The lower pull-down allows you to select",
                 "the measure of interest from the following options; ",
                 "Cumulative case incidence, Cumulative death incidence,",
                 "hospital bed occupancy and ITU bed occupancy (The details",
                 "of the methods used to calculate these parameters are given",
                 "in the ‘About this site’ tab)",
                 tags$br(),tags$br(),
                 "For example, if you compare the trajectory for cumulative",
                 "case incidence for São Paulo municipality with the",
                 "trajectories for all municipalities in São Paolo state",
                 "and (orange) and all municipalities in Brazil, you would",
                 "see this output:",
                 tags$br(),tags$br(),
                 tags$h5("Cumulative case plot"),
                 imageOutput("saopaulo", 
                             # height="100px", 
                             # width="10%", 
                             inline=TRUE),
                 
                 tags$br(),tags$br(),
                 tags$h5("Timing of interventions plot"),
                 "This graph shows the time different interventions were",
                 "announced relative to when the epidemic began in each",
                 "municipality. The time origin (day 0) for each municipality",
                 "is the first day on which a case incidence of greater than",
                 "1 case per 1000 residents was reported. Each black dot",
                 "represents one municipality with the red dots representing",
                 "the municipality selected in the dropdown menu. Dots in ",
                 "the blue area represent municipalities where the intervention",
                 "was announced before the local Covid-19 epidemic began while",
                 "dots in the red area show municipalities where interventions",
                 "were only announced after the outbreak had begun. The box",
                 "plot summarises the median, interquartile range and range",
                 "of timings for each municipality. The further right the red",
                 "dot is on this plot the later in the epidemic interventions",
                 "were announced compared to other areas.",
                 tags$br(),
                 imageOutput("boxplot", 
                             # height="100px", 
                             # width="10%", 
                             inline=TRUE),
                 
                 tags$br(),tags$br(),
                 tags$h4("Trends tab"),
                 
                 
                 tags$br(),tags$br(),
                 "This tab allows the user to compare the characteristics of",
                 "municipalities with worse or better Covid-19 epidemics.",
                 "Using the pulldown tab you can select to view either national",
                 "data or the data for one particular state.",
                 tags$br(),tags$br(),
                 "The barcharts show differences in age-standardised incidence",
                 "at different points in the epidemic for subsets of",
                 "municipalities grouped according to particular",
                 "characteristics. The vertical line shows the range of the",
                 "data for each subset and the rectangle shows the value for",
                 "the median and interquartile range. The sets of lines show",
                 "the data in 10 day intervals (from 10 to 70 days) from the",
                 "day at which an incidence of 1 case per 1000 people was",
                 "reported in a municipality.",
                 tags$br(),tags$br(),
                 "For example, the upper left plot (shown below) shows the",
                 "association between area population density and incidence.",
                 "The larger the gap between the bars the larger the",
                 "difference in the Covid-19 epidemic between high and low",
                 "population density areas. More detailed analysis can be",
                 "conducted by downloading the data from the data download tab.",
                 tags$br(),tags$br(),
                 imageOutput("quantiles", 
                             inline=TRUE),
                 tags$br(),tags$br(),
                
                 tags$h4("Data download tab"),
                "If you wish to carry out your own analyses on the standardised",
                "data you can download the full dataset in comma separated",
                "variable (csv) format from here. A description of the",
                "variables included is shown below.",
                tags$br(),
                tags$b("area"), 
                "- Municipality name and State (2 letter code). (NB",
                "UTF-8 encoding should be used to correctly format the place",
                "names)",
                tags$br(),
                tags$b("date_end"), 
                "- Date of data update (there is one row per municipality per",
                "day)",
                tags$br(),
                tags$b("cum_cases"),
                "- Cumulative case count",
                tags$br(),
                tags$b("cum_deaths"), 
                "- Cumulative death count",
                tags$br(),
                tags$b("bed_occ_2_5,bed_occ_50 & bed_occ_97_5"),
                "- 2.5% 50% & 97.5% credible intervals for the predicted",
                "number of hospital beds occupied by patients with COVID-19",
                "based on the cumulative data",
                tags$br(),
                tags$b("itu_bed_occ_2_5,itu_bed_occ_50 & itu_bed_occ_97_5"),
                "- 2.5% 50% & 97.5% credible intervals for the predicted",
                "number of Intensive care unit beds occupied by patients with",
                "COVID-19 based on the cumulative data.",
                tags$br(),
                tags$b("standardised_cases"), 
                "- Age standardised cumulative case incidence",
                tags$br(),
                tags$b("statdardised_deaths"), 
                "- Age standardised cumulative death incidence",
                tags$br(),
                tags$b("stan_bed_occ_2_5,stan_bed_occ_50 & stan_bed_occ_97_5"),
                "- 2.5% 50% & 97.5% credible intervals for the predicted",
                "number of hospital beds occupied by patients with COVID-19",
                "based on the age standardised data.",
                tags$br(),
                tags$b("stan_itu_bed_occ_2_5,stan_itu_bed_occ_50 & stan_itu_bed_occ_97_5"),
                "- 2.5% 50% & 97.5% credible intervals for the predicted",
                "number of Intensive care unit beds occupied by patients with",
                "COVID-19 based on the age standardised data.",
                tags$br(),
                tags$b("region"),
                "- State (2 letter code)",
                tags$br(),
                tags$b("popden"),
                "- Population Density (individuals per km2)",
                tags$br(),
                tags$b("sdi"), 
                "- Social demogrpahic index for the mucipality (See Covariates",
                "in the 'About this site' - tab).",
                tags$br(),
                tags$b("piped_water"),
                "- Proportion of population in the municipality with access",
                "to a piped water supply.",
                tags$br(),
                tags$b("sewage_or_septic"),
                "- Proportion of population in the municipality with access",
                "to sewage system or a septic tank",
                tags$br(),
                tags$b("travel_time"), 
                "- The land-based travel time between the most densely",
                "populated area in the municipality and the most densely",
                "populated area in the corresponding State x - The longitude",
                "of the municiplaity in decimal degrees y - The latitude of",
                "the municiplaity in decimal degrees.",
                tags$b("days_since_start"),
                "- The number of days since the age standardised rate was",
                "greater than 1 case per 1000 residents",
                 
                 tags$br(),tags$br(),
                 
                tags$b("The following variables are indicators which are coded",
                       "1 if the intervention described had been initiated by",
                       "this date;"),
                
                tags$br(),
                tags$b("awareness_campaigns_start"),
                "- COVID awareness campaigns",
                tags$br(),
                tags$b("border_closure_start"),
                "- International border closures",
                tags$br(),
                tags$b("domestic_travel_restrictions_start"),
                "- Domestic travel restrictions",
                tags$br(),
                tags$b("economic_measures_start"),
                "- Economic interventions",
                tags$br(),
                tags$b("border_health_screening_start"),
                "- Health screening at international borders",
                tags$br(),
                tags$b("international_flight_suspension_start"),
                "- International flights suspended",
                tags$br(),
                tags$b("isolation_quarrantine_start"),
                "- Isolation and quarantine for those infected with COVID-19",
                tags$br(),
                tags$b("limit_on_public_gatherings_start"),
                "- A limit was placed on public gatherings",
                tags$br(),
                tags$b("schools_closure_start"),
                "- School closures initiated",
                tags$br(),
                tags$b("workplace_closure_start"),
                "- Workplace closures initiated",
                 
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
        
        db <- Brazil_cases_sp %>%
            dplyr::filter(date_end <= input$plot_date)
        
        if (input$outcome_select=="Cases") { 
            
            # trim to just latest number of cumulative cases / incidence
            db <- aggregate(cum_cases ~ Area + X + Y, 
                            data = db, 
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
        if (input$outcome_select=="Case incidence") { 
            db            <- aggregate(standardised_cases ~ Area + X + Y, 
                                       data = db,
                                       FUN = max)
            names(db)[4]  <- "outcome"
            db$size       <- sqrt(db$outcome*4)
            db            <- st_as_sf(db, coords = c("X", "Y"))
            st_crs(db)    <- 4326
            spatial       <- as(db, "Spatial")
            return(spatial)
        }
        if (input$outcome_select=="Death incidence") { 
            db            <- aggregate(standardised_deaths ~ Area + X + Y, 
                                       data = db,
                                       FUN = max)
            names(db)[4]  <- "outcome"
            db$size       <- sqrt(db$outcome*4)
            db            <- st_as_sf(db, coords = c("X", "Y"))
            st_crs(db)    <- 4326
            spatial       <- as(db, "Spatial")
            return(spatial)
        }
        if (input$outcome_select=="Deaths") { 
            db            <- aggregate(cum_deaths ~ Area + X + Y, 
                                       data = db,
                                       FUN = max)
            names(db)[4]  <- "outcome"
            db$size       <- sqrt(db$outcome*4)
            db            <- st_as_sf(db, coords = c("X", "Y"))
            st_crs(db)    <- 4326
            spatial       <- as(db, "Spatial")
            return(spatial)
        }
    })
    
    output$map <- renderLeaflet({
        
        popup <- input$outcome_select
        
        my_bks <- c(0, round(exp(seq(log1p(0), 
                                     log1p(max(spatial_reactive_db()$outcome)),
                                     length = 5))))
        
        pal <- colorNumeric("YlOrBr", 
                        NULL)
        
        # pal <- colorNumeric(palette = "YlGnBu",
        #     domain = quantile(spatial_reactive_db()$outcome)
        # )
        
        # pal <- colorNumeric(palette=scico::scico(5, palette="lajolla"), 
        # domain = spatial_reactive_db()$outcome)
        
        leaflet(data = spatial_reactive_db(),
                options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
            # addProviderTiles(providers$MtbMap) %>%
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.35)) %>%
            addProviderTiles(providers$Stamen.TonerLabels) %>%
            # addProviderTiles("CartoDB.Positron", 
            #                  options = providerTileOptions(opacity = 1, 
            #                                                minZoom = 3, 
            #                                                maxZoom = 7), 
            #                  group = "Open Street Map") %>%
            addCircleMarkers(col = ~pal(outcome), 
                             opacity = 0.9,
                             radius = 5, 
                             weight=1,
                             popup= ~paste0("<b>", Area, "</b>", 
                                            "<br/>",                                            popup, ": ",
                                            round(outcome, 1),
                                            "<br/>",
                                            "Date: ",
                                            input$plot_date)) %>%
            # addTiles() %>%
            addLegend("bottomright",
                      pal = pal,
                      values = ~log1p(outcome),
                      labFormat = labelFormat(
                          transform=function(x) round(expm1(x),1)),
                      opacity = 0.7, 
                      title = input$outcome_select)
        
    })
    
    cumulative_reactive_db <- reactive({
        cumsum_cases <- dplyr::filter(Brazil_cases_time, 
                                      date_end <= input$plot_date)
    })
    
    munic_reactive_db <- reactive({
        cumsum_cases <- dplyr::filter(Brazil_cases_areas, 
                                      date_end <= input$plot_date)
    })
    
    output$cumulative_plot <- renderPlot({
        p1 <- ggplot(cumulative_reactive_db(), aes(x = date_end, 
                                                   y = cum_cases)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d",
                         limits = c(min(Brazil_cases_time$date_end),
                                    DateUntil)) +
            labs(y= "Cumulative cases", x = "Date")
        
        
        p2 <- ggplot(munic_reactive_db(), aes(x = date_end,
                                              y = Area)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d",
                         limits = c(min(Brazil_cases_time$date_end),
                                    DateUntil)) +
            labs(y="Municipalities with > 50 cases", x="Date")
        
        ggarrange(p1, p2, nrow=2)
    })
    
    area_db1 <- reactive({
        
        # preprocessing to re-route beginign of the epidemic depending 
        # on chosen area
        x_dat <- re.route.origin(BigStandard$standardised_incidence)
        
        # and add intervention timign data
        x_dat <- district.start.date.find(x_dat, BigStandard$Intervention)
        
        x_dat$Area   <- as.character(x_dat$Area)
        # x_dat$Region <- str_sub(x_dat$Area, start= -2)
        x_dat %<>% inner_join(states)
        
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
    
    # output$state_selector <- renderUI({ #creates State select box object called in ui
    #     pickerInput(inputId = "region_select", #name of input
    #                 label = h5("Select a State"), #label displayed in ui
    #                 choices = sort(unique(as.character(Brazil_cases_sp$Name))),
    #                 selected = "Sao Paulo") #default choice (not required)
    # })
    
    output$area_selector <- renderUI({#creates County select box object called in ui
        
        areas <- as.character(Brazil_cases_sp$Area[as.character(Brazil_cases_sp$Name) ==
                                                       input$region_select])
        data_available <- sort(areas[!is.na(areas)])
        pickerInput(inputId = "area_select", #name of input
                    label = h5("Select a Municipality"), #label displayed in ui
                    choices = unique(data_available), #calls list of available counties
                    selected = areas[1])
    })
    
    # output$state_selector2 <- renderUI({ #creates State select box object called in ui
    #     pickerInput(inputId = "region_select2", #name of input
    #                 label = h5("Select National or a State"), #label displayed in ui
    #                 choices = c("National", 
    #                             sort(unique(as.character(Brazil_cases_sp$Name)))),
    #                 selected = "National") #default choice (not required)
    # })
    
    
    output$p1 <- renderPlotly({
        
        g1 <-  ggplot(area_db1()$of1, aes(x = Days_since_start,
                                          y = outcome,
                                          group = Area,
                                          # use "text" for hovering
                                          text = paste0(input$outcome_select2,
                                                        ": ",
                                                        round(outcome,2)))) +
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
                      color = "grey", size=0.3) +
            geom_line(data = area_db1()$of1[area_db1()$of1$Name == input$region_select, ],
                      color = "orange", size=0.3) +
            geom_line(data = area_db1()$of1[area_db1()$of1$Area == input$area_select, ],
                      color = "#ef6f6a", size=0.9) +
            ylab(area_db1()$of2) +
            # xlim(0, max(Days_since_start)+5) +
            ggtitle(input$outcome_select2) +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(trans='log10') 
        
        # Use ggplotly to hover text over plot
        ggplotly(g1, tooltip = c("x","text", "Area"))
    })
    
    
    output$legend <- renderImage({
        
        return(list(
            src = "input_data/legend2.png",
            contentType = "image/png",
            alt = "Legend"
        ))  
    }, deleteFile = FALSE)
    
    output$saopaulo <- renderImage({
        
        return(list(
            src = "input_data/sp2.png",
            contentType = "image/png",
            alt = "Image"
        ))  
    }, deleteFile = FALSE)
    
    output$boxplot <- renderImage({
        
        return(list(
            src = "input_data/bxp2.png",
            contentType = "image/png",
            alt = "Image"
        ))  
    }, deleteFile = FALSE)
    
    output$quantiles <- renderImage({
        
        return(list(
            src = "input_data/quant2.png",
            contentType = "image/png",
            alt = "Image"
        ))  
    }, deleteFile = FALSE)
    
    output$selected_text <- renderText({
        paste("After accounting for different population sizes",
              "and age structures of COVID-19 affected municipalities,",
              "the outbreak in",
              substr(input$area_select, 1, nchar(input$area_select)-3),
              "is currently tracking", "<b>", area_db1()$of3, "</b>",
              "compared to other areas in Brazil")
        
    })
    
    
    plotdb <- reactive({
        # preprocessing to re-route beginign of the epidemic depending on chosen area
        z_dat <- re.route.origin(BigStandard$standardised_incidence,
                                 Zerotrim = FALSE)
        
        # and add intervention timing data
        z_dat <- district.start.date.find(z_dat, BigStandard$Intervention)
        
        # all interventions
        int_opts <- colnames(z_dat)[grepl("start", colnames(z_dat))]
        int_opts = int_opts[!int_opts == "Days_since_start"]
        
        
        # loop through interventions aggregating at the area level
        int_first <- matrix(NA, nrow = length(unique(z_dat$Area)),
                            ncol = length(int_opts))
        for(i in 1:length(int_opts)){
            int_first[, i] = aggregate(as.formula(paste0(int_opts[i], 
                                                         " ~ Area")), 
                                       data = z_dat, FUN = min)[, 2]
        }
        colnames(int_first)= gsub("_start", "", int_opts)
        
        
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
                                levels = sort(unique(Int_long$time)),ordered = TRUE)
        
        # standardise intervention column name
        colnames(Int_long)[3] = "Intervention_type"
        
        # precompute a variable that states whether interventions in the local area were later or earlier than the mean
        E_L <- Int_long[Int_long$Area ==
                            input$area_select, "Intervention_type"] >= 
            aggregate(Intervention_type ~ time, 
                      Int_long,FUN = mean)$Intervention_type
        Int_long$PlotCol = "black"
        #Int_long$PlotCol[Int_long$Area == input$area_select] = c("blue", "red")[E_L + 1]
        #colopts <- unique(Int_long$PlotCol)
        Int_long$PlotCol[Int_long$Area == input$area_select] = "red"
        
        # polygons for before and after outbreak began
        poly <- data.frame(y = c(min(Int_long$Intervention_type),
                                 min(Int_long$Intervention_type),
                                 rep(0, 4),
                                 max(Int_long$Intervention_type),
                                 max(Int_long$Intervention_type)),
                           x = c(0, 11, 11, 0, 0, 11, 11, 0),
                           Fcol = c(rep("blue", 4), rep("red", 4)))
        
        dbx <- list(of1=Int_long,
                    of2=poly,
                    of3=E_L,
                    of4=z_dat)
    })
    
    output$p2 <- renderPlot({
        
        
        p2 <- ggplot(plotdb()$of1, aes(x=time, y=Intervention_type)) +
            geom_boxplot(aes(x=time, y=Intervention_type,
                             color = "black", alpha = 0.65), 
                         outlier.shape = NA, show.legend = FALSE)+
            geom_polygon(data = plotdb()$of2, 
                         aes(x = x, y =y, fill = Fcol, 
                             alpha = 0.5), 
                         show.legend = FALSE) +
            scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
            geom_jitter(size = 2, alpha = 0.1) +
            geom_boxplot(aes(x=time, y=Intervention_type, 
                             color = "black", alpha = 0.6),
                         outlier.shape = NA, show.legend = FALSE)+
            geom_point(aes(x=time, y=Intervention_type), 
                       data = plotdb()$of1[plotdb()$of1$Area == input$area_select, ],
                       color = rgb(1,0,0,0.5), size= 3) +
            scale_color_manual(labels = c("Other areas", 
                                          input$area_select),
                               values= c("black", "#00BA38")) +
            xlab("") +
            ylab("Days since start of the outbreak (>10 cases)") +
            annotate(geom="text", x=10.75, y=mean(plotdb()$of1$Intervention_type),
                     label="Before outbreak") +
            annotate(geom="text", x=10.75, y=10, label="Outbreak") +
            coord_flip() +
            ggtitle("Timing of interventions in different municipalities") +
            theme_classic()
        p2
    })
    
    output$intervention_text <- renderText({
        
        paste("On average, interventions were put in place ",
              "<b>", c("earlier", "later")[median(plotdb()$of3) + 1], "</b>",
              " in the outbreak in",
              substr(input$area_select, 1, nchar(input$area_select)-3),
              "compared to other municipalities in Brazil")
    })
    
    area_db2 <- reactive({
        
        s_dat <- plotdb()$of4
        
        s_dat$Area   <- as.character(s_dat$Area)
        s_dat %<>% inner_join(states)
        
        if(input$region_select2 != "National"){
            trend_s_dat <- s_dat[s_dat$Name == input$region_select2, ]
        }else{
            trend_s_dat <- s_dat
        }
        
        # and remove municipalities with NAs in their covariate data
        trend_s_dat <- trend_s_dat[!is.na(trend_s_dat$SDI), ]
        
        popdenDF <- aggregate(popden ~ Area, max, data=trend_s_dat)
        #head(popdenDF)
        popdenQuartile<-quantile(popdenDF$popden, probs=(0:4)/4)
        popdenDF$popdenQuartile <- cut(popdenDF$popden, popdenQuartile, 
                                       include.lowest=TRUE)
        Q_labels <- paste0("Q", 1:4)
        Q_labels[1] = paste0(Q_labels[1], " (Lowest density)")
        Q_labels[4] = paste0(Q_labels[4], " (Highest density)")
        levels(popdenDF$popdenQuartile) <- Q_labels
        popdenDF$popdenQuartile<-as.character(popdenDF$popdenQuartile)
        #table(popdenDF$popdenQuartile, exclude=NULL)
        
        SDIDF<-aggregate(SDI ~ Area, max, data=trend_s_dat)
        #head(SDIDF)
        SDIQuartile<-quantile(SDIDF$SDI, probs=(0:4)/4)
        SDIDF$SDIQuartile <- cut(SDIDF$SDI, SDIQuartile, include.lowest=TRUE)
        Q_labels <- paste0("Q", 1:4)
        Q_labels[1] = paste0(Q_labels[1], " (Least developed)")
        Q_labels[4] = paste0(Q_labels[4], " (Most developed)")
        levels(SDIDF$SDIQuartile) <- Q_labels
        SDIDF$SDIQuartile<-as.character(SDIDF$SDIQuartile)
        #table(SDIDF$SDIQuartile, exclude=NULL)
        
        PipedDF                       <- aggregate(Piped_water ~ Area, max, 
                                                   data=trend_s_dat)
        PipedQuartile                 <- quantile(PipedDF$Piped_water,
                                                  probs=(0:4)/4)
        PipedDF$PipedQuartile         <- cut(PipedDF$Piped_water,
                                             PipedQuartile, 
                                             include.lowest=TRUE)
        Q_labels                      <- paste0("Q", 1:4)
        Q_labels[1]                   <- paste0(Q_labels[1],
                                                " (Least piped water)")
        Q_labels[4]                   <- paste0(Q_labels[4],
                                                " (Most piped water)")
        levels(PipedDF$PipedQuartile) <- Q_labels
        PipedDF$PipedQuartile         <- as.character(PipedDF$PipedQuartile)
        
        SewDF                     <- aggregate(Sewage_or_septic ~ Area,
                                               max, data=trend_s_dat)
        SewQuartile               <- quantile(SewDF$Sewage_or_septic,
                                              probs=(0:4)/4)
        SewDF$SewQuartile         <- cut(SewDF$Sewage_or_septic, 
                                         SewQuartile, 
                                         include.lowest=TRUE)
        Q_labels                  <- paste0("Q", 1:4)
        Q_labels[1]               <- paste0(Q_labels[1], 
                                            " (Least sewerage)")
        Q_labels[4]               <- paste0(Q_labels[4], 
                                            " (Most sewerage)")
        levels(SewDF$SewQuartile) <- Q_labels
        SewDF$SewQuartile         <- as.character(SewDF$SewQuartile)
        
        TravDF                      <- aggregate(Travel_time ~ Area, 
                                                 max, data=trend_s_dat)
        TravQuartile                <- quantile(TravDF$Travel_time, 
                                                probs=(0:4)/4)
        TravDF$TravQuartile         <- cut(TravDF$Travel_time, 
                                           TravQuartile, 
                                           include.lowest=TRUE)
        Q_labels                    <- paste0("Q", 1:4)
        Q_labels[1]                 <- paste0(Q_labels[1], 
                                              " (Least accessible)")
        Q_labels[4]                 <- paste0(Q_labels[4],
                                              " (Most accessible)")
        levels(TravDF$TravQuartile) <- Q_labels
        TravDF$TravQuartile         <- as.character(TravDF$TravQuartile)
        
        
        #dim(AreaProfilesDF)
        AreaProfilesDF <- merge(x=trend_s_dat, y=popdenDF,
                                by="Area", all.x=T, all.y=F)
        AreaProfilesDF <- merge(x=AreaProfilesDF, y=SDIDF, 
                                by="Area", all.x=T, all.y=F)
        AreaProfilesDF <- merge(x=AreaProfilesDF, y=PipedDF,
                                by="Area", all.x=T, all.y=F)
        AreaProfilesDF <- merge(x=AreaProfilesDF, y=SewDF, 
                                by="Area", all.x=T, all.y=F)
        AreaProfilesDF <- merge(x=AreaProfilesDF, y=TravDF, 
                                by="Area", all.x=T, all.y=F)
        
        TimePointsVector <- as.numeric(names(table(
            trend_s_dat$Days_since_start %/% 10)))
        TimePointsVector <- 10*TimePointsVector[TimePointsVector>0]
        
        QuartileTimeDF<-AreaProfilesDF[AreaProfilesDF$Days_since_start %in% 
                                           TimePointsVector,
                                       c("Area", "Days_since_start",
                                         "standardised_cases",
                                         "popdenQuartile", 
                                         "SDIQuartile",
                                         "PipedQuartile",
                                         "SewQuartile", 
                                         "TravQuartile")]
        
        # filter out timepoints that don't have all 4 quartiles
        missingQs        <- table(QuartileTimeDF$Days_since_start,
                                  QuartileTimeDF$popdenQuartile)
        missingQsDelete  <- as.numeric(rownames(missingQs)[apply(
            missingQs, 1, function(x) any(x == 0))])
        QuartileTimeDF   <- QuartileTimeDF[!(
            QuartileTimeDF$Days_since_start %in% missingQsDelete), ]
        
        # bp_ofile <- list(of1=QuartileTimeDF,
        #                  of2=SDIQuartile,
        #                  of3=PipedQuartile,
        #                  of4=SewQuartile,
        #                  of5=TravQuartile)
    })
    
    output$p3 <- renderPlot({
        
        g3 <- ggplot(area_db2(),
                     aes(x=factor(Days_since_start),
                         y=standardised_cases,
                         group=interaction(factor(Days_since_start),
                                           factor(popdenQuartile)))) +
            geom_boxplot(aes(fill=factor(popdenQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Cumulative ncidence per 1,000 people \n(log scale, outliers omitted)") +
            scale_y_log10() +
            theme_minimal() +
            scale_fill_brewer(palette="BuPu",name = "Area population density\n (Quartiles)") +
            # ggtitle("Population density") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size = 14))
        
        
        g4 <- ggplot(area_db2(),
                     aes(x=factor(Days_since_start),
                         y=standardised_cases,
                         group=interaction(factor(Days_since_start),
                                           factor(SDIQuartile)))) +
            geom_boxplot(aes(fill=factor(SDIQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Cumulative incidence per 1,000 people \n(log scale, outliers omitted)") +
            scale_fill_brewer(palette="BuPu",
                              name = "Area Socio-demographic\n index (Quartiles)") +
            theme_minimal() +
            # ggtitle("Socio Demographic Index") +
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

        g5 <- ggplot(area_db2(),
                     aes(x=factor(Days_since_start),
                         y=standardised_cases,
                         group=interaction(factor(Days_since_start),
                                           factor(PipedQuartile)))) +
            geom_boxplot(aes(fill=factor(PipedQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Cumulative incidence per 1,000 people \n(log scale, outliers omitted)") +
            scale_fill_brewer(palette="BuPu",
                              name = "Proportion of households \nwith piped water (Quartiles)") +
            scale_y_log10() +
            theme_minimal() +
            # ggtitle("Access to piped water") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size = 14))

        g6 <- ggplot(area_db2(),
                     aes(x=factor(Days_since_start),
                         y=standardised_cases,
                         group=interaction(factor(Days_since_start),
                                           factor(SewQuartile)))) +
            geom_boxplot(aes(fill=factor(SewQuartile)), outlier.shape=NA) +
            labs(x = "Days since start",
                 y = "Cumulative incidence per 1,000 people \n(log scale, outliers omitted)") +
            scale_fill_brewer(palette="BuPu",
                              name = "Proportion of households \nconnected to the sewerage \nnetwork (Quartiles)") +
            scale_y_log10() +
            theme_minimal() +
            # ggtitle("Access to piped water") +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12)) +
            theme(strip.text.x = element_text(size =14))

        ggarrange(g3, g4, g5, g6,
                  labels=NULL,
                  ncol=2, nrow=2, label.x=-0.03,
                  font.label=list(size=18, face="bold"),
                  common.legend=FALSE, legend="right")

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