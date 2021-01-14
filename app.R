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

# options(rsconnect.max.bundle.size=50000000000)

# Required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
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
# if(!require(scico)) install.packages("scico", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(plotROC)) install.packages("plotROC", repos = "http://cran.us.r-project.org")

# Load global variables
app_title <- "COVID-19 Local Information Comparison (CLIC Brazil)"

options(shiny.sanitize.errors = TRUE)

source(file.path("input_data", "OB_standardisation_functions.R"))

Measure   <- "Age standardised incidence"
DateUntil <- Sys.Date()

drop_auth(rdstoken = "token.rds")

# Search and download latest version available on site
# target <- gsub("-", "_", DateUntil)
target <- "Brazil_BigStandard_results_2021"
sear   <- drop_search(target)
try({
    drop_download(sear$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target2 <- "current_total_cases"
sear2   <- drop_search(target2)
try({
    drop_download(sear2$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target3 <- "Trends_plots2020_06_23"
sear3   <- drop_search(target3)
try({
    drop_download(sear3$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target4 <- "peak.rds"
sear4   <- drop_search(target4)
try({
    drop_download(sear4$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target5 <- "AUCplot.rdata"
sear5   <- drop_search(target5)
try({
    drop_download(sear5$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target6 <- "AUCDF.rdata"
sear6   <- drop_search(target6)
try({
    drop_download(sear6$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target7 <- "Brazil_rt_prediction."
sear7   <- drop_search(target7, mode="filename")
try({
    drop_download(sear7$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

cut_off_date = "2020-04-01"

# Read data
peakDF        <- readRDS("input_data/Peak.rds")
all_plot_data <- readRDS("input_data/Brazil_rt_prediction-current.RDS")
load("input_data/AUCplot.rdata")
load("input_data/AUCDF.rdata")

# Look at max Rt value by group 
max_Rt_vals <- all_plot_data %>% group_by(city_state) %>% top_n(1,Rtotalhat)

# city/state combinations
city_states <- sort(unique(as.character(all_plot_data$city_state)))

# date  omit NAs
peakDF <- peakDF[!is.na(peakDF$X), ]
peakDF <- peakDF[!is.na(peakDF$PredictProb), ]

# make sf object
peakSF <- st_as_sf(peakDF, coords = c("X", "Y"))

# DUMMY
# dfr <- data.frame(units=rep(LETTERS[1:5], each=20), 
#                   x=rnorm(100),

# names(Trends_plot_list)[1] <- "National"

# Load in pre-computed BigWrap dataset
myvar <- str_sub(sear$matches[[1]]$metadata$path_display, start= -43)
load(paste0("input_data/", myvar))

load(file.path("input_data/Trends_plots2020_06_23.RData"))

# Load State names and abbreviations
states <- readRDS(file.path("input_data", "statesBR.RDS")) %>%
    rename(Region="UF")

Brazil_cases  <- BigStandard$standardised_incidence
# Brazil_deaths <- BigStandard$standardised_deaths

# Load map
myMap <- rgdal::readOGR("input_data", "Brazil_AD2_shape")

# Measure <- "Age standardised incidence"
DateUntil <- Sys.Date()

Brazil_cases_sp <- Brazil_cases
rm(Brazil_cases)

# Add full State names
Brazil_cases_sp %<>% inner_join(states)

# date trim
Brazil_cases_sp <- Brazil_cases_sp[Brazil_cases_sp$date_end <= DateUntil, ]
Brazil_cases_sp <- Brazil_cases_sp[!is.na(Brazil_cases_sp$X), ]

# trim to just latest number of cumulative cases / incidence
Brazil_cases_cum_cases <- aggregate(cum_cases ~ Area + X + Y, 
                                    data = Brazil_cases_sp, FUN = max)

# extract dates from cv data
min_date <- as.Date(min(Brazil_cases_sp$date_end),"%Y-%m-%d")
max_date <- as.Date(max(Brazil_cases_sp$date_end),"%Y-%m-%d")

Brazil_cases_time <- aggregate(cum_cases ~ date_end, 
                               data = Brazil_cases_sp, FUN = sum)

Brazil_cases_sp2   <- Brazil_cases_sp[Brazil_cases_sp$cum_cases > 50, ]
Brazil_cases_areas <- aggregate(Area ~ date_end, data = Brazil_cases_sp2,
                                FUN = length)
rm(Brazil_cases_sp2)

# preprocessing to re-route beginign of the epidemic depending 
# on chosen area
x_dat <- re.route.origin(BigStandard$standardised_incidence)

# and add intervention timing data
x_dat <- district.start.date.find(x_dat, BigStandard$Intervention)

# Filter to only those areas with >50 cum_cases
# x_dat %<>% dplyr::filter(cum_cases > 400)

x_dat$Area   <- as.character(x_dat$Area)
# x_dat$Region <- str_sub(x_dat$Area, start= -2)
x_dat %<>% inner_join(states) 

timeSUM    <- cbind(aggregate(standardised_cases ~ Days_since_start,
                              data=x_dat, FUN=quantile, probs = 0.33)[, 2],
                    aggregate(standardised_cases ~ Days_since_start,
                              data=x_dat, FUN=quantile, probs = 0.5)[, 2],
                    aggregate(standardised_cases ~ Days_since_start,
                              data=x_dat, FUN=quantile, probs = 0.66)[, 2])

# preprocessing to re-route beginign of the epidemic depending on chosen area
z_dat <- re.route.origin(BigStandard$standardised_incidence,
                         Zerotrim = FALSE)

# and add intervention timing data
z_dat <- district.start.date.find(z_dat, BigStandard$Intervention)

# all interventions
int_opts <- colnames(z_dat)[grepl("start", colnames(z_dat))]
int_opts <- int_opts[!int_opts == "Days_since_start"]


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
int_first <- data.frame(Area = sort(unique(z_dat$Area)),
                        int_first)

# now reshape into long format
Int_long <- reshape(int_first,
                    times = colnames(int_first)[-1],
                    varying = list(2:ncol(int_first)),
                    direction = "long")

# formatting
Int_long$time <- as.character(Int_long$time)
Int_long$time <- gsub("_", " ", Int_long$time)

# reordering to maintain alphabetical order
Int_long$time <- factor(Int_long$time,
                        levels  = sort(unique(Int_long$time)),
                        ordered = TRUE)

# standardise intervention column name
colnames(Int_long)[3] = "Intervention_type"


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
    
    tabPanel(title=uiOutput("tab_title1"),
             
             div(
                 class="outer",
                 tags$head(includeCSS("styles.css")),
                 mapviewOutput("map",
                               width="100%",
                               height=700),
                 
                 absolutePanel(
                     top = 140, 
                     right = 380,
                     width = 90,
                     draggable = FALSE,
                     selectInput("language", "Language", c("EN", "PR"),
                                 width = '70px')
                 ),
                 
                 absolutePanel(id = "controls", 
                               top = 50, 
                               left = 20,
                               width = 350,
                               draggable = FALSE,
                               prettyRadioButtons('outcome_select',
                                                  label=em(h4(uiOutput("outcome_label"),
                                                              align="center")),
                                                  choices = c('Case incidence/Incidência de casos',
                                                              'Cases/Casos',
                                                              "Death incidence/Incidência de óbitos",
                                                              "Deaths/Óbitos"),
                                                  selected = 'Case incidence/Incidência de casos',
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
                                           label = h5(uiOutput("date_label1"), 
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
    
    tabPanel(title=uiOutput("tab_title2"),
             
             sidebarLayout(
                 sidebarPanel(
                     width=4,
                     
                     # htmlOutput("state_selector"),
                     pickerInput("region_select",
                                 h5(uiOutput("state_label1")),
                                 choices = sort(unique(as.character(Brazil_cases_sp$Name))),
                                 selected = "Sao Paulo",
                                 multiple = FALSE),
                     
                     htmlOutput("area_selector"),
                     
                     pickerInput("outcome_select2",
                                 h5(uiOutput("measure_label1")),
                                 choices = c("Cumulative case incidence / Incidência acumulada de casos", 
                                             "Cumulative death incidence / Incidência acumulada de óbitos",
                                             "Hospital bed occupancy / Ocupação de leitos hospitalares",
                                             "ITU bed occupancy / Ocupação de leitos de UTI"),
                                 selected = c("Cumulative case incidence / Incidência acumulada de casos"),
                                 multiple = FALSE),
                     
                     uiOutput("outcome_slider"),
                     
                     br(),
                     
                     
                 ),
                 
                 mainPanel(
                     # tabsetPanel(
                     tabPanel("",
                              plotOutput("p1", height="400px"),
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
    
    tabPanel(title=uiOutput("tab_title3"),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
             sidebarLayout(
                 sidebarPanel(
                     width=3,
                     
                     pickerInput("region_select2",
                                 h5(uiOutput("national_state_select")),
                                 choices = c("National",
                                             sort(
                                                 unique(
                                                     as.character(
                                                         Brazil_cases_sp$Name)))),
                                 selected = "National",
                                 multiple = FALSE),
                     
                 ),
                 
                 mainPanel(
                     # tabsetPanel(
                     tabPanel("",
                              plotOutput("p3",
                                         height="900px"),
                              br(),
                              uiOutput("list1"),
                              br(), br()
                              
                     )
                     # )
                 )
             )
    ),
    
    tabPanel(title=uiOutput("tab_title8"),
             
             sidebarLayout(
                 sidebarPanel(
                     width=4,
                     
                     # htmlOutput("state_selector"),
                     pickerInput("region_select_rt1",
                                 h5(uiOutput("state_label_rt1")),
                                 choices = city_states,
                                 selected = "São Caetano do Sul_SP",
                                 multiple = FALSE),
                     pickerInput("region_select_rt2",
                                 h5(uiOutput("state_label_rt2")),
                                 choices = city_states,
                                 selected = "São Paulo_SP",
                                 multiple = FALSE),
                     pickerInput("region_select_rt3",
                                 h5(uiOutput("state_label_rt3")),
                                 choices = city_states,
                                 selected = "Águas de Lindóia_SP",
                                 multiple = FALSE),
                     pickerInput("region_select_rt4",
                                 h5(uiOutput("state_label_rt4")),
                                 choices = city_states,
                                 selected = "Agudos_SP",
                                 multiple = FALSE),
                     
                     br(),
                     
                     
                 ),
                 
                 mainPanel(
                     # tabsetPanel(
                     tabPanel("",
                              plotOutput("p_rt",
                                         height="400px"),
                              tags$br(), tags$br(),
                              tags$h3("Rt plot"),
                              tags$p("This plot shows changes in the average
                                     reproduction number (Rt) within the chosen 
                                     municipalities over time. The reproduction 
                                     number estimates the average number of 
                                     secondary cases generated by one infectious
                                     person and values below 1.0 indicate that
                                     the number of new infections is reducing.
                                     Estimates were made using the APE renewal
                                     approach from Parag et al. 2019 . We assume 
                                     a mean serial interval (time between 
                                     successive transmission events) of 2.97 
                                     days (standard deviation 3.29) based on 
                                     recent data from Brazil Prete et al. 2019.
                                     Reproduction number estimates are updated 
                                     daily but only produced up to 14 days ago 
                                     due to delays in diagnosing and reporting
                                     more recent cases that would lead to 
                                     underestimates of Rt if more recent data 
                                     were used. We assume testing, case 
                                     confirmation and reporting efforts are 
                                     constant over time. Increases or decreases
                                     will increase of decrease Rt respectively.
                                     We also assume that all reported cases are 
                                     locally acquired and not imported. In order
                                     to correct for under-reporting of cases on 
                                     particular days of the week (particularly 
                                     on Sundays and Mondays) we applied a 
                                     smoothing function to the incident case 
                                     numbers which re-distributed the daily case
                                     counts in order to get roughly equal numbers
                                     of cases reported for each day of the week.
                                     The following limitations in the method 
                                     should be noted:"),
                              tags$li("We only generate Rt estimates for places
                              that have reported data for at least 30 days and 
                              which have a total of at least 100 cases reported."),
                              tags$li("We did not account for uncertainty in 
                              the serial interval. In addition, our approach
                              does not account for negative support in the 
                              serial interval. This means our estimates may be 
                              overly precise and/or biased."),
                              tags$li("We used a mean shift to map reported cases
                              back to the date of infection. This approach is 
                              known to produce Rt estimates that are biased 
                              downwards when Rt is greater than 1 and biased 
                              upwards when Rt is less than 1. We also did not 
                              account for uncertainties in the delay from onset 
                              to report or from infection to onset our estimates."),
                              tags$li("The APE renewal approach from Parag et al. 
                              optimises the window across the whole time series 
                              considered. This means that the smoothness of
                              historic estimates may change as new data becomes 
                              available and in areas with prolonged stable 
                              periods real time estimates will be biased towards
                              smoother changes over time."),
                              tags$li("To account for right truncation of
                              reported cases we only produce estimates up to 10 
                              days ago. However this data may still be incomplete 
                              and therefore our latest estimates may be biased
                              downward."),
                             
                              br())
                     
                     # )
                 )
             )
    ),
    
    tabPanel(title=uiOutput("tab_title4"),
             
             fluidRow(
                 column(7, 
                        mapviewOutput("map2",
                                      width="100%",
                                      height=550)),
                 column(4, plotOutput("aucplot",
                                      width="100%",
                                      height=550))
             ),
             tags$br(), tags$br(),
             tags$h3("Predicted probabilities map"),
             tags$p("This section predicts the probability that
                   each municipality will experience a new record
                   incidence in the future, i.e. that it has not yet 
                   passed its peak incidence. More specifically, the 
                   map shows the estimated probability that a new 
                   record incidence will occur in the next 30 days. 
                   Darker colours in the map correspond to a higher 
                   probability of setting a new record. In other words,
                   a high probability means that at least one day out 
                   of the next 30 is likely to have an incidence higher 
                   than any observed so far. Likewise, a small probability 
                   means that the daily incidence over the next 30 days is
                   likely to stay less than previous highs."),
             tags$p("Model details. The probabilities are estimated by Cox 
                   regression in the R software (the “coxph” function in
                   the “survival” package). The event being modelled is 
                   the setting of a new record daily standardized incidence.
                   In general, each municipality has set more than one record
                   incidence since the start of its outbreak. The analysis 
                   time is the number of days since the threshold number of 
                   cases was met and the municipality started to be tracked. 
                   The covariates, selected by likelihood ratio test, are:"),
             tags$li("difference between the latest daily incidence and the 
                    record incidence,"),
             tags$li("as before, but based on the incidence 6 days ago,"),
             tags$li("as before, but based on the incidence 7 days ago,"),
             tags$li("state of Brazil (categorical variable)"),
             tags$li("socioeconomic development index (SDI)."),
             tags$p("Clustering within municipality is included as a 
                   frailty term."),
             tags$p("To estimate the probability of a new record being set
                   over the next 30 days, the", 
                    tags$a(href = 'https://stat.ethz.ch/R-manual/R-patched/library/survival/html/predict.coxph.html',
                           'predict'), "method for the", 
                    tags$em("coxph"), "function is used. First, the expected 
                   number of times that a record will be set over the next 30 
                   days is estimated. Then, the estimated probability of
                   any new record being set in that time is calculated
                   as 1-exp(-1 x expected number of new records). This 
                   takes into account the baseline hazard."),
             
             tags$h3("ROC curve"),
             tags$p("To assess the predictive performance of the model, 
                   the data were divided into training and test datasets. 
                   The training dataset was obtained by omitting the last 
                   30 days of data from each municipality. Then, the above
                   Cox regression model was applied to the training dataset, 
                   to predict the 30 days that had been omitted. For each 
                   municipality, the prediction was whether or not a new 
                   record high daily incidence was set. These predictions
                   were compared with the actual occurrence of new record 
                   highs. These results are displayed in the form of a ROC
                   (receiver operating characteristic) curve."),
             tags$br(), tags$br()
    ),
    
    tabPanel(title=uiOutput("tab_title5"),
             tags$h4(uiOutput("download_data_label")),
             numericInput("maxrows",
                          uiOutput("rows_label"), 15),
             downloadButton("downloadCsv",
                            uiOutput("download_label")),
             tags$br(), tags$br(),
             verbatimTextOutput("rawtable"),
             tags$br(),tags$br()
    ),
    
    
    tabPanel(title=uiOutput("tab_title6"),
             tags$div(
                 tags$h4(uiOutput("data_sources")),
                 tags$h5(uiOutput("covid_label")),
                 uiOutput("text1"),
                 tags$br(),
                 tags$h5(uiOutput("age_label")),
                 uiOutput("text2"),
                 tags$br(),
                 tags$h5(uiOutput("age_label2")),
                 uiOutput("text3"),
                 tags$br(),
                 tags$h5(uiOutput("intervention_label")), 
                 uiOutput("text4"),
                 tags$br(),
                 
                 tags$h4(uiOutput("methods")),
                 tags$h5(uiOutput("age_label3")),
                 uiOutput("text5"),
                 tags$br(),
                 
                 tags$h5(uiOutput("covariates")),
                 uiOutput("text6"),
                 
                 tags$br(),
                 
                 tags$h4(uiOutput("authors")),
                 "Dr Oliver Brady",
                 tags$br(),
                 "Dr Paul Mee",
                 tags$br(),
                 "Dr Felipe J Colón-González",
                 tags$br(),
                 "Dr Andreza A de Souza Santos",
                 tags$br(),
                 "Dr Andre Luis Acosta",
                 tags$br(),
                 "Dr Neal Alexander",
                 tags$br(),
                 "CMMID nCov working group",
                 tags$br(),
                 "CADDE working group",
                 
                 tags$br(), tags$br(),
                 tags$h4(uiOutput("funding")), 
                 uiOutput("text14"),
                 
                 
                 tags$br(), tags$br(),
                 tags$h4(uiOutput("disclaimer")), 
                 uiOutput("text7"),
                 
                 tags$br(),
                 tags$h4(uiOutput("contact")),
                 tags$a(href="oliver.brady@lshtm.ac.uk",
                        "Oliver.Brady@lshtm.ac.uk"),
                 "London School of Hygiene and Tropical Medicine",
                 "Keppel Street",
                 "London",
                 "WC1E 7HT",
                 tags$br(), tags$br(), tags$br()
             )
    ),
    tabPanel(title=uiOutput("tab_title7"),
             tags$div(
                 tags$h3("COVID-19 Local Information Comparison"),
                 tags$h4("(CLIC Brazil)"),
                 tags$br(), 
                 tags$h4(uiOutput("intro")),
                 uiOutput("text8"),
                 
                 tags$br(),
                 tags$h4(uiOutput("nat_overview")),
                 uiOutput("text9"),
                 
                 tags$br(),
                 tags$h4(uiOutput("local_area")),
                 uiOutput("text10"),
                 
                 tags$br(),
                 tags$h5(uiOutput("plot1_inset")),
                 imageOutput("saopaulo", 
                             inline=TRUE),
                 
                 tags$br(),tags$br(),
                 tags$h5(uiOutput("timing")),
                 uiOutput("text11"),
                 tags$br(),
                 imageOutput("boxplot", 
                             inline=TRUE),
                 
                 tags$br(),tags$br(),
                 tags$h4(uiOutput("trends")),
                 uiOutput("text12"),
                 tags$br(),
                 imageOutput("quantiles", 
                             inline=TRUE),
                 tags$br(),tags$br(),
                 
                 tags$h4(uiOutput("data_download")),
                 uiOutput("text13"),
                 
                 tags$br(), tags$br(), tags$br()
             )
    )
    
    
)

server <- function(input, output, session) {
    
    output$outcome_label <- renderText({
        switch(input$language, "EN"="Select Variable",
               "PR"="Selecionar variável") 
    })
    
    output$tab_title1 <- renderText({
        switch(input$language, "EN"="National overview",
               "PR"="Visão nacional geral") 
    })
    
    output$date_label1 <- renderText({
        switch(input$language, "EN"="Show cumulatice cases before",
               "PR"="Mostrar casos cumulativos antes de") 
    })
    
    output$tab_title2 <- renderText({
        switch(input$language, "EN"="Local area comparison",
               "PR"="Comparação de áreas") 
    })
    
    output$state_label1 <- renderText({
        switch(input$language, "EN"="Select a State",
               "PR"="Selecione um Estado") 
    })
    
    output$measure_label1 <- renderText({
        switch(input$language, "EN"="Select a measure",
               "PR"="Selecione uma métrica") 
    })
    
    output$munip_label1 <- renderText({
        switch(input$language, "EN"="Select a municipality",
               "PR"="Selecione um município") 
    })
    
    output$tab_title3 <- renderText({
        switch(input$language, "EN"="Trends",
               "PR"="Tendências") 
    })
    
    output$national_state_select <- renderText({
        switch(input$language, "EN"="Select National or a State",
               "PR"="Selecione entre Nacional ou Estado") 
    })
    
    output$state_label_rt1 <- renderText({
        switch(input$language, "EN"="Select a City-State",
               "PR"="Selecione uma Cidade-Estado") 
    })
    output$state_label_rt2 <- renderText({
        switch(input$language, "EN"="Select a City-State",
               "PR"="Selecione uma Cidade-Estado") 
    })
    output$state_label_rt3 <- renderText({
        switch(input$language, "EN"="Select a City-State",
               "PR"="Selecione uma Cidade-Estado") 
    })
    output$state_label_rt4 <- renderText({
        switch(input$language, "EN"="Select a City-State",
               "PR"="Selecione uma Cidade-Estado") 
    })
    
    output$list1 <- renderUI({
        switch(input$language, "EN"=
                   HTML("<ul><li>Currently there is a trend towards higher 
             incidence rates in lower density areas.</li> 
             <li>Currently there are no clear differences between 
             incidence with higher or lower development.</li>
             <li>Currently there is a trend towards higher 
             incidence rates in areas with less access to the 
             sewerage network.</li>
             <li>Currently there is a trend towards higher 
             incidence rates in areas more accessible areas, 
             particularly later on in the epidemic. </li></ul>"),
               "PR"=
                   HTML("<ul><li>Atualmente há uma tendência para maiores taxas 
            de incidência em áreas de menor densidade.</li>
            <li>Atualmente não há diferenças claras entre incidências 
            e maior ou menor desenvolvimento.</li>
            <li>Atualmente há uma tendência para maiores taxas de incidência 
            em áreas com menos acesso à rede de esgoto.</li>
            <li>Atualmente, há uma tendência para maiores taxas 
            de incidência em áreas mais acessíveis, particularmente
            mais tarde na epidemia.</li></ul>")
        )
    })
    
    output$tab_title4 <- renderText({
        switch(input$language, "EN"="Model forecast",
               "PR"="Previsão") 
    })
    
    output$available_label <- renderText({
        switch(input$language, "EN"="Available soon",
               "PR"="Disponível em breve") 
    })
    
    output$tab_title5 <- renderText({
        switch(input$language, "EN"="Download data",
               "PR"="Baixar dados") 
    })
    
    output$download_data_label <- renderText({
        switch(input$language, "EN"="Download local area data",
               "PR"="Baixar dados locais") 
    })
    
    output$tab_title6 <- renderText({
        switch(input$language, "EN"="About this site",
               "PR"="Sobre este site") 
    })
    
    output$data_sources <- renderText({
        switch(input$language, "EN"="Data sources",
               "PR"="Fontes de dados") 
    })
    
    output$covid_label <- renderText({
        switch(input$language, "EN"="(1) COVID-19 Cases",
               "PR"="(1) Casos de COVID-19") 
    })
    
    
    output$age_label <- renderText({
        switch(input$language, "EN"="(2) Age distribution of COVID-19 cases",
               "PR"="(2) Distribuição etária dos casos COVID-19") 
    })
    
    output$age_label2 <- renderText({
        switch(input$language, "EN"="(3) Age distribution data",
               "PR"="(3) Dados de distribuição etária") 
    })
    
    output$intervention_label <- renderText({
        switch(input$language, "EN"="(4) COVID-19 Intervention data",
               "PR"="(4) Dados de intervenção para COVID-19") 
    })
    
    output$methods <- renderText({
        switch(input$language, "EN"="Methods",
               "PR"="Métodos") 
    })
    
    output$age_label3 <- renderText({
        switch(input$language, "EN"="Age and population standardisation process",
               "PR"="Processo de padronização de idade e população") 
    })
    
    output$covariates <- renderText({
        switch(input$language, "EN"="Covariates",
               "PR"="Covariáveis") 
    })
    
    output$authors <- renderText({
        switch(input$language, "EN"="Authors",
               "PR"="Autores") 
    })
    
    output$funding <- renderText({
        switch(input$language, "EN"="Funding",
               "PR"="Financiamento") 
    })
    
    output$disclaimer <- renderText({
        switch(input$language, "EN"="Disclaimer",
               "PR"="Aviso legal") 
    })
    
    output$contact <- renderText({
        switch(input$language, "EN"="Contact",
               "PR"="Contato") 
    })
    
    output$tab_title7 <- renderText({
        switch(input$language, "EN"="How to use",
               "PR"="Como usar") 
    })
    
    output$tab_title8 <- renderText({
        switch(input$language, "EN"="Rt plot",
               "PR"="Gráfico de Rt") 
    })
    
    output$intro <- renderText({
        switch(input$language, "EN"="Introduction",
               "PR"="Introdução") 
    })
    
    output$nat_overview <- renderText({
        switch(input$language, "EN"="National Overview tab",
               "PR"="Guia de visão nacional geral") 
    })
    
    output$local_area <- renderText({
        switch(input$language, "EN"="Local area comparison tab",
               "PR"="Guia de comparação de áreas") 
    })
    
    output$plot1_inset <- renderText({
        switch(input$language, "EN"="Cumulative case plot",
               "PR"="Gráfico de casos acumulados") 
    })
    
    output$timing <- renderText({
        switch(input$language, "EN"="Timing of interventions plot",
               "PR"="Gráfico de momento para intervenções") 
    })
    
    output$trends <- renderText({
        switch(input$language, "EN"="Trends tab",
               "PR"="Guia sobre tendências") 
    })
    
    output$data_download <- renderText({
        switch(input$language, "EN"="Data download tab",
               "PR"="Guia Baixar dados") 
    })
    
    output$text1 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The data on COVID-19 cases aggregated by municipality 
        is obtained from the Brasil.IO COVID-19 project repository which is, 
        updated on a daily basis. Municipality is the second administrative, 
        level in Brazil (below state). There are 5,570 municipalities in, 
        Brazil with an average of 37,728 inhabitants per municipality., 
        This dataset contains confirmed COVID-19 cases and deaths obtained,
        from the bulletins of the State Health Secretariats across the country., 
        Data is obtained from this source and our application is updated daily, 
        at 09:00 GMT. For more information see:
        <a href = 'https://brasil.io/dataset/covid19/caso_full/',>
             brasil.io </a></u>"),
               "PR"=
                   HTML("Os dados dos casos COVID-19 agregados por município são,
             obtidos do repositório do projeto Brasil.IO COVID-19,
             que é atualizado diariamente. Município é o,
             segundo nível administrativo no Brasil (abaixo do estado).,
             Existem 5.570 municípios no Brasil com uma média,
             de 37.728 habitantes por município. Este conjunto de dados,
             contém casos confirmados de COVID-19 e mortes obtidas de,
             boletins das Secretarias Estaduais de Saúde em todo o país,
             país. Os dados são obtidos dessa fonte e nosso,
             aplicativo é atualizado diariamente às 09:00 GMT. Para mais,
             informações, consulte:
        <a href = 'https://brasil.io/dataset/covid19/caso_full/',>
        brasil.io </a></u>")
        )
    })
    
    output$text2 <- renderUI({
        switch(input$language, "EN"=
                   HTML("This was derived from data on notified COVID-19 cases
                 reported throughout Brazil between 2nd Feb and 8th June,
                 2020 collected by the Brazilian Ministry of Health and is
                 used with their permission.</u>"),
               "PR"=
                   HTML("Isso foi derivado de dados sobre casos notificados 
                    do COVID-19 reportado em todo o Brasil entre 2 de 
                    fevereiro e 8 de junho 2020 coletado pelo Ministério da 
                    Saúde do Brasil e é usado com a permissão deles.</u>")
        )
    })
    
    output$text3 <- renderUI({
        switch(input$language, "EN"=
                   HTML("Data on the distribution of the population by age 
                        for each municipality was obtained from the Instituto
                        Brasileiro de Geografia e Estatisitica (IBGE) national 
                        demographic census for 2010. The data was downloaded
                        from <a href = 'https://sidra.ibge.gov.br/tabela/3107',>
                        IBGE </a></u>"),
               "PR"=
                   HTML("Dados sobre a distribuição da população por idade 
                        para cada um município foi obtido do Instituto 
                        Brasileiro de Censo demográfico nacional Geografia 
                        e Estatisitica (IBGE) para 2010. Os dados foram 
                        baixados de <a href = 'https://sidra.ibge.gov.br/tabela/3107',>
                        IBGE </a></u>")
        )
    })
    
    output$text4 <- renderUI({
        switch(input$language, "EN"=
                   HTML("Data on the types of interventions implemented, and 
                   the dates of their introduction were extracted from data 
                   collated by the 
                   <a href='https://www.cepal.org/en/topics/covid-19',>
                   Cepal Observatory </a> with edits and updates on timing of
                   interventions at the state level by Dr Andreza A de Souza 
                   Santos, director of the Brazilian Studies
                   <a href='https://www.lac.ox.ac.uk/brazilian-studies-programme#/',>
                   Programme </a>. This dataset is summarised in a recent 
                   pre-print 
                   <a href='https://www.medrxiv.org/content/10.1101/2020.04.25.20077396v1',>
                   manuscript </a> </u>"),
               "PR"=
                   HTML("Dados sobre os tipos de intervenções implementadas e as
                        datas de introdução foram extraídas dos dados ordenado 
                        pelo
                       <a href='https://www.cepal.org/en/topics/covid-19',>
                        Cepal Observatory </a> com edições e atualizações pelo
                        Dr Andreza A de Souza Santos, diretora do Programa
                        de Estudos  
                        <a href='https://www.lac.ox.ac.uk/brazilian-studies-programme#/',>
                        Brasileiros </a>. O conjunto de dados está resumido em 
                        uma recente
                       <a href='https://www.medrxiv.org/content/10.1101/2020.04.25.20077396v1',>
                        pré-impressão </a>.</u>")
        )
    })
    
    output$text5 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The aim of our standardisation process is to make
                   data comparable between areas that have different 
                   population sizes and age profiles. We first assign the 
                   number of cases reported in each area (1) a hypothetical
                   age distribution based on the Brazilian national age 
                   distribution of COVID-19 cases (2). We then use local 
                   area age-stratified population data (3) to calculate 
                   age-specific incidence (standardised incidence) which 
                   is a measure that is comparable between areas. A measure
                   of standardised incidence of COVID-19 cases per 10,000 
                   inhabitants is then calculated based on the national 
                   age profile of Brazil for visualisation in this 
                   application. Days since the start of the outbreak are
                   calculated from whenever each area passes a cumulative 
                   incidence threshold equivalent to 1 case per 10,000 
                   inhabitants. </u>"),
               "PR"=
                   HTML("O objetivo do nosso processo de padronização é criar
                        dados comparáveis entre áreas com diferentes tamanhos
                        de populações e perfis de idade. Primeiro atribuímos
                        o número de casos relatados em cada área (1) uma 
                        distribuição etária hipotética com base na 
                        distribuição etária nacional brasileira dos casos 
                        COVID-19 (2). Em seguida, usamos os dados da 
                        população estratificada por área local dados 
                        (3) para calcular a incidência específica por
                        idade (incidência padronizada), que é uma medida 
                        comparável entre as áreas. Uma medida da incidência
                        padronizada de casos COVID-19 por 10.000 habitantes 
                        é então calculada com base no perfil etário nacional
                        do Brasil para visualização nestas aplicações. Os 
                        dias desde o início do surto são calculados a partir 
                        do momento em que cada área ultrapassar um limiar de
                        incidência cumulativo equivalente a 1 caso por 10.000
                        habitantes.</u>")
        )
    })
    
    output$text6 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The Socio Demographic Index (SDI) is a composite 
                        relative measure of development ranging from 0 
                        (lowest) to 1 (highest). It is calculated using 
                        the average rankings of income per capita, fertility
                        rate and mean years of education among all
                        municipalities as measured by the 2010 IBGE census. 
                        Variables for population density, access to piped 
                        water and access to sewage system or septic tank were 
                        also obtained from the 2010 IBGE census. Travel time
                        to biggest city in the state was calculated using 
                        <a href='https://www.worldpop.org',>  WorldPop </a> 
                        population data and the 
                        <a href='https://developers.google.com/earth-engine/datasets/catalog/Oxford_MAP_friction_surface_2015_v1_0',> 
                        Malaria Atlas Project </a>
                        travel time friction surface using the 
                        <a href='https://malariaatlas.org/application-project/malariaatlas_package/',>
                        malariaAtlas </a> R 
                        package accumulated cost route finding algorithm. 
                        Travel time represents the land-based travel time 
                        between the most densely populated area in the 
                        municipality that is experiencing the COVID-19 
                        outbreak and the most densely populated area in the 
                        corresponding state. We may expect higher COVID-19 
                        incidence rates in highly accessible areas due to 
                        frequent re-introduction of the virus, or conversely 
                        we may expect higher incidence rates in remote areas
                        due to the increased challenge of adhering to movement
                        restrictions.  </u>"),
               "PR"=
                   HTML("O Índice Sociodemográfico (SDI) é uma medida 
                    relativa de desenvolvimento composta que varia de 0 
                    (mais baixo) a 1 (mais alto). É calculado usando as 
                    classificações médias de renda per capita, taxa de 
                    fertilidade e anos médios de educação entre todos os 
                    municípios, conforme medido pelo Censo do IBGE de 2010. 
                    Variáveis para densidade populacional, acesso à água 
                    encanada e acesso ao sistema de esgoto ou fossa séptica
                    também foram obtidas no censo do IBGE de 2010. O tempo 
                    de viagem para a maior cidade do estado foi calculado 
                    usando dados da população do 
                    <a href='https://www.worldpop.org',>  WorldPop </a> 
                    e a superfície de atrito do tempo de viagem do
                    <a href='https://developers.google.com/earth-engine/datasets/catalog/Oxford_MAP_friction_surface_2015_v1_0',> 
                    Malaria Atlas Project </a> usando o usando o algoritmo 
                    de localização de rotas de custo acumulado do pacote R
                    <a href='https://malariaatlas.org/application-project/malariaatlas_package/',>
                    malariaAtlas </a>. O tempo de viagem representa o tempo 
                    de deslocamento em terra entre a área mais densamente 
                    povoada do município que está sofrendo o surto de 
                    COVID-19 e a área mais densamente povoada no estado
                    correspondente. Podemos esperar taxas de incidência mais
                    altas do COVID-19 em áreas altamente acessíveis devido à 
                    reintrodução frequente do vírus, ou, inversamente, 
                    podemos esperar taxas de incidência mais altas em áreas 
                    remotas devido ao maior desafio de aderir às restrições
                    de movimento.</u>")
        )
    })
    
    output$text7 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The aim of this site is to complement the resources 
                        provided by the Brazilian ministry of health. This 
                        app has been developed for research purposes only 
                        and is not suitable to use as medical advice or to 
                        assess your personal level of risk. Data are provided 
                        without any warranty of any kind, either express or 
                        implied. The entire risk arising out of the use or 
                        performance of the app and associated data remains 
                        with you. In no event shall the London School of 
                        Hygiene and Tropical Medicine (LSHTM), the authors, 
                        or anyone else involved in the creation, production, 
                        or delivery of the app be liable for any damages 
                        whatsoever including, without limitation, damages for 
                        loss of business profits, business interruption, loss
                        of business information, or other pecuniary loss 
                        arising out of the use of or inability to use this 
                        app even if LSHTM has been advised of the possibility
                        of such damages. </u>"),
               "PR"=
                   HTML("O objetivo deste site é complementar os recursos 
                        fornecidos pelo Ministério da Saúde do Brasil. 
                        Este aplicativo foi desenvolvido apenas para fins de
                        pesquisa e não é adequado para use como orientação 
                        médica ou para avaliar seu nível pessoal de risco. Os
                        dados são fornecidos sem qualquer garantia de qualquer
                        tipo expresso ou implícito. Todo o risco decorrente de
                        o uso ou desempenho do aplicativo e dados associados
                        permanece com você. Em nenhum caso a London School of
                        Higiene e Medicina Tropical (LSHTM), os autores ou
                        qualquer pessoa envolvida na criação, produção ou
                        a entrega do aplicativo será responsável por quaisquer
                        danos incluindo, sem limitação, danos por perda de
                        negócios lucros, interrupção de negócios, perda de 
                        informações comerciais ou outra perda pecuniária
                        decorrente do uso de ou incapacidade de usar este
                        aplicativo, mesmo que o LSHTM tenha sido alertada da
                        possibilidade de tais danos. </u>")
        )
    })
    
    output$text8 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The COVID-19 Local Information Comparison (CLIC 
                        Brazil) web application allows local public health
                        decision makers and researchers to compare the current
                        COVID-19 epidemic in different local areas 
                        (municipalities) across Brazil. It aims to:
                        <li> Identify hotspots of COVID-19 disease.</li>
                        <li>Compare the outbreak trajectories between
                        municipalities to understand where the epidemic is 
                        growing fastest.</li>
                        <li>Assess the socioeconomic drivers of COVID-19 risk.</li> </u>"),
               "PR"=
                   HTML("O aplicativo web de comparação de informações locais 
                        COVID-19 (CLIC Brasil) permite que tomadores de decisão
                        e pesquisadores em saúde pública comparem a atual 
                        epidemia de COVID-19 em diferentes áreas (municípios) 
                        em todo o Brasil. Tem como objetivo:
                        <li> Identifique os pontos ativos da doença COVID-19.</li>
                        <li> Compare as trajetórias de surto entre municípios 
                        para entender onde a epidemia está crescendo mais 
                        rapidamente. </li>
                        <li> Avalie os fatores socioeconômicos do risco COVID-19.</li></u>")
        )
    })
    
    output$text9 <- renderUI({
        switch(input$language, "EN"=
                   HTML("This tab shows a map of Brazil in which each 
                        municipality with a COVID-19 outbreak is indicated by 
                        a circle. By selecting the options under 'Select 
                        Variable', the map can be configured to display either
                        the total number of COVID-19 cases or the incidence per 
                        10,000 people in the population. Cirlces with a dartker 
                        shading indicate higher values. By sliding the 'date'
                        button you can get a snapshot of the epidemic on 
                        previous days and explore how the epidemic has evolved.
                        The plots and text box provide summary information on 
                        the growth of the national epidemic. </u>"),
               "PR"=
                   HTML("Esse guia mostra um mapa do Brasil em que cada 
                        município com um número de COVID-19 é indicado por um
                        círculo. Ao selecionar as opções em 'Selecionar
                        variável', o mapa pode ser configurado para exibir o
                        número total de casos COVID-19 ou com incidência de 
                        10.000 pessoas na população. Cirlces com um sombreador 
                        de dartker ver valores mais altos. Ao deslizar o botão
                        'data', você pode obter um instantâneo da epidemia nos 
                        dias anteriores e explorar como a epidemia evoluiu. 
                        As parcelas e uma caixa de texto contêm informações 
                        resumidas sobre o crescimento da epidemia nacional. </u>")
        )
    })
    
    output$text10 <- renderUI({
        switch(input$language, "EN"=
                   HTML("The main graph on this tab shows the growth of the 
                   epidemic over time in each muncipality. Case numbers have
                   been 'age-standardised' to allow comparisons between 
                   municipalities with different population sizes and age 
                   structures. The timing of the start of the outbreak in each
                   muncipality has also been standardised to the first day 
                   on which a case incidence of greater than 1 case per 10,000 
                   residents was reported. - Using the top pull-down list on 
                   the left you select the state in which you municipality of 
                   interest is located the trajectories for all municpalities 
                   in the state will be highlighted in orange on the plot. 
                   - The central pull-down list allows you to select the 
                   municipality of interest, these are labelled with the 
                   Municipality name and the State name. - The lower pull-down
                   allows you to select the measure of interest from the
                   following options; Cumulative case incidence, Cumulative
                   death incidence, hospital bed occupancy and ITU bed 
                   occupancy (The details of the methods used to calculate 
                   these parameters are given in the 'About this site' tab) 
                   </br> </br> For example, if you compare the trajectory for 
                    cumulative case incidence for São Paulo municipality with 
                    the trajectories for all municipalities in São Paolo 
                    State and (orange) and all municipalities in Brazil, you 
                    would see this output: </u>"),
               "PR"=
                   HTML("O gráfico principal desta guia mostra o crescimento da 
                    epidemia ao longo do tempo em cada município. Os números de 
                    casos foram ‘padronizados por idade’ para permitir 
                    comparações entre municípios com diferentes tamanhos 
                    populacionais e estruturas etárias. O momento do início do 
                    surto em cada município também foi padronizado para o 
                    primeiro dia em que foi relatada uma incidência de casos
                    superior a 1 caso por 10.000 habitantes. - Usando a lista 
                    suspensa superior à esquerda, você seleciona o estado em 
                    que seu município de interesse está localizado. As 
                    trajetórias de todos os municípios do estado serão 
                    destacadas em laranja no gráfico. - A lista suspensa 
                    central permite selecionar o município de interesse; estes
                    são rotulados com o nome do município e o nome do estado. 
                    - O menu suspenso inferior permite selecionar a medida de 
                    interesse das seguintes opções; Incidência cumulativa de 
                    casos, incidência cumulativa de óbitos, ocupação de leitos
                    hospitalares e ocupação de leitos de UTI (os detalhes dos 
                    métodos usados para calcular esses parâmetros são 
                    fornecidos na guia 'Sobre este site') </br> </br> 
                    Por exemplo, se você comparar a trajetória de incidência
                    cumulativa de casos no município de São Paulo com as 
                    trajetórias de todos os municípios do estado de São Paulo 
                    e (laranja) e de todos os municípios do Brasil, você verá 
                    este resultado: </u>")
        )
    })
    
    output$text11 <- renderUI({
        switch(input$language, "EN"=
                   HTML("This graph shows the time different interventions were 
                        announced relative to when the epidemic began in each 
                        municipality. The time origin (day 0) for each 
                        municipality is the first day on which a case incidence
                        of greater than 1 case per 10,000 residents was reported. 
                        Each black dot represents one municipality with the red 
                        dots representing the municipality selected in the 
                        dropdown menu. Dots in the blue area represent 
                        municipalities where the intervention was announced 
                        before the local Covid-19 epidemic began while dots in 
                        the red area show municipalities where interventions 
                        were only announced after the outbreak had begun. The 
                        box plot summarises the median, interquartile range and 
                        range of timings for each municipality. The further 
                        right the red dot is on this plot the later in the 
                        epidemic interventions were announced compared to other 
                        areas. </u>"),
               "PR"=
                   HTML("Este gráfico mostra o momento em que diferentes 
                        intervenções foram anunciadas em relação ao início da
                        progressão da epidemia em cada município. O momento 
                        inicial (dia 0) em cada município é o primeiro dia em 
                        que foi relatada uma incidência de caso superior a 1 
                        caso por 10.000 residentes. Cada ponto preto representa 
                        um município, com os pontos vermelhos representando o
                        município selecionado no menu suspenso. Os pontos na 
                        área azul representam os municípios onde a intervenção 
                        foi anunciada antes do início da epidemia de Covid-19, 
                        enquanto os pontos na área vermelha mostram os 
                        municípios onde as intervenções só foram anunciadas 
                        após o início do surto. O gráfico da caixa resume a 
                        mediana, intervalo interquartil e intervalo de tempos 
                        para cada município. Quanto mais à direita o ponto 
                        vermelho estiver nessa trama, mais tarde nas intervenções 
                        epidêmicas foram anunciadas em comparação com outras 
                        áreas. </u>")
        )
    })
    
    output$text12 <- renderUI({
        switch(input$language, "EN"=
                   HTML("This tab allows the user to compare the characteristics
                   of municipalities with worse or better COVID-19 epidemics. 
                   Using the pulldown tab you can select to view either national
                   data or the data for one particular state. 
                   </br> </br>
                   The barcharts show differences in age-standardised 
                   incidence at different points in the epidemic for subsets of
                   municipalities grouped according to particular characteristics. 
                   The vertical line shows the range of the data for each subset
                   and the rectangle shows the value for the median and 
                   interquartile range. The sets of lines show the data in 10
                   day intervals (from 10 to 70 days) from the day at which an 
                   incidence of 1 case per 10,000 people was reported in a 
                   municipality. </br>
                   </br> For example, the upper left plot (shown below) shows 
                        the association between area population density and 
                        incidence. The larger the gap between the bars the 
                        larger the difference in the COVID-19 epidemic between
                        high and low population density areas. More detailed 
                        analysis can be conducted by downloading the data from 
                        the data download tab.  </u>"),
               "PR"=
                   HTML("Essa guia permite ao usuário comparar as características 
                        dos municípios com piores ou melhores epidemias de 
                        COVID-19. Usando a guia suspenso, você pode selecionar
                        para visualizar os dados nacionais ou os dados de um 
                        estado específico. </br> </br>
                        Os gráficos de barras mostram
                        diferenças na incidência padronizada por idade em
                        diferentes pontos da epidemia para subconjuntos de 
                        municípios agrupados de acordo com características 
                        particulares. A linha vertical mostra o intervalo dos
                        dados para cada subconjunto e o retângulo mostra o valor
                        do intervalo mediano e interquartil. Os conjuntos de 
                        linhas mostram os dados em intervalos de 10 dias (de 10 
                        a 70 dias) a partir do dia em que uma incidência de 1 
                        caso por 10.000 pessoas foi relatada em um município.
                        </br> </br>
                        Por exemplo, o gráfico abaixo mostra a associação 
                        entre a densidade populacional da área e a incidência. 
                        Quanto maior a diferença entre as barras, maior a 
                        diferença na epidemia de COVID-19 entre áreas de alta e
                        baixa densidade populacional. Uma análise mais detalhada 
                        pode ser realizada baixando os dados na guia de download
                        de dados. </u>")
        )
    })
    
    output$text13 <- renderUI({
        switch(input$language, "EN"=
                   HTML("If you wish to carry out your own analyses on the
                   standardised data you can download the full dataset in comma
                   separated variable (csv) format from here. A description of 
                   the variables included is shown below. 
                   </br><b> area - </b> Municipality name and State (2 letter 
                   code). (NB UTF-8 encoding should be used to correctly format
                   the place names) 
                   </br><b> date_end - </b> Date of data update (there is one
                   row per municipality per day) 
                   </br><b> cum_cases - </b> Cumulative case count 
                   </br><b> cum_deaths - </b> Cumulative death count 
                   </br><b> bed_occ_2_5,bed_occ_50 & bed_occ_97_5 - </b> 2.5%, 
                   50% & 97.5% credible intervals for the predicted number of 
                   hospital beds occupied by patients with COVID-19 based on 
                   the cumulative data 
                   </br><b> itu_bed_occ_2_5,itu_bed_occ_50 & itu_bed_occ_97_5 - </b>
                   2.5%, 50% & 97.5% credible intervals for the predicted number
                   of Intensive care unit beds occupied by patients with COVID-19 
                   based on the cumulative data. 
                   </br><b> standardised_cases - </b> Age standardised cumulative
                   case incidence 
                   </br><b> statdardised_deaths - </b> Age standardised 
                   cumulative death incidence 
                   </br><b> stan_bed_occ_2_5,stan_bed_occ_50 & 
                   stan_bed_occ_97_5 - </b>
                   2.5% 50% & 97.5% credible intervals for the predicted number 
                   of hospital beds occupied by patients with COVID-19 based on
                   the age standardised data. 
                   </br><b> stan_itu_bed_occ_2_5,stan_itu_bed_occ_50 & 
                   stan_itu_bed_occ_97_5 - </b> 2.5% 50% & 97.5% credible 
                   intervals for the predicted number of Intensive care unit 
                   beds occupied by patients with COVID-19 based on the age 
                   standardised data. 
                   </br><b> region - </b> State (2 letter code) 
                   </br><b> popden - </b> Population Density (individuals per 
                   km<sup>2</sup>) 
                   </br><b> sdi - </b> Social demogrpahic index for the 
                   mucipality (See Covariates in the 'About this site' - tab)
                   </br><b> piped_water - </b> Proportion of population in the 
                   municipality with access to a piped water supply
                   </br><b> sewage_or_septic - </b> Proportion of population in 
                   the municipality with access to sewage system or a septic 
                   tank 
                   </br><b> travel_time - </b> The land-based travel time between 
                   the most densely populated area in the municipality and the
                   most densely populated area in the corresponding State
                   </br><b> x - </b> The longitude of the municiplaity in decimal 
                   degrees 
                   </br><b> y - </b> The latitude of the municiplaity in decimal
                   degrees
                   </br><b> days_since_start - </b> The number of days since the
                   age standardised incidene was greater than 1 case per 10,000 
                   residents
                    </br></br>
                    The following variables are indicators which are coded 1 if 
                    the intervention described had been initiated by this date; 
                    </br><b> awareness_campaigns_start - </b> COVID-19 awareness 
                    campaigns 
                    </br><b> border_closure_start - </b> International border 
                    closures 
                    </br><b> domestic_travel_restrictions_start - </b> Domestic 
                    travel restrictions 
                    </br><b> economic_measures_start - </b> Economic interventions
                    </br><b> border_health_screening_start - </b> Health screening 
                    at international borders
                    </br><b> international_flight_suspension_start - </b> 
                    International flights suspended 
                    </br><b> isolation_quarrantine_start - </b>  Isolation and 
                    quarantine for those infected with COVID-19
                    </br><b> limit_on_public_gatherings_start -  </b> A limit 
                    was placed on public gatherings 
                    </br><b> schools_closure_start - </b>  School closures 
                    initiated 
                    </br><b> workplace_closure_start - </b>  Workplace closures 
                    initiated </u>"),
               "PR"=
                   HTML("Se você deseja realizar suas próprias análises nos 
                        dados padronizados, pode fazer o download do conjunto 
                        de dados completo em formato de variável separada por
                        vírgula (csv) aqui. Uma descrição das variáveis 
                        incluídas é mostrada abaixo.
                        </br><b> area </b> - Nome e estado do município (código
                        de 2 letras). (A codificação NB UTF-8 deve ser usada 
                        para formatar corretamente os nomes dos locais)
                        </br><b> date_end </b> - Data da atualização dos dados (há 
                        uma linha por município por dia)
                        </br><b> cum_cases </b> - Contagem acumulada de casos
                        </br><b> cum_deaths </b> - Contagem de óbitos acumulados
                        </br><b> bed_occ_2_5,bed_occ_50 & bed_occ_97_5 </b> - 
                        Intervalos credíveis de 2,5% 50% e 97,5% para o número
                        previsto de leitos hospitalares ocupados por pacientes 
                        com COVID-19 com base nos dados acumulados 
                        </br><b> itu_bed_occ_2_5,itu_bed_occ_50 & 
                        itu_bed_occ_97_5 </b> - Intervalos credíveis de 2,5%; 
                        50% e 97,5% para o número previsto de leitos de unidades 
                        de terapia intensiva (UTI) ocupados por pacientes com 
                        COVID-19 com base nos dados acumulados.
                        </br><b> standardised_cases </b> - Incidência cumulativa 
                        de casos padronizada por idade
                        </br><b> statdardised_deaths </b> - Incidência de óbito
                        cumulativa padronizada por idade
                        </br><b> stan_bed_occ_2_5,stan_bed_occ_50 & 
                        stan_bed_occ_97_5 </b> - Intervalos credíveis de 2,5%; 
                        50% e 97,5% para o número previsto de leitos hospitalares 
                        ocupados por pacientes com COVID-19 com base nos dados 
                        padronizados por idade.
                        </br><b> stan_itu_bed_occ_2_5,stan_itu_bed_occ_50 &
                        stan_itu_bed_occ_97_5 </b> - Intervalos credíveis de 2,5%; 
                        50% e 97,5% para o número previsto de leitos de unidades 
                        de terapia intensiva ocupados por pacientes com COVID-19 
                        com base nos dados padronizados por idade
                        </br><b> region </b> - Estados (código de duas letras)
                        </br><b> popden </b> - Densidade populacional (indivíduos 
                        por km<sup>2</sup>)
                        </br><b> sdi </b> - Índice sociodemográfico do município 
                        (consulte Covariáveis na guia 'Sobre este site')
                        </br><b> piped_water </b> - Proporção da população no 
                        município com acesso a um abastecimento de água encanada.
                        </br><b> sewage_or_septic </b> - Proporção da população 
                        no município com acesso ao sistema de esgoto ou a uma 
                        fossa séptica
                        </br><b> travel_time </b> - O tempo de viagem terrestre 
                        entre a área mais densamente povoada do município e a 
                        área mais densamente povoada do Estado correspondente. 
                        </br><b> x </b> - longitude do município em graus 
                        decimais
                        </br><b> y </b>- latitude do município em graus decimais.
                        </br><b> days_since_start </b> - O número de dias desde 
                        a taxa padronizada de idade superior a 1 caso por 10.000 
                        residentes
                        </br></br>
                        As seguintes variáveis são indicadores codificados em 1 
                        se a intervenção descrita tivesse sido iniciada até essa 
                        data;
                        </br><b> awareness_campaigns_start </b> - Campanhas de
                        conscientização sobre COVID-19
                        </br><b> border_closure_start </b> - Fechamento de
                        fronteiras internacionais
                        </br><b> domestic_travel_restrictions_start </b> - 
                        Restrições de viagens domésticas 
                        </br><b> economic_measures_start </b> - Intervenções 
                        econômicas 
                        </br><b> border_health_screening_start </b> - Rastreio 
                        de saúde nas fronteiras internacionais
                        </br><b> international_flight_suspension_start </b> - 
                        Voos internacionais suspensos
                        </br><b> isolation_quarrantine_start </b> - Isolamento 
                        e quarentena para aqueles infectados com COVID-19
                        </br><b> limit_on_public_gatherings_start </b>
                        - Foi estabelecido um limite para reuniões públicas
                        </br><b> schools_closure_start </b> - Início do
                        fechamento da escola
                        </br><b> workplace_closure_start </b> - Fechos de
                        trabalho iniciados </u>")
        )
    })
    
    output$text14 <- renderUI({
        switch(input$language, "EN"=
                   HTML("This app was developed as part of the CADDE 
                        <a href = 'https://www.caddecentre.org',> project </a>
                        This project was supported through a São Paulo Research 
                        Foundation (FAPESP) and Medical Research Council CADDE 
                        partnership award (MR/S0195/1 and FAPESP 18/14389-0). </u>"),
               "PR"=
                   HTML("Este site foi desenvolvido como parte do projeto 
                         <a href = 'https://www.caddecentre.org',> CADDE </a>.
                         Este projeto foi apoiado por la Fundação de Amparo 
                         à Pesquisa do Estado de São Paulo (FAPESP) e 
                         un premio de parceria entre CADDE e Conselho de
                         Pesquisa Médica (MR/S0195/1 e FAPESP 18/14389-0) </u>")
        )
    })
    
    
    spatial_reactive_db <- reactive({
        
        db <- Brazil_cases_sp %>%
            dplyr::filter(date_end <= input$plot_date)
        
        if (input$outcome_select=="Cases/Casos") { 
            
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
        if (input$outcome_select=="Case incidence/Incidência de casos") { 
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
        if (input$outcome_select=="Death incidence/Incidência de óbitos") { 
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
        if (input$outcome_select=="Deaths/Óbitos") { 
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
                                            "<br/>",                   
                                            popup, ": ",
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
    
    y_lab1 <- reactive({
        if(input$language=="EN"){
            return("Cumulative cases")
        }
        if(input$language=="PR"){
            return("Casos acumulados")
        }
    })
    
    y_lab2 <- reactive({
        if(input$language=="EN"){
            return("Municipalities with > 50 cases")
        }
        if(input$language=="PR"){
            return("Municípios com > 50 casos")
        }
    })
    
    x_lab1 <- reactive({
        if(input$language=="EN"){
            return("Date")
        }
        if(input$language=="PR"){
            return("Data")
        }
    })
    
    output$cumulative_plot <- renderPlot({
        p1 <- ggplot(cumulative_reactive_db(), aes(x = date_end, 
                                                   y = cum_cases)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d",
                         limits = c(min(Brazil_cases_time$date_end),
                                    DateUntil)) +
            xlab(x_lab1()) + ylab(y_lab1())
        
        
        p2 <- ggplot(munic_reactive_db(), aes(x = date_end,
                                              y = Area)) +
            geom_line() + 
            theme_minimal() +
            scale_x_date(date_labels = "%b %d",
                         limits = c(min(Brazil_cases_time$date_end),
                                    DateUntil)) +
            xlab(x_lab1()) + ylab(y_lab2())
        
        ggarrange(p1, p2, nrow=2)
    })
    
    area_db1 <- reactive({
        
        maxDaysSince <- max(x_dat$Days_since_start[x_dat$Area == input$area_select])
        x_dat_com    <- x_dat[x_dat$Days_since_start <= maxDaysSince, ]
        
        # compariosn matrix
        comp_mat <- cbind(x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 1],
                          x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 2],
                          x_dat[x_dat$Area == input$area_select,
                                "standardised_cases"] >= timeSUM[, 3])
        comp_mat_sum <- as.logical(apply(comp_mat, 2, median))
        
        if(all(comp_mat_sum)){OB_sum = "above average"}
        if(sum(comp_mat_sum) < 3){OB_sum = "average"}
        if(sum(comp_mat_sum) < 2){OB_sum = "below average"}
        
        if(all(comp_mat_sum)){OB_sum2 = "abaixo da média"}
        if(sum(comp_mat_sum) < 3){OB_sum2 = "média"}
        if(sum(comp_mat_sum) < 2){OB_sum2 = "acima da média"}
        
        
        if(input$outcome_select2=="Hospital bed occupancy / Ocupação de leitos hospitalares") {
            x_dat %<>%
                dplyr::rename(outcome=Stan_Bed_occ_50) %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        } else if(input$outcome_select2=="Cumulative death incidence / Incidência acumulada de óbitos") {
            x_dat %<>%
                dplyr::rename(outcome=standardised_deaths)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2),
                              outcome=outcome  * 1000)
        } else if(input$outcome_select2=="ITU bed occupancy / Ocupação de leitos de UTI"){
            x_dat %<>%
                dplyr::rename(outcome= Stan_ITU_Bed_occ_50)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
        } else if(input$outcome_select2=="Cumulative case incidence / Incidência acumulada de casos"){
            x_dat %<>%
                dplyr::rename(outcome=standardised_cases)  %>%
                dplyr::mutate(Region=str_sub(Area, start= -2))
            
        }
        
        if(input$outcome_select2=="Hospital bed occupancy / Ocupação de leitos hospitalares" &
           input$language=="EN") {
            my_label <- "Hospital bed occupancy \n per 10,000 residents (log scale)"
        }
        if(input$outcome_select2=="Hospital bed occupancy / Ocupação de leitos hospitalares" &
           input$language=="PR") {
            my_label <- "Ocupação de leitos hospitalares por 10.000 \n residentes (escala logarítmica)"
        }
        if(input$outcome_select2=="ITU bed occupancy / Ocupação de leitos de UTI" &
           input$language=="EN") {
            my_label <- "ITU bed occupancy per 10,000 residents \n (log scale)"
        }
        if(input$outcome_select2=="ITU bed occupancy / Ocupação de leitos de UTI" &
           input$language=="PR") {
            my_label <- "Ocupação de leitos em UTI por 10.000 \n residentes (escala logarítmica)"
        }
        if(input$outcome_select2=="Cumulative death incidence / Incidência acumulada de óbitos" &
           input$language=="EN") {
            my_label <- "Standardised death incidence per 10,000 \n residents (log scale)"
        }
        if(input$outcome_select2=="Cumulative death incidence / Incidência acumulada de óbitos" &
           input$language=="PR") {
            my_label <- "Incidência padronizada de óbitos por 10.000 \n residentes (escala logarítmica)"
        }
        if(input$outcome_select2=="Cumulative case incidence / Incidência acumulada de casos" &
           input$language=="EN"){
            my_label <- "Standardised case incidence per 10,000 \n residents (log scale)"
        }
        if(input$outcome_select2=="Cumulative case incidence / Incidência acumulada de casos" &
           input$language=="PR"){
            my_label <- "Incidência padronizada de casos por 10.000 \n residentes (escala logarítmica)"
        }
        
        
        if(input$language=="EN"){
            x_label <- "Days since start of the outbreak (incidence above 1 case per 10,000 residents)" 
        }
        if(input$language=="PR"){
            x_label <- "Dias desde o início do surto (incidência acima de 1 caso por 10.000 residentes)"
        }
        
        outdata <- list(of1=x_dat,
                        of2=x_label,
                        of3=OB_sum,
                        of4=OB_sum2,
                        of5=my_label)
        
    })
    
    output$rawtable2 <- renderPrint({
        orig <- options(width = 1000)
        print(head(area_db1()$of1$Area), row.names = FALSE)
        options(orig)
    })
    
    output$area_selector <- renderUI({# creates Munip select box object called in ui
        
        areas <- as.character(
            Brazil_cases_sp$Area[as.character(Brazil_cases_sp$Name) ==
                                     input$region_select & Brazil_cases_sp$cum_cases > 100])
        data_available <- unique(sort(areas[!is.na(areas)]))
        pickerInput(inputId = "area_select", # name of input
                    label = h5("Select a Municipality"), # label displayed in ui
                    choices = data_available, # calls list of available counties
                    selected = data_available[1])
    })
    
    
    output$p1 <- renderPlot({
        
        g1 <-  ggplot(area_db1()$of1, aes(x = Days_since_start,
                                          y = outcome,
                                          group = Area,
                                          # use "text" for hovering with plotly
                                          # text = paste0(input$outcome_select2,
                                          #               ": ",
                                          #               round(outcome,2)))
        )) +
            theme_minimal() +
            xlab(area_db1()$of2) +
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
                      color = "#ef6f6a", size=1.2) +
            labs(y=area_db1()$of5) +
            ggtitle(input$outcome_select2) +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(trans='log10') +
            geom_label(data = area_db1()$of1[area_db1()$of1$Area == input$area_select, ] %>% 
                           filter(date_end == last(as.Date(date_end))), 
                       aes(label = Area), 
                       color = "#ef6f6a")
        g1
    })
    
    
    output$legend <- renderImage({
        
        if(input$language=="EN"){
            return(list(
                src = "input_data/legend2.png",
                contentType = "image/png",
                alt = "Legend"
            ))  
        }
        if(input$language=="PR"){
            return(list(
                src = "input_data/legend2pr.png",
                contentType = "image/png",
                alt = "Legend"
            ))  
        }
        
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
        if(input$language=="EN"){
            paste("After accounting for different population sizes",
                  "and age structures of COVID-19 affected municipalities,",
                  "the outbreak in",
                  substr(input$area_select, 1, nchar(input$area_select)-3),
                  "is currently tracking", "<b>", area_db1()$of3, "</b>",
                  "compared to other areas in Brazil")
        }
        if(input$language=="PR")
            paste("Após considerar os diferentes tamanhos populacionais e estruturas etárias dos municípios afetados pelo COVID-19, o surto em",
                  substr(input$area_select, 1, nchar(input$area_select)-3),
                  "está rastreando atualmente", "<b>", area_db1()$of4, "</b>",
                  "em comparação com outras áreas no Brasil")
    })
    
    
    plotdb <- reactive({
        
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
    
    gg_title <- reactive({
        if(input$language=="EN"){
            return("Timing of interventions in different municipalities")
        }
        if(input$language=="PR"){
            return("Momento das intervenções em diferentes municípios")
        }
    })
    
    
    y_label3 <- reactive({
        if(input$language=="EN"){
            return("Days since start of the outbreak (incidence above 1 case per 10,000 residents)")
        }
        if(input$language=="PR"){
            return("Dias desde o início do surto (incidência acima de 1 caso por 10.000 residentes)")
        }
    })
    
    plot_anot1 <- reactive({
        if(input$language=="EN"){
            return("Before outbreak")
        }
        if(input$language=="PR"){
            return("Antes do surto")
        }
    })
    
    plot_anot1 <- reactive({
        if(input$language=="EN"){
            return("Before outbreak")
        }
        if(input$language=="PR"){
            return("Antes do surto")
        }
    })
    
    plot_anot2 <- reactive({
        if(input$language=="EN"){
            return("Outbreak")
        }
        if(input$language=="PR"){
            return("Surto")
        }
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
            ylab(y_label3()) +
            annotate(geom="text", x=10.75, y=mean(plotdb()$of1$Intervention_type),
                     label=plot_anot1()) +
            annotate(geom="text", x=10.75, y=10, label=plot_anot2()) +
            coord_flip() +
            ggtitle(gg_title()) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(size=18))
        p2
    })
    
    output$intervention_text <- renderText({
        
        if(input$language=="EN"){
            paste("On average, interventions were put in place ",
                  "<b>", c("earlier", "later")[median(plotdb()$of3) + 1], "</b>",
                  " in the outbreak in",
                  substr(input$area_select, 1, nchar(input$area_select)-3),
                  "compared to other municipalities in Brazil")
        }
        if(input$language=="PR"){
            paste("Em média, foram implementadas intervenções ",
                  "<b>", c("antecipadamente", "posteriormente")[median(plotdb()$of3) + 1], 
                  "</b>",
                  " no surto em",
                  substr(input$area_select, 1, nchar(input$area_select)-3),
                  "comparado a outros municípios do Brasil")
        }
    })
    
    
    output$p3 <- renderPlot({
        
        if(input$region_select2 != "National"){
            myRegion <- unique(Brazil_cases_sp$Region[Brazil_cases_sp$Name == 
                                                          input$region_select2])
        } else {
            myRegion <- "National"
        }
        
        if(input$language=="EN"){
            g3 <- Trends_plot_list[[myRegion]][["Density"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14))
            
            g4 <- Trends_plot_list[[myRegion]][["SDI"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14))
            
            g5 <- Trends_plot_list[[myRegion]][["Sewerage"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14))
            
            g6 <- Trends_plot_list[[myRegion]][["Travel"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14))
        }
        if(input$language=="PR"){
            g3 <- Trends_plot_list[[myRegion]][["Density"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14)) +
                labs(x = "Dias desde o início",
                     y = "Incidência cumulativa por 10.000 pessoas \n(escala logarítmica, outliers omitidos)") +
                guides(fill=guide_legend(title="Densidade populacional da área\n (Quartiles)"))
            g4 <- Trends_plot_list[[myRegion]][["SDI"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14)) +
                labs(x = "Dias desde o início",
                     y = "Incidência cumulativa por 10.000 pessoas \n(escala logarítmica, outliers omitidos)") +
                guides(fill=guide_legend(title="Área Sociodemográfica\n index (Quartiles)"))
            
            g5 <- Trends_plot_list[[myRegion]][["Sewerage"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14)) +
                labs(x = "Dias desde o início",
                     y = "Incidência cumulativa por 10.000 pessoas \n(escala logarítmica, outliers omitidos)") +
                guides(fill=guide_legend(title="Proporção de famílias \ncom água encanada (Quartiles)"))
            
            g6 <- Trends_plot_list[[myRegion]][["Travel"]] +
                theme_minimal() +
                theme(legend.text=element_text(size=12),
                      legend.title=element_text(size=12)) +
                theme(axis.text.x = element_text(size=12),
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=12),
                      axis.title.y = element_text(size=12)) +
                theme(strip.text.x = element_text(size = 14)) +
                labs(x = "Dias desde o início",
                     y = "Incidência cumulativa por 10.000 pessoas \n(escala logarítmica, outliers omitidos)") +
                guides(fill=guide_legend(title="Proporção de famílias \nconectadas à rede de esgotos\n(Quartiles)"))
        }
        
        ggarrange(g3, g4, g5, g6,
                  labels=NULL,
                  ncol=2, nrow=2, label.x=-0.03,
                  font.label=list(size=18, face="bold"),
                  common.legend=FALSE, legend="right")
        
    })
    
    data_rt <- reactive({
        
        plot_data <- all_plot_data[ which(((all_plot_data$city_state==input$region_select_rt1) | 
                                               (all_plot_data$city_state==input$region_select_rt2) | 
                                               (all_plot_data$city_state==input$region_select_rt3) | 
                                               (all_plot_data$city_state==input$region_select_rt4)) & 
                                              (all_plot_data$Date>=format(as.Date(cut_off_date), 
                                                                          "%Y-%m-%d"))) ,]
        plot_title     <-  "Estimated Rt from April 1st 2020 with 95%CI"
        plot_sub_title <- "Mean Serial Interval = 2.9 days  - Plotting 90 days of data up to 14 days ago to account for reporting delays"
        
        ## Add ranges to data
        ## Set plot ranges
        ## leave this in case we decide to include ranges for complete and incomplete data 
        xlim2 <- plot_data$Date[nrow(plot_data) - 10]
        
        ### Plot range of estimates for Rt
        plot_data$Municipality <- plot_data$city_state
        
        ### Selecting y range 
        tmp_dat <- plot_data %>% 
            group_by(city_state) %>% 
            arrange(desc(Rtotalhat)) %>% 
            slice(1) %>% 
            ungroup()
        ymax_lim <- min(tmp_dat$Rtotalhat) + 0.1
        
        tmp_dat <- plot_data %>% 
            group_by(city_state) %>% 
            arrange(Rtotalhat) %>% 
            slice(1) %>% 
            ungroup()
        ymin_lim <- min(tmp_dat$Rtotalhat) - 0.1
        
        ymax_lim = 1.8
        ymin_lim = 0.5
        xmax_lim = max(plot_data$Date) 
        xmin_lim = xmax_lim - 90
        
        
        db_out <- list(of1=plot_data,
                       of2=plot_title,
                       of3=plot_sub_title,
                       of4=xlim2,
                       of5=ymax_lim,
                       of6=ymin_lim,
                       of7=xmax_lim,
                       of8=xmin_lim)
        
    }) 
    
    # Rt tab
    output$p_rt <- renderPlot({
        
        g1 <-  ggplot(data=data_rt()$of1, 
                        aes(x = Date, y = Rtotalhat, 
                            color=Municipality, 
                            group=Municipality))+ 
                     geom_line() +
                     geom_ribbon(aes(ymin = Lower_CI, 
                                     ymax = Upper_CI,
                                     fill = Municipality),
                                 linetype="blank",alpha=0.1) + 
                     xlab("Date") +
                     ylab("Rt") +
                     ggtitle(label    = data_rt()$of2, 
                             subtitle = data_rt()$of3) +
                     geom_hline(yintercept=1.0, linetype="solid") +
                     scale_x_date(date_breaks = "1 week", date_labels="%d-%b") +
                     coord_cartesian(ylim = c(data_rt()$of6, data_rt()$of5),
                                     xlim = c(data_rt()$of8, data_rt()$of7)) +
                     theme_minimal()
        g1
    })
    
    
    
    
    # Tab 4
    output$map2 <- renderLeaflet({
        
        
        my_bks <- seq(0,1, by=0.2)
        
        pal <- colorNumeric("YlOrBr", NULL)
        
        leaflet(data = peakSF,
                options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.35)) %>%
            addProviderTiles(providers$Stamen.TonerLabels) %>%
            addCircleMarkers(col = ~pal(PredictProb), 
                             opacity = 0.9,
                             radius = 5, 
                             weight=1,
                             popup= ~paste0("<b>", Area, "</b>", 
                                            "<br/>", " Predicted probability: ",
                                            round(PredictProb, 1),
                                            "<br/>")) %>%
            # addTiles() %>%
            addLegend("bottomright",
                      pal = pal,
                      values = ~log1p(PredictProb),
                      labFormat = labelFormat(
                          transform=function(x) round(expm1(x),1)),
                      opacity = 0.7, 
                      title = "Predicted probabilities")
        
    })
    
    auc_data <- reactive({
        AUCplot +
            theme_minimal() +
            theme(legend.text=element_text(size=12),
                  legend.title=element_text(size=12)) +
            theme(axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x = element_text(size=14),
                  axis.title.y = element_text(size=14)) +
            theme(strip.text.x = element_text(size = 14)) +
            xlab("False positive fraction") +
            ylab("True positive fraction") +
            annotate("text", x=0.65, y=0.275,
                     label=paste0("The area under this ROC curve is ",
                                  round(AUCDF[1, "AUC"], 3)))
        
    })
    
    output$aucplot <- renderPlot({
        auc_data()
        # ggplot(dfr,  aes(x, y)) +
        #     geom_point()
        # 
    })
    
    
    output$rows_label = renderText({
        switch(input$language, "EN"="Number of rows to show",
               "PR"="Número de linhas a mostrar")
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
               "PR"="Baixar dados como CSV")
    })
}

shinyApp(ui, server)

