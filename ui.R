library(shiny)
library(shinythemes)
library(shinyjs)
library(markdown)
library(DT)
library(shinyjqui)
library(shinycssloaders)

source("global.params.R")

fluidPage(
  useShinyjs(),
  theme = shinythemes::shinytheme("journal"),
  tags$head( tags$style(
    HTML('
         #topbar {
            background-color: #f8f5f0;
         }
         ')
  )),
  
  # Application title
  headerPanel(list(
    tags$head(tags$style("body {background-color: white; }")),
    "T-CLIM",
    HTML(
      '<img src="eCLIM.png", height="100px",
      style="float:left"/>',
      '<p style="color:purple"> Climate x Transcriptome association browser </p>'
    )
  )),

  
  tabsetPanel(type = "tabs",

              tabPanel("Environmental association Viz", id = "topbar",
                       # Sidebar
                       
              
                       
                       downloadButton("downloadData", label="Download complete TxE dataset", class = "butt1"),
                       # style font family as well in addition to background and font color
                       tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;} .butt1{font-family: Courier New}")), 
                       # pick gene and download button

                       fluidPage( id = "topbar",
                          
                                  fluidRow(
                                    column(8,
                                           br(),
                                           selectizeInput(inputId = 'gene.profile',
                                                          label = p("Select Gene of Interest",
                                                                    actionLink("GeneInfoButton", "", icon = icon("info-circle"))),
                                                          choices = genes.in.menu.rsa,
                                                          selected = genes.in.menu.rsa[1],
                                                          multiple = FALSE)
                                    ),
                                    column(2,
                                           br(),
                                           radioButtons("GeneLevelMetric",
                                                        label = p("2. Select Gene Level Metric",
                                                                  actionLink("MetricInfoButton", "", icon = icon("info-circle"))),
                                                        choices = list("Gene id" = 1, "Gene symbol" = 2),
                                                        selected = 1)
                                    ),
                                    column(3, align="right",
                                           br(),
                                           br(),
                                           #                                            h5(strong("3. Download Profile Data (CSV)")),
                                           
                                           downloadButton("downloadData_profile","Download T x E for selected gene")
                                    )
                                  )#,
                                  # HTML("NB: to differentiate between inert and essential genes use the RSA gene level metric (see the ABOUT tab)")
                       ),
                       
                       # sensitivity profile and data table output
                       fluidPage(
                         column(12,
                                h3("Environmental association", align = "left"),
                                plotOutput("ProfilePlot", 
                                           brush = brushOpts(id = "plot_brush", direction = "x"),
                                           height = 500)
                         ),
                         fluidPage(1),
                         column(12,
                                h3("Selected environments", align = "left"),
                                div(
                                  dataTableOutput("cellline_info"),
                                  style = "font-size:80%"
                                )
                         )
                       )
              ),
              
              
              tabPanel("About",
                       sidebarPanel(
                         width = 3,
                         wellPanel(a(h4('Please cite us in any publication that utilizes information from Arabidopsis CLIMtools:'),  href = "https://www.nature.com/articles/s41559-018-0754-5", h6('Ferrero-Serrano, Á & Assmann SM. Phenotypic and genome-wide association with the local environment of Arabidopsis. Nature Ecology & Evolution. doi: 10.1038/s41559-018-0754-5 (2019)' ))),
                         wellPanel(
                           a("Tweets by @ClimTools", class = "twitter-timeline"
                             , href = "https://twitter.com/ClimTools"),
                           style = "overflow-y:scroll; max-height: 1000px"
                         ),
                         h6('Contact us: clim.tools.lab@gmail.com'), wellPanel(tags$a(div(
                           img(src = 'github.png',  align = "middle"), style = "text-align: center;"
                         ), href = "https://github.com/CLIMtools/AraCLIM"))
                       ),
                       fluidPage(
                         br(),
                         fluidRow(
                           column(2,""),
                           column(8, 
                                  h3(strong("Information")),
                                  br(),
                                  HTML("This tool provides information of the association between the natural variation in the transcript abundance of any gene of interest and either one of the more than 400 environmental variables"),
                                  br(),
                                  br(),
                                  h4(strong("Methodology")),
                                  HTML("TWAS analysis for each of the more than 400 environmental variables in this study was performed using the set 558 accessions within the set of 879 Eurasian accessions in this study that had information on transcript abundance from a previous study (Kawakatsu, Taiji, et al. 2016; DOI: 10.1016/j.cell.2016.06.044)."),
                                  br(),
                                  br(),
                                  h4(strong("Reference")),
                                  p("NA(", a("link",
                                                                                                                                                                                                                                       href = "NA"), ")"),
                                  h4(strong("APP")),
                                  p( "This app has been developed based on the app developed for Project DRIVE ", a("here", href = "http://dx.doi.org/10.17632/y3ds55n88r.4"), "."),
                                  br(),
                                  br(),
                                  
                           )
                         )
                       )
              )
  )
)



