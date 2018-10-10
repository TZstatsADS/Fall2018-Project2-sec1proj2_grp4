packages.used=c("shiny", "plotly", "shinydashboard", "leaflet", "shinythemes","setBackgroundImage")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shinythemes)
library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(highcharter)
library(shinyWidgets)

load("./iips.Rdata")
load("./charge_by_states.RData")

navbarPage(title = "Hospital Finder",
             theme = shinytheme("flatly"),
           
        #Tab "Hospital Recommendation"
           
    navbarMenu("Hospital Recommendation",
              
        #Sub-tab "General Selection"
               tabPanel("General Selection",sidebarLayout(position = "right",
                             sidebarPanel(
      selectInput("state", label = "State", 
                choices = c("Select","AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
                            "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
                            "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
                            "TX","UT","VT","VA","WA","WV","WI","WY"), selected = "Select"),
      selectInput("type", label = "Hospital Type", 
                  choices = c("Select","Acute Care Hospitals","Critical Access Hospitals","Childrens"), selected = "Select"),
      selectInput("DRG", label = "Diagnosis-Related Group", choices = c(levels(iips_data$DRG.Definition), "Select"), selected = "Select"),
      sliderInput("cost", label = "Budget", min = 2000, max = 20000, value = 20000),
      selectInput("EM", label ="Emergency Service", choices = c("Select","Yes", "No"), selected = "Select"),
      
      hr(),
      
      submitButton("Find Your Hospital Today!",width='100%'))
    ,
    mainPanel(
      fluidRow(
        tabBox(width=12,
               tabPanel(title="Map",width = 12,solidHeader = T,leafletOutput("map"))
        ),
        tabBox(width = 12,
               
               tabPanel('Hospital General Information',
                        dataTableOutput("tableinfo"),
                        tags$style(type="text/css", '#myTable tfoot {display:none;}'))
              )
      )
      
        
        )
        
      )),
    
        #Sub-tab "Personalized Recommendation"
    tabPanel("Personalized Recommendation",sidebarLayout(position = "right",
                                  sidebarPanel(
             radioButtons("care1",label = "Mortality Rate",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care2",label = "Safety of Care",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care3",label = "Readmission Rate",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care4",label = "Patient Experience",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care5",label = "Effectiveness of Care",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care6",label = "Timeliness of Care",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             radioButtons("care7",label = "Efficient Use of Medical Imaging",
                          choices = list("Care a Lot"=3,"Just Care"=2,"Care Less"=1),
                          selected = 2, inline = T),
             submitButton("Find Your Hospital Today!",width='100%')),
             mainPanel(
               fluidRow(
                 tabBox(width=12,
                        tabPanel(title="Map",width = 12,solidHeader = T,leafletOutput("map1"))
                 ),
                 tabBox(width = 12,
                        
                        tabPanel('MediCare Assessment',
                                 dataTableOutput("tableinfo1"),
                                 tags$style(type="text/css", '#myTable tfoot {display:none;}')),
                        tabPanel('Personalized Ranking',
                                 dataTableOutput("tablerank1"),
                                 tags$style(type="text/css", '#myTable tfoot {display:none;}')
                        ))
               )
             )
             ))),
             
        #Tab "Key Statistics"
    navbarMenu("Key Statistics",
               
               tabPanel("General Statistics",
                        sidebarLayout(
                          sidebarPanel(
                            h2("Why we care about Medicare Payments?"),
                            helpText("In this section, we have an interactive map that represents the number of hospitals in each state. 
                                      We also have a graph that represents the ratio of medicare payments to covered charges for 25 hospitals that have the highest ratio. 
                                     In fact, this graph tells that hospitals received more in medicare payments than what they charged. 
                                     As such, this tells us about a possible error in entering and/or processing such information.")
                            ),
                          mainPanel(
                          tabsetPanel(
                            tabPanel("Hospital Count",highchartOutput("plot1")),
                            tabPanel("Ratio of Medicare Payments to Covered Charges",plotOutput("plot11"), textOutput("explain0")),
                            tabPanel("Hospital Quality in Each State",plotOutput("plot12"))
                          )
                        ))),
               
               tabPanel("Cost Variation",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Creat Map"),
                            
                            selectInput("drgCode", 
                                        label = "Select DRG:",
                                        choices = unique(iips_data$DRG.Definition),
                                        selected = unique(iips_data$DRG.Definition)[1]
                                        
                            ),
                            submitButton("Submit",width='100%')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Average Covered Cost",highchartOutput("plot2")),
                              tabPanel("Average Medicare Payments",highchartOutput("plot3")),
                              tabPanel("Average Total Payments",highchartOutput("plot4"))
                            )
                          )                                                                            
                        )
               ),
               tabPanel("Hospital Comparison",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("drgCode2",
                                        label = "Select DRG:",
                                        choices = unique(iips_data$DRG.Definition),
                                        selected = unique(iips_data$DRG.Definition)[1]
                            ),
                            selectInput("hospital",
                                        label = "Select Hospital:",
                                        choices = unique(iips_data$Provider.Name),
                                        selected = unique(iips_data$Provider.Name)[1],
                                        multiple = TRUE
                            ),
                            checkboxInput("all","Select All", value= TRUE),
                            submitButton("Submit",width='100%')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Hospital Comparison",htmlOutput("plot5"))
                            )
                            
                          )
                        )
                        
               )),
    
    navbarMenu("Further Information",
               setBackgroundImage(src = "https://media.istockphoto.com/photos/blue-soft-background-picture-id689752378?k=6&m=689752378&s=612x612&w=0&h=6KFi1iqcd5LVwA0bzgrdF1jzSK2fS6hfBqAdcNJnB-c="),
               tabPanel("About Us",
                        
                        #includeCSS("theme.css"),
                        mainPanel(
                          h2(textOutput("read0")),
                          textOutput("read1"),
                          hr(),
                          textOutput("read2"),
                          textOutput("read3"),
                          textOutput("read4"),
                          textOutput("read5"),
                          textOutput("read6"),
                          hr(),
                          textOutput("read7"))),
               
               tabPanel("About the Manual",
                        mainPanel(
                          textOutput("read8"),
                          hr(),
                          h2(textOutput("read9")),
                          textOutput("read10"),
                          textOutput("read11"),
                          hr(),
                          h3(textOutput("read12")),
                          hr(),
                          textOutput("read13"),
                          textOutput("read14"),
                          textOutput("read15"),
                          textOutput("read16"),
                          textOutput("read17"),
                          textOutput("read18"),
                          textOutput("read19"),
                          textOutput("read20"),
                          hr(),
                          textOutput("read21"),
                          textOutput("read22"),
                          textOutput("read23"))),
               tabPanel("About the Data",
                        mainPanel(
                          h2(textOutput("read24")),
                          hr(),
                          textOutput("read25"),
                          textOutput("read26"),
                          textOutput("read27"),
                          textOutput("read28"),
                          textOutput("read29"),
                          textOutput("read30"),
                          textOutput("read31"),
                          textOutput("read32"),
                          textOutput("read33"),
                          textOutput("read34"),
                          a("Here",href = "https://www.medicare.gov/hospitalcompare/Data/Measure-groups.html"),
                          hr(),
                          textOutput("read35"),
                          textOutput("read36"),
                          textOutput("read37"),
                          textOutput("read38"),
                          textOutput("read39"),
                          textOutput("read40"),
                          textOutput("read41")
                        )
                        )
               
               )
    
    )

  
