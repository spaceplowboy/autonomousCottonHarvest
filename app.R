library(dygraphs)
library(shiny)
library(xts)
library(htmlwidgets)
#library(rhandsontable) # possible use rhandsomtables for inputs?
#library(shinydashboard)
#library(shinyMatrix)
library(plotly)
library(DT)

ui <- function(request) {
  fluidPage( 
  
h2("Cotton Harvest Machinery Decision Tool - Beta"), img(src="cotton.jpg", style="width: 350px", align="right"), #img(src="ksre.png",  style="width: 250px", align="right"),  

p("Shiny App developed by:", a("Terry Griffin", href = "mailto:twgriffin@ksu.edu", label = "Contact Admin"), "Kansas State University"),
p(" Prepared for: Cotton Inc. as part of 2018-2019 Project 18-475 Economics of Whole-farm Swarm-Bots for Cotton Production in the US"),
p(" Beta version in development of this educational tool, errors in programming are likely. Comments welcome"),

  selectInput(inputId = "stateInput", label = strong("Choose state (populates state-level data*)"),
              choices = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "FLORIDA", "GEORGIA", "KANSAS", "LOUISIANA", "MISSISSIPPI", "MISSOURI", "NEW MEXICO", "NORTH CAROLINA","OKLAHOMA","SOUTH CAROLINA",  "TENNESSEE", "TEXAS", "VIRGINIA"),
              selected = "KANSAS"),
tags$h6("*data from  pertinent crop reporting districts for KS and MO"),
br(),
bookmarkButton(),
br(),
br(),

#titlePanel(title=div(img(src="picture.jpg"), "My Title")),

  tabsetPanel(
    
    tabPanel("Data viz: days suitable & crop progress", fluid=TRUE, 
             br(),
             h5("Graphs are for exploratory data visualization, 
             users should note the beginning and ending harvest dates for their respective states 
                (choosing dates beyond these will return errors in remaining tabs)."),
             h5("Some states do not report DSFW after specific dates; setting end date will not allow additional days in those cases."),
             #br(),
             fluidRow(
                  
                 column(width = 6,
                      h2(""),
                                 dygraphOutput("dygraph2", height='400px',width='720px')),
                 column(width = 5,
                      h2(""),
                      dygraphOutput("dygraph3",height='400px',width='720px'))
               ),
             br(),
             h5("More detail available:
             Griffin and Barnes 2017. Available Time to Plant and Harvest Cotton across the Cotton Belt. The Journal of Cotton Science 21:8–17.              
                https://www.cotton.org/journal/2017-21/1/upload/JCS21-008.pdf"), 
             h5("Data source: USDA NASS https://quickstats.nass.usda.gov/"),
             br()
             ),
    
    tabPanel("Acreage & capacity: single harvester", fluid=TRUE,
             sidebarLayout(
                sidebarPanel(width = 4,
                          tags$h4(tags$strong("Set intended harvest start and end weeks")),
                          dateRangeInput("harvDates", htmlOutput("mostActiveHarv"), start = "2019-09-02", end   = "2019-09-16", format = "M-d"),
                          tags$a("Previously reported most active dates by USDA NASS (2010)", href="https://downloads.usda.library.cornell.edu/usda-esmis/files/vm40xr56k/dv13zw65p/w9505297d/planting-10-29-2010.pdf", target="_blank"),
                          hr(),
                          tags$h4(tags$strong("Harvest capacity calculator inputs")),
                          sliderInput(inputId = "fe", label = "field efficiency (%) (round on-board modules ~90%)", value=75, min=1, max=100),
                          sliderInput(inputId = "mph", label = "ground speed (mph)", value=4.2, min=0.1, max=12),
                          sliderInput(inputId = "swath", label = "harvest swath width (feet) (6-row 38-inch is 228 inches)", value=228, min=1, max=480),
                          sliderInput(inputId = "hrs", label = "hours worked per day (machine hours)", value=9, min=0, max=24, step=0.5),
                          sliderInput(inputId = "days", label = "days worked per week", value=6.5, min=0, max=7, step=0.5)), #end of sidebarpanel
                  mainPanel(fluidRow(
                              #hr(),
                              #dygraphOutput("dygraph4",height='450px',width='720px'),    
                              hr(),
                              tags$h4(htmlOutput("capacityTitle")),
#                              tags$h4(paste("Probability of harvesting acreage in ", state0(), sep="")),
                              plotlyOutput("plot4", height = '450px', width='720px')
                              )) #end of mainPanel
                ) #end of sidbarlayout
           ),#end of tabpanel for capacity       

tabPanel("Acreage & capacity: multiple harvesters", fluid=TRUE,
  sidebarLayout( sidebarPanel(width=4,
                   #           tags$h4(tags$strong("testing")),
                 tags$h4(tags$strong("Intended harvest weeks for 1st harvester")),
                 dateRangeInput("harvDates1", htmlOutput("mostActiveHarv1"), start = "2019-09-02", end   = "2019-09-16", format = "M-d"),
                 hr(),
                 tags$h4(tags$strong("Intended harvest weeks for 2nd harvester")),
                 dateRangeInput("harvDates2", "",#htmlOutput("mostActiveHarv2"), 
                                start = "2019-09-02", end   = "2019-09-16", format = "M-d"),
                 tags$a("Previously reported most active dates by USDA NASS (2010)", href="https://downloads.usda.library.cornell.edu/usda-esmis/files/vm40xr56k/dv13zw65p/w9505297d/planting-10-29-2010.pdf", target="_blank"),
                 hr(),
                 tags$h4(tags$strong("Harvest capacity calculator inputs")),
                 div(style="display: inline-block;vertical-align:top; width: 200px;", tags$h5(tags$strong("1st harvester"), align = "center")),
                 div(style="display: inline-block;vertical-align:top; width: 200px;", tags$h5(tags$strong("2nd harvester"), align = "center")),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     numericInput(inputId = "units1", label = "number of units", value=1, min=1, max=1000)),
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     numericInput(inputId = "units2", label = "number of units", value=1, min=1, max=1000)),

                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "fe1", label = "field efficiency (%)", value=75, min=1, max=100)),
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "fe2", label = "field efficiency (%)", value=75, min=1, max=100)),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "mph1", label = "ground speed (mph)", value=4.2, min=0.1, max=12)),
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "mph2", label = "ground speed (mph)", value=4.2, min=0.1, max=12)),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "swath1", label = "harvest swath width (inches)", value=228, min=1, max=480)),
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "swath2", label = "harvest swath width (inches)", value=228, min=1, max=480)),
                 
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "hrs1", label = "hours worked per day (machine hours)", value=9, min=0, max=24, step=0.5)),
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "hrs2", label = "hours worked per day (machine hours)", value=9, min=0, max=24, step=0.5)),                 

                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "days1", label = "days worked per week", value=6.5, min=0, max=7, step=0.5)), #end of sidebarpanel
                 div(style="display: inline-block;vertical-align:top; width: 200px;",
                     sliderInput(inputId = "days2", label = "days worked per week", value=6.5, min=0, max=7, step=0.5))), #end of sidebarpanel
                 mainPanel(fluidRow(
                   hr(),
                   tags$h4(htmlOutput("capacityTitle2")),
                #   tags$hr(tags$strong("test2")),
                   plotlyOutput("plot4dualHarvesters", height = '450px', width='820px')
                   
                   
                 ))
                  )
  
),



tabPanel("Yield penalties: forthcoming", fluid=TRUE,
         tags$h3("More to come: development in progress"),
         tags$h4("Unique yield potential matrix will be provided for each state; and allow user to customize. Example data below were suggested for Arkansas but have not been validated"),
         tags$h4("Yield potential matrix assuming sinlge-harvest (status quo) for cotton planting (rows) and harvest (columns) date combinations; by week of"),
         DTOutput('yieldPenalty'),
         br()
                          
         
         
),

    tabPanel("Enter economic parameters", fluid=TRUE,
             sidebarLayout(
               sidebarPanel(width=3,
                            tags$h4(tags$strong("Revenue inputs")),
                            hr(),
                            numericInput(inputId = "yield", label = "Expected lint yield (pounds per acre)", step = 1.0, value = 900, min=0, max=9000),
                            numericInput(inputId = "turnout", label = "Expected turnout (%)", step = 1.0, value = 38, min=0, max=100),
                            numericInput(inputId = "seedLint", label = "Expected pounds of seed per pound of lint", step = 0.1, value = 1.4, min=0, max=100),
                            numericInput(inputId = "fees", label = "Ginning, bagging, ties, and fees per lint pound", step = 0.01, value = 0.1, min=0, max=100),
                            numericInput(inputId = "seedPrice", label = "Cotton seed price ($ per ton)", step = 1.0, value = 200, min=0, max=2000),
                            numericInput(inputId = "seedPrice", label = "Cotton lint price per pound", step = 0.01, value = .74, min=0, max=5),
    hr(),
  tags$h4(tags$strong("Defoliation and tarp expenses")),
                   hr(),
                   numericInput(inputId = "defCost", label = "Defoliants ($ per acre)", step = 0.01, value = 6.25, min=0, max=150),
                   numericInput(inputId = "appCost", label = "Application cost ($ per acre)", step = 0.01, value = 2, min=0, max=150),
                   numericInput(inputId = "tarpCost", label = "Module tarps cost ($ for whole farm)", step = 1.0, value = 40000, min=0, max=100000),
              
    hr(),    
    tags$h4(tags$strong("Harvester system 1 inputs")),
                   hr(),
                   numericInput(inputId = "purchPrice", label = "Purchase price ($ per machine)", value = 780000, min=0, max=1500000),
                   numericInput(inputId = "usefuleLife", label = "Useful life (years)", value = 7, min=1, max=20),
                   numericInput(inputId = "annUse", label = "Annual use (hours)", value = 222, min=0, max=800),
                   numericInput(inputId = "annUseCotton", label = "Annual use in cotton harvest (hours)", value = 222, min=0, max=500),
                   numericInput(inputId = "salvage", label = "Salvage value ($)", value = 150000, min=0, max=1000000),
                   numericInput(inputId = "insure", label = "Annual insurance/housing/taxes", value = 10000, min=0, max=50000),
                   numericInput(inputId = "fuelUse", label = "Fuel use (gallons per hour)", value = 12, min=0, max=50),
                   numericInput(inputId = "fuelPrice", label = "Fuel price ($ per gallon)", step = 0.01, value = 3, min=0, max=10),
                   numericInput(inputId = "repairs", label = "Annual repairs ($ per year)", value = 18000, min=0, max=50000),
                   numericInput(inputId = "wages", label = "Labor rate ($ per hour)", step = 0.01, value = 13, min=0, max=50),
                   numericInput(inputId = "downPay", label = "Percent down payment (%)", value = 30, min=0, max=100),
                   numericInput(inputId = "note", label = "Length of note (years)", value = 4, min=0, max=12),
                   numericInput(inputId = "interest", label = "Percent interes rate for loan (%)", step = 0.01, value = 8, min=0, max=20),
                   numericInput(inputId = "numMachines", label = "Number of machines", value = 1, min=0, max=12)
            ), #endofsidebarpanel
      mainPanel(tags$h4(tags$strong("More to come soon: development in progress")),
           
                )
            )#sidebarlayout
    ),#tabpanel

tabPanel("Change log", fluid=TRUE,
         tags$h3("Remove this tab before initial release of version 1: 3 June 2020 testing"),
         br(),
         tags$h3("Initial beta version developed during 2019 and presented to precision cotton workshop at 2020 Beltwide"),
         tags$h4("Changes and updates listed below were made during spring 2020"),
         tags$h5("Explantory notes added in tabs."),
         tags$h5(" Changed harvester width units from feet to inches. Should we leave harvester1 as feet or keep both in inches?  per discussions and suggestions with Glen Rains during February 2020"),
         tags$h5("Bookmark function added to save parameter settings and set-up. per discussions and suggestions with Glen Rains during February 2020"),
         tags$h5("Change minimum speed from 0.5 mph to 0.1 mph"),

                  br(),
         tags$h4("Changes and updates forthcoming although unsure if feasible/possible; send suggestions to Terry Griffin"),
         tags$h5("Update data with 2019 observations from USDA NASS"),
         tags$h5("Autonomous harvest can begin at 10%? open bolls, yield penalty matrix should begin then for each state"),
         tags$h5("customize by users' location; i.e. users in Georgia experience state set = to Georgia. Try session$clientData https://shiny.rstudio.com/articles/client-data.html"),
         tags$h5("Connect API to USDA NASS to auto-populate data? or just update data once a year? Kansas (up to 2017) and Missouri (up to 2016) no longer report DSFW by CRD but could update the crop progress stats for those states"),
         tags$h5("Output CSV of graphs.  per discussions and suggestions with Glen Rains during February 2020. Note: plotly graphs can be downloaded as *.png already"),
         tags$h5("include something like picker efficiency or boll losses, selecting things like:
            % bolls open for harvesting (would be around 70% for current harvest start date, ), 
            % bolls left after harvest, % bolls fallen from weather or equipment.  per discussions and suggestions with Glen Rains during February 2020"),
         tags$h5("thinking harvest speed for robots would be variable. At the beginning with fewer bolls, 
         the harvester would move through the field faster and then as bolls numbers get more dense, it would slow down. 
         Setting by bolls picked per minute which may be more or less consistent if ground speed is variable.  per discussions and suggestions with Glen Rains during February 2020"),
         tags$h5("provide date of event and percentage for destruction? For example, given other parameters (planting date, 
         state, etc,), how much is lost on Sept 1 if 50% of open bolls are gone and 25% of plants were destroyed (cannot 
         get anymore bolls opening) from hurricane, hail, tornado or just heavy rain. Atlantic Hurricane season is June 1 
         to November 30. Could be calculated separately from the data you already provide. 
         Want to play around with this as it could be a major benefit to be able to harvest as bolls are opening and not 
         be exposed for 2-3 months for weather events to destroy part of the crop. We could start with this and add a 
         probabilistic component to come up with predictions.  per discussions and suggestions with Glen Rains during February 2020
                  "),                                       
         tags$h5("you may do it similar for harvest.  You have the graph and adjustments, but maybe you could have a table 
         also that shows the prominent values for certain acreages, like for 1200, 1300 etc.  per Wes Porter
         "),
         tags$h5("allow user to input their own DSFW numbers per Wes Porter"),
         tags$h5("allow user to modify their own start and end dates per Wes Porter"),
         tags$h5("yield and quality by harvest date economics")
         
),

tabPanel("Contributors", fluid=TRUE,
         tags$h3("Project team, contributors, and reviewers"),
         tags$h4("Terry Griffin"),
         tags$h4("Gregg Ibendahl, Kansas State University"),
         tags$h4("Glen Rains, University of Georgia, February 2020"),
         tags$h4("Wes Porter, University of Georgia, 23 October 2019"),
         tags$h4("Other1"),
         tags$h4("Other2")
         
),

tabPanel("Instructions", fluid=TRUE,
          tags$h3("Intructions for use of tool"),
          tags$h4("contact Terry Griffin with any questions"),
          tags$h4("Select desired cotton state from dropdown list (this can be done from any tab"),
         tags$h4("usually start with left tab and work toward the right; navigate through all the tabs especially data viz and acreage & capacity for now"),
         tags$h4("The data viz tab allows user to get an idea of the number of days suitable for fieldwork and typical crop progress for planting, boll opening, and harvest"),
          tags$h4("navigate through all the tabs"),
          tags$h4("under the 'Acreage and Capacity' tabs, dates must be set for available data; each state has different date ranges. Otherwise an error will be returned"),
          tags$h4("If use desires to 'save' their place, click on 'Bookmark' and copy the URL for next vist")
          
)

    )#end of tabsetpanel
)#end of fluidpage for ui

  
}
  
  
    
server<-function(input, output, session){
  
  library(tidyverse)
  library(plyr)
  library(tmap)
  library(extrafont)
  library(Hmisc)
  library(dygraphs)

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  state0<-reactive({
    sapply(tolower(input$stateInput), simpleCap)
  })

    cottonHarvProg<-reactive({
    cottonHarvProg0000<-read.csv("data/cottonHarvProg.csv")
    cottonHarvProg0<-cottonHarvProg0000
    cottonHarvProg0$value<-as.numeric(gsub(",", "", cottonHarvProg0$Value))
    cottonHarvProg0$harvPerc<-as.numeric(cottonHarvProg0$value)
    cottonHarvProg0$year<-as.numeric(as.character(cottonHarvProg0$year))
    return(cottonHarvProg0)})

  bollDat<-reactive({
    bollDat0000<-read.csv("data/cottonBollProg.csv")
    bollDat0<-bollDat0000
    bollDat0$value<-as.numeric(gsub(",", "", bollDat0$Value))
    bollDat0$harvPerc<-as.numeric(bollDat0$value)
    bollDat0$year<-as.numeric(as.character(bollDat0$year))
    return(bollDat0)})

    DSFWdat <-reactive({
      DSFW0000<-read.csv("data/DSFW.csv")
      #head(DSFW0000)
      #KS<-read.csv("data/DSFWdat4MO.csv")
      #KS<-KS[,c(2, 4,11)]
      #colnames(KS)<-c("year",  "begin_code", "SC")
      #head(KS)
      #KS<-KS[complete.cases(KS[,3]),]
      #test0<-subset(DSFW0000, state_name=="MISSOURI")
      #test00<-subset(DSFW0000, state_name!="MISSOURI")
      #test<-left_join(test0, KS, by = c("begin_code", "year"))
      #test$Value<-test$SC
      #dim(test)
      #test<-test[,1:42]
      #test1<-rbind(test, test00)
      #write.csv(test1, "data/DSFW.csv" )
      dim(DSFW0000)
      DSFW0 <-  subset(DSFW0000, state_name == input$stateInput)
      DSFW0<-DSFW0[complete.cases(DSFW0[,42]),]
      return(DSFW0)
      })
  
    DSFWdat4MH <-reactive({
      DSFW0000<-read.csv("data/DSFW.csv")
      #head(DSFW0000)
      #KS<-read.csv("data/DSFWdat4MO.csv")
      #KS<-KS[,c(2, 4,11)]
      #colnames(KS)<-c("year",  "begin_code", "SC")
      #head(KS)
      #KS<-KS[complete.cases(KS[,3]),]
      #test0<-subset(DSFW0000, state_name=="MISSOURI")
      #test00<-subset(DSFW0000, state_name!="MISSOURI")
      #test<-left_join(test0, KS, by = c("begin_code", "year"))
      #test$Value<-test$SC
      #dim(test)
      #test<-test[,1:42]
      #test1<-rbind(test, test00)
      #write.csv(test1, "data/DSFW.csv" )
      dim(DSFW0000)
      DSFW0 <-  subset(DSFW0000, state_name == input$stateInput)
      DSFW0<-DSFW0[complete.cases(DSFW0[,42]),]
      return(DSFW0)
    })
    
  dat4graph5yrsp <-reactive({
      cottonPlantProg0000<-read.csv("data/cottonPlantProg.csv")
      last5yearsp<-subset(cottonPlantProg0000, state_name == input$stateInput)#stateNames[3])# %in% input$stateInput)
      last5yearsp$value<-as.numeric(as.character(last5yearsp$Value))
      last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
      dat4graph5yrsp<-aggregate(value~begin_code1, data=last5yearsp, FUN = "mean")
      colnames(dat4graph5yrsp)<-c("begin_code", "plantProg")
      write.csv(dat4graph5yrsp, "testHere.csv")
      return(dat4graph5yrsp)
   })
   
  DSFWdat4dygraph <- reactive({ 
      dsfw<-DSFWdat()
      mins<-min(levels(factor(dsfw$begin_code)))
      numRows<-nlevels(factor(dsfw$begin_code))-as.numeric(mins)+1
      mat4longterm<-matrix("NA", nrow = numRows, ncol = 4)
      start0<-as.numeric(mins)
      end0<-nlevels(factor(dsfw$begin_code))
      nums0<-as.numeric(mins)

  for(k in start0:end0){
    j<-k-nums0+1
    dat4longtermDSFW<-subset(dsfw, begin_code==k)
    if(length(dat4longtermDSFW$Value)>=3){
    q4chart<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(.2, .5, .8))} else {
      q4chart<-c("NA", "NA", "NA")
    }
    mat4longterm[j,1]<-k
    mat4longterm[j,2]<-q4chart[1]
    mat4longterm[j,3]<-q4chart[2]
    mat4longterm[j,4]<-q4chart[3]
  }
    
    colnames(mat4longterm)<-c("WOY", "Bad20th", "Median50th", "Good80th")
    mat4LT<-as.data.frame(mat4longterm)
    mat4LT$WOY<-   as.numeric(as.character(mat4LT$WOY))
    mat4LT$Bad20th<-   round(as.numeric(as.character(mat4LT$Bad20th)),1)
    mat4LT$Median50th<-round(as.numeric(as.character(mat4LT$Median50th)),1)
    mat4LT$Good80th<-  round(as.numeric(as.character(mat4LT$Good80th)) ,1)
    mat4LT0<-mat4LT[1:52,]
    mat4LT0<-mat4LT0[complete.cases(mat4LT0[ ,1]),]
    row.names(mat4LT0)<-format(strptime(as.character(as.Date(paste(2019, mat4LT0$WOY, 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%Y-%m-%d")
    mat4LT0<-(mat4LT0[,2:4])
    return(mat4LT0)

          })
  
     presAnnotation <- function(dygraph, x, text) {
      dygraph %>%
      dyAnnotation(x, text, attachAtBottom = TRUE, width = 260)
  }
  
  getMonth <- 'function(d){
               var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
               return monthNames[d.getMonth()];
               }'
  
  getMonthDay <- 'function(d) {
                var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                date = new Date(d);
                return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'
  

  output$dygraph2 <- renderDygraph({
      dygraph(DSFWdat4dygraph(), main=paste("Long-term fieldwork trends for ", state0()," (1995 to present)", sep="")) %>%
      dyAxis("y", label = "Days per week", valueRange = c(0,7.8)) %>%
     dyAxis("x", pixelsPerLabel=40, valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) %>%
    dyOptions(drawPoints = TRUE, pointSize = 2, connectSeparatedPoints = TRUE) %>% #  dySeries("perc", label = "percentatage") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Dark2")) %>%
    dySeries(strokeWidth = 3) %>%
    dySeries("Bad20th", label = "20th percentile") %>%
    dySeries("Median50th", label = "50th percentile") %>%
    dySeries("Good80th", label = "80th percentile") %>%
    dyLegend(show = "auto", width = 600) %>% 
    #presAnnotation("20", text = "Data source: USDA NASS") %>%
      dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.4,
                hideOnMouseOut = FALSE)
  })
  
  begin<-15#input$begin
  end<-85#input$end
  medp<-50
  
  begin15p<-reactive({
    dat0000<-dat4graph5yrsp()
    dat000<-dat0000[,2:3]
    dat00<-min(which(abs(dat000$plantProg-begin)==min(abs(dat000$plantProg-begin)))) 
    dat0<-dat000$begin_code[dat00]#+1 
    return(dat0)
    })

  end85p<-reactive({ 
    dat0000<-dat4graph5yrsp()
    dat000<-dat0000[,2:3]
    dat00<-min(which(abs(dat000$plantProg-end)==min(abs(dat000$plantProg-end))))
    dat0<-dat000$begin_code[dat00] 
    return(dat0)
  })

  medianPlant<-reactive({ 
    dat0000<-dat4graph5yrsp()
    dat000<-dat0000[,2:3]
    dat00<-min(which(abs(dat000$plantProg-medp)==min(abs(dat000$plantProg-medp)))) 
    dat0<-dat000$begin_code[dat00] 
    return(dat0)
  })

  numWeeks<-reactive({end85p()-begin15p()+1  })
  
  pDSFWdat<-reactive({
    dsfw<-DSFWdat()
    pDSFWdat0<-subset(dsfw, begin_code>=begin15p()-1 & begin_code<=end85p())
    pDSFWdat0$value<-as.numeric(as.character(pDSFWdat0$Value))
    pDSFWdat0$year<-as.numeric(as.character(pDSFWdat0$year))
    return(pDSFWdat0)
    })

  
  DSFWdat3<-reactive({
    DSFWdatptest<-aggregate(value~year, data=pDSFWdat(), FUN = "length")
    DSFWdat0000<-aggregate(value~year, data=pDSFWdat(), FUN = "sum")
    DSFWdat2<-merge(DSFWdat0000,DSFWdatptest, by="year")
    DSFWdat33<-subset(DSFWdat2, value.y==numWeeks())
    return(DSFWdat33)
        })
  
  numYearsp<-reactive({length(DSFWdat3()$year)})
  
  bolls5years4graph<-reactive({
    bollDat00<-bollDat()
    bolls000<-subset(bollDat00, state_name==input$stateInput)
    bolls5years<-subset(bolls000, year>=2014)
    bolls5years$value<-as.numeric(as.character(bolls5years$Value))
    bolls5years4graph0<-aggregate(value~begin_code, data=bolls5years, FUN="mean")
    colnames(bolls5years4graph0)<-c("begin_code", "bollProg")
    write.csv(bolls5years4graph0, "bollTEst.csv")
    return(bolls5years4graph0)
  })
  
  hdat4graph5yrs<-reactive({
    hdat0000<-cottonHarvProg()
    hdat000<-subset(hdat0000, state_name==input$stateInput)#stateNames[3])
    hdat000<-subset(hdat000, begin_code>=10)
    hlast5years0<-subset(hdat000, year>=2014)
    hlast5years0$value<-as.numeric(as.character(hlast5years0$Value))
    hdat4graph5yrs0<-aggregate(value~begin_code, data=hlast5years0, FUN = "mean")
    colnames(hdat4graph5yrs0)<-c("begin_code", "harvestProg")
    hdat4graph5yrs0<-subset(hdat4graph5yrs0, begin_code<=52)
    return(hdat4graph5yrs0)
  })
  
  progDat<-reactive({
    progDat00<-full_join(dat4graph5yrsp(),bolls5years4graph())
    progDat0<-full_join(progDat00,hdat4graph5yrs())
    progDat1<-arrange(progDat0, begin_code)
    row.names(progDat1)<-format(strptime(as.character(as.Date(paste(2019, progDat1$begin_code, 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%Y-%m-%d")
    progDat2<-progDat1[,2:4]
    return(progDat2)
    })

    output$dygraph3 <- renderDygraph({
      dygraph(progDat(), main = paste("Cotton Progress for ", state0(), " (2014 to 2018)",  sep="")) %>%
    dyAxis("x", pixelsPerLabel=40, valueRange = c(0, 52), valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth)) %>%
    dyAxis("y", label = "Percent progress (%)") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
    dySeries("plantProg", label = "planting") %>%
    dySeries("bollProg", label = "open bolls") %>%
    dySeries("harvestProg", label = "harvest") %>%
      dyLegend(show = "auto", width = 280) 
  })
  
  hbegin15t<-reactive({
    dat00<-hdat4graph5yrs()
    beginPeriod<-min(which(abs(dat00$harvestProg-begin)==min(abs(dat00$harvestProg-begin))))
    twg0<-as.numeric(dat00$begin_code[beginPeriod])#+1
    return(twg0)
  })

  hend85t<-reactive({
    dat00<-hdat4graph5yrs()
    endPeriod<-min(which(abs(dat00$harvestProg-end)==min(abs(dat00$harvestProg-end))))
    twg0<-as.numeric(dat00$begin_code[endPeriod])
    return(twg0)
    })
 
  output$mostActiveHarv <- renderUI({
    HTML(paste("most active ", state0(), " harvest dates: ",
               format(strptime(as.character(as.Date(paste(2019, hbegin15t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), " to ", 
               format(strptime(as.character(as.Date(paste(2019, hend85t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), sep=""))
  })

  output$capacityTitle<-renderUI({
    HTML(paste("Probability of harvesting acreage in ", state0(), sep=""))
  })

  hbegin15<-reactive({
    woy1<-as.numeric(strftime(input$harvDates[1], format = "%V"))-1
    return(woy1)
  })
  hend85<-reactive({
    woy2<-  as.numeric(strftime(input$harvDates[2], format = "%V"))
    return(woy2)
  })
  
  
  hend85_52<-reactive({
    d1<-hbegin15()
    d2<-hend85()
    twg000<-ifelse(d2<d1,52,d2)
    return(twg000)
  })
  
   hDSFWdat<-reactive({
    dsfw<-DSFWdat()
    d1<-hbegin15()#read.csv("twg0101.csv")
    d2<-hend85()#read.csv("twg0301.csv")
    test4placeHolder<-ifelse(d2<d1, 52, d2)
    hDSFWdat0<-subset(dsfw, begin_code>=d1 & begin_code<=test4placeHolder)
    hDSFWdat0$value<-as.numeric(as.character(hDSFWdat0$Value))
    hDSFWdat0$year<-as.numeric(as.character(hDSFWdat0$year))
    return(hDSFWdat0)})
  
  
  BollsDSFWdat<-reactive({
    dsfw<-DSFWdat()
    beginBot<-as.numeric(bolls5years4graph()$begin_code[which.min(abs(bolls5years4graph()$bollProg-3))])
    endBot<-as.numeric(max(bolls5years4graph()$begin_code))+1
    BollsDSFWdat0<-subset(dsfw, begin_code>=beginBot & begin_code<=endBot)
    BollsDSFWdat0$value<-as.numeric(as.character(BollsDSFWdat0$Value))
    return(BollsDSFWdat0)
    })
  
    BollTest<-reactive({aggregate(value~year, data=BollsDSFWdat(), FUN = "sum")})
  
    hDSFWdath<-reactive({aggregate(value~year, data=hDSFWdat(), FUN = "sum")})
    hnumWeeks<-reactive({ifelse(hend85()<hbegin15(), hend85_52()-hbegin15()+1+hend85(), hend85()-hbegin15()+1)})
    hDSFWdat2<-reactive({
      hDSFWdattest<-aggregate(value~year, data=hDSFWdat(), FUN = "length")
      dat0<-merge(hDSFWdath(),hDSFWdattest,by="year")
      return(dat0)
    })
    
    
    hDSFWdat4<-reactive({
      hDSFWdat3<-subset(hDSFWdat2(), value.y==hnumWeeks())
      aggregate(value.x~year, data=hDSFWdat3, FUN = "sum")})
    
    numYearsh<-reactive({length(hDSFWdat4()$year)})
    
  acres2plot2<-reactive({
    dat4<-hDSFWdat4()
    dat4$acres<-round(dat4$value.x*input$hrs*input$fe/100*input$mph*input$swath*(input$days/7)/99, 0)
    acres2plot<-round(quantile(as.numeric(as.character(round(dat4$acres, 0))), probs=c(seq(.01,1.0,0.01))),0)
    acres2plot0<-matrix(NA,nrow=100, ncol=2)
    acres2plot0[,1]<-unname(acres2plot)
    acres2plot0[,2]<-seq(100,1,-1)#100-row.names(acres2plot2)[]  
    acres2plot00<-as.data.frame(acres2plot0)#cbind(acres, perc)
    colnames(acres2plot00)<-c("acres", "perc")
    return(acres2plot00)
  })
  
    midAcre<-reactive({subset(acres2plot2(), perc==50)[1]})
    upperAcres<- reactive({subset(acres2plot2(), perc==25)[1]})
    maxAcres <- reactive({subset(acres2plot2(), perc==1)[1]})
  
    # this dygraph replaced with plotly and no longer being used
    output$dygraph4 <- renderDygraph({
      dygraph(acres2plot2(), main=paste("Probability of harvesting acreage in ", state0(), sep="")) %>%
      dyAxis("y", label = "probability of completion") %>%
      dyAxis("x", label = "annual acreage per harvester") %>%
      dyOptions(drawPoints = TRUE, pointSize = 2, maxNumberWidth = 6, colors = "purple") %>% 
      dySeries(strokeWidth = 3, "perc", label = "percentage") %>%
      dyLegend(show = "follow", width = 120) %>%
      dyShading(from = midAcre()[1], to = upperAcres()[1], color = "khaki") %>%
      dyShading(from = upperAcres()[1], to = maxAcres()[1], color = "orangered") 
      })  
  
    output$plot4 <- renderPlotly({
      dat4shade<-acres2plot2()
      rect1 <- list(
        type = "rect",
        fillcolor = "khaki",
        line = list(color = "khaki"),
        opacity = 0.3,
        x0 = 0,#subset(acres2plot2(), perc==50)[1],#midAcre()[1],#1500, 
        x1 = 10000,
        y0 = 50, 
        y1 = 25
      )
      
      rect2 <- list(
        type = "rect",
        fillcolor = "orangered",
        line = list(color = "orangered"),
        opacity = 0.3,
        x0 = 0, 
        x1 = 10000,
        y0 = 25, 
        y1 = 0
      )
      
      maxRange<- max(dat4shade$acres)
      minRange<- min(dat4shade$acres)
      
      plot_ly(data=acres2plot2(), x=~acres, y=~perc, type = 'scatter', mode = 'lines+markers',#) %>%#,   mode="markers",
            hoverinfo = 'text', line=list(color="purple"),
            text = ~paste(perc, '% probability of harvesting ', acres, " acres", sep=""), 
            marker = list(size = 3, line=list(color="purple"), color = 'green')) %>%
      layout(#title = list(text=paste("Probability of harvesting acreage in ", state0(), sep=""), y=1.0),
             xaxis = list(title = "annual acreage per harvester", hoverformat = ".0f", spikemode  = 'across+toaxis', range=c(0.995*minRange,1.005*maxRange)),
             yaxis = list(dtick = 10, title ="probability of completion", spikemode  = 'across+toaxis'),
             shapes=list(rect1,rect2))
    })
    

    ### dual harvester comparison
    
    output$mostActiveHarv1 <- renderUI({
      HTML(paste("most active ", state0(), " harvest dates: ",
                 format(strptime(as.character(as.Date(paste(2019, hbegin15t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), " to ", 
                 format(strptime(as.character(as.Date(paste(2019, hend85t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), sep=""))
    })
    
    
    output$mostActiveHarv2 <- renderUI({
      HTML(paste("most active ", state0(), " harvest dates: ",
                 format(strptime(as.character(as.Date(paste(2019, hbegin15t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), " to ", 
                 format(strptime(as.character(as.Date(paste(2019, hend85t(), 1, sep="-"), "%Y-%U-%u")), "%Y-%m-%d"), "%b-%d"), sep=""))
    })
    
    output$capacityTitle2<-renderUI({
      HTML(paste("Probability of harvesting acreage in ", state0(), sep=""))
    })
    
    
    hbegin152<-reactive({
      woy1<-as.numeric(strftime(input$harvDates2[1], format = "%V"))-1
    #  write.csv(woy1, "woy1.csv")
      return(woy1)
    })
    hend852<-reactive({
      woy2<-  as.numeric(strftime(input$harvDates2[2], format = "%V"))
   #   write.csv(woy2, "woy2.csv")
      return(woy2)
    })
    
    hbegin151<-reactive({
      woy1<-as.numeric(strftime(input$harvDates1[1], format = "%V"))-1
      #  write.csv(woy1, "woy1.csv")
      return(woy1)
    })
    hend851<-reactive({
      woy2<-  as.numeric(strftime(input$harvDates1[2], format = "%V"))
      #   write.csv(woy2, "woy2.csv")
      return(woy2)
    })
    
      
    hend85_522<-reactive({
      d1<-hbegin152()
      d2<-hend852()
      twg000<-ifelse(d2<d1,52,d2)
      #      write.csv(twg000, "twg000.csv")
      return(twg000)
    })
    hend85_521<-reactive({
      d1<-hbegin151()
      d2<-hend851()
      twg000<-ifelse(d2<d1,52,d2)
      #      write.csv(twg000, "twg000.csv")
      return(twg000)
    })
    
    hDSFWdat4MH<-reactive({
      dsfw<-DSFWdat4MH()
      d1<-hbegin152()#read.csv("twg0101.csv")
      d2<-hend852()#read.csv("twg0301.csv")
      test4placeHolder<-ifelse(d2<d1, 52, d2)
      hDSFWdat0<-subset(dsfw, begin_code>=d1 & begin_code<=test4placeHolder)
      hDSFWdat0<-as.data.frame(hDSFWdat0)
      hDSFWdat0$value<-as.numeric(as.character(hDSFWdat0$Value))
      hDSFWdat0$year<-as.numeric(as.character(hDSFWdat0$year))
      #     write.csv(hDSFWdat0, "hDSFWdat0.csv")
      return(hDSFWdat0)})

    hDSFWdat4MH1<-reactive({
      dsfw<-DSFWdat4MH()
      d1<-hbegin151()#read.csv("twg0101.csv")
      d2<-hend851()#read.csv("twg0301.csv")
      test4placeHolder<-ifelse(d2<d1, 52, d2)
      hDSFWdat0<-subset(dsfw, begin_code>=d1 & begin_code<=test4placeHolder)
      hDSFWdat0<-as.data.frame(hDSFWdat0)
      hDSFWdat0$value<-as.numeric(as.character(hDSFWdat0$Value))
      hDSFWdat0$year<-as.numeric(as.character(hDSFWdat0$year))
      #     write.csv(hDSFWdat0, "hDSFWdat0.csv")
      return(hDSFWdat0)})
    
    hDSFWdath2<-reactive({aggregate(value~year, data=hDSFWdat4MH(), FUN = "sum")})
    hnumWeeks2<-reactive({ifelse(hend852()<hbegin152(), hend85_522()-hbegin152()+1+hend852(), hend852()-hbegin152()+1)})
    hDSFWdat2MH<-reactive({
      hDSFWdattest<-aggregate(value~year, data=hDSFWdat4MH(), FUN = "length")
      write.csv(hDSFWdattest, "test1.csv")
      dat0<-merge(hDSFWdath2(),hDSFWdattest,by="year")
      write.csv(dat0, "dat0.csv")
            return(dat0)
    })

    hDSFWdath1<-reactive({aggregate(value~year, data=hDSFWdat4MH1(), FUN = "sum")})
    hnumWeeks1<-reactive({ifelse(hend851()<hbegin151(), hend85_521()-hbegin151()+1+hend851(), hend851()-hbegin151()+1)})
    hDSFWdat2MH1<-reactive({
      hDSFWdattest<-aggregate(value~year, data=hDSFWdat4MH1(), FUN = "length")
      #write.csv(hDSFWdattest, "test1.csv")
      dat0<-merge(hDSFWdath1(),hDSFWdattest,by="year")
#      write.csv(dat0, "dat0.csv")
      return(dat0)
    })
    
        
    hDSFWdat42<-reactive({
      hDSFWdat3<-subset(hDSFWdat2MH(), value.y==hnumWeeks2())
      test<-aggregate(value.x~year, data=hDSFWdat3, FUN = "sum")
     # write.csv(test, "test321.csv")
      return(test)
      })
    
    hDSFWdat41<-reactive({
      hDSFWdat3<-subset(hDSFWdat2MH1(), value.y==hnumWeeks1())
      test<-aggregate(value.x~year, data=hDSFWdat3, FUN = "sum")
    #  write.csv(test, "test321.csv")
      return(test)
    })
    numYearsh2<-reactive({length(hDSFWdat42()$year)})
    
    numYearsh1<-reactive({length(hDSFWdat41()$year)})
    
    acres2plot4MH<-reactive({
      dat4<-hDSFWdat42()
      dat1<-hDSFWdat41()
      #dat4<-read.csv("test321.csv")
      dat4$acres1<-round(dat1$value.x*input$hrs1*input$fe1/100*input$mph1*input$swath1*(input$days1/7)*input$units1/99, 0)
      #dat4$acres1<-round(dat4$value.x*9*90/100*4*19*(7/7)/8.25, 0)
      dat4$acres2<-round(dat4$value.x*input$hrs2*input$fe2/100*input$mph2*input$swath2*(input$days2/7)*input$units2/99, 0)
      #dat4$acres2<-round(dat4$value.x*9*70/100*4*19*(5/7)/8.25, 0)
      acres2plot1<-round(quantile(as.numeric(as.character(round(dat4$acres1, 0))), probs=c(seq(.01,1.0,0.01))),0)
      acres2plot2<-round(quantile(as.numeric(as.character(round(dat4$acres2, 0))), probs=c(seq(.01,1.0,0.01))),0)
      acres2plot0<-matrix(NA,nrow=100, ncol=3)
      acres2plot0[,1]<-unname(acres2plot1)
      acres2plot0[,2]<-unname(acres2plot2)
      acres2plot0[,3]<-seq(100,1,-1)#100-row.names(acres2plot2)[]  
      acres2plot00<-as.data.frame(acres2plot0)#cbind(acres, perc)
      colnames(acres2plot00)<-c("acres1", "acres2","perc")
      write.csv(acres2plot00, "test1234.csv")
      return(acres2plot00)
    })
    
    midAcre2<-reactive({subset(acres2plot4MH(), perc==50)[1]})
    upperAcres2<- reactive({subset(acres2plot4MH(), perc==25)[1]})
    maxAcres2 <- reactive({subset(acres2plot4MH(), perc==1)[1]})
    
    output$plot4dualHarvesters <- renderPlotly({
      dat4shade<-acres2plot4MH()
      rect1 <- list(
        type = "rect",
        fillcolor = "khaki",
        line = list(color = "khaki"),
        opacity = 0.3,
        x0 = 0,#subset(acres2plot2(), perc==50)[1],#midAcre()[1],#1500, 
        x1 = 10000,
        y0 = 50, 
        y1 = 25
      )
      
      rect2 <- list(
        type = "rect",
        fillcolor = "orangered",
        line = list(color = "orangered"),
        opacity = 0.3,
        x0 = 0, 
        x1 = 10000,
        y0 = 25, 
        y1 = 0
      )
      
      maxRange<- max(c(dat4shade$acres1, dat4shade$acres2))
      minRange<- min(c(dat4shade$acres1, dat4shade$acres2))
      
      plot_ly(data=dat4shade, x=~acres1, y=~perc, type = 'scatter', mode = 'lines+markers',#) %>%#,   mode="markers",
              line=list(color="purple"), name = "1st harvester", 
              hoverinfo = 'text', 
              hovertemplate = ~paste(perc, '% probability of harvesting ',  "%{x:.0f}", " acres", sep=""), 
              marker = list(size = 3, line=list(color="purple"), color = 'purple')) %>%
        add_trace(x = ~acres2, name = '2nd harvester', mode = 'lines+markers', color="orange", 
                  line = list(color="orange"),
                  marker = list(size = 3, line=list(color="orange"), color="orange")) %>%
        layout(#title = list(text=paste("Probability of harvesting acreage in ", state0(), sep=""), y=1.0),
          xaxis = list(title = "annual acreage per harvester", hoverformat = ".0f", hovermde = 'closest',
          spikemode  = 'across+toaxis', range=c(0.995*minRange,1.005*maxRange)),
          yaxis = list(dtick = 10, title ="probability of completion", spikemode  = 'across+toaxis'),
          shapes=list(rect1,rect2))
    })
    
    
    
    ##########
    ### yield penalties
    # next iteration, give users ability to edit cells for use in analysis
  
    output$yieldPenalty = renderDT({
    yieldPenalty<-read.csv("data/cottonYieldPenalty.csv", header = TRUE, row.names = 1)
    changeNames<-colnames(yieldPenalty)
    colnames(yieldPenalty)<-gsub("X", "", changeNames)
        yieldPenalty<-as.data.frame(yieldPenalty)
        rownames(yieldPenalty)<-format(as.Date(paste(2019, rownames(yieldPenalty), 1, sep="-"), "%Y-%U-%u"), "%b %d")
        colnames(yieldPenalty)<-format(as.Date(paste(2019, colnames(yieldPenalty), 1, sep="-"), "%Y-%U-%u"), "%b %d")
        yieldPenalty
        datatable(yieldPenalty, editable='cell', 
                  options = list(searching = FALSE, dom = 't', ordering = F)) %>% 
                  formatStyle(columns = colnames(yieldPenalty), fontSize = '100%') #%>%
    })
    
    ##

        ##
    
  }

shinyApp(ui=ui, server=server, enableBookmarking = "url")

