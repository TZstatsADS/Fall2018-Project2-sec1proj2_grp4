packages.used=c("dplyr", "plotly", "shiny", "leaflet", "scales", 
                "lattice", "htmltools", "maps", "data.table", 
                "dtplyr", "mapproj", "randomForest", "ggplot2", "rpart", "viridisLite",
                "viridis", "highcharter", "googleVis")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(htmltools)
library(maps)
library(plotly)
library(data.table)
library(dtplyr)
library(mapproj)
library(randomForest)
library(ggplot2)
library(rpart)

library(viridisLite)
library(viridis)
library(highcharter)
library(googleVis)

data("USArrests", package = "datasets")
data("usgeojson")

dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)), 
                        c = substring(viridis(5 + 1), 0, 7)) %>% 
  list_parse2()

# calculate scores for a hospital 

calScore <- function(row,care.vec){
  # weight suggested for 7 criterion
  origin.weight <- c(11,11,11,11,2,2,2) 
  # care weight for 7 criterion
  care.weight <- origin.weight*care.vec/sum(origin.weight*care.vec)
  # hospital scores for 7 criterion
  criterion.score <- as.numeric(c(row[c(15,17,19,21,23,25,27)]))
  
  temp <- ifelse(is.na(criterion.score),0,care.weight)
  update.weight <- temp/sum(temp)
  
  score <- update.weight*criterion.score
  return(sum(score,na.rm = TRUE))
}

# switch payment to dollar signs

payswitch <- function(payment){
  if(is.na(payment)) {return("Not Avaliable")}
  else {if(payment<=1.5) {return("$")}
    else{if(payment<2.5) {return("$$")}
      else{return("$$$")}}}
}

# switch overall rating

orswitch <- function(rating){
  if(is.na(rating)){return("Not Available")}
  else {return(as.numeric(rating))}
}

shinyServer(function(input, output){
  #read data
  load("./hos.RData")
  load("./importance.RData")
  load("./df.RData")
  load("./hospital_ratings.RData")
  load("./iips.Rdata")
  load("./charge_by_states.RData")
  
  data <- hos
  iips <- iips_data
  
  #Inputs
  
  state<-reactive({state<-input$state})
  type <- reactive({type <- input$type})
  EM<- reactive({EM<- input$EM})
  DRG<- reactive({DRG<- input$DRG})
  cost<- reactive({cost<- input$cost})
  drgCode<- reactive({drgCode<- input$drgCode})
  drgCode2<- reactive({drgCode2<- input$drgCode2})
  hospital<- reactive({hospital<- input$hospital})
  sel<- reactive({sel<- input$all})
  
  care1 <- reactive({input$care1}) # Mortality
  care2 <- reactive({input$care2}) # Safety of care
  care3 <- reactive({input$care3}) # Readmission rate
  care4 <- reactive({input$care4}) # Patient experience
  care5 <- reactive({input$care5}) # Effectiveness of care
  care6 <- reactive({input$care6}) # Timeliness of care
  care7 <- reactive({input$care7}) # Efficient use of medical imaging
  
  
 
   v1<-reactive({
    if (state() == "Select") {v1<-hos} 
    else {
      selectstate<-state()
      v1<- hos %>% filter(State == state())}})  
  
  n1<- reactive({
    if (EM() == "Select"){n1<- v1()}
    else {
      selectEM <- EM()
      n1<- v1() %>% filter(Emergency.Services == EM())}
  })
  
  n2<- reactive({
    if (DRG() == "Select"){n2<- n1()}
    else{
      selectDRG <- DRG()
      iips_d<- iips_data[DRG()==iips$DRG.Definition[is.na(iips$DRG.Definition)== F],"Provider.ID"]
      n2<- n1()[n1()$Provider.ID %in% iips_d ,]
    }
  })
  
  n3<- reactive({
    if (cost() == 20000){n3<- n2()}
    else{
      selectcost <- cost()
      iips_c<- iips_data[iips$Average.Total.Payments[is.na(iips$Average.Total.Payments)== F]< cost(),"Provider.ID"]
      n2<- n1()[n1()$Provider.ID %in% iips_c ,]
    }
  })
  
  
  v2 <- reactive({
    if (type() == "Select") {v2 <- n3()}
    else{
      selecttype <- type()
      v2 <- n3() %>% filter(Hospital.Type == type())}})
  
  care.origin <- reactive(care.origin <- c(care1(),care2(),care3(),
                                           care4(),care5(),care6(),care7()))
  # Dataset for the selected state
  data.state <- reactive(data.state <- v2())
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(data.state()),1,calScore,care.vec = care.vec()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(v2()),1,calScore,care.vec = care.vec()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  # ranks for hospitals
  rk <- reactive(floor(frankv(score(),order = -1,ties.method = "min")))
  
  v3 <- reactive({v3 <- cbind(v2(),Order = ord(),Rank = rk())})
  
  # Outputs
  
  output$tableinfo = renderDataTable(
      {
        data1 <- v2()
        infotable <- data1[, c(2, 3, 5, 4, 6, 8, 9, 13)]
        infotable$Hospital.overall.rating <- apply(data.frame(as.numeric(data1$Hospital.overall.rating)),
                                                          1,orswitch)
       colnames(infotable) <- c("Hospital Name","Address","State","City","ZIP","TEL","Type","Overall Rating")
        infotable

    },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
  
  
  
  output$tableinfo1 = renderDataTable(
    {
      data1 <- v2()
      infotable <- data1[, c(2, 3, 4, 9, 30, 31, 32, 33, 34, 13)]
      infotable$Hospital.overall.rating <- apply(data.frame(as.numeric(data1$Hospital.overall.rating)),
                                                 1,orswitch)
      colnames(infotable) <- c("Hospital Name","Address","City","Type", "Mortality", "Safety", "Readmission",
                               "Patient Experience", "Effectiveness", "Overall Rating")
      infotable
      
    },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
  
  
  output$tablerank1 = renderDataTable({
    rankedtable <- cbind(v3()$Rank[ord()],v3()[ord(),c(2,3,4,5,6,8,9,29)])
    
    rankedtable$payment <- apply(data.frame(rankedtable$payment),1,payswitch)
    colnames(rankedtable) <- c("Rank","Hospital Name","Address","City",
                               "State","ZIP","TEL","Type","Cost")
    
    rankedtable
  },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
  

  hospIcons <- iconList(emergency = makeIcon("emergency_icon.png", iconWidth = 25, iconHeight =25),
                        critical = makeIcon("critical_icon.png", iconWidth = 25, iconHeight =25),
                        children = makeIcon("children_icon.png", iconWidth = 20, iconHeight =25))
  
  
  
  output$map <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Hospital.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),

                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())
  })
  
  output$map1 <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Hospital.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),
                     
                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())
  })
  
  
  output$VI <- renderPlotly({
    
    b <- ggplot(importance.df, aes(x=Variables, y=MeanDecreaseGini)) +   
         geom_point(size = importance.df$MeanDecreaseGini/12,  
                    color = c("#999999", "#E69F00", "#56B4E9", "#009E73",  
                              "#F0E442", "#0072B2", "#D55E00"),  
                    alpha = 0.6) +
         theme(axis.text.x = element_text(angle = 40))+
         ggtitle('Variable Importance')+
         ylab("Mean Drop Gini")+
         theme(plot.title=element_text(hjust=0.5))
    
    ggplotly(b) %>% layout(height = 700, width = 1000)
    
  }
  
  )
  
  output$NHS <- renderPlotly({
  
    c <- ggplot(df, aes(x=State, y = Freq))+
      geom_bar(stat="identity", aes(fill = df$Freq))+
      labs(title= "Number of Hospitals by State", x="State", y=NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90, size = 8))+
      theme(plot.title= element_text(hjust=0.5, vjust=1))+
      scale_y_continuous(expand = c(0,0))+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(c) %>% layout(height = 700, width = 1000)
    
      c + scale_fill_continuous(name="Frequency")
  }
  
  )
  
  output$HQS <- renderPlotly({
    
    d <- ggplot(hospital_ratings.df, aes(x=State, y=HospitalRating))+
      geom_bar(stat="identity", alpha = 0.7, fill = "#009E73") +
      labs(title="Hospital Quality by State", x="State", y="Quality - OverallRating (1-5)")+
      theme_classic()+
      theme(axis.text.x=element_text(angle=90, hjust=1, size = 8))+
      theme(plot.title=element_text(hjust=0.5))+
      ylim(0,5)+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(d) %>% layout(height = 700, width = 1000)
    
  }
  
  )
  output$plot1 <- renderHighchart({
    highchart() %>%
      hc_add_series_map(usgeojson, charge_by_states, name = "Number of Hospitals",
                        value = "Total", joinBy = c("woename", "statename"), dataLabels = list(enabled = TRUE, format = '{point.name}'))  %>% 
      hc_colorAxis(stops = dshmstops) %>%
      hc_credits(enabled = TRUE, text = "Source :  https://www.medicare.gov ") %>%
      hc_add_theme(hc_theme_google())  %>%
      hc_mapNavigation(enabled = TRUE)
    
  })
  
  
  
  output$plot2 <- renderHighchart({
    data2 <- iips_data %>%
      filter(DRG.Definition == drgCode()) %>%
      group_by(Provider.State) %>% 
      dplyr::summarise(mean_ac = mean(Average.Covered.Charges))
    data2$statename <- state.name[match(data2$Provider.State, state.abb)]
    #data2[is.na(data)] <- "DC"
    
    highchart() %>%
      hc_add_series_map(usgeojson, data2, name = "Average Covered Cost",
                        value = "mean_ac", joinBy = c("woename", "statename"), dataLabels = list(enabled = TRUE, format = '{point.name}'))  %>% 
      hc_colorAxis(stops = dshmstops) %>%
      hc_credits(enabled = TRUE, text = "Source :  https://www.medicare.gov ") %>%
      hc_add_theme(hc_theme_google())  %>%
      hc_mapNavigation(enabled = TRUE)
  })
  
  output$plot3 <- renderHighchart({
    data3 <- iips_data %>%
      filter(DRG.Definition == drgCode()) %>%
      group_by(Provider.State) %>% 
      dplyr::summarise(mean_am = mean(Average.Medicare.Payments))
    data3$statename <- state.name[match(data3$Provider.State, state.abb)]
    #data3[is.na(data)] <- "DC"
    
    highchart() %>%
      hc_add_series_map(usgeojson, data3, name = "Average Medicare Cost",
                        value = "mean_am", joinBy = c("woename", "statename"), dataLabels = list(enabled = TRUE, format = '{point.name}'))  %>% 
      hc_colorAxis(stops = dshmstops) %>%
      hc_credits(enabled = TRUE, text = "Source :  https://www.medicare.gov ") %>%
      hc_add_theme(hc_theme_google())  %>%
      hc_mapNavigation(enabled = TRUE)
  })
  
  
  output$plot4 <- renderHighchart({
    data4 <- iips_data %>%
      filter(DRG.Definition == drgCode()) %>%
      group_by(Provider.State) %>% 
      dplyr::summarise(mean_am = mean(Average.Total.Payments))
    data4$statename <- state.name[match(data4$Provider.State, state.abb)]
    #data4[is.na(data)] <- "DC"
    
    highchart() %>%
      hc_add_series_map(usgeojson, data4, name = "Average Total Payments",
                        value = "mean_am", joinBy = c("woename", "statename"), dataLabels = list(enabled = TRUE, format = '{point.name}'))  %>% 
      hc_colorAxis(stops = dshmstops) %>%
      hc_credits(enabled = TRUE, text = "Source :  https://www.medicare.gov ") %>%
      hc_add_theme(hc_theme_google())  %>%
      hc_mapNavigation(enabled = TRUE)
  })

  
  output$plot5 = renderGvis(
    
    if (sel()== TRUE){
    gvisColumnChart(
        iips_data %>% filter(DRG.Definition == drgCode2()),
        xvar="Provider.Name",
        yvar=c("Average.Covered.Charges","Average.Medicare.Payments"),
        options = list(
          height = 600,
          legend = "{position: 'top'}",
          hAxis = paste("{",
                        "slantedText: true",
                        "}")))}
      
      else{
        gvisColumnChart(
      iips_data %>% filter(DRG.Definition == drgCode2() &
                             Provider.Name %in% hospital()),
      xvar="Provider.Name",
      yvar=c("Average.Covered.Charges","Average.Medicare.Payments"),
      options = list(
        height = 600,
        legend = "{position: 'top'}",
        hAxis = paste("{",
                      "slantedText: true",
                      "}")))}
    )
  
  output$plot11 <- renderPlot({
    
    iips_data %>%
      mutate(ratio = Average.Total.Payments/(Average.Covered.Charges+Average.Medicare.Payments+Average.Total.Payments)) %>%
      group_by(Provider.Name) %>%
      summarize(m.ratio = mean(ratio)) %>%
      arrange(-m.ratio) %>%
      tail(25) %>%
      ggplot(aes(x=reorder(Provider.Name, m.ratio), y=m.ratio)) +
      geom_bar(stat="identity", fill="#009E73", color="white") +
      ggtitle("Ratio of Total Payments to Total Charges") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="Hospital Name", y="% Ratio") +
      theme(text=element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      geom_text(aes(x=Provider.Name, y=round(m.ratio,2), label=round(m.ratio,2), pos=2, vjust=-0.5)) +
      scale_y_continuous(limits = c(0,0.25), labels = scales::percent)
    
    
    
  })
  
  output$plot12 <- renderPlot({
    
    hospital.ratings.df <- hos %>%
      group_by(State) %>%
      summarise(HospitalRating=mean(as.numeric(Hospital.overall.rating), na.rm=TRUE)) %>%
      data.frame()
    hospital.ratings.df <- hospital.ratings.df[-4,] #removing AS with NaN
    
    ggplot(hospital.ratings.df) +
        geom_bar(aes(x=reorder(State, -HospitalRating), y=HospitalRating), stat="identity", color="white", fill="#009E73") +
      ggtitle("Hospital Quality in Each State") +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="State", y="Quality Overall Rating (1-5)") +
      theme(axis.text.x=element_text(angle=90, hjust=1, size = 8)) +
      theme(plot.title=element_text(hjust=0.5)) +
      ylim(0,5)
    
    
  })
  
  output$explain0<- renderText({"This graph represents the hospitals with the lowest ratios of total payments to the sum of total payments, covered charges and medicare payments. These are the top 25 hospitals where patients relatively paid the lowest for their medical care out of the total medical charges."})
  
  output$read0<- renderText({"About Us"})
  output$read1<- renderText({"This app was developed by a team of 5 dedicated members at Columbia Univerisity in Fall 2018:"})
  output$read2<- renderText({"Ghada, Jerfel ( gj2261@barnard.edu )"})
  output$read3<- renderText({"Jin, Peiqi ( pj2324@columbia.edu )"})
  output$read4<- renderText({"li, Xiaoyi ( xl2694@columbia.edu )"})
  output$read5<- renderText({"Wang, Zehan ( zw2457@columbia.edu )"})
  output$read6<- renderText({"Wei, Xiaojie ( xw2536@columbia.edu )"})
  output$read7<- renderText({"Each one of these Columbia University students contributed to this application so they can make your access to information readily available and more personalized. 
    We wish you good health and we hope that you find our app of use."})
  
  output$read8<- renderText({"Hello there! Do you worry about the quality of hospitals that you choose? Are you concerned if the costs exceed your budget limit? Please check our Hospital Finder app!"})
  output$read9<- renderText({"User Manual:"})
  output$read10<- renderText({"If you click on the Hospital Recommendation tab, you will have two options: General Selection and Personalized Recommendation. "})
  output$read11<- renderText({"General Selection helps you find hospitals based on your state, medical condition, the hospital time, and your budget. As for Personalized Recommendation, it helps you get more detailed information about the quality of the hospital that you chose based on the General Selection findings."})
  output$read12<- renderText({"Please follow the instructions below for:"})
  
  output$read13<- renderText({"General Selection:"})
  output$read14<- renderText({"Step 1: Select the State that you live in or that you need to go to. Also, identify your hospital type."})
  output$read15<- renderText({"Step 2: Select the medical condition, if it is known to you. You can find it under 'DRG'."})
  output$read16<- renderText({"Step 3: Choose your budget by sliding the cost bar. If you slide the bar to the right end, there won't be a budget limitation."})
  output$read17<- renderText({"Step 4: Decide whether or not you need urgent care, and refer to 'Emergency Services' selection, then submit your selection by clicking on 'Find Your Hospital Today!'"})
  output$read18<- renderText({"Step 5: Check all hospital basic information in the Medicare Assessment table. "})
  output$read19<- renderText({"Step 6: Click on the interactive map to see the exact location of the hospital. You will find relevant information to this hospital on the map as well. "})
  output$read20<- renderText({"Step 7: Go to the right hospital and hope you get better soon!"})
  
  output$read21<- renderText({"Personalized Recommendation"})
  output$read22<- renderText({"Please tell us how much you care about the Mortality Rate, Safety of Care, Readmission Rate, Patient experience, Effectiveness of Care, Timeliness of Care, Efficient Use of Medical Imaging."})
  output$read23<- renderText({"Once you click on 'Find Your Hospital Today!' you will get our personalized ranking of the hospitals for you!"})
  
  output$read24<- renderText({"About the Data:"})
  output$read25<- renderText({"I. Personalized Ranking: It is important to note that there are criterias for certain measurements in order to produce the personalized ranking. Please note that:"})
  output$read26<- renderText({""})
  output$read27<- renderText({"1. Mortality measures the death rate of patients."})
  output$read28<- renderText({"2. Safety of care measures the rate of certain complications and infections."})
  output$read29<- renderText({"3. Readmission measures the rate of unplanned readmission after treatment."})
  output$read30<- renderText({"4. Patient experience measures how well patients feel during treatment, surgery and hospitalization."})
  output$read31<- renderText({"5. Effectiveness of care measures how appropriately patients are treated."})
  output$read32<- renderText({"6. Timeliness of care measures the time patients waiting."})
  output$read33<- renderText({"7. Efficient use of medical imaging measures how efficiently the hospitals using medical imaging such as MRI and CT scans."})
  output$read34<- renderText({"Please follow this link for more information"})
  
  output$read35<- renderText({"II. Costs: It is important to clarify the different charges/costs mentioned in the data:"})
  output$read36<- renderText({"1. Covered Charges: these are charges covered by the insurance companies of the patients."})
  output$read37<- renderText({"2. Medicare Payments: these are payments made by the federal health program on behalf of:"})
  output$read38<- renderText({"> Individuals who are 65 years old or older"})
  output$read39<- renderText({"> Young individuals with disabilities "})
  output$read40<- renderText({"> Individuals with End-Stage Renal Disease (permanent kidney failure requiring dialysis or a transplant, sometimes called ESRD)"})
  output$read41<- renderText({"3. Total Payments: these are the payments that the patients make."})
  
 
  
  })

