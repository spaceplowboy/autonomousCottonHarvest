
setwd("~/conferences/2020/2020BeltwideCotton/cottonRdecisionTool")

rm(list = ls())

library(tidyverse)
#library(httr)
#library(jsonlite)
library(plyr)
library(tmap)
#library(strucchange)
#library(reshape2)
#library(usdarnass) # negates necessity for API
library(extrafont)
library(Hmisc)

library(dygraphs)
#font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

###### UI
#machinery user inputs; option A and option B to compare 2 machines or picker v stripper etc
fe<-.9
mph<-4.2
swath<-19
wr<-fe*mph*swath/8.25
hrs<-9 # hours per day
days<-7 #days per week worked, 0 to 7; 6, 6.5, 7 commonly used
acres<-2000 # desired annual acres covered

#cottonHarvest<-nass_data(year=">=1950", agg_level_desc = "STATE", 
#                         short_desc = "COTTON - ACRES HARVESTED",
#                         reference_period_desc = "YEAR")
#write.csv(cottonHarvest, "cottonHarvest.csv")
cottonHarvest<-read.csv("data/cottonHarvest.csv")

# dropdown selection state or region of interest
state<-levels(factor(cottonHarvest$state_name))[10]
state0<-capitalize(tolower(state))


######


#DSFW<-nass_data(year=">=1995", agg_level_desc = "STATE", 
 #               short_desc = "FIELDWORK - DAYS SUITABLE, MEASURED IN DAYS / WEEK ")

#DSFW<-subset(DSFW, state_name!="US TOTAL")
#write.csv(DSFW, "DSFW.csv")
DSFW<-read.csv("data/DSFW.csv")
############

#cottonPlantProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
 #                          short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT PLANTED")
#cottonPlantProg<-subset(cottonPlantProg, state_name!="US TOTAL")

#write.csv(cottonPlantProg, "cottonPlantProg.csv")
cottonPlantProg<-read.csv("data/cottonPlantProg.csv")

#cottonHarvProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
 #                         short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT HARVESTED")
#cottonHarvProg<-subset(cottonHarvProg, state_name!="US TOTAL")

#write.csv(cottonHarvProg, "cottonHarvProg.csv")
cottonHarvProg<-read.csv("data/cottonHarvProg.csv")

#cottonBollProg<-nass_data(year=">=2005", agg_level_desc = "STATE", 
 #                         short_desc = "COTTON, UPLAND - PROGRESS, MEASURED IN PCT BOLLS OPENING")
#cottonBollProg<-subset(cottonBollProg, state_name!="US TOTAL")

#write.csv(cottonBollProg, "cottonBollProg.csv")
cottonBollProg<-read.csv("data/cottonBollProg.csv")


bollDat<-cottonBollProg

harvDat<-cottonHarvProg
harvDat$value<-as.numeric(gsub(",", "", harvDat$Value))
harvDat$harvPerc<-as.numeric(harvDat$value)
harvDat$year<-as.numeric(as.character(harvDat$year))

bollDat<-cottonBollProg
bollDat$value<-as.numeric(gsub(",", "", bollDat$Value))
bollDat$harvPerc<-as.numeric(bollDat$value)
bollDat$year<-as.numeric(as.character(bollDat$year))

######


#cottonHarvest<-nass_data(year="2018", agg_level_desc = "STATE", 
 #                        short_desc = "COTTON - ACRES HARVESTED",
  #                       source_desc="SURVEY",
   #                      reference_period_desc = "YEAR")



######


######

tab4desc<-matrix("NA", nrow=nlevels(factor(cottonPlantProg$state_name)), ncol=8)
mat4lm<-matrix("NA", nrow=nlevels(factor(cottonPlantProg$state_name)), ncol = 7)
mat4ratio<-matrix("NA", nrow=nlevels(factor(cottonPlantProg$state_name)), ncol=4)

DSFWdat<-subset(DSFW, state_name==state)
  plantdat<-subset(cottonPlantProg, state_name==state)
  last5yearsp<-subset(plantdat, year>=2014)
  last5yearsp$value<-as.numeric(as.character(last5yearsp$Value))
  last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
  last10yearsp<-subset(plantdat, year>=2008)
  dat4graph5yrsp<-aggregate(value~begin_code1, data=last5yearsp, FUN = "mean")
  #dat4graph10yrsp<-aggregate(Value~begin_code, data=last10yearsp, FUN = "mean")
  
#  png(paste("myPlot", state,".png", sep=""), 
 #     width=6, height=6, units="in", res=720)
  #par(mfrow=c(2,2))
      
  #by begin codes
  numRows<-nlevels(factor(DSFWdat$begin_code))-as.numeric(min(levels(factor(DSFWdat$begin_code))))+1
  mat4longterm<-matrix("NA", nrow = numRows, ncol = 4)
  mat4longterm2<-matrix("NA", nrow = numRows, ncol = 101)
  
  for(k in as.numeric(min(levels(factor(DSFWdat$begin_code)))):nlevels(factor(DSFWdat$begin_code))){
#      k=11
    WOY<-k
    j<-k-as.numeric(min(levels(factor(DSFWdat$begin_code))))+1
    dat4longtermDSFW<-subset(DSFWdat, begin_code==WOY)
    q4chart<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(.2, .5, .8))
    
    mat4longterm[j,1]<-k
    mat4longterm[j,2]<-q4chart[1]
    mat4longterm[j,3]<-q4chart[2]
    mat4longterm[j,4]<-q4chart[3]

    
    #q4chart2<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(seq(.01,1.0,0.01)))
    #mat4longterm2[j,1]<-k
    #mat4longterm2[j,2]<-q4chart2[1]
    #mat4longterm2[j,3]<-q4chart[2]
    #mat4longterm2[j,4]<-q4chart[3]
    
  }
  
  colnames(mat4longterm)<-c("WOY", "Bad20th", "Median50th", "Good80th")
  mat4LT<-as.data.frame(mat4longterm)
  mat4LT$Bad20th<-   as.numeric(as.character(mat4LT$Bad20th))
  mat4LT$Median50th<-as.numeric(as.character(mat4LT$Median50th))
  mat4LT$Good80th<-  as.numeric(as.character(mat4LT$Good80th)) 
  
  plot(mat4longterm[,1], mat4longterm[,4], xlim=c(1,50), ylim=c(0,7), type="l", lwd=2, main="Fieldwork probabilities",
       xlab="Week of Year", ylab="Days per week", lty=1, col="green")
  lines(mat4longterm[,1], mat4longterm[,3], col="orange", lty=2, lwd=2)
  lines(mat4longterm[,1], mat4longterm[,2], col="red", lty=3, lwd=2)
  legend(5, 1,  c("80th", "50th", "20th"), col=c("green", "orange","red"), lty=c(1,2,3), horiz=TRUE, cex = .7, bty = "n") 
  
  DSFWdat4dygraph<- as.data.frame(  cbind(as.numeric(mat4longterm[,1]), as.numeric(mat4longterm[,2]),as.numeric(mat4longterm[,3]),as.numeric(mat4longterm[,4])))
  DSFWdat4dygraph[1:7,2:4]<-"NA"
  dygraph(DSFWdat4dygraph, main=paste("Probability of suitable day for fieldwork for ", state0, sep="")) %>%
    dyAxis("y", label = "Days per week") %>%
    dyAxis("x", label = "Week of year") %>%
    dyOptions(drawPoints = TRUE, pointSize = 2, connectSeparatedPoints = FALSE) %>% #  dySeries("perc", label = "percentatage") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Dark2")) %>%
    dySeries(strokeWidth = 3) %>%
    dySeries("V2", label = "bad 20th percentile") %>%
    dySeries("V3", label = "median 50th percentile") %>%
    dySeries("V4", label = "good 80th percentile") %>%
    dyLegend(show = "auto", width = 380) 
  
  begin=15
  medp=50
  end=85
  beginPeriodp<-min(which(abs(dat4graph5yrsp$value-begin)==min(abs(dat4graph5yrsp$value-begin))))
  begin15p<-dat4graph5yrsp$begin_code1[beginPeriodp]+1
#  begin<-
#  end<-strftime('2019-10-30', format = "%V")
  
  endPeriodp<-min(which(abs(dat4graph5yrsp$value-end)==min(abs(dat4graph5yrsp$value-end))))
  end85p<-dat4graph5yrsp$begin_code1[endPeriodp]
  medPeriodp<-min(which(abs(dat4graph5yrsp$value-medp)==min(abs(dat4graph5yrsp$value-medp))))
  medianPlant<-dat4graph5yrsp$begin_code1[medPeriodp]
  
  
  numWeeks<-end85p-begin15p+1

    pDSFWdat<-subset(DSFWdat, begin_code>=begin15p & begin_code<=end85p)
    pDSFWdat$value<-as.numeric(as.character(pDSFWdat$Value))
    pDSFWdat$year<-as.numeric(as.character(pDSFWdat$year))
    DSFWdatptest<-aggregate(value~year, data=pDSFWdat, FUN = "length")
    DSFWdatp<-aggregate(value~year, data=pDSFWdat, FUN = "sum")
    DSFWdat2<-merge(DSFWdatp,DSFWdatptest,by="year")
    DSFWdat3<-subset(DSFWdat2, value.y==numWeeks)
    
    numYearsp<-length(DSFWdat3$year)
    
    hdat<-subset(cottonHarvProg, state_name==state)
    bolls<-subset(bollDat, state_name==state)
    summary(factor(cottonHarvProg$state_name))
    
    bolls5years<-subset(bolls, year>=2014)
    bolls5years$value<-as.numeric(as.character(bolls5years$Value))
    
    hlast5years<-subset(hdat, year>=2014)
    hlast5years$value<-as.numeric(as.character(hlast5years$Value))
    #last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
    #hlast10years<-subset(hdat, year>=2008)
  hdat4graph5yrs<-aggregate(value~begin_code, data=hlast5years, FUN = "mean")
  bolls5years4graph<-aggregate(value~begin_code, data=bolls5years, FUN="mean")
  #hdat4graph10yrs<-aggregate(Value~begin_code, data=hlast10years, FUN = "mean")
  beginBot<-as.numeric(bolls5years4graph$begin_code[which.min(abs(bolls5years4graph$value-3))])
  endBot<-as.numeric(max(bolls5years4graph$begin_code))+1
  
  
  colnames(dat4graph5yrsp)<-c("begin_code", "plantProg")
  colnames(hdat4graph5yrs)<-c("begin_code", "harvestProg")
  colnames(bolls5years4graph)<-c("begin_code", "bollProg")
  progDat0<-full_join(dat4graph5yrsp,bolls5years4graph)
  progDat1<-full_join(progDat0,hdat4graph5yrs)
  progDat<-arrange(progDat1, begin_code)
  
  dygraph(progDat, main = paste("Cotton Progress for ", state0, sep="")) %>%
  dyAxis("x", label = "Week of year", valueRange = c(0, 52)) %>%
  dyAxis("y", label = "Percent progress (%)") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Dark2")) %>%
    dySeries("plantProg", label = "planting") %>%
    dySeries("bollProg", label = "open bolls") %>%
    dySeries("harvestProg", label = "harvest") %>%
    dyLegend(show = "auto", width = 380) 
    

  
  beginPeriod<-min(which(abs(hdat4graph5yrs$harvestProg-begin)==min(abs(hdat4graph5yrs$harvestProg-begin))))
  hbegin15<-as.numeric(hdat4graph5yrs$begin_code[beginPeriod])+1
  endPeriod<-min(which(abs(hdat4graph5yrs$harvestProg-end)==min(abs(hdat4graph5yrs$harvestProg-end))))
  hend85<-as.numeric(hdat4graph5yrs$begin_code[endPeriod])
  medhPeriod<-min(which(abs(hdat4graph5yrs$harvestProg-medp)==min(abs(hdat4graph5yrs$harvestProg-medp))))
  medh85<-as.numeric(hdat4graph5yrs$begin_code[medhPeriod])
  
  woy1<-"2019-09-01"
  woy2<-"2019-10-30"
  
  strftime(c("2014-03-16", "2014-03-17","2014-03-18", "2014-01-01"), format = "%V")
  hbegin15<-as.numeric(strftime(woy1, format = "%V"))
  hend85<-as.numeric(strftime(woy2, format = "%V"))
  
  
  hend85_52<-ifelse(hend85<hbegin15,52,hend85)
  
  bedBollPeriod<-min(which(abs(  bolls5years4graph$bollProg-medp)==min(abs(  bolls5years4graph$bollProg-medp))))
  medBolls<-as.numeric(  bolls5years4graph$begin_code[bedBollPeriod])
  
  test4placeHolder<-ifelse(hend85<hbegin15, 52, hend85)
    
   hDSFWdat<-subset(DSFWdat, begin_code>=hbegin15 & begin_code<=test4placeHolder)

   BollsDSFWdat<-subset(DSFWdat, begin_code>=beginBot & begin_code<=endBot)
   
  hDSFWdat$value<-as.numeric(as.character(hDSFWdat$Value))
  hDSFWdat$year<-as.numeric(as.character(hDSFWdat$year))
  
  hDSFWdattest<-aggregate(value~year, data=hDSFWdat, FUN = "length")
  BollsDSFWdat$value<-as.numeric(as.character(BollsDSFWdat$Value))
  BollTest<-aggregate(value~year, data=BollsDSFWdat, FUN = "sum")
  
  
  hDSFWdath<-aggregate(value~year, data=hDSFWdat, FUN = "sum")
  hnumWeeks<-ifelse(hend85<hbegin15, hend85_52-hbegin15+1+hend85, hend85-hbegin15+1)
  hDSFWdat2<-merge(hDSFWdath,hDSFWdattest,by="year")
  hDSFWdat3<-subset(hDSFWdat2, value.y==hnumWeeks)
  
  hDSFWdat4<-aggregate(value.x~year, data=hDSFWdat3, FUN = "sum")

  numYearsh<-length(hDSFWdat4$year)
  
###########
#dygraphs

hDSFWdat4$acres<-round(hDSFWdat4$value.x*hrs*wr, 0)
acres2plot<-quantile(as.numeric(as.character(hDSFWdat4$acres)), probs=c(seq(.01,1.0,0.01)))
acres2plot2<-round(data.frame(acres2plot),0)
acres2plot2$perc<-seq(100,1,-1)#100-row.names(acres2plot2)[]  

midAcre<- subset(acres2plot2, perc==50)
upperAcres<- subset(acres2plot2, perc==25)
maxAcres <- subset(acres2plot2, perc==1)

dygraph(acres2plot2, main=paste("Probability of harvesting acreage for ", state0, sep="")) %>%
  dyAxis("y", label = "Probability") %>%
  dyAxis("x", label = "Acres") %>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>% #  dySeries("perc", label = "percentatage") %>%
  dySeries(strokeWidth = 3) %>%
  dySeries("perc", label = "percentatage") %>%
  dyLegend(show = "auto", width = 380) %>%
  dyShading(from = midAcre[1], to = upperAcres[1], color = "yellow") %>%
  dyShading(from = upperAcres[1], to = maxAcres[1], color = "red") 
