#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(robotstxt)
library(lubridate)
library(stringr)
library(jsonlite)

# webscraping processes
# website 1

webpage1_1 <- read_html("http://feeds.airnowapi.org/rss/forecast/113.xml")
pitt = webpage1_1 %>%
  html_nodes("body") %>%
  html_text()
todaypitt=str_extract(pitt,"(?<=Today).*?(?=AQI)")
todaypitt=strsplit(todaypitt,"-")
todaypitt=strsplit(todaypitt[[1]][2]," ")
todaypitt=todaypitt[[1]][2]
tomorrowpitt=str_extract(pitt,"(?<=Tomorrow).*?(?=AQI)")
tomorrowpitt=strsplit(tomorrowpitt,"-")
tomorrowpitt=strsplit(tomorrowpitt[[1]][2]," ")
tomorrowpitt=tomorrowpitt[[1]][2]
webpage1_2 <- read_html("http://feeds.airnowapi.org/rss/forecast/352.xml")
LC = webpage1_2 %>%
  html_nodes("body") %>%
  html_text()
todayLC=str_extract(LC,"(?<=Today).*?(?=AQI)")
todayLC=strsplit(todayLC,"-")
todayLC=strsplit(todayLC[[1]][2]," ")
todayLC=todayLC[[1]][2]
tomorrowLC=str_extract(LC,"(?<=Tomorrow).*?(?=AQI)")
tomorrowLC=strsplit(tomorrowLC,"-")
tomorrowLC=strsplit(tomorrowLC[[1]][2]," ")
tomorrowLC=tomorrowLC[[1]][2]


# website 2
webpage2 <- read_html("https://www.ahs.dep.pa.gov/AQPartnersWeb/forecast.aspx?vargroup=sw")
todayforecast <- webpage2 %>%
  html_nodes("body form div div div div div div") %>%
  html_text()
head(todayforecast)
discriptions = todayforecast[1]
todayforecast = todayforecast[2]
todayforecast = strsplit(todayforecast, "\r\n")
todayforecast = todayforecast[[1]][2]
todayforecast = gsub("  ","",todayforecast)

# website 3
# 09,15,21,0,9,15
webpage4 <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.427&lon=-80.0107&lg=english&&FcstType=digital")
date = as.character(Sys.Date())
date = strsplit(date,"-")
date = paste(date[[1]][2],date[[1]][3],sep="/")
p2 <- webpage4 %>%
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p2 = p2[-c(1,402)]
dim(p2) = c(25,32)
p2 = t(p2)
booldate = FALSE
if(p2[1,2]==date){
  booldate = TRUE
}
time = as.numeric(p2[2,2])# this time need to be <=9, which means this code should be run before 9am everyday
todaymorningwind = "--"
todayafternoonwind = "--"
todayeveningwind = "--"
todayovernightwind = "--"
tomorrowmorningwind = "--"
tomorrowafternoonwind = "--"
web4_1 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",9-time+15+9,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
web4_2 = paste("https://forecast.weather.gov/MapClick.php?w0=t&w1=td&w2=wc&w3=sfcwind&w3u=1&w4=sky&w5=pop&w6=rh&w7=rain&w8=thunder&w9=snow&w10=fzg&w11=sleet&w13u=0&w16u=1&w17u=1&AheadHour=",9-time+24+15+9,"&Submit=Submit&FcstType=digital&textField1=40.427&textField2=-80.0107&site=all&unit=0&dd=&bw=",sep="")
webpage4_1=read_html(web4_1)
p4_1 <- webpage4_1 %>%
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p4_1 = p4_1[-c(1,402)]
dim(p4_1) = c(25,32)
p4_1 = t(p4_1)
todaymorningwind = paste(p4_1[7,2],p4_1[6,2],sep=" - ")
todayafternoonwind = paste(p4_1[7,8],p4_1[6,8],sep=" - ")
todayeveningwind = paste(p4_1[7,14],p4_1[6,14],sep=" - ")
todayovernightwind = paste(p4_1[7,17],p4_1[6,17],sep=" - ")
webpage4_2=read_html(web4_2)
p4_2 <- webpage4_2 %>%
  html_nodes(xpath="/html/body/table[6]/tr/td") %>%
  html_text()
p4_2 = p4_2[-c(1,402)]
dim(p4_2) = c(25,32)
p4_2 = t(p4_2)
tomorrowmorningwind = paste(p4_2[7,2],p4_2[6,2],sep=" - ")
tomorrowafternoonwind = paste(p4_2[7,8],p4_2[6,8],sep=" - ")

# Website 4 - Air Dispersion Index
# For this website, the numbers in the first three columns of the first table after the "ADI Early" and "ADI Late" rows will be scraped
# These consist of a number and a description
times<-strptime(Sys.time(),"%Y-%m-%d %H:%M:%S") # Extract the system date and time
y<-as.character(format(times,"%Y")) # Extract and store the year value
m<-as.character(format(times,"%m")) # Extract and store the month value
d<-as.character(format(times,"%d")) # Extract and store the day value
h<-as.character(format(times,"%H")) # Extract and store the hour value

if (as.numeric(h)>=4 & as.numeric(h)<12){
  link4<-"https://forecast.weather.gov/product.php?site=NWS&product=FWF&issuedby=PBZ"
} else {
  link4<-"https://forecast.weather.gov/product.php?site=NWS&issuedby=PBZ&product=FWF&format=CI&version=2&glossary=0"
}

page4<-read_html(link4) # Read in correct website link
table4<-page4 %>% # Select the node containing the data, in  this case, all of the tables on the site will be scraped at once
  html_nodes(".glossaryProduct") %>%
  html_text()
adiearly<-str_extract(table4,'ADI\\searly.{1,}') # Extract ADI Early row from the first table
adilate<-str_extract(table4,'ADI\\slate.{1,}') # Extract ADI Late row from the first table
# ADI Early
adiearlysplit<-str_extract_all(adiearly,'\\d{1,2}\\s.{1,9}') # Split the row into each number-description pair
adiearlytoday<-trimws(adiearlysplit[[1]][1],whitespace=" ") # First pair is the "today" column, take the pair and remove any spaces before and after the text
aetodvalue<-str_extract(adiearlytoday,'\\d{1,2}') # Extracts just the number from the today pair
aetoddesc<-str_extract(adiearlytoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the today pair
adiearlytonight<-trimws(adiearlysplit[[1]][2],whitespace=" ") # Second pair is the "tonight" column, take the pair and remove any spaces before and after the text
aetonvalue<-str_extract(adiearlytonight,'\\d{1,2}') # Extracts just the number from the tonight pair
aetondesc<-str_extract(adiearlytonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the tonight pair
adiearlytomorrow<-trimws(adiearlysplit[[1]][3],whitespace=" ") # Third pair is the "tomorrow" column, take the pair and remove any spaces before and after the text
aetomvalue<-str_extract(adiearlytomorrow,'\\d{1,2}') # Extracts just the number from the tonight pair
aetomdesc<-str_extract(adiearlytomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}')) # Extracts the description after the tonight pair

# ADI Late
adilatesplit<-str_extract_all(adilate,'\\d{1,2}\\s.{1,9}') # Code here is identical to the ADI Early code, process is the same as above
adilatetoday<-trimws(adilatesplit[[1]][1],whitespace=" ")
altodvalue<-str_extract(adilatetoday,'\\d{1,2}')
altoddesc<-str_extract(adilatetoday,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adilatetonight<-trimws(adilatesplit[[1]][2],whitespace=" ")
altonvalue<-str_extract(adilatetonight,'\\d{1,2}')
altondesc<-str_extract(adilatetonight,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))
adilatetomorrow<-trimws(adilatesplit[[1]][3],whitespace=" ")
altomvalue<-str_extract(adilatetomorrow,'\\d{1,2}')
altomdesc<-str_extract(adilatetomorrow,regex('[:alpha:]{1,}[:space:]{1}[:alpha:]{1,}|[:alpha:]{1,}'))

# These if-else statements change the descriptions of the ADIs if they are "Gen Poor" or "Gen Good" to "Generally Poor" or "Generally Good"
if(aetoddesc=="Gen Poor"){
  aetoddesc<-"Generally Poor"
} else if (aetoddesc=="Gen Good"){
  aetoddesc<-"Generally Good"
}
if(altoddesc=="Gen Poor"){
  altoddesc<-"Generally Poor"
} else if (altoddesc=="Gen Good"){
  altoddesc<-"Generally Good"
}

if(aetondesc=="Gen Poor"){
  aetondesc<-"Generally Poor"
} else if (aetondesc=="Gen Good"){
  aetondesc<-"Generally Good"
}

if(altondesc=="Gen Poor"){
  altondesc<-"Generally Poor"
} else if (altondesc=="Gen Good"){
  altondesc<-"Generally Good"
}

if(aetomdesc=="Gen Poor"){
  aetomdesc<-"Generally Poor"
} else if (aetomdesc=="Gen Good"){
  aetomdesc<-"Generally Good"
}

if(altomdesc=="Gen Poor"){
  altomdesc<-"Generally Poor"
} else if (altomdesc=="Gen Good"){
  altomdesc<-"Generally Good"
}

# These lines combine the number and description into what will be shown on the report
todaymorning<-paste(aetoddesc,"-",aetodvalue)
todayafternoon<-paste(altoddesc,"-",altodvalue)
tonightevening<-paste(aetondesc,"-",aetonvalue)
tonightovernight<-paste(altondesc,"-",altonvalue)
tomorrowmorning<-paste(aetomdesc,"-",aetomvalue)
tomorrowafternoon<-paste(altomdesc,"-",altomvalue)

# Website 5
# Data is scaraped here to calculate the Inversion Strength and Inversion Depths for the day
# Website is updated at 7AM every day

if (as.numeric(h)<8){
  yesterday<-strptime(as.Date(Sys.Date())-1,"%Y-%m-%d") # Using yesterday's date if website hasn't updated yet
  y5<-as.character(format(yesterday,"%Y"))
  m5<-as.character(format(yesterday,"%m"))
  d5<-as.character(format(yesterday,"%d")) 
  link5<-paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",y5,"&MONTH=",m5,"&FROM=",d5,"00&TO=",d5,"00&STNM=72520",sep="") # Place the year, month, and day values into the link to get the data for the day
} else {
  link5<-paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=",y,"&MONTH=",m,"&FROM=",d,"00&TO=",d,"00&STNM=72520",sep="")
}

page5<-read_html(link5) # Read link
table5<-page5 %>% # Extract the node containing the data, which is the whole table in this case
  html_nodes("pre") %>%
  html_text()

fivestrength<-function(x){ # Create a description value for how strong the Surface Inversion Strength is based on its value
  if (x==0){
    return("None")
  } else if (x>0 & x<1){
    return("Slight")
  } else if (x>=1 & x<3){
    return("Weak")
  } else if (x>=3 & x<5){
    return("Moderate")
  } else return("Strong")
}

if (is_empty(table5)==FALSE){
  fiveextract<-str_extract_all(table5,'\\n.{22}') # Extracts and splits each row of data
  fivenum<-fiveextract[[1]][-c(1:4)] # Removes the first four rows
  digits<-trimws(substr(fivenum,3,22),which=c("left")) # Removes the new line characters at the start and any whitespace characters
  digitssplit<-str_extract_all(digits,'.{1}\\d{1,}.{1}\\d|\\d{1,}.{1}\\d') # Splits the numbers in each row
  pressure<-lapply(digitssplit,`[[`,1) # Label first column values as "pressure"
  height<-lapply(digitssplit,`[[`,2) # Label second column values as "height"
  digitssplit[[1]][3]<-"" # Add an empty string for the third entry in third column since it's not present
  temperature<-lapply(digitssplit,`[[`,3) # Label third column values as "temperature" 
  five<-cbind(as.numeric(pressure),as.numeric(height),as.numeric(temperature)) # Combine these variables into a new data frame
  colnames(five)<-c("Pressure (hPa)","Height (m)","Temperature (C)") # Add names to each column with their units of measurement
  five<-five[-1,] # Remove first row, since it has no temperature value
  
  # Calculation for Surface Inversion Strength and Inversion Depth
  tempdiff<-diff(five[,3]) # Create a differenced list of the temperature column, each entry subtracted from the next
  surfaceinversion<-five[which(tempdiff<0),3][1]-five[,3][1] # If the temperature increases as height increases, take the peak temperature and subtract it from the surface temperature to get Surface Inversion Strength
  inversiondepth<-five[which(tempdiff<0),2][1]-five[,2][1] # Take the height of the peak temperature and subtract the surface height (359 m) to get Inversion Depth
  
  ## To Calculate Break Time
  breaktemp<-(((inversiondepth/100)+five[which(tempdiff<0),3][1])*9/5)+32 # Take this number and match it to the weather forecast. The time of day when this temperature is reached is the break time.
  
  ## Calculations for Surface Inversion Breaks
  temp5 <- paste(round(surfaceinversion,1),"°C")
  depth5 <- paste((inversiondepth),"m")
  time5 <- "9am" 
  scale5<- fivestrength(surfaceinversion)
  
  # Determining if there are any upper inversions
  sentence<-five[which(five[,2]<1000),] # Take all temperature values below 1000 m
  tempdiffunder1k<-diff(sentence[,3]) # Make differenced list
  e<-which(tempdiffunder1k<0) # Find any differences less than 0
  f<-diff(e) # Make second differenced list
  g<-which(f>1) # Find any values greater than 1
  upperinversion<-function(){ # Function to detect any inversion that is not a surface inversion and print "Yes" or "No"
    if (is.na(tempdiffunder1k[e[g[1]+1]])){
      return("No upper inversion starting below ~1000 m is reported")
    } else return("Yes, an upper inversion starting below ~1000 m is reported")
}

inversion5 <- upperinversion()
} else {
  surfaceinversion<-"--"
  inversiondepth<-"--"
  inversion5<-"--"
  temp5<-paste("--","°C")
  depth5<-paste("--","m")
  time5<-"--"
  scale5<-"--"
}
# Website 6
# Used to calculate the Inversion Strength for the next day
# Website is updated at 7AM every day 

if (as.numeric(h)<8){
  link6<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&start_year=",y,"&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d),"&start_hour=12&start_min=0&n_hrs=1&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&startSecs=",as.numeric(as.POSIXct(Sys.Date()))+30000,"&endSecs=",as.numeric(as.POSIXct(Sys.Date()))+33600,sep="") # Link with the system's year, month, day, and epoch times
} else {
  link6<-paste("https://rucsoundings.noaa.gov/get_soundings.cgi?data_source=GFS&start_year=",y,"&start_month_name=",month.abb[as.numeric(m)],"&start_mday=",as.numeric(d)+1,"&start_hour=12&start_min=0&n_hrs=1&fcst_len=shortest&airport=PIT&text=Ascii%20text%20%28GSL%20format%29&hydrometeors=false&startSecs=",as.numeric(as.POSIXct(Sys.Date()+1))+43200,"&endSecs=",as.numeric(as.POSIXct(Sys.Date()+1))+46800,sep="")
}

page6<-read_html(link6) # Read link
table6<-page6 %>% # Select nodes with the needed data which is the full table
  html_nodes("p") %>%
  html_text()

sixextract<-str_extract_all(table6,'\\d.{1,}') # Remove all new line characters
sixrows<-sixextract[[1]][-c(1:6)] # Remove first 6 rows since important data starts on row 7
splitsix<-str_extract_all(sixrows,'-\\d{1,}|\\d{1,}') # Separate all numbers into their own entries
type<-lapply(splitsix,`[[`,1) # Label first column as "Type"
pressure<-lapply(splitsix,`[[`,2) # Label second column as "Pressure"
height<-lapply(splitsix,`[[`,3) # Label third column as "Height"
temp<-lapply(splitsix,`[[`,4) # Label fourth column as "temp"
dewpoint<-lapply(splitsix,`[[`,5) # Label fifth column as "Dewpoint"
winddirection<-lapply(splitsix,`[[`,6) # Label sixth column as "Wind Direction"
windspeed<-lapply(splitsix,`[[`,7) # Label seventh column as "Wind Speed"

# Create data frame with all the columns combined. Some columns need their data modified by dividing by 10
six<-as.data.frame(cbind(type,as.numeric(pressure)/10,height,as.numeric(temp)/10,as.numeric(dewpoint)/10,winddirection,windspeed)) 

colnames(six)<-c("Type","Pressure (mb)","Height (m)","Temperature (C)","Dew Point (C)","Wind Direction (Degrees)","Wind Speed (Knots)") # Give each column their names and units
tempdiffsix<-diff(unlist(six[,4])) # Create a differenced list of the temperature column, each entry subtracted from the next
surfaceinversion2<-six[which(tempdiffsix<0),4][[1]]-unlist(six[,4])[1] # If the temperature increases as height increases, take the peak temperature and subtract it from the surface temperature to get Surface Inversion Strength
inversiondepth2<-as.numeric(six[which(tempdiffsix<0),3][[1]])-as.numeric(unlist(six[,3])[1]) # Take the height of the peak temperature and subtract the surface height (359 m) to get Inversion Depth

sixout<-function(x){ # Create a description value for how strong the Surface Inversion Strength is based on its value
  if (x==0){
    return("None")
  } else if (x>0 & x<1){
    return("Slight")
  } else if (x>=1 & x<3){
    return("Weak")
  } else if (x>=3 & x<5){
    return("Moderate")
  } else return("Strong")
}

# current date for report
currentDate <- Sys.Date()
title <- paste("Air Quality Forecast and Dispersion Outlook of Allegheny County, Pennsylvania for", as.character(currentDate))

# AQI
aqi <- data.frame(
    "Forecast Period"= c("Today","Tomorrow"),
    "Pittsburgh Area"=c(todaypitt,tomorrowpitt),
    "Liberty-Clairton Area"=c(todayLC,tomorrowLC))

aqi_forecast <- todayforecast[1]

# ADI
adi <- data.frame(
    "Forecast Period" = c("Today Morning","Today Afternoon", "Tonight Evening", 
                         "Tonight Overnight", "Tomorrow Morning", "Tomorrow Afternoon"),
    "Atmospheric Dispersion Index" = c(todaymorning,todayafternoon,tonightevening,tonightovernight,tomorrowmorning,tomorrowafternoon),
    "Surface Inversion Strength" = c(fivestrength(surfaceinversion),"--","--","--",sixout(surfaceinversion2),"--"),
    "Wind(dir,mph)"=c(todaymorningwind,todayafternoonwind,todayeveningwind,todayovernightwind,tomorrowmorningwind,tomorrowafternoonwind))






# Define server for data output
server <- function(input, output) {
    
    # define thw title
    output$title <- renderText(title)
    
    # define the data table for AQI
    output$aqi_table <- renderTable(aqi)
    output$aqi_text <- renderText(aqi_forecast)
    
    # define the data table for ADI
    output$adi_table <- renderTable(adi)
    
    # define the inversion report
    output$inversion_line1 <- renderText({
      HTML(paste0("</b>","This morning’s surface inversion of ","<b>", temp5,
                  "</b>"," with a depth of ","<b>",depth5,"</b>",
                  " is estimated to break at ","<b>",time5,"</b>","."))
    })
    
    output$inversion_line2 <- renderText({
      HTML(paste0("</b>","This surface inversion can be characterized as: ",
                  "<b>", scale5,"</b>","."))
    })
      
      output$inversion_line3 <- renderText({
        HTML(inversion5)      
    })
    
}

# Define UI for application
ui <- fluidPage(

    # title
    fluidRow(
        column(8, h2(textOutput('title')), offset = 2)
),
    
    # air quality forecast
    p(h4(div("Air Quality Forecast:",style = "color:blue")), 
    h5("This is the daily forecasted Air Quality Index (AQI) for each area 
    provided by the PA Department of Environmental Protection. The AQI is based 
    on PM2.5 or Ozone, whichever is forecasted to be higher.")),
    
    fluidRow(
        column(6, wellPanel(tableOutput('aqi_table'))),
        column(6, p(div("Today's Forecast:",style = "color:blue"), textOutput('aqi_text')))
),
    
    fluidRow(
        column(6, "See Page 2 for the Air Quality Index guide"),
        column(6, a("Data provided by the PA Department of Environmental Protection", 
                    href="https://www.ahs.dep.pa.gov/AQPartnersWeb/forecast_home.aspx"))
),

    br(),
    br(),


    # Air Dispersion 36-Hour Forecast:
    p(h4(div("ACHD Air Dispersion 36-Hour Forecast:",style = "color:blue"), 
    h5("This is the dispersion forecast for Allegheny County starting from this 
    morning through tomorrow afternoon. The atmospheric dispersion index is a 
    rating of the atmosphere’s ability to transport pollution away from its 
    source and is based on emissions and weather. Better atmospheric dispersion 
    can improve air quality."))),

    fluidRow(
        column(8, wellPanel(tableOutput('adi_table')),offset = 2),
        ),    

    p("Data provided by the National Weather Service (NWS) ",
      a("Fire Weather Planning Forecast", 
        href = "https://forecast.weather.gov/product.php?site=NWS&product=FWF&issuedby=PBZ"),
      "and",
      a("PIT NWS Products", 
        href = "http://weather.uwyo.edu/upperair/sounding.html")),

    br(),
    br(),

# guide for ADI and AQI
    div(img(src = "guide.png", height = 400, width = 800), style="text-align: center;"),

    br(),
    br(),

# air quality forecast
    p(h4(div("ACHD Surface Temperature Inversion Report:",style = "color:blue")), 
    h5("This is the 7 AM surface-based temperature inversion report for Allegheny County.")),

    uiOutput(outputId = "inversion_line1"),
    uiOutput(outputId = "inversion_line2"),
    uiOutput(outputId = "inversion_line3"),

  br(),
  br(),
  
  # Surface Temperature Inversion Report meaning
  div(img(src = "report_mean.png", height = 500, width = 1000), style="text-align: center;"),

    
)


# Preview the UI in the console
shinyApp(ui, server)









