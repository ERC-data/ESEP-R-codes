# SANEDI DSM project 
# This is a script to read in timeslice data and produce load profiles
# 18 Jan 2017 Bryce: This script works, the plotting however does not reflect the adjusted max_peak demand. The reason is the adjustment happens 
# is the adjustment happens after the adjustment. 

library(XLConnect)
library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

getTSID = function(srch,tstab){
  # essentially takes the date/day of year (doy) etc. and returns the id depending on if you want dayid,seasonid,blockid etc. 
  
  #srch is the input number to get the associated season or daytype id. for daytypes this would be number of the day of week, for seasontype 
    #this would be the day number of the year ie, 1 is 1st day of year, 365 is the last. 
  
  #tstab is 's', 'd' or 'nd' etc. for season day or datename
  
  #returned is the seasontype id, or daytype id for the associated tstab = 'm' or 'd'. 
  
  if(tstab == 's'){#srch is doy => return the season id for this
    firstT = (ts_table$doy >=srch)
    lastT = (ts_table$doy <=srch)
    ind1 = sum(lastT) #pos of last true
    ind2 = sum(!(firstT))+1 #pos of first true
    
    ans2 = ts_table[c(ind1,ind2),1][1]
    }
  if(tstab == 'd'){
    #get the daytype ID and return it. 
    mycols = data.frame() #empty
    if((tstab %in% c('s','d','ns','nd') ) == 0){
      print("!!! not the correct input!!!")
      return(999)}
    
    mycols = ts_table$wkday
    anstab = ts_table$DayID
    
    N = length(mycols)
    ans = vector()
    for (i in 1:N){
      
      #split this element
      x = mycols[i]
      
      y = as.numeric(unlist(regmatches(x,gregexpr('\\(?[0-9]+',x)))) #turn into vector of numbers
      
      if(sum(srch == y) != 0){# srch is in this element of moncols.
        
        # return the index i when found
        ans = append(ans,TRUE)
        
      }else{
        ans = append(ans,FALSE)
      }
      
    }#end forloop
    
    #get the ID we are looking for: 
    ans2 = anstab[ans][1] #get first occurance 
  }
  if(tstab == 'ns'){
    #return season name
    
    ans2 =  ts_table[(ts_table$SeasonID == srch),( names(ts_table)%in%'SeasonName')][1]
  }
  if(tstab == 'nd'){
    #return daytype name
    ans2 =  ts_table[(ts_table$DayID == srch),( names(ts_table)%in%'DayTypeName')][1]
    
  }
  return(ans2)
}


#         need data in format

# Date                 | DataVar
# datestamp (anyformat)| value

scalefactor =1 # use this to convert MWh to GWh etc.
AdjustForPeak = 0 # 1 = yes, 0 = no. Do you want to adjust for the peak getting averaged over during calcs? ie. insert the peak into the Timeslices

#filepath locations
  datafilepath = "C:/Users/01425453/Desktop/WindProfiles/WindProfileData.csv" # location of the data. 
  TSfilepath = "C:/Users/01425453/Desktop/WindProfiles/timeslice_data.xlsx"# location of user defined Timeslices

#read in dataset and user timeslice designation from excel    
  loadData = read.csv(datafilepath,stringsAsFactors = F) 
  ts_table = readWorksheetFromFile(TSfilepath,sheet = 'TS')

#add day of week and season ID's to the dataset
  #loadData$Date = loadData$Date + 60 #add one minute to shift all dates to match the excel TIMESLICES dates 
  #loadData$Date = as.Date(loadData$Date,"%y/%m/%d %H:%M")#convert the date data to date format in R
  
  #loadData$Date = as.POSIXlt(loadData$Date,format = "%y%m%d %H:%M")#convert the date data to date format in R
  
  #add columns for the day/month/year
    loadData$year = year(loadData$Date)
    loadData = loadData[loadData$year == loadData[1,'year'],] #get only one year's worth of the data. it complicates dates summerising 'coz of month 1 day 1 in 2009<> month 1 day 1 in 2010 
    loadData$month = month(loadData$Date)
    loadData$day = wday(loadData$Date)-1# 1 IS monday
    loadData$doy = yday(loadData$Date)
    loadData$hour = hour(loadData$Date)
    loadData$min = minute(loadData$Date)
    
    loadData = loadData[,-c(1)] #drop date column

    #IF number of years > 1 then average to one whole year    
    if(length(unique(loadData$year))>1){
      
      loadData = loadData %>% group_by(month,day,doy,hour,min)%>%
        summarise(DataVar = mean(DataVar))}
    #if detail is lower res than hour (ie 15mins) average over the hour
    if(length(unique(loadData$min))>1){
      loadData = loadData %>% group_by(month,day,doy,hour)%>%
        summarise(DataVar = mean(DataVar))
    } 
    
#Get the ts table into long format
  blockids.long = melt(ts_table[,!(names(ts_table)%in% c('SeasonName','DayTypeName'))],id.vars = c('SeasonID','DayID','doy','wkday'))
  names(blockids.long)[[5]] = 'hour'
  names(blockids.long)[[6]] = 'BlockID'

#APPLYING USER DEFINED TIMESLICE PROFILES
  
  #get and add the corresponding Season and daytype IDs for months and days respectively:
  #automatic by month:
  
    loadData$SeasonID = mapply(getTSID,loadData$doy,'s') #this is removed, this is if we specify months. 
    loadData$DayID = mapply(getTSID,loadData$day,'d')
    loadData$SeasonName = mapply(getTSID,loadData$SeasonID,'ns')
    loadData$DayTypeName = mapply(getTSID,loadData$DayID,'nd')
    loadData$SeasonName = paste(loadData$SeasonName,loadData$DayTypeName,sep = '-')
    
    loadData = loadData[,!(names(loadData)%in% 'DayTypeName')] #remove DayTypeName column
  
  #if in wide format
    #create long format of loadData
      loadData.long = melt(loadData,id.vars = c('doy','SeasonID','DayID','month','day','SeasonName','hour'))
      names(loadData.long)[names(loadData.long) == 'value'] = as.character(loadData.long[1,'variable']) #rename variable and value
      #drop 'variable' name
      loadData.long = loadData.long[,!(names(loadData.long)%in%c('variable'))]

if(AdjustForPeak){
  #ADD THE MAXIMUM BLOCK INDEX. this is so we dont lose the peak
    loadData.long$max = (loadData.long$DataVar == max(loadData.long$DataVar))
}

#ADD THE BLOCK ID'S TO THE DATA by season day and hour:
  blockids = blockids.long[,(names(blockids.long) %in% c('SeasonID','DayID','hour','BlockID'))]
  blockids$hour = as.numeric(gsub('X','',blockids$hour))
  
  loadDataFull = merge(loadData.long,blockids, by = c('SeasonID','DayID','hour'))
  
  #Join the ID's together:
  loadDataFull$SDB = paste(paste(paste('S',loadDataFull$SeasonID,sep= ''),paste('D',loadDataFull$DayID,sep =''),sep =''),paste('B',loadDataFull$BlockID,sep = ''),sep = '')

  #count the number of hours associated with each SDB (to get shares for later computation):
  loadDataFull = as.data.table(loadDataFull)
  loadDataFull = loadDataFull[,':='(hrcount = .N), by = SDB]

#SUMMARISE DATA INTO  USEFULL DATAFRAMES
  
  #for insightful plotting - plot by 24 hour period for each seasonName
    loadDataHR <- loadDataFull %>% 
        group_by(SeasonID,DayID,BlockID,SeasonName,hour) %>%
        summarise(DataVar_sum = sum(DataVar)/1000,DataVar_avg = mean(DataVar))

  #SUMMERISE FOR MODEL INPUT
     
    
    # summerise and reduce data into dimensions of TS user defined eg: S1D2B3 
    loadDataSmry<- loadDataFull %>% 
      group_by(SDB,SeasonName,hrcount) %>%
      summarise(DataVar_sum = sum(DataVar)/scalefactor,DataVar_avg = mean(DataVar))
    
      if(abs(sum(loadDataSmry$hrcount)-8760)>5){
        print('!!!!NUMBER OF HOURS IN YEAR IS NOT CLOSE TO 8760!!!!!')
      }
      if(AdjustForPeak){
      #Adjust the profile to include true peak
        print('ADJUSTING FOR PEAK')
        loadDataFull = as.data.frame(loadDataFull)
        loadDataSmry = as.data.frame(loadDataSmry)
        maxSDB = loadDataFull[loadDataFull$max == TRUE,(names(loadDataFull)%in% c('SDB'))] #the SDB ID associated with peak
    
        loadDataSmry$DataVarShare = loadDataSmry$DataVar_sum/sum(loadDataSmry[loadDataSmry$SDB != maxSDB,'DataVar_sum'])
        
        loadDataSmry$DataVar_wPeak = loadDataSmry$DataVar_avg
        #insert the peak into the right place
        loadDataSmry[loadDataSmry$SDB == maxSDB,'DataVar_wPeak'] = loadDataFull[loadDataFull$max == TRUE,'DataVar']
      
      #calculate the new GWh after adjusting for peak
      loadDataSmry$DataVar_new = loadDataSmry$DataVar_wPeak*loadDataSmry$hrcount/scalefactor
      
      #with new peak, redistribute energy/quantity to other TS's to balance the year to original data
      total_to_replace = loadDataSmry[loadDataSmry$SDB == maxSDB,'DataVar_new'] - loadDataSmry[loadDataSmry$SDB == maxSDB,'DataVar_sum']
      
      loadDataSmry[(loadDataSmry$SDB != maxSDB),'DataVar_new'] = loadDataSmry[(loadDataSmry$SDB != maxSDB),'DataVar_new'] - 
        total_to_replace*(loadDataSmry[(loadDataSmry$SDB != maxSDB),'DataVarShare'])
      
      loadDataSmry$DataVar_avg_new = (loadDataSmry$DataVar_new/loadDataSmry$hrcount)*scalefactor
      }else{
        #if dont adjust, we need to have the same column name
        loadDataSmry$DataVar_avg_new = loadDataSmry$DataVar_avg
      }
      
#PLOTTING
      
#plotting load curve for each season+daytype by HOURLY
  plotdat = loadDataHR
  #plot all seasons by hour:
  #NOTE: this is not peak adjusted - its loadDataHR, not loadDataSmry which has the peak adjustment
  load_profile_plot <- ggplot(plotdat,aes(x = hour,y = DataVar_avg,group = SeasonName,color = SeasonName))+geom_line(size = 1.2)

#plotting load duration curve (incomplete)
  dat = loadDataSmry
  dat = dat[order(dat$MW_avg_new,decreasing = TRUE),]
  dat$hr_sum = cumsum(dat$hrcount)
  
  hr = seq(1,8760)
  MW = 0
  dat2 = data.frame(hr,MW)
  dat2$MW = dat[dat2$hr < dat$hr_sum ,'MW_avg_new']
  load_duration_curve <- ggplot(dat,aes(x = hr_sum,y = MW_avg_new))+geom_line()
  