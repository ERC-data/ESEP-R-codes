# 9 june 2016
# Bryce Mccall
# SANEDI DSM project 
# This is a script to read in timeslice data and produce load profiles

# 18 Jan 2017 Bryce: This script works, the plotting however does not reflect the adjusted max_peak demand. The reason is the adjustment happens 
# is the adjustment happens after the adjustment. 

library(XLConnect)
library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)

getTSID = function(srch,tstab){
  # essentially takes the date/day of year (doy) etc. and returns the id depending on if you want dayid,seasonid,blockid etc. 
  
  #srch is the input number to get the associated season or daytype id. for daytypes this would be number of the day of week, for seasontype 
  #this would be the day number of the year ie, 1 is 1st day of year, 365 is the last. 
  
  #tstab is 
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

#read in dataset and user ts designation from excel
filepath = 'C:/Users/01425453/Google Drive/SATIM/R codes and outputs/Timeslicer SANEDI/timeslice_data.xlsx'

loadData = readWorksheetFromFile(filepath, sheet ='data')
ts_table = readWorksheetFromFile(filepath,sheet = 'TS')

#add day of week and season ID's to the dataset

  #loadData$Date = as.POSIXlt(loadData$Date,format = "%y%m%d") #convert the date data to date format in R
  
  ####
  loadData$Date = loadData$Date + 60 #add one minute to shift all dates to match the excel TIMESLICES dates 
  ####
  
  loadData$Date = as.POSIXlt(loadData$Date,format = "%y%m%d")#convert the date data to date format in R
  #add columns for the day/month/year
  loadData$month = loadData$Date$mon +1 
  loadData$day = loadData$Date$w # 1 IS monday
  loadData$doy = loadData$Date$yday +1
  loadData = loadData[,-1] #drop date column
  
#Get the ts table into long format
  blockids.long = melt(ts_table[,!(names(ts_table)%in% c('SeasonName','DayTypeName'))],id.vars = c('SeasonID','DayID','doy','wkday'))
  names(blockids.long)[[5]] = 'hr'
  names(blockids.long)[[6]] = 'BlockID'

#get and add the corresponding Season and daytype IDs for months and days respectively:
  #automatic by month:
  loadData$SeasonID = mapply(getTSID,loadData$doy,'s') #this is removed, this is if we specify months. 
  loadData$DayID = mapply(getTSID,loadData$day,'d')
  loadData$SeasonName = mapply(getTSID,loadData$SeasonID,'ns')
  loadData$DayTypeName = mapply(getTSID,loadData$DayID,'nd')
  loadData$SeasonName = paste(loadData$SeasonName,loadData$DayTypeName,sep = '-')
  loadData = loadData[,!(names(loadData)%in% 'DayTypeName')]
  #create long format of loadData
  loadData.long = melt(loadData,id.vars = c('doy','SeasonID','DayID','month','day','SeasonName'))
  names(loadData.long)[c(length(names(loadData.long))-1,length(names(loadData.long)))] = c('hr','MWh')

#ADD THE MAXIMUM BLOCK INDEX
  loadData.long$max = (loadData.long$MWh == max(loadData.long$MWh))
#remove 'x' from the hr column
  loadData.long$hr = as.numeric(gsub('X','',loadData.long$hr))
  
#ADD THE BLOCK ID'S TO THE DATA by season day and hour:
  blockids = blockids.long[,(names(blockids.long) %in% c('SeasonID','DayID','hr','BlockID'))]
  blockids$hr = as.numeric(gsub('X','',blockids$hr))
  
  loadDataFull = merge(loadData.long,blockids, by = c('SeasonID','DayID','hr'))
  
  #Join the ID's together:
  loadDataFull$SDB = paste(paste(paste('S',loadDataFull$SeasonID,sep= ''),paste('D',loadDataFull$DayID,sep =''),sep =''),paste('B',loadDataFull$BlockID,sep = ''),sep = '')

  #count the number of hours associated with each SDB (to get shares for later computation):
  loadDataFull = as.data.table(loadDataFull)
  loadDataFull = loadDataFull[,':='(hrcount = .N), by = SDB]

#SUMMARISE DATA INTO  USEFULL DATAFRAMES
  
  #for insightful plotting later
  loadDataHR <- loadDataFull %>% 
      group_by(SeasonID,DayID,BlockID,SeasonName,hr) %>%
      summarise(GWh_sum = sum(MWh)/1000,MW_avg = mean(MWh))

  #model input 
  loadDataSmry<- loadDataFull %>% 
    group_by(SDB,SeasonName,hrcount) %>%
    summarise(GWh_sum = sum(MWh)/1000,MW_avg = mean(MWh))
    
    #Adjust the profile to include true peak
    loadDataFull = as.data.frame(loadDataFull)
    loadDataSmry = as.data.frame(loadDataSmry)
    maxSDB = loadDataFull[loadDataFull$max == TRUE,(names(loadDataFull)%in% c('SDB'))] #the SDB ID associated with peak

    loadDataSmry$Eshare = loadDataSmry$GWh_sum/sum(loadDataSmry[loadDataSmry$SDB != maxSDB,'GWh_sum'])
    
    loadDataSmry$MW_wPeak = loadDataSmry$MW_avg
    #insert the peak into the right place
    loadDataSmry[loadDataSmry$SDB == maxSDB,'MW_wPeak'] = loadDataFull[loadDataFull$max == TRUE,'MWh']
    
    #calculate the new GWh 
    loadDataSmry$GWh_new = loadDataSmry$MW_wPeak*loadDataSmry$hrcount/1000
    totE = loadDataSmry[loadDataSmry$SDB == maxSDB,'GWh_new'] - loadDataSmry[loadDataSmry$SDB == maxSDB,'GWh_sum']#with new peak, redistribute energy to other TS's to balance the year to original data
    
    loadDataSmry[(loadDataSmry$SDB != maxSDB),'GWh_new'] = loadDataSmry[(loadDataSmry$SDB != maxSDB),'GWh_new'] - 
      totE*(loadDataSmry[(loadDataSmry$SDB != maxSDB),'Eshare'])
    loadDataSmry$MW_avg_new = (loadDataSmry$GWh_new/loadDataSmry$hrcount)*1000
  
#plotting load curve for each season+daytype by HOURLY
  plotdat = loadDataHR
  #plot all seasons by hour:
  load_profile_plot <- ggplot(plotdat,aes(x = hr,y = MW_avg,group = SeasonName,color = SeasonName))+geom_line(size = 1.2)

#plotting load duration curve (incomplete)
  dat = loadDataSmry
  dat = dat[order(dat$MW_avg_new,decreasing = TRUE),]
  dat$hr_sum = cumsum(dat$hrcount)
  
  hr = seq(1,8760)
  MW = 0
  dat2 = data.frame(hr,MW)
  dat2$MW = dat[dat2$hr < dat$hr_sum ,'MW_avg_new']
  load_duration_curve <- ggplot(dat,aes(x = hr_sum,y = MW_avg_new))+geom_line()
  