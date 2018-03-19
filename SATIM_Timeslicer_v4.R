# SANEDI DSM project 
# This is a script to read in timeslice data and produce load profiles matching the timeslices for SATIM
# writes a csv file with 'loadDataSmry' which is the aggregated timeslice data. 

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
  
  #Update: only some of this function is used now. Havent cleaned out the useless bits yet. 
  
  if(tstab == 's'){#srch is doy => return the season id for this
    #is srch in season ranges
    #loop thru each season in user list and find if this doy is in its start and end days
      for(i in seq(1,length(ts_table_seasons$SeasonName))){
        
        season = ts_table_seasons[i,'SeasonName']
        #in case userdefined ts has duplicate row seasonname entries (like 8ts format)
        #then generate sequence with both defined start and end days
        
        startday = ts_table_seasons[ts_table_seasons$SeasonName == season,'Start_doy']
        endday = ts_table_seasons[ts_table_seasons$SeasonName ==season ,'Stop_doy']
        
        myseq = 0
          for(j in seq(1,length(startday))){
            myseq = c(myseq,seq(startday[j],endday[j]))  
          }
        
        myseq = myseq[myseq!=0]#get rid of zero
        
          if(srch %in%myseq){
            ans2 = ts_table_seasons[i,'SeasonID']
          }
        
      }
      
  }
  
  if(tstab == 'd'){
    #get the daytype ID and return it. 
    #srch is 'day' which is day of week (0 to 6)
    
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
    
    ans2 =  ts_table_seasons[(ts_table_seasons$SeasonID == srch),( names(ts_table_seasons)%in%'SeasonName')][1]
  }
  if(tstab == 'nd'){
    #return daytype name
    ans2 =  ts_table[(ts_table$DayID == srch),( names(ts_table)%in%'DayTypeName')][1]
    
  }
  return(ans2)
}


#         need data in format:

# Date                 | DataVar
# datestamp (anyformat)| value (integer or double)

nameThisWorkOfYours= 'SolarProfile8ts'
scalefactor =1 # use this to convert MWh to GWh etc.
AdjustForPeak = 0 # 1 = yes, 0 = no. Do you want to adjust for the peak getting averaged over during calcs? ie. insert the peak into the Timeslices (loadDataSmry) at the end. 
dataiswideformat = 0 #is data long or wide format. Not properly incorporated. Porbably going to take this out and just have all data in long format. 


#filepath locations
  datafilepath = "C:/Users/01425453/Desktop/SolarProfiles/RE_dispatch_data.csv" # location of the data. 
  TSfilepath = "C:/Users/01425453/Google Drive/SATIM/R codes and outputs/Timeslicer SANEDI/timeslice_data.xlsx"# location of user defined Timeslices
  saveCSVFilePath = "C:/Users/01425453/Google Drive/SATIM/R codes and outputs/Timeslicer SANEDI" #where you want to save the sliced up data. 
  
#read in dataset and user timeslice designation from excel    
  loadData = read.csv(datafilepath,stringsAsFactors = F) 
  ts_table_seasons = readWorksheetFromFile(TSfilepath,sheet = 'Seasons')
  ts_table_days = readWorksheetFromFile(TSfilepath,sheet = 'Days')

#Make any modifications to the data here
  loadData[,c(3,4,5,6,7,8)] = sapply(loadData[,c(3,4,5,6,7,8)],as.numeric)
  loadData[is.na(loadData)] = 0
  loadData$DATE = as.Date(loadData$DATE)
  
  loadData = loadData %>% mutate(PV_cf = PV_kWh/PV_kW,wind_cf = Wind_kWh/Wind_kW)
  
  
  
#convert ts_table_days into long format: expanding over wday
  ts_table_days2 = data.frame()
  for(i in seq(1,length(ts_table_days$SeasonName))){
    #loop over each row in ts_table_days
    #for each row, split all 'wday' into a vector
    #make a column in new matrix with duplicate 'Season' in first column but with all wday in second column and assing 
    wday = as.character(strsplit(ts_table_days[i,'wday'],',')[[1]])
    SeasonName= sprintf(ts_table_days[i,'SeasonName'],seq(1,length(wday)))
    
    tmp = data.frame(SeasonName,wday) #create new dataframe
    tmp$DayID = ts_table_days[i,'DayID']#add the dayID
    tmp$DayTypeName = ts_table_days[i,'DayTypeName'] #add the daytypename detail
    tmp = cbind(tmp,ts_table_days[i,-c(1,2,3,4)]) #add all the hours for this row
  
    ts_table_days2 = rbind(ts_table_days2,tmp)# add this to the new dataframe
    tmp = 0
  }
  ts_table_days = ts_table_days2
  x = ts_table_seasons[,names(ts_table_seasons)%in%c('SeasonName','SeasonID')]
  x = x[!duplicated(x),]
  ts_table_days = merge(ts_table_days,x,by = 'SeasonName')
  x = melt(ts_table_days,id.vars = c('SeasonName','SeasonID','wday','DayID','DayTypeName'))
  names(x)[names(x)%in%c('variable','value')] = c('hour','BlockID')
  blockids = x
  blockids$hour = as.numeric(gsub('X','',blockids$hour))#remove those silly X's
  
#add day of week and season ID's to the dataset
  #loadData$Date = loadData$Date + 60 #add one minute to shift all dates to match the excel TIMESLICES dates 
  #loadData$Date = as.Date(loadData$Date,"%y/%m/%d %H:%M")#convert the date data to date format in R
  
  #loadData$Date = as.POSIXlt(loadData$Date,format = "%y%m%d %H:%M")#convert the date data to date format in R
  
  #add columns for the day/month/year
    loadData$year = year(loadData$Date)
    loadData = loadData[loadData$year == loadData[1,'year'],] #get only the first year's worth of the data. Aggregating over diff years can be tricky
                                                              #(day of year 10 in year1 may not be same weekday as day of year 10 in year 2), and isnt 
                                                              #incorporated here. 
    loadData$month = month(loadData$Date)
    loadData$wday = wday(loadData$Date)-1# 1 IS monday
    loadData$doy = yday(loadData$Date)
    loadData$hour = hour(loadData$Date)
    loadData$min = minute(loadData$Date)
    
    loadData = loadData[,-c(1)] #drop date column

    #IF number of years > 1 then average to one whole year    
    if(length(unique(loadData$year))>1){
      
      loadData = loadData %>% group_by(month,wday,doy,hour,min)%>%
        summarise(DataVar = mean(DataVar))}
    #if detail is lower res than hour (ie 15mins) average over the hour
    if(length(unique(loadData$min))>1){
      loadData = loadData %>% group_by(month,wday,doy,hour)%>%
        summarise(DataVar = mean(DataVar))
    } 

#APPLYING USER DEFINED TIMESLICE PROFILES
  
  #get and add the corresponding Season and daytype IDs for months and days respectively:
  
    #add season detail
    loadData$SeasonID = mapply(getTSID,loadData$doy,'s') #this is removed, this is if we specify months. 
    loadData$SeasonName = mapply(getTSID,loadData$SeasonID,'ns')
    
    #add day detail
    loadData = merge(loadData,blockids,by.x = c('SeasonID','wday','hour'),by.y = c('SeasonID','wday','hour'),all.x = T)
    names(loadData)[names(loadData)=='SeasonName.x'] = 'SeasonName' #seasonname.x and y are from merge. removing the one. 
    
    #make full seasonname - joining the season and daytype names
    loadData$SeasonName = paste(loadData$SeasonName,loadData$DayTypeName,sep = '-')
    loadData = loadData[,!(names(loadData)%in% c('DayTypeName','SeasonName.y'))] #remove these names
  
  #if data is in wide format
    if(dataiswideformat){
    #create long format of loadData
      loadData.long = melt(loadData,id.vars = c('doy','SeasonID','DayID','month','day','SeasonName','hour'))
      names(loadData.long)[names(loadData.long) == 'value'] = as.character(loadData.long[1,'variable']) #rename variable and value
      #drop 'variable' name
      loadData.long = loadData.long[,!(names(loadData.long)%in%c('variable'))]
    }
    if(AdjustForPeak){
      #ADD THE MAXIMUM BLOCK INDEX. this is so we dont lose the peak
        loadData.long$max = (loadData.long$DataVar == max(loadData.long$DataVar))
    }

#Join the ID's together for full Timeslice ID
  loadDataFull = loadData
  loadDataFull$SDB = paste(paste(paste('S',loadDataFull$SeasonID,sep= ''),paste('D',loadDataFull$DayID,sep =''),sep =''),paste('B',loadDataFull$BlockID,sep = ''),sep = '')

  #count the number of hours associated with each SDB (to get shares for later computation):
  loadDataFull = as.data.table(loadDataFull)
  loadDataFull = loadDataFull[,':='(hrcount = .N), by = SDB]

#SUMMARISE DATA INTO  USEFULL DATAFRAMES
  
  #for insightful plotting - plot by 24 hour period for each seasonName
    loadDataHR <- loadDataFull %>% 
        group_by(SeasonID,DayID,BlockID,SeasonName,hour) %>%
        summarise(DataVar_sum = sum(DataVar)/scalefactor,DataVar_avg = mean(DataVar))

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
    
        loadDataSmry$DataVarShare = loadDataSmry$DataVar_sum/sum(loadDataSmry[loadDataSmry$SDB != maxSDB,'DataVar_sum']) #calculate shares (of energy)
        loadDataSmry$DataVar_wPeak = loadDataSmry$DataVar_avg
        loadDataSmry[loadDataSmry$SDB == maxSDB,'DataVar_wPeak'] = loadDataFull[loadDataFull$max == TRUE,'DataVar'] #insert the peak into the right place
      
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

#write the CSV file
write.csv(loadDataSmry,paste(saveCSVFilePath,paste(nameThisWorkOfYours,'.csv',sep=''),sep ='/'))
#PLOTTING
     
#plotting load curve for each season+daytype by HOURLY
  plotdat = loadDataHR
  #plot all seasons by hour:
  #NOTE: this is not peak adjusted - its loadDataHR, not loadDataSmry which has the peak adjustment
  profilePlot <- ggplot(plotdat,aes(x = hour,y = DataVar_avg,group = SeasonName,color = SeasonName))+geom_line(size = 1.2)
  
#hourly avg. plotted with block timeslices overlayed
  SDB_hr = loadDataFull %>% group_by(SeasonID,DayID,BlockID,SeasonName)%>% summarise(DataVar_sdb = mean(DataVar))
  course_and_fine_profile = merge(loadDataHR,SDB_hr)
  loadPlot = ggplot(course_and_fine_profile)+geom_line(aes(x= hour,y = DataVar_avg,color = SeasonName))+geom_step(aes(x = hour, y = DataVar_sdb,color = SeasonName),size = 2)+
    facet_grid(~SeasonName)
  ggsave(paste(saveCSVFilePath,paste(nameThisWorkOfYours,'.png',sep=''),sep ='/'),loadPlot,width = 20,height = 15)
  
#plotting load duration curve (incomplete)
  dat = loadDataSmry
  dat = dat[order(dat$DataVar_avg_new,decreasing = TRUE),]
  dat$hr_sum = cumsum(dat$hrcount)
  
  hr = seq(1,8760)
  DataVar = 0
  dat2 = data.frame(hr,DataVar)
  dat2$DataVar = dat[dat2$hr < dat$hr_sum ,'DataVar_avg_new']
  load_duration_curve <- ggplot(dat,aes(x = hr_sum,y = DataVar_avg_new))+geom_line()
  