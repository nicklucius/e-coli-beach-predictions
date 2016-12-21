source("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/split_sheets.R")

# Read-in data
df2006 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2006 Lab Results.xls", 2006)
df2007 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2007 Lab Results.xls", 2007)
df2008 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2008 Lab Results.xls", 2008)
df2009 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2009 Lab Results.xls", 2009)
df2010 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2010 Lab Results.xls", 2010)
df2011 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2011 Lab Results.xls", 2011)
df2012 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2012 Lab Results.xls", 2012)
df2013 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2013 Lab Results.xls", 2013)
df2014 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2014 Lab Results.xls", 2014)
df2015 <- split_sheets("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/2015 Lab Results.xlsx", 2015)

# Combine Data
beach_readings <- rbind(df2006, df2007, df2008, df2009, df2010, df2011, df2012, df2013, df2014, df2015)

# Clean Data
## Remove greater or less-than markings
beach_readings$Reading.1 <- factor(gsub(">", "", beach_readings$Reading.1)) # Remove greater-than marks
beach_readings$Reading.2 <- factor(gsub(">", "", beach_readings$Reading.2)) # Remove greater-than marks
beach_readings$Escherichia.coli <- factor(gsub(">", "", beach_readings$Escherichia.coli)) # Remove greater-than marks
beach_readings$Reading.1 <- factor(gsub("<", "", beach_readings$Reading.1)) # Remove less-than marks
beach_readings$Reading.2 <- factor(gsub("<", "", beach_readings$Reading.2)) # Remove less-than marks
beach_readings$Escherichia.coli <- factor(gsub("<", "", beach_readings$Escherichia.coli)) # Remove less-than marks

beach_readings <- unite_(beach_readings, "Full_date", c("Date", "Year"), sep=" ", remove=F)
beach_readings$Full_date <- as.Date(beach_readings$Full_date, format="%B %d %Y") #now dates are sortable
beach_readings$Weekday <- weekdays(beach_readings$Full_date) #add day of week
beach_readings$Month <- format(beach_readings$Full_date,"%B")
beach_readings$Day <- format(beach_readings$Full_date, "%d")

##Remove problematic dates
beach_readings <- beach_readings[-which(beach_readings$Full_date %in% c(as.Date("2006-07-06"), as.Date("2006-07-08"), as.Date("2006-07-09"))),]

##Remove 6 duplicates (12 total observations) -- beaches that have more than one reading on a day
beach_readings=beach_readings[-which(beach_readings$Full_date=="2006-07-19" & beach_readings$Client.ID=="Jarvis/ Fargo"),]
beach_readings=beach_readings[-which(beach_readings$Full_date=="2006-08-24" & beach_readings$Client.ID=="Jarvis"),]
beach_readings=beach_readings[-which(beach_readings$Full_date=="2006-07-19" & beach_readings$Client.ID=="Hollywood/Ostermann"),]
beach_readings=beach_readings[-which(beach_readings$Full_date=="2007-06-08" & beach_readings$Client.ID=="Hollywood/Thorndale*"),]
beach_readings=beach_readings[-which(beach_readings$Full_date=="2008-06-08" & beach_readings$Client.ID=="Hollywood/Thorndale*"),]
beach_readings=beach_readings[-which(beach_readings$Full_date=="2006-08-08" & beach_readings$Client.ID=="Loyola"),]

##Remove 66 observations with date specified as "PM" -- which creates more than one reading per beach on a day
beach_readings=beach_readings[!grepl("PM",beach_readings$Date),]

##Normalize beach names using names found on cpdbeaches.com
cleanbeachnames <- read.csv("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/cleanbeachnames.csv", stringsAsFactors=FALSE)
changenames <- setNames(cleanbeachnames$New, cleanbeachnames$Old) 
beach_readings$Client.ID <- sapply(beach_readings$Client.ID, function (x) gsub("^\\s+|\\s+$", "", x)) #delete leading and trailing spaces
beach_readings$Client.ID <- changenames[beach_readings$Client.ID]  

##Clean Drek Data so they match beach_readings$Client.ID
drekdata <- read.csv("data/DrekBeach/daily_summaries_drekb.csv", stringsAsFactors = F)
names(drekdata) <- c("Beach", "Date", "Drek_Reading", "Drek_Prediction", "Drek_Worst_Swim_Status")
drekdata$Date <- as.Date(drekdata$Date, "%m/%d/%Y")
drekdata$Beach <- sapply(drekdata$Beach, function (x) gsub("^\\s+|\\s+$", "", x))
drekdata$Beach <- changenames[drekdata$Beach] 

##Merge drek with beach_readings 
beach_readings <- merge(beach_readings, drekdata, by.x = c("Client.ID", "Full_date"), by.y = c("Beach", "Date"), all.x=T)

##Final Clean of beach names for consistency from 2006 to 2015 -- now the the 20 beaches in the data set are the same as those with readings from 2010 through 2015
changenames.2 <- setNames(cleanbeachnames$Short_Names, cleanbeachnames$New) 
beach_readings$Beach_Name <- beach_readings$Client.ID
beach_readings$Beach_Name <- changenames.2[beach_readings$Beach_Name] 

#Remove all but instances with a beach name and at least 1 reading so that we only keep duplicate beaches WITH readings data (re: next line that removes duplicates)
beach_readings=beach_readings[which((!is.na(beach_readings$Reading.1) | !is.na(beach_readings$Reading.2)) & !is.na(beach_readings$Beach_Name)),]

#Remove all instances where same beach has more than one reading on a day, keeping only 1st instance - because some beaches merged during final name clean (e.g. hartigan, north shore, and tobey prinz become Albion)
beach_readings=beach_readings[!duplicated(beach_readings[c("Full_date", "Beach_Name")]), ] #keeps first instance, gets rid of dups

### Change readings to numeric data
beach_readings$Reading.1 <- as.numeric(as.character(beach_readings$Reading.1))
beach_readings$Reading.2 <- as.numeric(as.character(beach_readings$Reading.2))
beach_readings$Escherichia.coli <- as.numeric(as.character(beach_readings$Escherichia.coli))

###Remove outlier with 6488.0 value
beach_readings <- beach_readings[-which(beach_readings$Reading.2==6488.0),]

# Create measure variables
beach_readings$e_coli_geomean_actual_calculated <- round(apply(cbind(beach_readings$Reading.1,beach_readings$Reading.2), 1, geometric.mean, na.rm=T), 1) 

#create 1/0 for advisory at or over 235
beach_readings$elevated_levels_actual_calculated <- ifelse(beach_readings$e_coli_geomean_actual_calculated >= 235, 1, 0)
beach_readings$Drek_elevated_levels_predicted_calculated <- ifelse(beach_readings$Drek_Prediction >= 235, 1, 0)


# Bring in water sensor data (only available for last couple of years)
source("data/ExternalData/merge_water_sensor_data.r")

# Bring in weather sensor data (only available for last couple of years)
source("data/ExternalData/merge_weather_sensor_data.r")

# Bring in holiday data (only summer holidays)
source("data/ExternalData/merge_holiday_data.r")

# Bring in lock opening data
source("data/ExternalData/merge_lock_data.r")

#Beach_Name is lost when brining in water/weather/holiday/lock data -- this brings Beach_Name back in
beach_readings$Beach_Name <- beach_readings$Client.ID
beach_readings$Beach_Name <- changenames.2[beach_readings$Beach_Name]

#Bring in forecast.io daily weather data using cleaned Beach_Name
forecast_daily <- read.csv("data/ExternalData/forecastio_daily_weather.csv", stringsAsFactors = FALSE, row.names=NULL, header = T)
forecast_daily <- unique(forecast_daily)
forecast_daily$Beach_Name <- forecast_daily$beach
forecast_daily$Beach_Name <- changenames.2[forecast_daily$Beach_Name] #this cleans the beach names
forecast_daily=forecast_daily[!duplicated(forecast_daily[c("time", "Beach_Name")]), ] #remove duplicates, for example instances where North Shore & Hartigan appear on same day, due to merge on previous line
beach_readings <- merge(x=beach_readings, y=forecast_daily, by.x=c("Beach_Name", "Full_date"), by.y=c("Beach_Name", "time"), all.x = T, all.y = T)

#so that Client.ID is now the cleaned beaches, and we don't lose the old names
beach_readings$Old_Client.ID=beach_readings$Client.ID
beach_readings$Client.ID=beach_readings$Beach_Name
beach_readings=beach_readings[-which(colnames(beach_readings) %in% "Beach_Name")]

##Add time variables - most were lost during the weather/water/holiday/lock merge
beach_readings$Year <- as.numeric(format(beach_readings$Full_date, "%Y"))
beach_readings$Month <- format(beach_readings$Full_date,"%B")
beach_readings$Date <- format(beach_readings$Full_date,"%B %d")
beach_readings$Weekday <- weekdays(beach_readings$Full_date)  
beach_readings$Day_of_year <- as.numeric(format(beach_readings$Full_date, "%j"))  
beach_readings$Week<- format(beach_readings$Full_date, "%W")  
beach_readings$Day <- format(beach_readings$Full_date, "%d") #rename to Day_of_month? Other code may depend on this name


#create new dataframe with tidyr 
#each row is one date, and one column is the date
#other columns are beach e coli level (1 column for each beach)

library(ggplot2)

# plot 2015

new_df <- beach_readings[,c("Client.ID","Full_date","Escherichia.coli")]
new_df15 <- new_df[new_df$Full_date > as.Date("2015-01-01"),]
new_df15 <- new_df15[complete.cases(new_df15),]
ggplot(new_df15,aes(x=Full_date,y=Escherichia.coli,colour=Client.ID,group=Client.ID)) + geom_line() + ggtitle("Daily Levels E. Coli Levels (2015)") + labs(x="Date",y="Daily E. Coli Level") 

# write to csv for power BI

new_df <- spread(new_df, Client.ID, Escherichia.coli)
write.csv(new_df, file = "beaches_as_columns.csv", row.names = FALSE)

# bar plot of beaches by 235+ E. Coli days

elevated_df <- beach_readings[,c("Client.ID","elevated_levels_actual_calculated")]
elevated_df$elevated_levels_actual_calculated[is.na(elevated_df$elevated_levels_actual_calculated)] <- 0
agg_df <- aggregate(elevated_df$elevated_levels_actual_calculated, by=list(elevated_df$Client.ID), FUN=sum)
barplot(agg_df$x, names.arg = agg_df$Group.1, main = "E. Coli level above 235 (2006 - 2015)", xlab = "Beach", ylab = "Count")

# plot high-occurance beaches

high_beaches <- beach_readings[beach_readings$Client.ID == "12th"
                               | beach_readings$Client.ID == "31st"
                               | beach_readings$Client.ID == "57th"
                               | beach_readings$Client.ID == "63rd"
                               | beach_readings$Client.ID == "Calumet"
                               | beach_readings$Client.ID == "Montrose"
                               | beach_readings$Client.ID == "Rainbow"
                               | beach_readings$Client.ID == "South Shore"
                               ,]
high_beaches <- high_beaches[,c("Full_date","Client.ID","Escherichia.coli")]
high_beaches <- high_beaches[complete.cases(high_beaches),]
high_beaches <- spread(high_beaches, Client.ID, Escherichia.coli)
plot(high_beaches[,2:9], main = "Worst 8 Beaches E. Coli Levels (2006 - 2015)")

# plot low-occurance beaches

low_beaches <- beach_readings[beach_readings$Client.ID == "39th"
                               | beach_readings$Client.ID == "Albion"
                               | beach_readings$Client.ID == "Howard"
                               | beach_readings$Client.ID == "Jarvis"
                               | beach_readings$Client.ID == "Juneway"
                               | beach_readings$Client.ID == "Leone"
                               | beach_readings$Client.ID == "North Avenue"
                               | beach_readings$Client.ID == "Oak Street"
                               | beach_readings$Client.ID == "Rogers"
                               ,]
low_beaches <- low_beaches[,c("Full_date","Client.ID","Escherichia.coli")]
low_beaches <- low_beaches[complete.cases(low_beaches),]
low_beaches <- spread(low_beaches, Client.ID, Escherichia.coli)
plot(low_beaches[,2:10], main = "Best 9 Beaches E. Coli Levels (2006 - 2015)")

