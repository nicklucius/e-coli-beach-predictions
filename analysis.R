source("data/ChicagoParkDistrict/raw/Standard 18 hr Testing/split_sheets.R")

library(lubridate)
library(RSocrata)

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
# The 2016 read-in will bring in all readings after 1/1/2016
df2016 <- read.socrata("https://data.cityofchicago.org/resource/2ivx-z93u.csv?$where=culture_sample_1_timestamp > '2015-12-31T23:59:59'")
df2016 <- data.frame("Year" = 2016, "Date" = df2016$Culture.Sample.1.Timestamp,
                     "Laboratory.ID" = df2016$Culture.Test.ID, "Client.ID" = df2016$Beach, 
                     "Reading.1" = df2016$Culture.Sample.1.Reading, "Reading.2" = df2016$Culture.Sample.2.Reading,
                     "Escherichia.coli" = df2016$Culture.Reading.Mean, "Units" = NA, 
                     "Sample.Collection.Time" = df2016$Culture.Sample.2.Timestamp,
                     "DNA.Test.ID" = df2016$DNA.Test.ID,
                     "DNA.Sample.Timestamp" = df2016$DNA.Sample.Timestamp,
                     "DNA.Sample.1.Reading" = df2016$DNA.Sample.1.Reading,
                     "DNA.Sample.2.Reading" = df2016$DNA.Sample.2.Reading,
                     "DNA.Reading.Mean" = df2016$DNA.Reading.Mean)
df2016$Date <- as.character(df2016$Date, format='%B %d')

# Combine Data
beach_readings <- rbind.fill(df2006, df2007, df2008, df2009, df2010, df2011, df2012, df2013, df2014, df2015, df2016)

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

##add 2016 USGS/EPA predictions from data portal
predictions_2016 <- read.socrata("https://data.cityofchicago.org/resource/t62e-8nvc.csv")

#map values so we have predictions for all beaches, per CPD standards
predictions_12th <- data.frame("Beach" = "12th Street",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Ohio"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Ohio"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Ohio"])
predictions_31st <- data.frame("Beach" = "Margaret T Burroughs",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "63rdStreet"])
predictions_Oakwood <- data.frame("Beach" = "Oakwood",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "63rdStreet"])
predictions_57th <- data.frame("Beach" = "57th Street",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "63rdStreet"])
predictions_63rd <- data.frame("Beach" = "63rd Street",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "63rdStreet"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "63rdStreet"])
predictions_Fargo <- data.frame("Beach" = "Fargo",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Leone"])
predictions_Hartigan <- data.frame("Beach" = "Hartigan",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Leone"])
predictions_Howard <- data.frame("Beach" = "Howard",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Leone"])
predictions_Juneway <- data.frame("Beach" = "Juneway",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Leone"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Leone"])
predictions_North <- data.frame("Beach" = "North Avenue",
                                "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "OakStreet"],
                                "Drek_Reading" = NA,
                                "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "OakStreet"],
                                "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "OakStreet"])
predictions_Oak <- data.frame("Beach" = "Oak Street",
                               "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "OakStreet"],
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "OakStreet"],
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "OakStreet"])
predictions_Ohio <- data.frame("Beach" = "Ohio Street",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Ohio"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Ohio"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Ohio"])
predictions_Rogers <- data.frame("Beach" = "Rogers",
                                "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "Leone"],
                                "Drek_Reading" = NA,
                                "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "Leone"],
                                "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "Leone"])
predictions_SouthShore <- data.frame("Beach" = "South Shore",
                                 "Date" = predictions_2016$Date[predictions_2016$Beach.Name == "63rdStreet"],
                                 "Drek_Reading" = NA,
                                 "Drek_Prediction" = predictions_2016$Predicted.Level[predictions_2016$Beach.Name == "63rdStreet"],
                                 "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory[predictions_2016$Beach.Name == "63rdStreet"])

#align column names for merging
predictions_2016 <- data.frame("Beach" = predictions_2016$Beach.Name,
                               "Date" = predictions_2016$Date,
                               "Drek_Reading" = NA,
                               "Drek_Prediction" = predictions_2016$Predicted.Level,
                               "Drek_Worst_Swim_Status" = predictions_2016$Swim.Advisory)

predictions_2016 <- rbind(predictions_2016, predictions_12th, predictions_31st, predictions_Oakwood,
                          predictions_57th, predictions_Howard, predictions_Juneway, predictions_North, 
                          predictions_Rogers, predictions_SouthShore, predictions_Ohio, predictions_63rd, 
                          predictions_Oak, predictions_Fargo, predictions_Hartigan) 

drekdata <- rbind(drekdata, predictions_2016)
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
premerge_weather_2016 <- read.csv("python_src/2016_weather.csv", stringsAsFactors = FALSE, row.names=NULL, header = T)
beaches_2016 <- read.csv("data/ExternalData/Beach_Weather_2016_Mapping.csv", stringsAsFactors = FALSE, row.names=NULL, header = T)
beaches_2016$Date <- as.Date(beaches_2016$Date, "%m/%d/%Y")
beaches_2016$Date <- as.character(beaches_2016$Date)
merged_weather_2016 <- merge(beaches_2016, premerge_weather_2016)
forecast_daily_2016 <- data.frame("time" = merged_weather_2016$Date,
                                  "summary" = merged_weather_2016$summary,
                                  "icon" = merged_weather_2016$icon,
                                  "sunriseTime" = merged_weather_2016$sunriseTime,
                                  "sunsetTime" = merged_weather_2016$sunsetTime,
                                  "moonPhase" = merged_weather_2016$moonPhase,
                                  "precipIntensity" = merged_weather_2016$precipIntensity,
                                  "precipIntensityMax" = merged_weather_2016$precipIntensityMax,
                                  "precipProbability" = merged_weather_2016$precipProbability,
                                  "temperatureMin" = merged_weather_2016$temperatureMin,
                                  "temperatureMinTime" = merged_weather_2016$temperatureMinTime,
                                  "temperatureMax" = merged_weather_2016$temperatureMax,
                                  "temperatureMaxTime" = merged_weather_2016$temperatureMaxTime,
                                  "apparentTemperatureMin" = merged_weather_2016$apparentTemperatureMin,
                                  "apparentTemperatureMinTime" = merged_weather_2016$apparentTemperatureMinTime,
                                  "apparentTemperatureMax" = merged_weather_2016$apparentTemperatureMax,
                                  "apparentTemperatureMaxTime" = merged_weather_2016$apparentTemperatureMaxTime,
                                  "dewPoint" = merged_weather_2016$dewPoint,
                                  "humidity" = merged_weather_2016$humidity,
                                  "windSpeed" = merged_weather_2016$windSpeed,
                                  "windBearing" = merged_weather_2016$windBearing,
                                  "visibility" = merged_weather_2016$visibility,
                                  "cloudCover" = merged_weather_2016$cloudCover,
                                  "pressure" = merged_weather_2016$pressure,
                                  "beach" = merged_weather_2016$beach,
                                  "precipIntensityMaxTime" = merged_weather_2016$precipIntensityMaxTime,
                                  "precipType" = merged_weather_2016$precipType, stringsAsFactors=FALSE)
forecast_daily <- rbind (forecast_daily, forecast_daily_2016)
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
beach_readings$Weekday_code <- as.integer(wday(beach_readings$Full_date))
beach_readings$Month_code <- as.integer(as.character(beach_readings$Full_date, format="%m"))
beach_readings$Beach_code <- as.integer(sapply(beach_readings$Client.ID, function(x) {
                                      switch(x,
                                             "12th" = 1,
                                             "31st" = 2,
                                             "39th" = 3,
                                             "57th" = 4,
                                             "63rd" = 5,
                                             "Albion" = 6,
                                             "Calumet" = 7,
                                             "Foster" = 8,
                                             "Howard" = 9,
                                             "Jarvis" = 10,
                                             "Juneway" = 11,
                                             "Leone" = 12,
                                             "Montrose" = 13,
                                             "North Avenue" = 14,
                                             "Oak Street" = 15,
                                             "Ohio" = 16,
                                             "Osterman" = 17,
                                             "Rainbow" = 18,
                                             "Rogers" = 19,
                                             "South Shore" = 20)
                                      }
                                    ))


# Build naive logit model (today like yesterday)
# -----------------------------------------------------------

beach_readings_mod <- beach_readings[!is.na(beach_readings$Client.ID),]
beach_readings_mod <- beach_readings_mod[order(beach_readings_mod$Client.ID, beach_readings_mod$Full_date),]

# create "high reading" and "low reading"
beach_readings_mod$High.Reading <- mapply(max, beach_readings_mod$Reading.1, beach_readings_mod$Reading.2)
beach_readings_mod$Low.Reading <- mapply(min, beach_readings_mod$Reading.1, beach_readings_mod$Reading.2)

# create columns for yesterday's readings
library(useful)
temp <- split(beach_readings_mod, beach_readings_mod$Client.ID)
for (i in 1:length(temp)) {
  temp[[i]] <- shift.column(temp[[i]], columns=c("High.Reading","Low.Reading","e_coli_geomean_actual_calculated"), newNames=c("Yesterday.High.Reading", "Yesterday.Low.Reading", "Yesterday.Geomean"), len=1L, up=FALSE)
}
beach_readings_mod <- do.call("rbind", temp)

# use only records without NAs in predictors or response
beach_readings_mod <- beach_readings_mod[!is.na(beach_readings_mod$Yesterday.High.Reading) & !is.na(beach_readings_mod$Yesterday.Low.Reading) & !is.na(beach_readings_mod$Yesterday.Geomean)& !is.na(beach_readings_mod$elevated_levels_actual_calculated),]

# get train and test set
set.seed(12345)
smp_size <- floor(0.75 * nrow(beach_readings_mod))
train_ind <- sample(seq_len(nrow(beach_readings_mod)), size = smp_size)
train <- beach_readings_mod[train_ind, ]
test <- beach_readings_mod[-train_ind, ]

# fit naive logit model to training set
#fit <- glm(elevated_levels_actual_calculated ~ Yesterday.High.Reading + Yesterday.Low.Reading, data=train, family=binomial())
fit <- glm(elevated_levels_actual_calculated ~ Yesterday.Geomean, data=train, family=binomial())
summary(fit)

# evaluate model on test set
pred.prob=predict(fit, newdata=test, type="response")
pred.elevated <- rep(0,nrow(test))
pred.elevated[pred.prob>.5]=1
test$prediction <- pred.elevated
confmatrix=table(pred.elevated,test$elevated_levels_actual_calculated)
confmatrix
Recall=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[1,2])
Recall # 2%
Precision=confmatrix[2,2]/(confmatrix[2,2]+confmatrix[2,1])
Precision # 32%
Fscore=(2*Precision*Recall)/(Precision+Recall)
Fscore # 0.04
Misclassification=1-(sum(diag(confmatrix))/nrow(test))
Misclassification # 14%

# -----------------------------------------------------------


# Calculate confusion matrix for 2015 (EPA model)

beach_readings_2015 <- beach_readings[beach_readings$Year==2015 & 
                                         !is.na(beach_readings$Reading.1) & 
                                         !is.na(beach_readings$Reading.2) &
                                         !is.na(beach_readings$Drek_Prediction)
                                       , ]

###@ Analyze the relationship between Reading.1 and Reading.2 in 2015
plot(beach_readings_2015$Reading.1, beach_readings_2015$Reading.2)
plot(beach_readings_2015$Reading.1, beach_readings_2015$Reading.2, log=c('x', 'y'))
plot(log(beach_readings_2015$Reading.1)+1, log(beach_readings_2015$Reading.2)+1, log=c('x', 'y'))
llmodel <- lm(log(log(Reading.2)+1)~log(log(Reading.1)+1), data=beach_readings_2015)
summary(llmodel)
par(mfrow=c(2,2));plot(llmodel);par(mfrow=c(1,1))

#### True positive -- correctly identifying elevated levels
actual_positive <- sum(beach_readings_2015$elevated_levels_actual_calculated)
true_positive <- beach_readings_2015[beach_readings_2015$Drek_elevated_levels_predicted_calculated == 1 &
                                       beach_readings_2015$elevated_levels_actual_calculated == 1, ]$Drek_elevated_levels_predicted_calculated
true_positive_perc <- sum(true_positive) / actual_positive

#### True negative -- correctly identifying non-elevated levels
actual_negative <- nrow(beach_readings_2015) - sum(actual_positive)
true_negative <- beach_readings_2015[beach_readings_2015$Drek_elevated_levels_predicted_calculated == 0 &
                                       beach_readings_2015$elevated_levels_actual_calculated == 0, ]$Drek_elevated_levels_predicted_calculated
true_negative_perc <- length(true_negative) / actual_negative

#### False negative -- failing to predict elevated levels
false_negative <- beach_readings_2015[beach_readings_2015$Drek_elevated_levels_predicted_calculated == 0 &
                                        beach_readings_2015$elevated_levels_actual_calculated == 1, ]$Drek_elevated_levels_predicted_calculated
false_negative_perc <- length(false_negative) / actual_negative # Exposing those to e coli

#### False positive -- incorrectly identifying elevated levels
false_positive <- beach_readings_2015[beach_readings_2015$Drek_elevated_levels_predicted_calculated == 1 &
                                        beach_readings_2015$elevated_levels_actual_calculated == 0, ]$Drek_elevated_levels_predicted_calculated
false_positive_perc <- length(false_positive) / actual_positive # Ruins the fun for the day

confusion_matrix <- table(true_positive_perc, true_negative_perc, false_positive_perc, false_negative_perc)

# Residual Analysis
resid <- beach_readings_2015$Drek_Prediction - beach_readings_2015$e_coli_geomean_actual_calculated
hist(resid, breaks = 30, main="Difference between predicted value and actual \n (negative denotes underestimate)")
summary(resid)

# Analytics functions
prCurve <- function(truth, predicted_values) {
    recalls = c()
    precisions = c()
    for (threshold in seq(0, 500, 20)) {
        recalls = c(recalls, recall(truth, predicted_values >= threshold))
        precisions = c(precisions, precision(truth, predicted_values >= threshold))
    }
    lines(recalls ~ precisions)
}


recall <- function(truth, predict) {
    return(sum(predict[truth])/sum(truth))
}

precision <- function(truth, predict) {
    return(sum(predict[truth])/sum(predict))
}

measures <- read.csv('data/DrekBeach/daily_summaries_drekb.csv')

measures$Date <- as.Date(measures$Date, '%m/%d/%Y')

measures$tomorrow <- measures$Date + 1

measures <- merge(measures, measures[, !(names(measures) %in% c("Date"))],
                  by.x=c('Beach', 'Date'),
                  by.y=c('Beach', 'tomorrow'))

measures <- measures[,c(1,2,3,4,5,8,9)]

names(measures) <- c("beach", "date", "reading", "prediction", "status",
                     "yesterday_reading", "yesterday_prediction")

true_advisory_days <- measures$reading > 235

plot(c(0,1), c(0,1), type="n")

prCurve(true_advisory_days,  measures$yesterday_reading)

prCurve(true_advisory_days,  measures$prediction)

model.naive <- glm(reading ~ yesterday_reading*beach + weekdays(date)*beach, measures, family='poisson')
summary(model.naive)

prCurve(true_advisory_days,  exp(predict(model.naive)))

model.forecast <- glm(reading ~ prediction*beach + weekdays(date)*beach + months(date), measures, family='poisson')
summary(model.forecast)

prCurve(true_advisory_days,  exp(predict(model.forecast)))

#This function can be called with or without the vector of column names, names_of_columns_to_shift
#without the argument names_of_columns_to_shift, it defaults to all numeric columns in original_data_frame
shift_previous_data <- function(number_of_observations, original_data_frame, names_of_columns_to_shift=FALSE) {
  merged_data_frame <- data.frame()
  #remove rows where Client.ID == NA
  clean_data_frame <- original_data_frame[!is.na(original_data_frame$Client.ID),]         
  #subset by year
  for (year in unique(clean_data_frame$Year)) {        
    readings_by_year <- clean_data_frame[clean_data_frame$Year == year,]
    #subset by beach within year
    for (beach in unique(readings_by_year$Client.ID) ) {     
      readings_by_beach <- readings_by_year[readings_by_year$Client.ID == beach,]
      readings_by_beach <- readings_by_beach[order(readings_by_beach$Full_date),]
      #if no columns provided, use default (all numeric columns)
      if (names_of_columns_to_shift[1] == FALSE) {        
        readings_by_beach_columns <- readings_by_beach[sapply(readings_by_beach, is.numeric)]
        names_of_columns_to_shift <- colnames(readings_by_beach_columns)
      }
      #build new column names
      for (column in names_of_columns_to_shift) {     
        new_column_name <- paste(number_of_observations,"daysPrior",column,sep=".")
        new_column_values <- vector()
        #build new columns
        #f or first n rows, use NA bc no prior data to use
        for (n in 1:number_of_observations) {         
          new_column_values[n] <- NA            
        }
        #add previous data to new columns
        for (i in number_of_observations:nrow(readings_by_beach)) {  
          new_column_values <- c(new_column_values, readings_by_beach[,column][i-n])
        }
        #merge new columns with subsetted year/beach data
        readings_by_beach <- cbind(readings_by_beach, new_column_values)    
        colnames(readings_by_beach)[colnames(readings_by_beach)=="new_column_values"] <- new_column_name
      }
      #rebuild original dataframe adding the merged data from above
      merged_data_frame <- rbind(merged_data_frame, readings_by_beach)  
    }
  }
  merged_data_frame
}

#Principal Component Analysis

#beach_readings_pca <- beach_readings
#cols_to_remove <- c("Transducer.Depth.Min", "Transducer.Depth.Max", "Transducer.Depth.Mean", "Rain.Intensity.Min", "Interval.Rain.Min", "Holiday.Flag", "precipIntensityMaxTime")
#beach_readings_pca <- beach_readings_pca[,!names(beach_readings_pca) %in% cols_to_remove]
#beach_readings_pca_shifted <- shift_previous_data(1,beach_readings_pca)
#new_shifted_col_names <- setdiff(colnames(beach_readings_pca_shifted),colnames(beach_readings_pca))
#beach_readings_pca_shifted_new_only <- beach_readings_pca_shifted[,new_shifted_col_names]
#cols_to_remove <- c("1.daysPrior.Year", "1.daysPrior.Reading.1", "1.daysPrior.Reading.2", "1.daysPrior.Escherichia.coli", "1.daysPrior.Drek_Reading", "1.daysPrior.Drek_Prediction", "1.daysPrior.e_coli_geomean_actual_calculated", "1.daysPrior.elevated_levels_actual_calculated", "1.daysPrior.Drek_elevated_levels_predicted_calculated")
#beach_readings_pca_shifted_new_only <- beach_readings_pca_shifted_new_only[,!names(beach_readings_pca_shifted_new_only) %in% cols_to_remove]
#beach_readings_pca <- cbind(beach_readings_pca_shifted$Reading.1, beach_readings_pca_shifted$Reading.2, beach_readings_pca_shifted$e_coli_geomean_actual_calculated, beach_readings_pca_shifted_new_only)
#names(beach_readings_pca)[1:3] <- c("Reading.1", "Reading.2", "e_coli_geomean_actual_calculated")
#beach_readings_pca <-  beach_readings_pca[,sapply(beach_readings_pca, is.numeric)]
#na_count <- sapply(beach_readings_pca, function(y) sum(is.na(y))) #analyze NAs
#beach_readings_pca <- beach_readings_pca[,na_count < 10000] #enforce NA maximum
#beach_readings_pca <- beach_readings_pca[complete.cases(beach_readings_pca),] #remove NAs
#beach_readings_pca <- beach_readings_pca[complete.cases(beach_readings_pca),]
#beach_readings_pca <- scale(beach_readings_pca)
#pca <- prcomp(beach_readings_pca)
#plot(pca, type = "l")
#aload <- abs(pca$rotation[,1:5])
#relative_contribution_to_PC <- sweep(aload, 2, colSums(aload), "/")

## add columns for predictor beaches
## use for genetic testing analysis

add_DNAmean_column <- function(df, beach) {
  new_col <- c()
  for (row in c(1:nrow(df))) {
    this_date <- df$Full_date[row]
    new_value <- df[df$Client.ID == beach & df$Full_date == this_date,"DNA.Reading.Mean"]
    if (length(new_value) == 0)
      new_col <- c(new_col, NA)
    else
      new_col <- c(new_col, new_value)
  }
  df <- cbind(df, new_col)
  colnames(df)[ncol(df)] <- paste0(beach,"_DNAmean")
  df
}

add_geomean_column <- function(df, beach) {
  new_col <- c()
  for (row in c(1:nrow(df))) {
    this_date <- df$Full_date[row]
    new_value <- df[df$Client.ID == beach & df$Full_date == this_date,"Escherichia.coli"]
    if (length(new_value) == 0)
      new_col <- c(new_col, NA)
    else
      new_col <- c(new_col, new_value)
  }
  df <- cbind(df, new_col)
  colnames(df)[ncol(df)] <- paste0(beach,"_geomean")
  df
}

#beach_readings <- add_geomean_column(beach_readings,"12th")
#beach_readings <- add_geomean_column(beach_readings,"31st")
#beach_readings <- add_geomean_column(beach_readings,"39th")
#beach_readings <- add_geomean_column(beach_readings,"57th")
beach_readings <- add_geomean_column(beach_readings,"63rd")
#beach_readings <- add_geomean_column(beach_readings,"Albion")
beach_readings <- add_geomean_column(beach_readings,"Calumet")
#beach_readings <- add_geomean_column(beach_readings,"Foster")
beach_readings <- add_geomean_column(beach_readings,"Howard")
#beach_readings <- add_geomean_column(beach_readings,"Jarvis")
#beach_readings <- add_geomean_column(beach_readings,"Juneway")
#beach_readings <- add_geomean_column(beach_readings,"Leone")
beach_readings <- add_geomean_column(beach_readings,"Montrose")
#beach_readings <- add_geomean_column(beach_readings,"North Avenue")
#beach_readings <- add_geomean_column(beach_readings,"Oak Street")
#beach_readings <- add_geomean_column(beach_readings,"Ohio")
#beach_readings <- add_geomean_column(beach_readings,"Osterman")
beach_readings <- add_geomean_column(beach_readings,"Rainbow")
#beach_readings <- add_geomean_column(beach_readings,"Rogers")
beach_readings <- add_geomean_column(beach_readings,"South Shore")

beach_readings <- add_DNAmean_column(beach_readings,"63rd")
beach_readings <- add_DNAmean_column(beach_readings,"Calumet")
beach_readings <- add_DNAmean_column(beach_readings,"Montrose")
beach_readings <- add_DNAmean_column(beach_readings,"Rainbow")
beach_readings <- add_DNAmean_column(beach_readings,"South Shore")
# remove weekends before shifting data 
# as a result, 1 day prior to a Monday will be a Friday
# this is a choice made for modeling to avoid NAs on Mondays (or Tuesdays for 2.days.prior)
beach_readings <- beach_readings[beach_readings$Weekday_code > 1 & beach_readings$Weekday_code < 7,]
beach_readings <- shift_previous_data(1, beach_readings)
beach_readings <- shift_previous_data(2, beach_readings)

#remove drek prediction errors (readings under 0)
beach_readings$Drek_Prediction <- ifelse(beach_readings$Drek_Prediction >= 0, beach_readings$Drek_Prediction, NA)

##----------------------------------------------------------------------------------------------------------------
##  START MODELING AND VISUALIZATIONS
##----------------------------------------------------------------------------------------------------------------

library(randomForest)
library(reshape)
library(ggplot2)
#library(lda)
#library(MASS)


## Begin LDA
## LDA: http://www.r-bloggers.com/computing-and-visualizing-lda-in-r/
## Build out the year-by-year graph
## Each year seems to be different
## Run LDA for each year, get a model for each year
## See how predictors change
## Take out the days that the locks were open. 
## how to deal with binary data?
#x_LDA <- x[,c(2,12,15,18,19,101:271)]  #choose predictors
#x_LDA <- (x_LDA[,sapply(x_LDA, is.numeric)])# remove non-numerics
#na_count <- sapply(x_LDA, function(y) sum(is.na(y))) #analyze NAs
#x_LDA <- x_LDA[,na_count < 10000] #enforce NA maximum
#x_LDA_complete <- x_LDA[complete.cases(x_LDA),] #remove NAs
#x_LDA_complete_scaled <- data.frame(scale(x_LDA_complete)) #scale
#x_LDA_complete_scaled$elevated_levels_actual_calculated <- factor(x_LDA_complete$elevated_levels_actual_calculated) #replace scaled with binary
#x_LDA_complete_scaled$elevated_levels_actual_calculated <- factor(x_LDA_complete$'1.daysPrior.CRCW.Lock.Open') #replace scaled with binary
#x_LDA_complete_scaled$elevated_levels_actual_calculated <- factor(x_LDA_complete$'1.daysPrior.Wilmette.Lock.Open') #replace scaled with binary
#x_LDA_complete_scaled$elevated_levels_actual_calculated <- factor(x_LDA_complete$'2.daysPrior.Wilmette.Lock.Open') #replace scaled with binary
#x_LDA_complete <- x_LDA_complete[,-c(12,48)]
#LDA_results <- lda(elevated_levels_actual_calculated ~ ., x_LDA_complete)
#LDA_coefficients <- LDA_results$scaling[order(LDA_results$scaling),]

##----------------------------------------------------------------------------------------------------------------
## MODEL 1
##----------------------------------------------------------------------------------------------------------------

## Try more moving averages to get better weather trends
## https://www.researchgate.net/publication/23955184_Summer_E-coli_Patterns_and_Responses_along_23_Chicago_Beaches


#use the worst 4 beaches
#df <- df[df$Client.ID == "Calumet" | df$Client.ID == "63rd" | df$Client.ID == "Montrose" | df$Client.ID == "Rainbow",]

#-----------------------------------------------------------------------------------------------------------------
# Choose Predictors
#-----------------------------------------------------------------------------------------------------------------
  
df <- beach_readings
model1_data <- df
model1_data <- model1_data[-c(1575,2961),] #remove duplicate rows
model1_data <- model1_data[, c("e_coli_geomean_actual_calculated",
                               #"63rd_DNAmean", # current DNA test
                               #"South Shore_DNAmean", # current DNA test
                               #"Montrose_DNAmean", # current DNA test
                               #"Calumet_DNAmean", # current DNA test
                               #"Rainbow_DNAmean", # current DNA test
                               "63rd_geomean", 
                               "South Shore_geomean", 
                               "Montrose_geomean",
                               "Calumet_geomean", 
                               "Rainbow_geomean", 
                               "Howard_geomean",
                               "precipProbability",
                               "1.daysPrior.precipIntensity",
                               "Client.ID",
                               "Full_date", #use for validating by exact date
                               #"Year", #use for leave-one-year-out validation
                               #"Day_of_year", #use to create random data splits without day bleed
                               "Drek_Prediction")
                           ]
model1_data$Client.ID <- as.factor(model1_data$Client.ID)
#model1_data$Weekday_code <- as.factor(model1_data$Weekday_code)
#model1_data$Month <- as.factor(model1_data$Month)
names(model1_data)[2] <- "n63rd_geomean" # Random forest doesn't like column beginning with a number
names(model1_data)[3] <- "SouthShore_geomean" # Random forest doesn't like space in column name
names(model1_data)[9] <- "n1.daysPrior.precipIntensity" # Random forest doesn't like column beginning with a number
model1_data_vars <- ncol(model1_data)

#-----------------------------------------------------------------------------------------------------------------
# Loop through modeling once for each year
#-----------------------------------------------------------------------------------------------------------------

roc_curve <- data.frame()
for (year in c(2016)) {
  print(year)
  
#-----------------------------------------------------------------------------------------------------------------
# Create train / test sets
# Downsample to achieve balance between high and low e. coli days
#-----------------------------------------------------------------------------------------------------------------  
  
  ## TEST SET
  
  test <- model1_data[model1_data$Full_date >= "2015-01-01",]
  #test <- model1_data[model1_data$Year == year,]
  #test <- model1_data[model1_data$Year == year,
  #                    c(1:model1_data_vars-1)]
  # Reduce test set to non-predictor beaches
  test <- test[which(!test$Client.ID %in% c("Rainbow",
                                    "South Shore",
                                    "Montrose",
                                    "Calumet",
                                    "63rd",
                                    "Howard")),]
  test <- test[complete.cases(test),] #remove NAs from test data
  test_vars <- ncol(test)

  ## TRAIN SET
  
  train <- model1_data[,
                       c(1:model1_data_vars-1)] #remove EPA prediction from training data
  # Reduce train set to non-predictor beaches
  train <- train[which(!train$Client.ID %in% c("Rainbow",
                                            "South Shore",
                                            "Montrose",
                                            "Calumet",
                                            "63rd",
                                            "Howard")),]
  train <- train[train$Full_date < "2015-01-01" 
                 & train$Full_date > "2006-01-01",]
  #train <- train[!train$Year == year,]
  train <- train[complete.cases(train),] #remove NAs from train data
#  train_high <- train[train$e_coli_geomean_actual_calculated >= 200 
#                      & train$e_coli_geomean_actual_calculated < 2500,]
#  train_low <- train[train$e_coli_geomean_actual_calculated < 200,]
  # only use as many low days as you have high days
#  ind <- sample(c(1:nrow(train_low)), 
#                nrow(train_high), 
#                replace = TRUE)
#  train_balanced <- rbind(train_high, train_low[ind,])
#  train <- train_balanced
  train_vars <- ncol(train)

  ## the following will produce a random split
  ## this will replace everthing done with test/train above
  ## comment out if you want to use the above code
  
  #set.seed(111)
  #data_split <- model1_data[complete.cases(model1_data),]
  #even_days <- data_split[data_split$Day_of_year %% 2 == 0,]
  #odd_days <- data_split[data_split$Day_of_year %% 2 == 1,]
  #ind_even <- sample(2, nrow(even_days), replace = TRUE, prob=c(0.5, 0.5))
  #ind_odd <- sample(2, nrow(odd_days), replace = TRUE, prob=c(0.5, 0.5))
  #test_even <- even_days[ind_even == 2,]
  #test_odd <- odd_days[ind_odd == 2,] 
  #train_even <- even_days[ind_even == 1,c(1:model1_data_vars-1)] #remove EPA prediction from training data
  #train_odd <- odd_days[ind_odd == 1,c(1:model1_data_vars-1)] #remove EPA prediction from training data
  #test <- rbind(test_even, test_odd)
  #train <- rbind(train_even, train_odd)
  #train_vars <- ncol(train)
  #test_vars <- ncol(test)
  
#-----------------------------------------------------------------------------------------------------------------
# Model / Predict / Build ROC Curve
#-----------------------------------------------------------------------------------------------------------------   
  
  model <- randomForest(e_coli_geomean_actual_calculated ~ ., data = train[,c(1:(train_vars-1))])
  test$prediction <- predict(model, test[,c(1:(test_vars-2))])
  test$ecoli_binary <- ifelse(test$e_coli_geomean_actual_calculated >= 235, 1, 0)
  tpr = c()
  fpr = c()
  for (threshold in seq(0, 1500, 1)) {
    test$prediction_binary <- ifelse(test$prediction >= threshold, 1, 0)
    test$true_positive <- ifelse((test$ecoli_binary == 1 & test$prediction_binary  == 1), 1, 0)
    test$true_negative <- ifelse((test$ecoli_binary == 0 & test$prediction_binary  == 0), 1, 0)
    test$false_negative <- ifelse((test$ecoli_binary == 1 & test$prediction_binary  == 0), 1, 0)
    test$false_positive <- ifelse((test$ecoli_binary == 0 & test$prediction_binary  == 1), 1, 0)
    tpr = c(tpr, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_negative))))
    fpr = c(fpr, (sum(test$false_positive) / (sum(test$false_positive) + sum(test$true_negative))))
  }
  roc_curve_by_year <- data.frame(year, tpr, fpr)
  roc_curve <- rbind(roc_curve, roc_curve_by_year)
}
roc_curve$year <- as.factor(roc_curve$year)
#ggplot(data=roc_curve, aes(x=fpr, y=tpr, color=year)) + geom_path()

#-----------------------------------------------------------------------------------------------------------------
# Build EPA ROC Curve for same test set
#----------------------------------------------------------------------------------------------------------------- 

epa_tpr = c()
epa_fpr = c()
for (threshold in seq(0, 1500, 1)) {
  test$Drek_binary <- ifelse(test$Drek_Prediction >= threshold, 1, 0)
  test$true_positive <- ifelse((test$ecoli_binary == 1 & test$Drek_binary  == 1), 1, 0)
  test$true_negative <- ifelse((test$ecoli_binary == 0 & test$Drek_binary  == 0), 1, 0)
  test$false_negative <- ifelse((test$ecoli_binary == 1 & test$Drek_binary  == 0), 1, 0)
  test$false_positive <- ifelse((test$ecoli_binary == 0 & test$Drek_binary  == 1), 1, 0)
  epa_tpr <- c(epa_tpr, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_negative))))
  epa_fpr <- c(epa_fpr, (sum(test$false_positive) / (sum(test$false_positive) + sum(test$true_negative))))
}

#-----------------------------------------------------------------------------------------------------------------
# Plot ROC Curves 
#----------------------------------------------------------------------------------------------------------------- 

p <- ggplot() 
p + 
  geom_path(aes(x = fpr, y = tpr), 
            color = "blue") + 
  geom_path(aes(x = epa_fpr, y = epa_tpr), 
            color = "red") + 
  ylim(0,1) + 
  xlim(0,1) + 
  ggtitle("2015-2016 Geomean Model ROC Curve")
p + 
  geom_path(aes(x = fpr, y = tpr), 
            color = "blue") + 
  geom_path(aes(x = epa_fpr, y = epa_tpr), 
            color = "red") + 
  ylim(0,.75) + 
  xlim(0,.1) + 
  ggtitle("2015-2016 Geomean Model ROC Curve")

#-----------------------------------------------------------------------------------------------------------------
# Plot PR Curves
#-----------------------------------------------------------------------------------------------------------------

precision = c()
recall = c()
for (threshold in seq(0, 1500, 1)) {
  test$prediction_binary <- ifelse(test$prediction >= threshold, 1, 0)
  test$true_positive <- ifelse((test$ecoli_binary == 1 & test$prediction_binary  == 1), 1, 0)
  test$true_negative <- ifelse((test$ecoli_binary == 0 & test$prediction_binary  == 0), 1, 0)
  test$false_negative <- ifelse((test$ecoli_binary == 1 & test$prediction_binary  == 0), 1, 0)
  test$false_positive <- ifelse((test$ecoli_binary == 0 & test$prediction_binary  == 1), 1, 0)
  precision = c(precision, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_positive))))
  recall = c(recall, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_negative))))
}
epa_precision = c()
epa_recall = c()
for (threshold in seq(0, 1500, 1)) {
  test$Drek_binary <- ifelse(test$Drek_Prediction >= threshold, 1, 0)
  test$true_positive <- ifelse((test$ecoli_binary == 1 & test$Drek_binary  == 1), 1, 0)
  test$true_negative <- ifelse((test$ecoli_binary == 0 & test$Drek_binary  == 0), 1, 0)
  test$false_negative <- ifelse((test$ecoli_binary == 1 & test$Drek_binary  == 0), 1, 0)
  test$false_positive <- ifelse((test$ecoli_binary == 0 & test$Drek_binary  == 1), 1, 0)
  epa_precision <- c(epa_precision, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_positive))))
  epa_recall <- c(epa_recall, (sum(test$true_positive) / (sum(test$true_positive) + sum(test$false_negative))))
}

p + 
  geom_path(aes(x = recall, y = precision),
            color = "blue") +
  geom_path(aes(x = epa_recall, y = epa_precision),
            color = "red") +
  ylim(0,1) + 
  xlim(0,1) +
  ggtitle("2015-2016 Geomean Model PR Curve")


#-----------------------------------------------------------------------------------------------------------------
# Look at Genetic Tests
#-----------------------------------------------------------------------------------------------------------------

#dna <- df[,c(1:15)] # remove culture test columns
#summary(dna)
#plot(dna$DNA.Sample.1.Reading, dna$DNA.Sample.2.Reading)
#plot(dna$DNA.Sample.1.Reading, dna$DNA.Sample.2.Reading, log=c('x', 'y'))
#plot(log(dna$DNA.Sample.1.Reading)+1, log(dna$DNA.Sample.2.Reading)+1, log=c('x', 'y'))
#plot(dna$Escherichia.coli, dna$DNA.Reading.Mean)
#plot(dna$Escherichia.coli, dna$DNA.Reading.Mean, log=c('x', 'y'))
#plot(log(dna$Escherichia.coli)+1, log(dna$DNA.Reading.Mean)+1, log=c('x', 'y'))
#llmodel <- lm(log(log(Escherichia.coli)+1)~log(log(dna$DNA.Reading.Mean)+1), data=dna)
#summary(llmodel)
#par(mfrow=c(2,2));plot(llmodel);par(mfrow=c(1,1))
#hist(dna$DNA.Reading.Mean)


#-----------------------------------------------------------------------------------------------------------------
# Calculate USGS Confusion Matrix
#-----------------------------------------------------------------------------------------------------------------

df_2015 <- beach_readings[beach_readings$Year == "2015",]
df_2015 <- df_2015[!is.na(df_2015$Drek_elevated_levels_predicted_calculated),]
df_2015 <- df_2015[!is.na(df_2015$elevated_levels_actual_calculated),]
tp <- ifelse((df_2015$elevated_levels_actual_calculated == 1 & df_2015$Drek_elevated_levels_predicted_calculated  == 1), 1, 0)
tn <- ifelse((df_2015$elevated_levels_actual_calculated == 0 & df_2015$Drek_elevated_levels_predicted_calculated  == 0), 1, 0)
fn <- ifelse((df_2015$elevated_levels_actual_calculated == 1 & df_2015$Drek_elevated_levels_predicted_calculated  == 0), 1, 0)
fp <- ifelse((df_2015$elevated_levels_actual_calculated == 0 & df_2015$Drek_elevated_levels_predicted_calculated  == 1), 1, 0)
print(paste0("True Positives = ", sum(tp)))
print(paste0("True Negatives = ", sum(tn)))
print(paste0("False Positives = ", sum(fp)))
print(paste0("False Negatives = ", sum(fn)))
print(paste0("2015 True Positive Rate = ",(sum(tp)/(sum(tp)+sum(fn)))))
print(paste0("2015 False Positive Rate = ",(sum(fp)/(sum(fp)+sum(tn)))))

df_2016 <- beach_readings[beach_readings$Year == "2016",]
df_2016 <- df_2016[!is.na(df_2016$Drek_elevated_levels_predicted_calculated),]
df_2016 <- df_2016[!is.na(df_2016$elevated_levels_actual_calculated),]
tp <- ifelse((df_2016$elevated_levels_actual_calculated == 1 & df_2016$Drek_elevated_levels_predicted_calculated  == 1), 1, 0)
tn <- ifelse((df_2016$elevated_levels_actual_calculated == 0 & df_2016$Drek_elevated_levels_predicted_calculated  == 0), 1, 0)
fn <- ifelse((df_2016$elevated_levels_actual_calculated == 1 & df_2016$Drek_elevated_levels_predicted_calculated  == 0), 1, 0)
fp <- ifelse((df_2016$elevated_levels_actual_calculated == 0 & df_2016$Drek_elevated_levels_predicted_calculated  == 1), 1, 0)
print(paste0("True Positives = ", sum(tp)))
print(paste0("True Negatives = ", sum(tn)))
print(paste0("False Positives = ", sum(fp)))
print(paste0("False Negatives = ", sum(fn)))
print(paste0("2016 True Positive Rate = ",(sum(tp)/(sum(tp)+sum(fn)))))
print(paste0("2016 False Positive Rate = ",(sum(fp)/(sum(fp)+sum(tn)))))