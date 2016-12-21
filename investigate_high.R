source("Master.R")
library('tidyr')
df_high <- df[df$Escherichia.coli > 1000 & !is.na(df$Escherichia.coli) & !is.na(df$Client.ID),]
set.seed(100)
ind <- c(1:nrow(df_high))
ind <- sample(ind, 10)
df_sample <- df_high[ind,]
# add three days prior to each incident
for (row in c(1:nrow(df_sample))) {
  beach <- df_sample$Client.ID[row]
  date <- df_sample$Date[row]
  prior_day1 <- date - 86400  #seconds in a day
  prior_day2 <- date - 172800
  prior_day3 <- date - 259200
  new_row1 <- df[df$Client.ID == beach & !is.na(df$Client.ID) & df$Date == prior_day1 & !is.na(df$Date),]
  new_row2 <- df[df$Client.ID  == beach & !is.na(df$Client.ID) & df$Date == prior_day2 & !is.na(df$Date),]
  new_row3 <- df[df$Client.ID  == beach & !is.na(df$Client.ID) & df$Date == prior_day3 & !is.na(df$Date),]
  df_sample <- rbind(df_sample, new_row1, new_row2, new_row3)
}
sixtythird_2006 <- df_sample[df_sample$Client.ID == "63rd",] 
ohio_2007 <- df_sample[df_sample$Client.ID == "Ohio",] 
thirtyfirst_2007 <- df_sample[df_sample$Client.ID == "31st" & df_sample$Year == "2007",] 
montrose_2008 <- df_sample[df_sample$Client.ID == "Montrose" & df_sample$Year == "2008",] 
thirtyninth_2009 <- df_sample[df_sample$Client.ID == "39th",] 
southshore_2010 <- df_sample[df_sample$Client.ID == "South Shore",] 
leone_2010 <- df_sample[df_sample$Client.ID == "Leone",] 
calumet_2011 <- df_sample[df_sample$Client.ID == "Calumet",] 
thirtyfirst_2011 <- df_sample[df_sample$Client.ID == "31st" & df_sample$Year == "2011",] 
montrose_2014 <- df_sample[df_sample$Client.ID == "Montrose" & df_sample$Year == "2014",] 

sixtythird_2006 <- gather(sixtythird_2006, "hour", "hourly_value", c(64:423))
sixtythird_2006 <- separate(sixtythird_2006, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

ohio_2007 <- gather(ohio_2007, "hour", "hourly_value", c(64:423))
ohio_2007 <- separate(ohio_2007, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

thirtyfirst_2007 <- gather(thirtyfirst_2007, "hour", "hourly_value", c(64:423))
thirtyfirst_2007 <- separate(thirtyfirst_2007, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

montrose_2008 <- gather(montrose_2008, "hour", "hourly_value", c(64:423))
montrose_2008 <- separate(montrose_2008, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

thirtyninth_2009 <- gather(thirtyninth_2009, "hour", "hourly_value", c(64:423))
thirtyninth_2009 <- separate(thirtyninth_2009, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

southshore_2010 <- gather(southshore_2010, "hour", "hourly_value", c(64:423))
southshore_2010 <- separate(southshore_2010, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

leone_2010 <- gather(leone_2010, "hour", "hourly_value", c(64:423))
leone_2010 <- separate(leone_2010, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

calumet_2011 <- gather(calumet_2011, "hour", "hourly_value", c(64:423))
calumet_2011 <- separate(calumet_2011, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

thirtyfirst_2011 <- gather(thirtyfirst_2011, "hour", "hourly_value", c(64:423))
thirtyfirst_2011 <- separate(thirtyfirst_2011, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")

montrose_2014 <- gather(montrose_2014, "hour", "hourly_value", c(64:423))
montrose_2014 <- separate(montrose_2014, "hour", c("hourly_measure", "interval", "hour"), sep="\\_")
