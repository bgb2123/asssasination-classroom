#Install Libraries
library(jsonlite)
library(sp)
library(rgdal)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dlm)

#Don't forget to set your working directory, wherever your files are.
setwd("~/Desktop") 


library(RColorBrewer)
percs <- c(0, 0.5, 0.75, 0.9, 0.95, 0.975, 1)
breaks <- quantile(1:nrow(dfMerged), percs)
cols <- brewer.pal(length(percs) - 1, "YlOrRd")

#Read in each file. 
#Starting from the first file, date 18th August.  
first <- read_json("gps/20200818114606.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates <- first[["features"]][["geometry"]][["coordinates"]]

lat <- sapply(strsplit(as.character(coordinates)," "), "[[", 2)
lat <- gsub("\\)", "", lat)
lat <- as.numeric(lat)

long <- sapply(strsplit(as.character(coordinates)," "), "[[", 1)
long <- gsub("c\\(", "", long)
long <- gsub(",", "", long)
long <- as.numeric(long)

#Modify time vector and adjust for timezone.
time <- first[["features"]][["properties"]][["time"]]
time_1 <- as.POSIXct(strptime(time, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create data frame of coordinates and time values.
df_1 <- data.frame(lat, long, time_1)

#Include time differences between the recordings and the ones above. 
time.diff.1 <-lag(lead(df_1$time_1, 1) - df_1$time_1)
df_1 <- data.frame(lat, long, time_1, time.diff.1)

#Add speed values. 
df_1$speed <- first[["features"]][["properties"]][["speed"]]

#Going through the same steps for each week. 
#Second week.
second <- read_json("gps/20200819132607.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_2 <- second[["features"]][["geometry"]][["coordinates"]]

lat_2 <- sapply(strsplit(as.character(coordinates_2)," "), "[[", 2)
lat_2 <- gsub("\\)", "", lat_2)
lat_2 <- as.numeric(lat_2)

long_2 <- sapply(strsplit(as.character(coordinates_2)," "), "[[", 1)
long_2 <- gsub("c\\(", "", long_2)
long_2 <- gsub(",", "", long_2)
long_2 <- as.numeric(long_2)

#Modify time vector
time_2 <- second[["features"]][["properties"]][["time"]]
time_2 <- as.POSIXct(strptime(time_2, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

df_2 <- data.frame(lat_2, long_2, time_2)
time.diff.2 <-lag(lead(df_2$time_2, 1) - df_2$time_2)
df_2 <- data.frame(lat_2, long_2, time_2, time.diff.2)

#Add speed values. 
df_2$speed <- second[["features"]][["properties"]][["speed"]]

#Third week
third <- read_json("gps/20200820151044.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_3 <- third[["features"]][["geometry"]][["coordinates"]]

lat_3 <- sapply(strsplit(as.character(coordinates_3)," "), "[[", 2)
lat_3 <- gsub("\\)", "", lat_3)
lat_3 <- as.numeric(lat_3)

long_3 <- sapply(strsplit(as.character(coordinates_3)," "), "[[", 1)
long_3 <- gsub("c\\(", "", long_3)
long_3 <- gsub(",", "", long_3)
long_3 <- as.numeric(long_3)

#Modify time vector
time_3 <- third[["features"]][["properties"]][["time"]]
time_3 <- as.POSIXct(strptime(time_3, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create data frame for 3rd week. 
df_3 <- data.frame(lat_3, long_3, time_3)
time.diff.3 <-lag(lead(df_3$time_3, 1) - df_3$time_3)
df_3 <- data.frame(lat_3, long_3, time_3, time.diff.3)

#Add speed values. 
df_3$speed <- third[["features"]][["properties"]][["speed"]]

#Fourth week
fourth <- read_json("gps/20200821111447.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_4 <- fourth[["features"]][["geometry"]][["coordinates"]]

lat_4 <- sapply(strsplit(as.character(coordinates_4)," "), "[[", 2)
lat_4 <- gsub("\\)", "", lat_4)
lat_4 <- as.numeric(lat_4)

long_4 <- sapply(strsplit(as.character(coordinates_4)," "), "[[", 1)
long_4 <- gsub("c\\(", "", long_4)
long_4 <- gsub(",", "", long_4)
long_4 <- as.numeric(long_4)

#Modify time vector
time_4 <- fourth[["features"]][["properties"]][["time"]]
time_4<- as.POSIXct(strptime(time_4, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create fourth week's data frame 
df_4 <- data.frame(lat_4, long_4, time_4)
time.diff.4 <-lag(lead(df_4$time_4, 1) - df_4$time_4)
df_4 <- data.frame(lat_4, long_4, time_4, time.diff.4)

#Add speed values. 
df_4$speed <- fourth[["features"]][["properties"]][["speed"]]

#Fifth week
fifth <- read_json("gps/20200824130857.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_5 <- fifth[["features"]][["geometry"]][["coordinates"]]

lat_5 <- sapply(strsplit(as.character(coordinates_5)," "), "[[", 2)
lat_5 <- gsub("\\)", "", lat_5)
lat_5 <- as.numeric(lat_5)

long_5 <- sapply(strsplit(as.character(coordinates_5)," "), "[[", 1)
long_5 <- gsub("c\\(", "", long_5)
long_5 <- gsub(",", "", long_5)
long_5 <- as.numeric(long_5)

#Modify time vector
time_5 <- fifth[["features"]][["properties"]][["time"]]
time_5<- as.POSIXct(strptime(time_5, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create fifth week's data frame.
df_5 <- data.frame(lat_5, long_5, time_5)
time.diff.5 <-lag(lead(df_5$time_5, 1) - df_5$time_5)
df_5<- data.frame(lat_5, long_5, time_5, time.diff.5)

#Add speed values. 
df_5$speed <- fifth[["features"]][["properties"]][["speed"]]

#Sixth week
sixth <- read_json("gps/20200825121346.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_6 <- sixth[["features"]][["geometry"]][["coordinates"]]

lat_6 <- sapply(strsplit(as.character(coordinates_6)," "), "[[", 2)
lat_6 <- gsub("\\)", "", lat_6)
lat_6 <- as.numeric(lat_6)

long_6 <- sapply(strsplit(as.character(coordinates_6)," "), "[[", 1)
long_6 <- gsub("c\\(", "", long_6)
long_6 <- gsub(",", "", long_6)
long_6 <- as.numeric(long_6)

#Modify time vector
time_6 <- sixth[["features"]][["properties"]][["time"]]
time_6<- as.POSIXct(strptime(time_6, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create sixth week's data frame.
df_6 <- data.frame(lat_6, long_6, time_6)
time.diff.6 <-lag(lead(df_6$time_6, 1) - df_6$time_6)
df_6<- data.frame(lat_6, long_6, time_6, time.diff.6)

#Add speed values. 
df_6$speed <- sixth[["features"]][["properties"]][["speed"]]

#Seventh week
seven <- read_json("gps/20200826131614.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_7 <- seven[["features"]][["geometry"]][["coordinates"]]

lat_7 <- sapply(strsplit(as.character(coordinates_7)," "), "[[", 2)
lat_7 <- gsub("\\)", "", lat_7)
lat_7 <- as.numeric(lat_7)

long_7 <- sapply(strsplit(as.character(coordinates_7)," "), "[[", 1)
long_7 <- gsub("c\\(", "", long_7)
long_7 <- gsub(",", "", long_7)
long_7 <- as.numeric(long_7)

#Modify time vector
time_7 <- seven[["features"]][["properties"]][["time"]]
time_7 <- as.POSIXct(strptime(time_7, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create sixth week's data frame.
df_7 <- data.frame(lat_7, long_7, time_7)
time.diff.7 <-lag(lead(df_7$time_7, 1) - df_7$time_7)
df_7<- data.frame(lat_7, long_7, time_7, time.diff.7)

#Add speed values. 
df_7$speed <- seven[["features"]][["properties"]][["speed"]]

#Eighth week
eight <- read_json("gps/20200827113234.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_8 <- eight[["features"]][["geometry"]][["coordinates"]]

lat_8 <- sapply(strsplit(as.character(coordinates_8)," "), "[[", 2)
lat_8 <- gsub("\\)", "", lat_8)
lat_8 <- as.numeric(lat_8)

long_8 <- sapply(strsplit(as.character(coordinates_8)," "), "[[", 1)
long_8 <- gsub("c\\(", "", long_8)
long_8 <- gsub(",", "", long_8)
long_8 <- as.numeric(long_8)

#Modify time vector
time_8 <- eight[["features"]][["properties"]][["time"]]
time_8 <- as.POSIXct(strptime(time_8, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create eighth week's data frame.
df_8 <- data.frame(lat_8, long_8, time_8)
time.diff.8 <-lag(lead(df_8$time_8, 1) - df_8$time_8)
df_8<- data.frame(lat_8, long_8, time_8, time.diff.8)

#Add speed values. 
df_8$speed <- eight[["features"]][["properties"]][["speed"]]

#Ninth week
ninth <- read_json("gps/20200828122627.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_9 <- ninth[["features"]][["geometry"]][["coordinates"]]

lat_9 <- sapply(strsplit(as.character(coordinates_9)," "), "[[", 2)
lat_9 <- gsub("\\)", "", lat_9)
lat_9 <- as.numeric(lat_9)

long_9 <- sapply(strsplit(as.character(coordinates_9)," "), "[[", 1)
long_9 <- gsub("c\\(", "", long_9)
long_9 <- gsub(",", "", long_9)
long_9 <- as.numeric(long_9)

#Modify time vector
time_9 <- ninth[["features"]][["properties"]][["time"]]
time_9 <- as.POSIXct(strptime(time_9, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create ninth week's data frame.
df_9 <- data.frame(lat_9, long_9, time_9)
time.diff.9 <-lag(lead(df_9$time_9, 1) - df_9$time_9)
df_9<- data.frame(lat_9, long_9, time_9, time.diff.9)

#Add speed values. 
df_9$speed <- ninth[["features"]][["properties"]][["speed"]]

#Tenth week
tenth <- read_json("gps/20200828130816.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_10 <- tenth[["features"]][["geometry"]][["coordinates"]]

lat_10 <- sapply(strsplit(as.character(coordinates_10)," "), "[[", 2)
lat_10 <- gsub("\\)", "", lat_10)
lat_10 <- as.numeric(lat_10)

long_10 <- sapply(strsplit(as.character(coordinates_10)," "), "[[", 1)
long_10 <- gsub("c\\(", "", long_10)
long_10 <- gsub(",", "", long_10)
long_10 <- as.numeric(long_10)

#Modify time vector
time_10 <- tenth[["features"]][["properties"]][["time"]]
time_10 <- as.POSIXct(strptime(time_10, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create tenth week's data frame.
df_10 <- data.frame(lat_10, long_10, time_10)
time.diff.10 <-lag(lead(df_10$time_10, 1) - df_10$time_10)
df_10<- data.frame(lat_10, long_10, time_10, time.diff.10)

#Add speed values. 
df_10$speed <- tenth[["features"]][["properties"]][["speed"]]

#Eleventh week
eleven <- read_json("gps/20200831115147.geojson", simplifyVector = T)

#Split coordinates to latitude and longitude 
coordinates_11 <- eleven[["features"]][["geometry"]][["coordinates"]]

lat_11 <- sapply(strsplit(as.character(coordinates_11)," "), "[[", 2)
lat_11 <- gsub("\\)", "", lat_11)
lat_11 <- as.numeric(lat_11)

long_11 <- sapply(strsplit(as.character(coordinates_11)," "), "[[", 1)
long_11 <- gsub("c\\(", "", long_11)
long_11 <- gsub(",", "", long_11)
long_11 <- as.numeric(long_11)

#Modify time vector
time_11 <- eleven[["features"]][["properties"]][["time"]]
time_11 <- as.POSIXct(strptime(time_11, tz = "MST", format = "%Y-%m-%dT%H:%M:%OS"))

#Create eleventh week's data frame.
df_11 <- data.frame(lat_11, long_11, time_11)
time.diff.11 <-lag(lead(df_11$time_11, 1) - df_11$time_11)
df_11<- data.frame(lat_11, long_11, time_11, time.diff.11)

#Add speed values. 
df_11$speed <- eleven[["features"]][["properties"]][["speed"]]

#Combine all dfs in a list and change column names. 
my.list <- list(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9,df_10, df_11)
my.list <- lapply(my.list, 
                  function(x) {names(x) <- c("lat", "long", "Time", "time_difference", "speed")
                  return(x)})

#Merge lists into one mega dataset. 
dfMerged <- do.call("rbind", my.list)

#Add combined coordinates to merged sets.
dfMerged$coordinates <- paste(dfMerged$lat, ",", dfMerged$long)
#Used this to get the most frequent coordinate, 
#taking latitude and longitude values of each recording as a whole.  
#Creating a mode function for coordinates in some sense.
tt.merged <- table(dfMerged$coordinates)
tt.merged[tt.merged==max(tt.merged)]

#Add weather 
weather <- read.csv("Weather.csv", stringsAsFactors = FALSE, header = TRUE)
weather <- weather[,2:3]

#Format weather date and time. 
weather$Time <- as.POSIXct(lubridate::parse_date_time(weather$Date_Time,tz ="MST", orders='mdyhm'))
weather$Temp <- as.numeric(weather$Temp)

#Convert the time formats of both data sets so that they match.
weather$Time <- format(weather$Time,"%D-%H")
weather$Time <- as.POSIXct(weather$Time)
dfMerged$Time <- format(dfMerged$Time,"%D-%H")
dfMerged$Time <- as.POSIXct(as.character(dfMerged$Time))

#Merge them together by their timestamps.
combined <- left_join(df_time,weather,by='Time') %>%
  mutate(time = lubridate::parse_date_time(time,orders='mdyh'))

#Create hex plot 
#Shows us frequencies of the coordinates. 
library(ggplot2)
first_plot <- ggplot(dfMerged, mapping = aes(lat, long))+
  geom_hex()+
  geom_point(x=46.8600804 , y=-113.9839519, color = "red")+ #Points created according to mode values.
  geom_point(x=46.8616699 , y=-113.9855465, color = "red")+
  geom_point(x=46.8689186 , y=-113.9893428, color = "red")+
  geom_point(x=46.8701026 , y=-113.9918634, color = "red")+
  geom_point(x=46.8713271 , y=-113.9952979, color = "red")+
  geom_point(x=46.8734358 , y=-113.9964836, color = "red")+
  geom_point(x=46.8743452 , y=-113.9940895, color = "red")+
  geom_point(x=46.8821394 , y=-113.998802, color = "red")+
  geom_point(x=46.88427 , y=-113.9968601, color = "red")
first_plot
#However, the modes of the repeated coordinates do not match the counts of the graph . 

#We can take a look at just week 2 to see what's going on. 
my.list.2 <- list(df_6, df_7, df_8, df_9, df_10, df_11)
my.list.2 <- lapply(my.list.2, 
                    function(x) {names(x) <- c("lat.2", "long.2", "time_MST.2","time_difference.2", "speed")
                    return(x)})

#Merge second week's list into one dataset.  
dfMerged.2 <- do.call("rbind", my.list.2)

#Add coordinate to merged set_2.
dfMerged.2$coordinates.2 <- paste(dfMerged.2$lat.2, ",", dfMerged.2$long.2)
tt.merged.2 <- table(dfMerged.2$coordinates.2)
tt.merged.2[tt.merged.2==max(tt.merged.2)]

#Plot second week's hexplot only.
second_plot <- ggplot(dfMerged.2, mapping = aes(lat.2, long.2))+
  geom_hex()+
  geom_point(x=46.8600804 , y=-113.9839519, color = "red")+
  geom_point(x=46.8616699 , y=-113.9855465, color = "red")+
  geom_point(x=46.8689186 , y=-113.9893428, color = "red")+
  geom_point(x=46.8713271 , y=-113.9926369, color = "red")+
  geom_point(x=46.8724778 , y=-113.9952979, color = "red")+
  geom_point(x=46.8734358 , y=-113.9964836, color = "red")+
  geom_point(x=46.8743452 , y=-113.9940895, color = "red")+
  geom_point(x=46.8821394 , y=-113.998802, color = "red")+
  geom_point(x=46.88427 , y=-113.9968601, color = "red")
second_plot
#The brightest hexagons do not correspond to the most repeated coordinates. 

#Removing the missing values find the median. 
second_per_rec =median(dfMerged$time_difference, na.rm=TRUE)
#The median time difference is 8 seconds.There are periods of inactivity, 
#where the subject is stationary. These indicate times that we can't bomb him.
print(paste("Number of seconds per record:", second_per_rec))
print(paste("Number of records per second:", 1/second_per_rec))
#When the target is stationary for over 2 minutes, he's considered safe. 
#We will record this as the number of sessions of inactivity.
inactive <-length(dfMerged[dfMerged$time_difference>120,])
#In this dataset, he's inactive 6 times. 

#Spatial Transformation
spat_df <-SpatialPointsDataFrame(coords=dfMerged[,c("long", "lat")],
                                 data=dfMerged['Time'],
                                 proj4string=CRS("+proj=longlat +datum=WGS84 +units=m"))
# This step converts the longitude/latitude -> UTM
utm_df <-spTransform(spat_df, CRSobj = "+proj=utm +zone=12 +datum=WGS84 +units=m") 
utm_coords <-coordinates(utm_df)

#Find the dates when he is stationary for more than 2 minutes.  
which(dfMerged$time_difference>120)

dfMerged[81,]$Time
dfMerged[214,]$Time
dfMerged[565,]$Time
dfMerged[1022,]$Time
dfMerged[1723,]$Time
dfMerged[2404,]$Time
dfMerged[3178,]$Time
dfMerged[3535,]$Time
dfMerged[3984,]$Time
dfMerged[4661,]$Time
dfMerged[5353,]$Time
dfMerged[5519,]$Time
dfMerged[5529,]$Time
#Aside from August 31st, he's stationary at the night when he's back home. 

dfMerged[5353,]$Time
#GPS data reveals he's at the University of Montana at that time. 
#Removing these times when he's at the university because he can't be bombed when stationary. 
dfMerged <- dfMerged[-c(5353:5529),]
spat_df <- spat_df[-c(5353:5529),]

#Manually create inbound and outbound trips according to your coordinates.
#0 = inbound (coming home from work), 1 = outbound (leaving home for work)
spat_df$session <- rep(NA,nrow(spat_df@data))
spat_df$session[1:80] <- 0
spat_df$session[81:90] <- 1
spat_df$session[91:213] <- 0
spat_df$session[214:339] <- 1
spat_df$session[340:564] <- 0
spat_df$session[565:693] <- 1
spat_df$session[694:1021] <- 0
spat_df$session[1022:1360] <- 1
spat_df$session[1361:1722] <- 0
spat_df$session[1723:2044] <- 1
spat_df$session[2045:2403] <- 0
spat_df$session[2404:2755] <- 1
spat_df$session[2756:3117] <- 0
spat_df$session[3118:3533] <- 1
spat_df$session[3534:3896] <- 0
spat_df$session[3897:4245] <- 1
spat_df$session[4246:4512] <- 0
spat_df$session[4513:4573] <- 1
spat_df$session[4574:4903] <- 0
spat_df$session[4904:5264] <- 1
spat_df$session[5265:5352] <- 0

outbound <- spat_df[spat_df[!is.na(spat_df$session==0),]$session==0,]
inbound <- spat_df[spat_df[!is.na(spat_df$session==1),]$session==1,]

#Plot the inbound and outbound plots separately.
plot(outbound$long,outbound$lat
points(inbound$long,inbound$lat,col='red')

#Fit DLM.
gps_variance <- 20^2
v_mat <-diag(c(gps_variance, gps_variance))
f_mat <-matrix(c(1,0,0,0, 0,1,0,0), nrow=2, byrow = TRUE)
dt <- 8
g_mat <-matrix(c(1, 0, dt, 0,
                 0, 1, 0, dt,
                 0, 0, 1, 0,
                 0, 0, 0, 1), byrow=TRUE, ncol=4)
avg_walk_speed_m_per_sec <- median(dfMerged$speed, na.rm=TRUE)
dlm_spec <-dlm(FF= f_mat,
               GG= g_mat,
               V = v_mat,
               W =diag(c(5, 5, 1, 1)^2),
               m0 =matrix(c(utm_coords[1, ],rep(avg_walk_speed_m_per_sec/8, 2)),ncol=1),
               # A vector by R defaults is a k by 1 matrix.
               C0 =diag(rep(10^2, 4)))
dlm_filter_mod <-dlmFilter(utm_coords, dlm_spec)
dlm_smooth_mod <-dlmSmooth(dlm_filter_mod)

#Plot Kalman Filter, Smoother and Raw data into one plot to see how they're doing.
#See the first 90 values for a clearer picture.(DAY1, WEEK1)
plot(cbind(coordinates(spat_df)[1:90, ],
           dlm_filter_mod$m[1:90, 1:2], 
           dlm_smooth_mod$s[1:90, 1:2]),ype='p', col =c("black", "red", "blue"), xlab="UTM X", ylab="UTM Y")
    # xaxs="i",yaxs="i",xaxt="n",yaxt="n")
legend("topright", col =c("black", "red", "blue"),pch = 1, 
       legend =c("raw", "kalman filter","kalman smoother"))
#axis(1, xaxp=c(46.85,46.9,10))
$axis(2, yaxp=c(-114.07,-113.98,10))
