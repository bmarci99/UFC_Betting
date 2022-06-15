library(RCurl)
library(stringr)
library(dplyr)
library(tidyr)
library("measurements")
library(lubridate)


## Reading In
x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/raw_fighter_details.csv")
fighter_details <- read.csv(text = x)[,1:6]

x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/raw_total_fight_data.csv")
df<- read.csv(text = x, sep = ';')

## attempt and landed
columns = c('R_SIG_STR.', 'B_SIG_STR.', 'R_TOTAL_STR.', 'B_TOTAL_STR.',
           'R_TD', 'B_TD', 'R_HEAD', 'B_HEAD', 'R_BODY','B_BODY', 'R_LEG', 'B_LEG', 
           'R_DISTANCE', 'B_DISTANCE', 'R_CLINCH','B_CLINCH', 'R_GROUND', 'B_GROUND')


for(i in 1:length(columns)) {                                        
  df = df %>% separate(columns[i], c(paste(columns[i],"_att"), paste(columns[i],"_landed")), " of ")                                                # Print results
}


## Percentages to fractions
pct_columns = c('R_SIG_STR_pct','B_SIG_STR_pct', 'R_TD_pct', 'B_TD_pct')

for(i in 1:length(pct_columns)) {                                        
  df[,pct_columns[i]] =  substr(df[,pct_columns[i]],1,nchar(df[,pct_columns[i]])-1)
  df[,pct_columns[i]] = as.numeric(df[,pct_columns[i]])/ 100
}


## Fighter Division

df$Fight_type =  substr(df$Fight_type,1,nchar(df$Fight_type)-5)
df$Fight_type = gsub(" Tournament Title", "", df$Fight_type)
df$Fight_type = gsub(" Title", "", df$Fight_type)
df$Fight_type = ifelse(grepl('Lightweight$', df$Fight_type), "Lightweight", df$Fight_type)
df$Fight_type = ifelse(grepl('Middleweight$', df$Fight_type), "Middleweight", df$Fight_type)
df$Fight_type = ifelse(grepl('Welterweight$', df$Fight_type), "Welterweight", df$Fight_type)
df$Fight_type = ifelse(grepl('Heavyweight$', df$Fight_type), "Heavyweight", df$Fight_type)
df$Fight_type = ifelse(grepl("Women's Bantamweight$", df$Fight_type), "Women's Bantamweight", df$Fight_type)
df$Fight_type = ifelse(grepl("Women's Featherweight$", df$Fight_type), "Women's Featherweight", df$Fight_type)
df$Fight_type = ifelse(grepl("Women's Flyweight$", df$Fight_type), "Women's Flyweight", df$Fight_type)
df$Fight_type = gsub("UFC ", "", df$Fight_type)
df$Fight_type = gsub("Interim ", "", df$Fight_type)
df$Fight_type = ifelse(grepl('^Ultimate ', df$Fight_type), word(df$Fight_type,-1), df$Fight_type)

df= df[!grepl("\\d", df$Fight_type),]



## Rounds

df$Format = gsub( " .*$", "", df$Format )
df$Format = gsub("No", 1, df$Format)


## Winners

for(i in 1:length(df$Winner)) {
  df$Winner[i] = ifelse(df$Winner[i] == df$R_fighter[i], "Red", "Blue")
}

## Fighter details data

fighter_details$Weight = gsub(" lbs.", "", fighter_details$Weight)

mat <- stringr::str_extract_all(fighter_details$Height, "\\d+", simplify = TRUE)
fighter_details$Height <- as.numeric(mat[, 1]) * 30.48 + as.numeric(mat[, 2]) * 2.54

fighter_details$Reach = gsub('"', '', fighter_details$Reach)


# merging the 2 datasets

joined1 = left_join(df, fighter_details, by = c("R_fighter" = "fighter_name"))
joined1 = joined1 %>% rename(R_Height = Height, R_Weight = Weight,R_Reach=Reach,R_Stance=Stance,R_DOB= DOB)

joined2 = left_join(joined1, fighter_details, by = c("B_fighter" = "fighter_name"))
joined2 = joined2 %>% rename(B_Height = Height, B_Weight = Weight, B_Reach=Reach, B_Stance=Stance, B_DOB= DOB)


# fixing dates and age

day_calc <- function(dates){
  x <- as.POSIXct(mdy(dates))
  return(floor(unclass(x)/86400))
}

joined2$days <- day_calc(joined2$date) # assign this to the dataset as days
joined2$month <- month(mdy(joined2$date)) # assign this to the dataset as month


joined2$age_B <- round((joined2$days - day_calc(joined2$B_DOB)) / 365,2)
joined2$age_R <- round((joined2$days - day_calc(joined2$R_DOB)) / 365,2)

joined2 = joined2[ , -which(names(joined2) %in% c("R_DOB","B_DOB","date"))]


## country
joined2$location <- sub('.*,\\s*', '', joined2$location)



write.csv(joined2, "final.csv")
