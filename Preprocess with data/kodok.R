library(RCurl)
library(stringr)

x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/data.csv")
data <- read.csv(text = x)



x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/preprocessed_data.csv")
prepdata <- read.csv(text = x)


x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/raw_fighter_details.csv")
fighter <- read.csv(text = x)


#x <- getURL("https://raw.githubusercontent.com/bmarci99/UFC_Betting/main/data/raw_total_fight_data.csv")
#total_fight <- read.csv(text = x, sep = ';')

data
prepdata
fighter


View(data)
View(prepdata)
View(fighter)

## data prep
data<-subset(data, Winner!="Draw")
# maybe get rid of the women too????



## Blue Fighter
b_cols <- names(data)[grep("^B_",names(data))]
b_dat <- data[,b_cols]
names(b_dat) <- str_replace(names(b_dat),"B_", "")

## Red Fighter
r_cols <- names(data)[grep("^R_",names(data))]
r_dat <- data[,r_cols]
names(r_dat) <- str_replace(names(r_dat),"R_", "")


opp_1 <- r_dat
names(opp_1) <- paste(names(opp_1), "_opp")
opp_2 <- b_dat
names(opp_2) <- paste(names(opp_2), "_opp")


dat_1 <- cbind.data.frame(b_dat, opp_1)
dat_1$Winner <- rep(0,nrow(dat_1))
dat_1$Winner[data$Winner == "Blue"] <- 1
dat_2 <- cbind.data.frame(r_dat, opp_2)
dat_2$Winner <- rep(0,nrow(dat_2))
dat_2$Winner[data$Winner == "Red"] <- 1

f_data <- rbind.data.frame(dat_1, dat_2)




# add remaining data (not related to fighters)
admindat <-  data[,c(3,4,5,7,8)]
admindat <- rbind.data.frame(admindat,admindat)

# combine all data
combined <- cbind(f_data,admindat)

# write csv
write.csv(combined,"latest.csv")
