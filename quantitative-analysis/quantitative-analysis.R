# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

source("../read-and-clean-data.R")
source("../colors.R")

library(effsize)
library(stringr)
library(sqldf)

# merge with host data

reuterkiez_hosts <- fread("../airbnb-data/reuterkiez_2017-03-22_host_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
reuterkiez_hosts <- reuterkiez_hosts[,c(
 "host_id",
 "created_at",
 "recommendation_count",
 "listings_count"
)]
names(reuterkiez_hosts) <- c(
  "host_id",
  "host_created_at",
  "host_recommendation_count",
  "host_listings_count"
)

# de-duplicate host data
nrow(reuterkiez_hosts)
# 3070
reuterkiez_hosts <- unique(reuterkiez_hosts)
nrow(reuterkiez_hosts)
# 2818

n_reuterkiez
# 750
reuterkiez <- merge(reuterkiez, reuterkiez_hosts, by.x="host_id", by.y="host_id", all.x=TRUE, all.y=FALSE)
remove(reuterkiez_hosts)
n_reuterkiez <- nrow(reuterkiez)
n_reuterkiez
# 750

unique_hosts_reuterkiez <- unique(reuterkiez[,c("host_id", "host_created_at")])
nrow(unique_hosts_reuterkiez)
# 708
length(which(is.na(unique_hosts_reuterkiez$host_created_at)))
# 2 --> no data available for two hosts

kudamm_kantstrasse_hosts <- fread("../airbnb-data/kudamm-kantstrasse_2017-03-22_host_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
kudamm_kantstrasse_hosts <- kudamm_kantstrasse_hosts[,c(
  "host_id",
  "created_at",
  "recommendation_count",
  "listings_count"
)]
names(kudamm_kantstrasse_hosts) <- c(
  "host_id",
  "host_created_at",
  "host_recommendation_count",
  "host_listings_count"
)

# de-duplicate host data
nrow(kudamm_kantstrasse_hosts)
# 435
kudamm_kantstrasse_hosts <- unique(kudamm_kantstrasse_hosts)
nrow(kudamm_kantstrasse_hosts)
# 380

n_kudamm_kantstrasse
# 210
kudamm_kantstrasse <- merge(kudamm_kantstrasse, kudamm_kantstrasse_hosts, by.x="host_id", by.y="host_id", all.x=TRUE, all.y=FALSE)
remove(kudamm_kantstrasse_hosts)
n_kudamm_kantstrasse <- nrow(kudamm_kantstrasse)
n_kudamm_kantstrasse
# 210

unique_hosts_kudamm_kantstrasse <- unique(kudamm_kantstrasse[,c("host_id", "host_created_at")])
nrow(unique_hosts_kudamm_kantstrasse)
# 190
length(which(is.na(unique_hosts_kudamm_kantstrasse$host_created_at)))
# 1 --> no data available for two hosts

merged_hosts <- unique(data.frame(rbind(unique_hosts_reuterkiez, unique_hosts_kudamm_kantstrasse)))
nrow(merged_hosts)
# 898
length(which(is.na(merged_hosts$host_created_at)))
# 3 --> no data available for three hosts


# analze hosts

# parse timestamps
Sys.setenv(TZ='UTC')

reuterkiez$host_created_at <- as.POSIXct(reuterkiez$host_created_at, tz="UTC")
reuterkiez$host_days_since_creation <- as.integer(difftime("2017-03-22", reuterkiez$host_created_at, units="days"))

kudamm_kantstrasse$host_created_at <- as.POSIXct(kudamm_kantstrasse$host_created_at, tz="UTC")
kudamm_kantstrasse$host_days_since_creation <- as.integer(difftime("2017-03-22", kudamm_kantstrasse$host_created_at, units="days"))

# "age" of host accounts

reuterkiez_hosts <- unique(reuterkiez[,c("host_id", "host_days_since_creation", "host_listings_count")])
kudamm_kantstrasse_hosts <- unique(kudamm_kantstrasse[,c("host_id", "host_days_since_creation", "host_listings_count")])

summary(reuterkiez_hosts$host_days_since_creation/365)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.005479 1.655000 2.651000 2.788000 3.758000 7.203000        2 

sd(!is.na(reuterkiez_hosts$host_days_since_creation/365))
# 0.0531118

summary(kudamm_kantstrasse_hosts$host_days_since_creation/365)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.005479 1.068000 2.458000 2.526000 3.816000 7.052000        1 

sd(!is.na(kudamm_kantstrasse_hosts$host_days_since_creation/365))
# 0.07254763

# number of listings

summary(reuterkiez_hosts$host_listings_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   1.000   1.000   1.184   1.000  20.000       2 

sd(!is.na(reuterkiez_hosts$host_listings_count))
# 0.0531118

table(reuterkiez_hosts$host_listings_count)
# 1   2   3   4   7  10  20 
# 622  68  11   2   1   1   1 

reuterkiez_host_listings_count <- ifelse(reuterkiez_hosts$host_listings_count>3, 4, reuterkiez_hosts$host_listings_count) 
t_listing_count <- table(factor(reuterkiez_host_listings_count, levels=1:4))
t_listing_count
#   1   2   3   >3 
# 622  68  11   5

n <- length(which(!is.na(reuterkiez_host_listings_count)))
n
# 706

round(t_listing_count/n*100, digits=1)
#  1    2    3    >3
# 88.1  9.6  1.6  0.7

summary(kudamm_kantstrasse_hosts$host_listings_count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   1.000   2.608   1.000 218.000       1 

sd(!is.na(kudamm_kantstrasse_hosts$host_listings_count))
# 0.07254763

table(kudamm_kantstrasse_hosts$host_listings_count)
# 1   2   3   4   5   6   8  13 218 
# 144  29   6   2   4   1   1   1   1 

kudamm_kantstrasse_host_listings_count <- ifelse(kudamm_kantstrasse_hosts$host_listings_count>3, 4, kudamm_kantstrasse_hosts$host_listings_count) 
t_listing_count <- table(factor(kudamm_kantstrasse_host_listings_count, levels=1:4))
t_listing_count
#   1   2   3   >3
# 144  29   6  10

n <- length(which(!is.na(kudamm_kantstrasse_host_listings_count)))
n
# 189

round(t_listing_count/n*100, digits=1)
# 1    2    3      >3
# 76.2 15.3  3.2  5.3

# merge hosts

merged_hosts <- unique(data.frame(rbind(reuterkiez[,c("host_id", "host_listings_count")], kudamm_kantstrasse[,c("host_id", "host_listings_count")])))

n <- length(which(!is.na(merged_hosts$host_listings_count)))
n
# 895

table(merged_hosts$host_listings_count)
#   1   2   3   4   5   6   7   8  10  13  20 218 
# 766  97  17   4   4   1   1   1   1   1   1   1 

sum(merged_hosts[!is.na(merged_hosts$host_listings_count),]$host_listings_count)
# 1329

# 1
sum(merged_hosts[!is.na(merged_hosts$host_listings_count) & merged_hosts$host_listings_count==1,]$host_listings_count)
# 766 (57.6%)

# 2
sum(merged_hosts[!is.na(merged_hosts$host_listings_count) & merged_hosts$host_listings_count==2,]$host_listings_count)
# 194 (14.6%)

# 3
sum(merged_hosts[!is.na(merged_hosts$host_listings_count) & merged_hosts$host_listings_count==3,]$host_listings_count)
# 51 (3.8%)

# >3
sum(merged_hosts[!is.na(merged_hosts$host_listings_count) & merged_hosts$host_listings_count>3,]$host_listings_count)
# 318 (23.9%)

merged_hosts_listings_count <- ifelse(merged_hosts$host_listings_count>3, 4, merged_hosts$host_listings_count) 
t_merged_hosts_listings_count <- table(factor(merged_hosts_listings_count, levels=1:4))
t_merged_hosts_listings_count
#  1   2   3   >3 
# 766  97  17  15 

round(t_merged_hosts_listings_count/n*100, digits=1)
#  1    2    3    >3
# 85.6 10.8  1.9  1.7 

# analysis per listing, not per host

reuterkiez$host_listings_count_normalized <- ifelse(reuterkiez$host_listings_count>3, 4, reuterkiez$host_listings_count) 
kudamm_kantstrasse$host_listings_count_normalized <- ifelse(kudamm_kantstrasse$host_listings_count>3, 4, kudamm_kantstrasse$host_listings_count) 
merged <- c(reuterkiez$host_listings_count_normalized, kudamm_kantstrasse$host_listings_count_normalized)

# reuterkiez
t_listing_count <- table(factor(reuterkiez$host_listings_count_normalized, levels=1:4))
t_listing_count
#   1   2   3  >3 
# 622  91  20  15 

n <- length(which(!is.na(reuterkiez$host_listings_count_normalized)))
n
# 748

round(t_listing_count/n*100, digits=1)
#    1    2    3    >3 
# 83.2 12.2  2.7  2.0 

#kudamm-kantstrasse
t_listing_count <- table(factor(kudamm_kantstrasse$host_listings_count_normalized, levels=1:4))
t_listing_count
# 1   2   3   >3 
# 144  39   9  17 

n <- length(which(!is.na(kudamm_kantstrasse$host_listings_count_normalized)))
n
# 209

round(t_listing_count/n*100, digits=1)
# 1    2    3    >3 
# 68.9 18.7  4.3  8.1 

# merged
t_listing_count <- table(factor(merged, levels=1:4))
t_listing_count
# 1   2   3   >3
# 766 130  29  32

n <- length(which(!is.na(merged)))
n
# 957

round(t_listing_count/n*100, digits=1)
#  1    2    3    >3
# 80.0 13.6  3.0  3.3 


# analyze listings

table(reuterkiez$property_type)
# Apartment Bed & Breakfast     Condominium           House            Loft           Train 
#   717               1              17               7               7               1 

table(kudamm_kantstrasse$property_type)
# Apartment Bed & Breakfast     Condominium      Guesthouse          Hostel           House            Loft 
#      189               1               7               7               2               1               3 

table(reuterkiez$room_type)
# Entire home/apt    Private room     Shared room 
#   390             358               2 

n <- length(which(!is.na(reuterkiez$room_type)))
n
# 750

table(reuterkiez$room_type)/n*100
# Entire home/apt    Private room     Shared room 
#    52.0000000      47.7333333       0.2666667 

table(kudamm_kantstrasse$room_type)
# Entire home/apt    Private room     Shared room 
#   116              90               4 

n <- length(which(!is.na(kudamm_kantstrasse$room_type)))
n
# 210

table(kudamm_kantstrasse$room_type)/n*100
# Entire home/apt    Private room     Shared room 
#    55.238095       42.857143        1.904762


summary(reuterkiez$price_euro)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.00   30.00   45.00   51.65   60.00  530.00 

sd(reuterkiez$price_euro)
# 37.84264

summary(kudamm_kantstrasse$price_euro)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   38.00   55.00   73.71   85.00  600.00 

sd(kudamm_kantstrasse$price_euro)
# 64.88322

# TODO: mention that room types are (almost) equally distributed

length(which(!is.na(reuterkiez$price_euro)))
# 750

length(which(!is.na(kudamm_kantstrasse$price_euro)))
# 210

wilcox.test(kudamm_kantstrasse$price_euro, 
            reuterkiez$price_euro,
            alternative="two.sided",
            paired=FALSE, correct=TRUE)
# W = 99228, p-value = 7.964e-09
# alternative hypothesis: true location shift is not equal to 0

cohen.d(kudamm_kantstrasse$price_euro, # "treatment"
        reuterkiez$price_euro, # "control"
        paired=FALSE)
# d estimate: 0.4887574 (small)
# 95 percent confidence interval:
#   inf       sup 
# 0.3338250 0.6436898 

# plot boxplot
quartz(type="pdf", file="figures/boxplots_price.pdf", width=12, height=5) # prevents unicode issues in pdf
#pdf("figures/boxplots_price.pdf", width=12, height=5)
par(
  bg="white",
  mar = c(4.5, 3, 1, 1)+0.1, # subplot margins (bottom, left, top, right)
  omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1.3,
  cex.lab=1.3,
  cex.axis=1.3
)

boxplot(kudamm_kantstrasse$price_euro,
        reuterkiez$price_euro,
        names=c("City West", "Kreuzkölln"),
        outline=FALSE,
        horizontal = TRUE,
        col=gray_lighter,
        main=""
        #main="Daily price of retrieved listings"
)
title(xlab="Price (Euro)", font.lab=1)

dev.off()


# word count

# title

# normalize whitespaces
title_reuterkiez <- gsub("[[:space:]]", " ", reuterkiez$name)
# remove punctuation
title_reuterkiez <- gsub("[^A-Za-z0-9 ]*", "", title_reuterkiez)
# count words (see https://stackoverflow.com/a/26406796)
title_reuterkiez_wordcount <- str_count(title_reuterkiez, "\\S+")

summary(title_reuterkiez_wordcount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   4.000   5.000   5.163   6.000  10.000

sd(title_reuterkiez_wordcount)
# 1.467109

# normalize whitespaces
title_kudamm_kantstrasse <- gsub("[[:space:]]", " ", kudamm_kantstrasse$name)
# remove punctuation
title_kudamm_kantstrasse <- gsub("[^A-Za-z0-9 ]*", "", title_kudamm_kantstrasse)
# count words (see https://stackoverflow.com/a/26406796)
title_kudamm_kantstrasse_wordcount <- str_count(title_kudamm_kantstrasse, "\\S+")

summary(title_kudamm_kantstrasse_wordcount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   4.000   5.000   4.848   6.000   9.000 

sd(title_kudamm_kantstrasse_wordcount)
# 1.609071


# description (including house rules)

description_reuterkiez <- gsub("[[:space:]]", " ", reuterkiez$description)
# remove punctuation
description_reuterkiez <- gsub("[^A-Za-z0-9 ]*", "", description_reuterkiez)
# count words (see https://stackoverflow.com/a/26406796)
description_reuterkiez_wordcount <- str_count(description_reuterkiez, "\\S+")

summary(description_reuterkiez_wordcount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.0    49.0   138.5   173.7   241.0  1304.0 

sd(description_reuterkiez_wordcount)
# 158.4401

description_kudamm_kantstrasse <- gsub("[[:space:]]", " ", kudamm_kantstrasse$description)
# remove punctuation
description_kudamm_kantstrasse <- gsub("[^A-Za-z0-9 ]*", "", description_kudamm_kantstrasse)
# count words (see https://stackoverflow.com/a/26406796)
description_kudamm_kantstrasse_wordcount <- str_count(description_kudamm_kantstrasse, "\\S+")

summary(description_kudamm_kantstrasse_wordcount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0    43.0   117.5   197.6   236.2  2091.0

sd(description_kudamm_kantstrasse_wordcount)
# 273.9813

# listing count per host in analyzed areas

reuterkiez_listings_per_host <- sqldf("SELECT host_id, count(room_id) as listing_count FROM reuterkiez GROUP BY host_id")
table(reuterkiez_listings_per_host$listing_count)
# 1   2   3   4 
# 675  27   3   3 

kudamm_kantstrasse_listings_per_host <- sqldf("SELECT host_id, count(room_id) as listing_count FROM kudamm_kantstrasse GROUP BY host_id")
table(kudamm_kantstrasse_listings_per_host$listing_count)
# 1   2   3   8 
# 177  11   1   1 

merged_listings <- data.frame(rbind(reuterkiez, kudamm_kantstrasse))
listings_per_host <- sqldf("SELECT host_id, count(room_id) as listing_count FROM merged_listings_per_host GROUP BY host_id")
table(listings_per_host$listing_count)
#   1   2   3   4   8 
# 852  38   4   3   1

