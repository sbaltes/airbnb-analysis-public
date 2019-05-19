# read and clean data

library(data.table)

kudamm_kantstrasse <- fread("../airbnb-data/kudamm-kantstrasse_2017-03-22_filtered.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
n_kudamm_kantstrasse <- nrow(kudamm_kantstrasse)
n_kudamm_kantstrasse
# 212

kudamm_kantstrasse_listing_data <- fread("../airbnb-data/kudamm-kantstrasse_2017-03-22_listing_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
kudamm_kantstrasse_listing_data <- kudamm_kantstrasse_listing_data[,c(
  "listing_id",
  "host_id",
  "room_type",
  "property_type",
  "name",
  "description",
  "house_rules",
  "summary",
  "space",
  "access",
  "interaction",
  "neighborhood_overview",
  "transit",
  "notes",
  "amenities",
  "description_locale",
  "lat",
  "lng",
  "instant_bookable",
  "price_native",
  "cleaning_fee_native",
  "listing_security_deposit_native",
  "weekly_price_factor",
  "weekly_price_native",
  "listing_monthly_price_native",
  "monthly_price_factor",
  "listing_price_for_extra_person_native",
  "listing_weekend_price_native"
)]
names(kudamm_kantstrasse_listing_data) <- c(
  "listing_id",
  "host_id",
  "room_type",
  "property_type",
  "name",
  "description",
  "house_rules",
  "summary",
  "space",
  "access",
  "interaction",
  "neighborhood_overview",
  "transit",
  "notes",
  "amenities",
  "description_language",
  "lat",
  "lng",
  "instant_bookable",
  "price_euro",
  "cleaning_fee_euro",
  "security_deposit_euro",
  "weekly_price_factor",
  "weekly_price_euro",
  "monthly_price_euro",
  "monthly_price_factor",
  "price_for_extra_person_euro",
  "weekend_price_euro"
)

kudamm_kantstrasse <- merge(kudamm_kantstrasse[,c("room_id")], kudamm_kantstrasse_listing_data, by.x="room_id", by.y="listing_id", all.x=TRUE, all.y=FALSE)
remove(kudamm_kantstrasse_listing_data)
n_kudamm_kantstrasse <- nrow(kudamm_kantstrasse)
n_kudamm_kantstrasse
# 212

n_kudamm_kantstrasse_excluded <- nrow(kudamm_kantstrasse[is.na(kudamm_kantstrasse$description)])
n_kudamm_kantstrasse_excluded
# 2
kudamm_kantstrasse[is.na(kudamm_kantstrasse$description)]$room_id
# 14444635 17811565
# these listings have a name/title, but retrieval of description failed
# --> exclude from sample
kudamm_kantstrasse <- kudamm_kantstrasse[!is.na(kudamm_kantstrasse$description)]
n_kudamm_kantstrasse <- nrow(kudamm_kantstrasse)
n_kudamm_kantstrasse
# 210


reuterkiez <- fread("../airbnb-data/reuterkiez_2017-03-22_filtered.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
n_reuterkiez <- nrow(reuterkiez)
n_reuterkiez
# 753

reuterkiez_listing_data <- fread("../airbnb-data/reuterkiez_2017-03-22_listing_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
reuterkiez_listing_data <- reuterkiez_listing_data[,c(
  "listing_id",
  "host_id",
  "room_type",
  "property_type",
  "name",
  "description",
  "house_rules",
  "summary",
  "space",
  "access",
  "interaction",
  "neighborhood_overview",
  "transit",
  "notes",
  "amenities",
  "description_locale",
  "lat",
  "lng",
  "instant_bookable",
  "price_native",
  "cleaning_fee_native",
  "listing_security_deposit_native",
  "weekly_price_factor",
  "weekly_price_native",
  "listing_monthly_price_native",
  "monthly_price_factor",
  "listing_price_for_extra_person_native",
  "listing_weekend_price_native"
)]
names(reuterkiez_listing_data) <- c(
  "listing_id",
  "host_id",
  "room_type",
  "property_type",
  "name",
  "description",
  "house_rules",
  "summary",
  "space",
  "access",
  "interaction",
  "neighborhood_overview",
  "transit",
  "notes",
  "amenities",
  "description_language",
  "lat",
  "lng",
  "instant_bookable",
  "price_euro",
  "cleaning_fee_euro",
  "security_deposit_euro",
  "weekly_price_factor",
  "weekly_price_euro",
  "monthly_price_euro",
  "monthly_price_factor",
  "price_for_extra_person_euro",
  "weekend_price_euro"
)

reuterkiez <- merge(reuterkiez[,c("room_id")], reuterkiez_listing_data, by.x="room_id", by.y="listing_id", all.x=TRUE, all.y=FALSE)
remove(reuterkiez_listing_data)
n_reuterkiez <- nrow(reuterkiez)
n_reuterkiez
# 753

n_reuterkiez_excluded <- nrow(reuterkiez[is.na(reuterkiez$description)])
n_reuterkiez_excluded
# 3
reuterkiez[is.na(reuterkiez$description)]$room_id
# 1750719 17313156 17785028
# these listings have a name/title, but retrieval of description failed
# --> exclude from sample
reuterkiez <- reuterkiez[!is.na(reuterkiez$description)]
n_reuterkiez <- nrow(reuterkiez)
n_reuterkiez
# 750

n <- n_kudamm_kantstrasse + n_reuterkiez + n_kudamm_kantstrasse_excluded +  n_reuterkiez_excluded
n
# 965
n <- n_kudamm_kantstrasse + n_reuterkiez
n
# 960
