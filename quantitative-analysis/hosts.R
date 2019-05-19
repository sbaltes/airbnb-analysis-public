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

library(stringr)

# read host data

reuterkiez_hosts <- fread("../airbnb-data/reuterkiez_2017-03-22_host_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "[]"))
reuterkiez_hosts <- reuterkiez_hosts[,c(
  "host_id",
  "created_at",
  "recommendation_count",
  "listings_count",
  "school",
  "work",
  "languages"
)]
names(reuterkiez_hosts) <- c(
  "host_id",
  "host_created_at",
  "host_recommendation_count",
  "host_listings_count",
  "school",
  "work",
  "languages"
)

# de-duplicate host data
nrow(reuterkiez_hosts)
# 3070
reuterkiez_hosts <- unique(reuterkiez_hosts)
nrow(reuterkiez_hosts)
# 2818


kudamm_kantstrasse_hosts <- fread("../airbnb-data/kudamm-kantstrasse_2017-03-22_host_data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "[]"))
kudamm_kantstrasse_hosts <- kudamm_kantstrasse_hosts[,c(
  "host_id",
  "created_at",
  "recommendation_count",
  "listings_count",
  "languages"
)]
names(kudamm_kantstrasse_hosts) <- c(
  "host_id",
  "host_created_at",
  "host_recommendation_count",
  "host_listings_count",
  "languages"
)

# de-duplicate host data
nrow(kudamm_kantstrasse_hosts)
# 435
kudamm_kantstrasse_hosts <- unique(kudamm_kantstrasse_hosts)
nrow(kudamm_kantstrasse_hosts)
# 380


# analyze languages
extract_languages <- function(input) {
  languages <- unlist(str_extract_all(input, "'\\S+'"))
  for (i in 1:length(languages)) {
    languages[i] <- str_remove_all(languages[i], "'")
  }
  return(languages)
}

get_unique_languages <- function(language_vector) {
  unique_languages <- character()
  for (i in 1:length(language_vector)) {
    languages <- extract_languages(language_vector[i])
    unique_languages <- c(unique_languages, languages)
  }
  unique_languages <- unique(unique_languages)
  return(unique_languages)
}

add_lanuages <- function(df) {
  unique_languages <- get_unique_languages(df$languages)
  
  for (language in unique_languages) {
    df[,language] <- as.logical(rep(FALSE, nrow(df)))
  }
  
  for (i in 1:nrow(df)) {
    languages <- unlist(str_extract_all(df[i]$languages, "'\\S+'"))
    for (j in 1:length(languages)) {
      languages[j] <- str_remove_all(languages[j], "'")
      df[i, languages[j]] <- TRUE 
    }
  }
  
  return(df)
}

count_languages <- function(df) {
  unique_languages <- get_unique_languages(df$languages)
  
  return(unlist(lapply(subset(df, select=unique_languages), function(l) {
    length(which(l))
  })))
}

plot_top10_languages <- function(language_info, name, main, max_y) {
  # plot boxplot
  quartz(type="pdf", file=paste0("figures/", name, ".pdf"), width=20, height=10) # prevents unicode issues in pdf
  par(
    bg="white",
    mar = c(4, 3.5, 2.5, 0)+0.1, # subplot margins (bottom, left, top, right)
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
  
  barplot(language_info[1:10],
          col=gray_selected,
          ylim=c(0, max_y),
          main=main
  )
  
  dev.off()
}


# reuterkiez

# remove hosts without language information
reuterkiez_hosts_languages <- reuterkiez_hosts[!is.na(reuterkiez_hosts$languages)]
nrow(reuterkiez_hosts_languages)
# 1303

# extract language info
reuterkiez_hosts_languages <- add_lanuages(reuterkiez_hosts_languages)

# export data
write.table(reuterkiez_hosts_languages, file="../airbnb-data/reuterkiez_hosts_languages.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# count languages
language_info <- count_languages(reuterkiez_hosts_languages)

language_info <- language_info[order(language_info, decreasing=TRUE)]
language_info
# English    Deutsch   Français    Español   Italiano  Português    Русский Nederlands    Svenska     Polski     Türkçe      Dansk      עברית       中文   Ελληνικά    العربية   Norsk     Magyar      Suomi     日本語 українська    Čeština     한국어      Hindi 
# 1281       1136        415        386        184         90         65         41         36         34         33         29         20          17         15         15    11        11          8          8          7          2          2          1 

plot_top10_languages(language_info, "reuterkiez_hosts_languages", "Host Languages (n=1,303)", 1500)


# kudamm

# remove hosts without language information
kudamm_kantstrasse_hosts_languages <- kudamm_kantstrasse_hosts[!is.na(kudamm_kantstrasse_hosts$languages)]
nrow(kudamm_kantstrasse_hosts_languages)
# 210

# extract language info
kudamm_kantstrasse_hosts_languages <- add_lanuages(kudamm_kantstrasse_hosts_languages)

# export data
write.table(kudamm_kantstrasse_hosts_languages, file="../airbnb-data/kudamm_kantstrasse_hosts_languages.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# count languages
language_info <- count_languages(kudamm_kantstrasse_hosts_languages)

language_info <- language_info[order(language_info, decreasing=TRUE)]
language_info
# English    Deutsch   Français    Español   Italiano    Русский     Polski  Português     Türkçe      עברית       中文    العربية українська      Dansk    Svenska   Ελληνικά     日本語 Nederland   Norsk      Hindi 
# 203        179         62         58         27         26         12          7          7          6          4          4          3          3          3          2          2          1      1          1 

plot_top10_languages(language_info, "kudamm_kantstrasse_hosts_languages", "Host Languages (n=210)", 250)
