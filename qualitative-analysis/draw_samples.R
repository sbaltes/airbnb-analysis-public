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

# draw samples
kudamm_kantstrasse_sample <- kudamm_kantstrasse[sample(n_kudamm_kantstrasse, 10),]
reuterkiez_sample <- reuterkiez[sample(n_reuterkiez, 100),]

# write sampled posts to CSV files
write.table(kudamm_kantstrasse_sample, file="data/kudamm-kantstrasse_sample_10_1.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(reuterkiez_sample, file="data/reuterkiez_sample_100_1.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
