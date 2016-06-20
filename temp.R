`technical score` <- read.csv2("data/MATCHING-Table 1.csv")
colnames(`technical score`)[colnames(`technical score`)=="FI.IMPACT.Ids"] <- "FI.IMPACT.IDENTIFIER"

`projects scores` <- merge(`projects-surveys`, `technical score`, all.x = TRUE)
remove(`technical score`, `projects-surveys`)