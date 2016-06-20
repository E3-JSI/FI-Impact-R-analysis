rm(list = ls()) # clear workspace
library(reshape)
library(ggplot2)
library(data.table)
source("utility.R")

# Import data
source("import")

# Merge data and clean up
accelerators <- merge(accelerators, `accelerators shorthand`)
db <- merge(surveys, accelerators, all.x = TRUE)
db <- merge(db, `technical scores`, all.x = TRUE)
db['additional-funding'] = 0 # 
for (id in additional.funding$V1) db[db$FI.IMPACT.IDENTIFIER == id, 'additional-funding'] = 'x'
remove(accelerators, `accelerators shorthand`, surveys)

# Import connectivity computations 
source("partners/partners.R")

# Compute practice scores
practice.scores.all <- round(get_ranking_matrix(scores_list('benchmark'), indicators_list('binary'), 1, 0), 2)
practice.scores.top <- round(get_ranking_matrix(scores_list('benchmark'), indicators_list('binary'), 0.3, 0), 2)
practice.scores.list <- get_practice_scores_list(scores_list('benchmark'), indicators_list('binary'), 0.3, 0)

# Compute ordinal practice and score correlations
source("correlations.R")
boxplots = c('Accelerator', indicators_list('category'), indicators_list('binary'))
save_boxplots(db, scores_list('benchmark'), boxplots)
save_boxplot_sample_sizes(db, scores_list('benchmark'), boxplots)
  
# Score overviews
overview_all = overview(db)
overview_growth = overview_mm(db)
write.csv(overview_all, file = "overview.csv")
write.csv(overview_growth, file = "overview_growth.csv")

# overview_acc = overview_accelerators(db)
# write_overview_pdf()

# Practice scores
for (score in scores_list('benchmark')) {
  # jpeg(filename=paste("histogram/", pretty(score), ".jpg", sep = ""))
  # s = get_list(db, score)
  # overview_histogram(unlist(s), 5, score, paste(pretty(score), " (n = ", length(s), ")", sep = ""))
  # dev.off()
  for (indicator in indicators_list('binary')) {
    png(filename=paste("comulative/", indicator, " - ", get_label(score), ".png", sep = ""))
    fi_ecdf_plot(db, score, indicator, 0.3, 0)
    dev.off()
  }
}