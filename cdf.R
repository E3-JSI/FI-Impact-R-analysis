# --------------------
# Computations
# --------------------

# Practice score plots
for (score in scores) {
  jpeg(filename=paste("cdf/", pretty(score), ".jpg", sep = ""))
  s = get.list(db, score)
  overview_histogram(unlist(s), 5, score, paste(pretty(score), " (n = ", length(s), ")", sep = ""))
  dev.off()
  for (indicator in benchmark.binary) {
    png(filename=paste("cdf/", indicator, " - ", get.label(score), ".png", sep = ""))
    fi_ecdf_plot(db, score, indicator, 0.3, 0)
    dev.off()
  }
}

jpeg(filename=paste("cdf/growth.jpg", sep = ""))
s = get.list(db, 'Growth.Score')
overview_histogram(unlist(s), 5, 'Growth.Score', paste('Mattermark Growth Score', " (n = ", length(s), ")", sep = ""))
dev.off()