# --------------------
# Functions
# --------------------
# Functions that output bin sizes for projects with respect to scores

overview <- function(db) {
  normalized = features.keyword('normalized')
  scores.overview = data.frame(
    normalized,
    columns.function(db, normalized, length),
    columns.function(db, normalized, mean),
    columns.function(db, normalized, median)
  )
  for (i in c(0:4)) {
    t = c()
    for (s in normalized) {
      t = c(t, count.range(get.list(db, s), i, i+1))
    }
    scores.overview = cbind(scores.overview, data.frame(t))
  }
  names(scores.overview) = c("score", "n", "mean", "median", "0 < x <= 1", "1 < x <= 2", "2 < x <= 3", "3 < x <= 4", "4 < x <= 5")
  scores.overview[, c("mean", "median")] <- round(scores.overview[, c("mean", "median")], digits = 2)
  scores.overview
}

get.mm.growth.overview <- function(db, acc) {
  df = db[which(!is.na(db["Growth.Score"])), c("Help", "Growth.Score")]
  list(
    'accelerator' = acc,
    'n' = nrow(df),
    'mean' = round(mean(df$Growth.Score), 2),
    'median' = median(df$Growth.Score)
  )
}

overview.mm <- function(db) {
  scores.overview = data.frame(get.mm.growth.overview(db, "all"))
  acc = split(db, db$Help)
  for (a in setdiff(names(acc), c(""))) {
    scores.overview = rbind(scores.overview, do.call(data.frame, get.mm.growth.overview(acc[[a]], a)))
  }
  scores.overview
}

overview.histogram <- function(data, brk, score, title) {
  hist(data,
       breaks = brk,
       main = title,
       col = "aliceblue",
       xlab = "Score",
       ylab = "Percentage",
       ylim = c(0, 2*max(modified.density(data)$y)),
       freq = FALSE)
  lines(modified.density(data))
}

overview.accelerators <- function(db) {
  overview.acc = list()
  acc = split(db, db$Help)
  for (a in names(acc)) {
    if (a != "") {
      table = overview(acc[[a]])
      overview.acc = c(overview.acc, table)
      write.csv(table, file = paste("output/overview-", a, ".csv", sep = ""))
      for (score in scores) {
        jpeg(file = paste("histogram/", a, "-", score, ".jpg", sep = ""))
        s = get.list(acc[[a]], score)
        b = if (is.log(score)) 5 else c(0:5)
        if (length(s) > 0) overview.histogram(s, b, score, paste(pretty(score), a))
        dev.off()
      }
    }
  }
  overview.acc
}

write.overview.pdf <- function() {
  pdf(file = paste("histogram/acc-scores.pdf", sep = ""))
  for (score in scores) {
    for (a in names(acc)) {
      if (a != "") {
        s = get.list(acc[[a]], score)
        b = if (is.log(score)) 5 else c(0:5)
        if (length(s) > 0) overview.histogram(s, b, score, paste(pretty(score), a))
      }
    }
  }
  dev.off()
}

# --------------------
# Computations
# --------------------

# Score overviews
overview.all = overview(db)
overview.growth = overview.mm(db)
write.csv(overview.all, file = "output/overview.csv")
write.csv(overview.growth, file = "output/overview_growth.csv")

overview.acc = overview.accelerators(db)

for (score in scores) {
  jpeg(filename=paste("histogram/", pretty(score), ".jpg", sep = ""))
  s = get.list(db, score)
  overview.histogram(unlist(s), 5, score, paste(pretty(score), " (n = ", length(s), ")", sep = ""))
  dev.off()
}

jpeg(filename=paste("histogram/growth.jpg", sep = ""))
s = get.list(db, 'Growth.Score')
overview.histogram(unlist(s), 5, 'Growth.Score', paste('Mattermark Growth Score', " (n = ", length(s), ")", sep = ""))
dev.off()
