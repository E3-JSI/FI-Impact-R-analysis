# --------------------
# Functions
# --------------------
# This script uses a few functions from binary.R

get_dichotomous_ttest <- function(dt, s, p, percent) {
  ok.sizes = TRUE #
  for (name in names(dt$parts)) ok.sizes = ok.sizes && (dt$parts[[name]]$n > 20)
  if (ok.sizes) {
    first = fi.practice.order(p, 1)
    second = fi.practice.order(p, 2)
    r = t.test(dt$parts[[first]]$data[s], dt$parts[[second]]$data[s])
    if (r$p.value < 0.05) {
      ttest = list(
        'practice' = pretty(p),
        'score' = pretty(s),
        'percent' = percent,
        't' = round(r$statistic, 2),
        'df' = round(r$parameter),
        'diff' = round(r$estimate['mean of x'] - r$estimate['mean of y'], 2),
        'p.value' = round(r$p.value, 3))
      return(ttest)
    }
  }
  return(FALSE)
}

get_ttests <- function(scores, practices) {
  cols = c('t', 'df', 'p.value')
  results <- data.frame()
  for (s in scores) {
    for (p in practices) {
      for (percent in c(1, 0.5, 0.4, 0.3, 0.2, 0.1)) {
        dt = fi.data(db, s, p, percent, 0)
        ttest = FALSE
        if (length(dt$parts) == 2) ttest = get_dichotomous_ttest(dt, s, p, percent)
        if (length(ttest) > 1) results <- rbind(results, do.call(data.frame, ttest))
      }
    }
  }
  parts = split(results, results$percent)
  for (name in names(parts)) {
    colnames(parts[[name]]) = c('practice', 'score', paste(name, c('percent', 't', 'df', 'diff', 'p.value'), sep = '.'))
  }
  result = Reduce(function(x, y) merge(x, y, all = TRUE, by = c('practice', 'score')), parts)
  result
}

# --------------------
# Computations
# --------------------

tt = get_ttests(scores, benchmark.binary)
tt = merge(tt, practice.scores.list, all.x = TRUE, by = c('practice', 'score'))
write.csv(tt, file = 'output/tt.csv')
