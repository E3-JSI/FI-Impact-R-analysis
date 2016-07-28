# --------------------
# Functions
# --------------------

# Compute correlations
get.correlations <- function(cols, rows, lower, upper) {
  m = length(rows)
  n = length(cols)
  a = matrix(rep(NA, m*n), nrow = m, ncol = n)
  for (i in c(1:m)) for (j in c(1:n)) {
    pair = c(rows[i], cols[j])
    dt = maybe.log.transform(db[complete.cases(db[, pair]), pair])
    if (nrow(dt) < 10) return(a)
    fail = FALSE
    corr = NA
    if (lower != 1 || upper != 0) {
      # if none of the columns are scores and interval is not everything, the computation fails
      # the interval is used for the first column in the pair
      if (pair[1] %in% benchmark.numeric || pair[2] %in% benchmark.numeric) {
        scoreColumn = if (pair[1] %in% benchmark.numeric) dt[pair[1]] else dt[pair[2]]
        max.score = get.cutoff(scoreColumn, upper)
        min.score = get.cutoff(scoreColumn, lower)
        dt = dt[which(scoreColumn >= min.score & scoreColumn <= max.score), pair]
      }
      else {
        fail = TRUE
      }
    }
    # compute correlation if interval and columns ok
    if (!fail) {
      corr = cor.test(dt[, c(pair[1])], dt[, c(pair[2])], method = "spearman", exact=FALSE)
      if (!is.na(corr$p.value) && corr$p.value <= 0.05) a[i,j] = corr$estimate
    }
  }
  colnames(a) = get.labels(cols)
  rownames(a) = get.labels(rows)
  round(a, 2)
}

# --------------------
# Computations
# --------------------

# all subgrantees (pairwise complete)
correlation = get.correlations(benchmark.numeric, benchmark.numeric, 1, 0)
write.csv(correlation, file = "output/correlation.csv")
correlation.n = count.pairwise(db[, benchmark.numeric])
colnames(correlation.n) = get.labels(benchmark.numeric)
rownames(correlation.n) = get.labels(benchmark.numeric)
write.csv(correlation.n, file = "output/correlation-samples.csv")
# top 30% subgrantees for indicators (pairwise complete)
correlation.indicators = get.correlations(scores, benchmark.numeric, 0.3, 0)

png(file = "output/correlation.png",
    bg = "transparent", width = 1000, height = 700, units = "px", pointsize = 10)
heat <- melt(correlation, id.var = "X1")
ggplot(heat, aes(as.factor(X1), X2, group=X2), notecex=0.075) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = heat$value, label = heat$value)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")
dev.off()

png(file = "output/correlationTop.png",
    bg = "transparent", width = 500, height = 500, units = "px", pointsize = 10)
heat <- melt(correlation.indicators, id.var = "X1")
ggplot(heat, aes(as.factor(X1), X2, group=X2), notecex=0.075) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = heat$value, label = heat$value)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")
dev.off()