# --------------------
# Functions
# --------------------

get_legend <- function(cutoff, n) {
  paste("top ", round(cutoff*100), "% (", n, ")", sep = "")
}
get_category_label <- function(label, n, percent) {
  paste(label, " (", n, "; ", percent, "%)", sep = "")
}

fi_practice_order <- function(category, index) {
  if (index == 1) if (category == "i10") "FUNNEL" else "x"
  else if (category == "i10") "PIPELINE" else "0"
}

fi_practice_score <- function(db, score, category, lower, upper) {
  dt = fi.data(db, score, category, lower, upper)
  max_score = if (upper > 0) dt$max_score else get.max.score(dt$data[score])
  first = fi_practice_order(category, 1)
  second = fi_practice_order(category, 2)
  if (dt$parts[[first]]$n * dt$parts[[second]]$n == 0) return(FALSE)
  score = integrate(function(x) ecdf(dt$parts[[first]]$data[,1])(x), dt$min_score, max_score, subdivisions=2000)[[1]]
  score = score - integrate(function(x) ecdf(dt$parts[[second]]$data[,1])(x), dt$min_score, max_score, subdivisions=2000)[[1]]
  normalization = -1 / (max_score-dt$min_score)
  # -1 fixes the direction of integration from max to min
  list(
    'score' = normalization * score,
    'n' = dt$rows,
    'n1' = dt$parts[[first]]$n,
    'n2' = dt$parts[[second]]$n
  )
}

fi_ecdf_plot <- function(db, score, category, lower, upper) {
  dt = fi.data(db, score, category, lower, upper)
  colorcode = c("red", "blue")
  legend_array = c(get_legend(lower, dt$rows))
  png(file = paste("cdf/", score, "-", category, ".png", sep=""),
      bg = "transparent", width = 500, height = 500, units = "px", pointsize = 10)
  plot(
    modified.ecdf(dt$data[score]),
    main = "",
    xlim = c(-1*dt$max_score, -1*dt$min_score), ylim = c(0, 1),
    xlab = "Score", ylab = "Comulative density", pch = ".",
    xaxt = "n"
  )
  title(main = list(
    paste(pretty(category), "-", pretty(score), "\n", round(as.numeric(fi_practice_score(db, score, category, lower, upper)[[1]][1]), 3), sep=" "),
    font = 3, cex = 1
  ))
  axis(1, at = -1*dt$ticks, labels = dt$labels)
  clr = 1
  for (index in c(1, 2)) {
    partial = dt$parts[[fi_practice_order(category, index)]]
    lines(modified.ecdf(partial$data[score]), col = colorcode[clr], lwd = 2.5, pch = ".")
    clr = clr+1
    legend_array = c(legend_array, get_category_label(partial$label, partial$n, round(100*partial$n/partial$all)))
  }
  legend(-1*dt$max_score, 1, bty = "n", legend_array, y.intersp = 1.3,
         lty = c(1, 1, 1),
         lwd = c(1, 2.5, 2.5),
         col = c("black", colorcode))
  dev.off()
}

fi_density_plot <- function(db, score, lower, upper) {
  dt = fi.data(db, score, '', lower, upper)
  plot(
    modified.density(dt$data),
    main = "",
    xlim = c(dt$min_score, dt$max_score), ylim = c(0, 0.5),
    xlab = "Score", ylab = "Density", pch = ".",
    xaxt = "n"
  )
  title(main = list(pretty(score), font = 3, cex = 1))
  axis(1, at = dt$ticks, labels = dt$labels)
  clr = 1
}

fi_category_split <- function(dt, category) {
  s = split(dt, dt[category])
  parts = list()
  for (index in names(s)) {
    obj = list(
      'label' = index,
      'data' = s[[index]],
      'n' = nrow(s[[index]])
    )
    if (index == "x") obj$label = "Yes"
    if (index == "0") obj$label = "No"
    new = list(obj)
    names(new) = list(index)
    parts = c(parts, new)
  }
  parts
}

get_practice_scores_list <- function(scores, practices, lower, upper) {
  ps <- data.frame()
  for (s in scores) {
    for (p in practices) {
      t = fi_practice_score(db, s, p, lower, upper)
      r = fi_practice_score(db, s, p, 1, 0)
      if (length(t) > 1 && length(r) > 1) {
        new_row = list(
          "practice" = pretty(p),
          "score" = pretty(s),
          "practice_score_top" = round(t$score, 2),
          "practice_score_all" = round(r$score, 2),
          "n_top" = t$n,
          "n_all" = r$n,
          "c1_top" = t$n1,
          "c1_all" = r$n1,
          "c2_top" = t$n2,
          "c2_all" = r$n2)
        ps <- rbind(ps, do.call(data.frame, new_row))
      }
    }
  }
  ps
}

# uses external variable db
get_practice_score_matrix <- function(scores, practices, lower, upper) {
  practice_scores = c()
  for (s in scores) {
    for (p in practices) {
      t = fi_practice_score(db, s, p, lower, upper)
      practice_scores = c(practice_scores, if (length(t) > 1) as.numeric(t$score) else NA)
    }
  }
  df = matrix(practice_scores, length(practices), length(scores))
  colnames(df) <- scores
  rownames(df) <- practices
  df
}

# --------------------
# Computations
# --------------------

practice.scores.all <- round(get_practice_score_matrix(scores, benchmark.binary, 1, 0), 2)
practice.scores.top <- round(get_practice_score_matrix(scores, benchmark.binary, 0.3, 0), 2)
practice.scores.list <- get_practice_scores_list(scores, benchmark.binary, 0.3, 0)

for (score in scores) {
  for (category in benchmark.binary) {
    fi_ecdf_plot(db, score, category, 0.3, 0)
  }
}

