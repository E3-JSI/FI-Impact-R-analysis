scores_list <- function(keyword) {
  for (opt in c('Category', 'Type', 'Computations')) if (sum(scores[opt] == keyword) > 0)
    return(as.vector(scores[which(scores[opt] == keyword), c('Score')]))
  return(c())
}
indicators_list <- function(keyword) {
  if (sum(indicators$Type == keyword) > 0)
    return(as.vector(indicators[which(indicators$Type == keyword), c('Id')]))
  return(c())
}
is_log <- function(score) { score %in% scores_list('log') }

get_label <- function(indicator) {
  if (indicator %in% scores$Score) return(scores[which(scores$Score == indicator), c('Label')])
  if (indicator %in% indicators$Id) return(indicators[which(indicators$Id == indicator), c('Label')])
}

# Functions
min_na <- function(x) { min(x, na.rm = TRUE) }
max_na <- function(x) { max(x, na.rm = TRUE) }
logmod <- function(x) { sign(x) * log(abs(x) + 1) } # natural logarithm
round_out <- function(x) { if (x < 0) floor(x) else ceiling(x) }

# String modifications
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
pretty <- function(x) {
  s <- strsplit(gsub("[\\._]", " ", x), " ")[[1]]
  trim(paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep="", collapse=" "))
}

count_pairwise <- function(df) {
  len = length(colnames(df))
  count = matrix(nrow = len, ncol = len)
  for (col1 in c(1:len)) {
    for (col2 in c(col1:len)) {
      wf = df[, c(col1, col2)]
      wf = wf[complete.cases(wf), ]
      count[col1, col2] = nrow(wf)
      count[col2, col1] = nrow(wf)
    }
  }
  colnames(count) <- colnames(df)
  rownames(count) <- colnames(df)
  count
}
get_cutoff <- function(list, percent) {
  wf = list[!is.na(list)]
  wf = sort(wf, decreasing = TRUE)
  if (percent > 0) wf[round(length(wf)*percent)] else wf[1]
}
get_list <- function(db, col) { db[!is.na(db[col]), col] }
columns_function <- function(db, columns, f) {
  r = c()
  for (col in columns) { r = c(r, f(get_list(db, col))) }
  r
}
count_range <- function(df, lower, upper) { length(df[which(df > lower & df <= upper)]) }

# use a dataframe of shape [, c(category, score)]
maybe_log_transform <- function(df) {
  for (c in colnames(df)) if (is_log(c)) df[[c]] <- logmod(df[[c]])
  df
}

# Plots
modified_density <- function(df) density(unlist(df), na.rm = TRUE) 
modified_ecdf <- function(df) ecdf(unlist(-1*df))

# assumes mattermark transformation has been done
get_min_score <- function(x) {
  if (is_log(colnames(x)[1])) min_na(x)
  else 0
}
get_max_score <- function(x) {
  if (is_log(colnames(x)[1])) max_na(x)
  else 5
}

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
  dt = fi_data(db, score, category, lower, upper)
  max_score = if (upper > 0) dt$max_score else get_max_score(dt$data[score])
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
    'n2' = dt$parts[[second]]$n,
    'test' = dt$parts)
}

fi_ecdf_plot <- function(db, score, category, lower, upper) {
  dt = fi_data(db, score, category, lower, upper)
  colorcode = c("red", "blue")
  legend_array = c(get_legend(lower, dt$rows))
  plot(
    modified_ecdf(dt$data[score]),
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
    lines(modified_ecdf(partial$data[score]), col = colorcode[clr], lwd = 2.5, pch = ".")
    clr = clr+1
    legend_array = c(legend_array, get_category_label(partial$label, partial$n, round(100*partial$n/partial$all)))
  }
  legend(-1*dt$max_score, 1, bty = "n", legend_array, y.intersp = 1.3,
    lty = c(1, 1, 1),
    lwd = c(1, 2.5, 2.5),
    col = c("black", colorcode))
}

fi_density_plot <- function(db, score, lower, upper) {
  dt = fi_data(db, score, '', lower, upper)
  plot(
    modified_density(dt$data),
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

fi_data <- function(db, score, category, lower, upper) {
  dt = maybe_log_transform(
    if (category == "") db[!is.na(db[score]), score, drop = FALSE]
    else db[complete.cases(db[, c(score, category)]), c(score, category)]
  )
  r = list(
    'max_score' = get_cutoff(dt[score], upper),
    'min_score' = get_cutoff(dt[score], lower),
    'n' = nrow(dt))
  r$data = dt[which(dt[score] >= r$min_score & dt[score] <= r$max_score), colnames(dt), drop = FALSE]
  r$rows = nrow(r$data)
  r$ticks = c(round_out(r$min_score):round_out(r$max_score))
  r$labels = if (is_log(score)) round(exp(r$ticks)) else r$ticks
  # split along the category
  if (category != "") {
    s = fi_category_split(dt, category)
    r$parts = fi_category_split(r$data, category)
    for (index in names(r$parts)) { r$parts[[index]]$all = s[[index]]$n }
  }
  return(r)
}

get_practice_scores_list <- function(scores, practices, lower, upper) {
  ps <- data.frame(
    "practice_score" = numeric(0),
    "n" = numeric(0),
    "practice" = character(0),
    "score" = character(0),
    "c1" = numeric(0),
    "c2" = numeric(0))
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
get_ranking_matrix <- function(scores, practices, lower, upper) {
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

correlation_heatmap <- function(matrix) {
  #matrix[lower.tri(matrix)] <- NA
  melted <- melt(matrix, na.rm = TRUE)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted, aes(X2, X1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
  # Print the heatmap
  print(ggheatmap)
  
  ggheatmap + 
    #geom_text(aes(X2, X1, label = value), color = "black", size = 4) +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  
}

overview <- function(db) {
  normalized = scores_list('normalized')
  scores_overview = data.frame(
    normalized,
    columns_function(db, normalized, length),
    columns_function(db, normalized, mean),
    columns_function(db, normalized, median)
  )
  for (i in c(0:4)) {
    t = c()
    for (s in normalized) {
      t = c(t, count_range(get_list(db, s), i, i+1))
    }
    scores_overview = cbind(scores_overview, data.frame(t))
  }
  names(scores_overview) = c("score", "n", "mean", "median", "0 < x <= 1", "1 < x <= 2", "2 < x <= 3", "3 < x <= 4", "4 < x <= 5")
  scores_overview[, c("mean", "median")] <- round(scores_overview[, c("mean", "median")], digits = 2)
  scores_overview
}

get_mm_growth_overview <- function(db, acc) {
  df = db[which(!is.na(db["Growth.Score"])), c("Help", "Growth.Score")]
  list(
    'accelerator' = acc,
    'n' = nrow(df),
    'mean' = round(mean(df$Growth.Score), 2),
    'median' = median(df$Growth.Score)
  )
}

overview_mm <- function(db) {
  scores_overview = data.frame(get_mm_growth_overview(db, "all"))
  acc = split(db, db$Help)
  for (a in setdiff(names(acc), c(""))) {
    scores_overview = rbind(scores_overview, do.call(data.frame, get_mm_growth_overview(acc[[a]], a)))
  }
  scores_overview
}

overview_histogram <- function(data, brk, score, title) {
  hist(data,
       breaks = brkb,
       main = title,
       col = "aliceblue",
       xlab = "Score",
       ylab = "Percentage",
       ylim = c(0, 2*max(modified_density(data)$y)),
       freq = FALSE)
  lines(modified_density(data))
}

overview_accelerators <- function(db) {
  overview_acc = list()
  acc = split(db, db$Help)
  for (a in names(acc)) {
    if (a != "") {
      table = overview(acc[[a]])
      overview_acc = c(overview_acc, table)
      write.csv(table, file = paste("output/overview-", a, ".csv", sep = ""))
      for (score in scores_list('benchmark')) {
        jpeg(file = paste("histogram/", a, "-", score, ".jpg", sep = ""))
        s = get_list(acc[[a]], score)
        b = if (is_log(score)) 5 else c(0:5)
        if (length(s) > 0) overview_histogram(s, b, score, paste(pretty(score), a))
        dev.off()
      }
    }
  }
  overview_acc
}

write_overview_pdf <- function() {
  pdf(file = paste("histogram/acc-scores.pdf", sep = ""))
  for (score in scores_list('benchmark')) {
    for (a in names(acc)) {
      if (a != "") {
        s = get_list(acc[[a]], score)
        b = if (is_log(score)) 5 else c(0:5)
        if (length(s) > 0) overview_histogram(s, b, score, paste(pretty(score), a))
      }
    }
  }
  dev.off()
}

get_indicator_correlations <- function(db, scr, ind, lower, upper) {
  m = length(ind)
  n = length(scr)
  a = mat.or.vec(m, n)
  for (i in c(1:m)) for (j in c(1:n)) {
    cols = c(ind[i], scr[j])
    dt = db[complete.cases(db[, cols]), cols]
    max_score = get_cutoff(dt[scr[j]], upper)
    min_score = get_cutoff(dt[scr[j]], lower)
    rows = nrow(dt)
    dt = dt[which(dt[scr[j]] >= min_score & dt[scr[j]] <= max_score), cols]
    a[i, j] = round(cor(dt, use="pairwise.complete.obs", method = "spearman")[1,2], 2)
  }
  colnames(a) = scr
  rownames(a) = ind
  a
}

save_boxplots <- function(db, scores, categories) {
  for (score in scores) for (category in categories) {
    dt = db[complete.cases(db[, c(score, category)]), c(score, category)]
    colnames(dt) <- c('score', 'category')
    jpeg(file = paste("boxplots/", category, "-", score, ".jpg", sep = ""))
    if (category %in% indicators_list('binary')) {
      par(mar = c(4, 4, 4, 4))
      boxplot(score ~ category, data = dt, main = paste(get_label(category), '-', get_label(score)))
    }
    else {
      par(mar = c(10, 4, 4, 2))
      boxplot(score ~ category, data = dt, las = 2, main = get_label(score))
    }
    dev.off()
  }
}

save_boxplot_sample_sizes <- function(db, scores, categories) {
  for (category in categories) {
    parts = split(db[c(category, scores)], db[category])
    options = names(parts)
    m = length(options)
    n = length(scores)
    a = mat.or.vec(m, n)
    rownames(a) = options
    colnames(a) = scores
    for (i in c(1:m)) for (j in c(1:n)) a[i, j] = nrow(parts[[options[i]]])
    write.csv(a, file = paste('output/boxplot-', category, '.csv', sep = ''))
  }
}

