# Features
# ----------
# Select features which satisfy certain conditions
# - features.keyword
# - features.keywords.intersect
# Does a feature have property 'log' (shortcut)
# - is.log
# Get display names of features
# - get.label
# - get.labels

# Choose all features with 'keyword' property in any of the columns
features.keyword <- function(keyword) {
  result = c()
  for (opt in colnames(features)) {
    to.add = as.vector(features[which(features[opt] == keyword), c('Column')])
    result <- union(result, to.add)
  }
  result
}
# Choose all features with all keywords
features.keywords.intersect <- function(keywords) {
  result = features.keyword(keywords[1])
  if (length(keywords) > 1) for (keyword in keywords[2:length(keywords)])
    result <- intersect(result, features.keyword(keyword))
  result
}
# Does the feature have property 'log'
is.log <- function(feature) { feature %in% features.keyword('log') }
# Get feature label
get.label <- function(feature) {
  result = ''
  if (feature %in% features$Column) result = features[which(features$Column == feature), c('Label')]
  as.character(result)
}
# Get a list of labels for the list of features
get.labels <- function(list) {
  l = length(list)
  result = rep('', l)
  for (i in c(1:l)) result[i] = get.label(list[i])
  result
}

# Functions
# ----------

min.na <- function(x) { min(x, na.rm = TRUE) }
max.na <- function(x) { max(x, na.rm = TRUE) }
logmod <- function(x) { sign(x) * log(abs(x) + 1) } # natural logarithm
round.out <- function(x) { if (x < 0) floor(x) else ceiling(x) } # round to the integer with larger absolute value

# String modifications
# --------------------
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
pretty <- function(x) {
  s <- strsplit(gsub("[\\._]", " ", x), " ")[[1]]
  trim(paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep="", collapse=" "))
}

# Data frame utilities
# --------------------
# Get a complete sub data drame with given two columns
# - get.pair.df
# Count sizes of complete sub data frames for correlations
# - count.pairwise
# Get list from column elements
# - get.list
# Dataframe transformations
# - columns.function
# - maybe.log.transform
# Values
# - get.cutoff
# - count.range
# - get.max.score
# Split and subset dataframe along category and bounds
# - fi.data

# Returns a complete sub data drame with given two columns
get.pair.df <- function(column1, column2) {
  db[complete.cases(db[, c(column1, column2)]), c(column1, column2)]
}
# Count sizes of complete sub data frames for correlations
count.pairwise <- function(df) {
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
# Returns list of column elements
get.list <- function(db, col) { db[!is.na(db[col]), col] }
# Dataframe transformation: return subdataframe with f applied to the columns
columns.function <- function(db, columns, f) {
  r = c()
  for (col in columns) { r = c(r, f(get.list(db, col))) }
  r
}
# Dataframe transformation: use logmod on column if corresponding feature is 'log'
maybe.log.transform <- function(df) {
  for (c in colnames(df)) if (is.log(c)) df[[c]] <- logmod(df[[c]])
  df
}
# Value: cutoff value in list at specified top percent
get.cutoff <- function(list, percent) {
  wf = list[!is.na(list)]
  wf = sort(wf, decreasing = TRUE)
  if (percent > 0) wf[round(length(wf)*percent)] else wf[1]
}
# Value: number of elements in list between specified values
count.range <- function(df, lower, upper) { length(df[which(df > lower & df <= upper)]) }
# Value: max score (normalized or log); assumes mattermark transformation has been done
get.max.score <- function(x) {
  if (is.log(colnames(x)[1])) max.na(x)
  else 5
}
# Split and subset dataframe along category and bounds
# Needs a numeric column as score and a binary feature as category
fi.data <- function(db, score, category, lower, upper) {
  dt = maybe.log.transform(
    if (category == "") db[!is.na(db[score]), score, drop = FALSE]
    else db[complete.cases(db[, c(score, category)]), c(score, category)]
  )
  r = list(
    'max_score' = get.cutoff(dt[score], upper),
    'min_score' = get.cutoff(dt[score], lower),
    'n' = nrow(dt))
  r$data = dt[which(dt[score] >= r$min_score & dt[score] <= r$max_score), colnames(dt), drop = FALSE]
  r$rows = nrow(r$data)
  r$ticks = c(round.out(r$min_score):round.out(r$max_score))
  r$labels = if (is.log(score)) round(exp(r$ticks)) else r$ticks
  # split along the category
  if (category != "") {
    s = fi_category_split(dt, category)
    r$parts = fi_category_split(r$data, category)
    for (index in names(r$parts)) { r$parts[[index]]$all = s[[index]]$n }
  }
  return(r)
}

# Plots
# ----------
modified.density <- function(df) density(unlist(df), na.rm = TRUE) 
modified.ecdf <- function(df) ecdf(unlist(-1*df))

