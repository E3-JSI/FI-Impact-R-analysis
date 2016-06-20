# all subgrantees (pairwise complete)
correlation_columns = c(scores_list('benchmark'), indicators_list('numeric'))
correlation_names = c(as.character(scores[which(scores$Score %in% scores_list('benchmark')), c('Label')]), as.character(indicators[which(indicators$Id %in% indicators_list('numeric')), c('Label')]))
correlation <- round(cor(db[, correlation_columns], use="pairwise.complete.obs", method = "spearman"), 2)
colnames(correlation) = correlation_names
rownames(correlation) = correlation_names
write.csv(correlation, file = "output/correlation.csv")
correlation_n = count_pairwise(db[, correlation_columns])
colnames(correlation_n) = correlation_names
rownames(correlation_n) = correlation_names
write.csv(correlation_n, file = "output/correlation-samples.csv")

# top 30% subgrantees for indicators (pairwise complete)
correlation_indicators = get_indicator_correlations(db, scores_list('benchmark'), indicators_list('numeric'), 0.3, 0)
rownames(correlation_indicators) = indicators[which(indicators$Id %in% indicators_list('numeric')), c("Label")]
colnames(correlation_indicators) = scores[which(scores$Score %in% scores_list('benchmark')), c("Label")]

png(file = paste("output/", "correlation", ".png", sep = ""),
    bg = "transparent", width = 1000, height = 700, units = "px", pointsize = 10)
heat <- melt(correlation, id.var = "X1")
ggplot(heat, aes(as.factor(X1), X2, group=X2), notecex=0.075) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = heat$value, label = heat$value)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")
dev.off()

png(file = paste("output/", "correlationTop", ".png", sep = ""),
    bg = "transparent", width = 700, height = 350, units = "px", pointsize = 10)
heat <- melt(correlation_indicators, id.var = "X1")
ggplot(heat, aes(as.factor(X1), X2, group=X2), notecex=0.075) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = heat$value, label = heat$value)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")
dev.off()

remove(heat)