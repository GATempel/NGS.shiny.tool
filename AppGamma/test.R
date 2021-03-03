test <-test[9:20]
test$Total <- rowSums(test)
test <- test[order(test$Total, decreasing = T),]
data4 <- rbind(test[1:10,], colSums(test[(10+1):nrow(test),]))
row.names(data4)[nrow(data4)] <- "#others"
data4$Total <- NULL
data4$Name <- row.names(data4)
data5 <- tidyr::pivot_longer(data4,
                             c("BW3-5","BW3-10"),
                             names_to = "Sample",
                             values_to = "Abundance")


# library
library(ggplot2)

# create a dataset

data <- dplyr::filter(Genus$Tidy, Sample %in% c("BW3-5","BW3-10"))

# Stacked
ggplot(data5, aes(fill=Name, y=Abundance, x=Sample)) +
  geom_bar(position="fill", stat = "identity")


if (input$Relative == T) {

  relataxa <- transform_sample_counts(SubPhyl,
                                      function(OTU) OTU/sum(OTU))
  relataxa <- prune_taxa(topX, relataxa)
  plot_bar(relataxa, fill = input$TRankC)
} else {
  plot_bar(displaytaxa, fill = input$TRankC)
}

x <- as.data.frame(
  NULL
)

test <- list(
  x = 1,
  y = 1,
  text = "No sample selected \nPlease select a sample",
  showarrow = FALSE,
  font = list(size = 28)
)
fig <- plot_ly(as.data.frame(NULL))
fig <- fig %>% layout(annotations = test,
                      yaxis = list(visible = FALSE),
                      xaxis = list(visible = FALSE))
fig
