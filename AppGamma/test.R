test2 <- CompletePhyl
oldDF <- as(sample_data(test2), "data.frame")
newDF <- subset(oldDF, Samplename %in% c("EpiL", "EpiOE", "R1"))
sample_data(test2) <- sample_data(newDF)


