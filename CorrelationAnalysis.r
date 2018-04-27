library(corrplot)
test.concept <- testitem[which(testitem$timespent >= 5 & testitem$timespent <= 60), c(5, 8, 13)]

test.concept <- group_by(test.concept, userReportId, conceptId)
t.test.concept <- summarise(test.concept, n = n(), sumCorrect = sum(correct))
t.test.concept <- mutate(t.test.concept, rate = sumCorrect/n)

t.concept.rate <- dcast(t.test.concept, userReportId~conceptId, value.var = "rate")

corr.concept <- cor(t.concept.rate[,-1])
for (i in 1:17){
  for (j in (i+1):18){
    try(c <- cor(t.concept.rate[, c(i+1, j+1)], use = "complete.obs"), silent = T)
    try(corr.concept[i, j] <- c[1,2], silent = T)
    try(corr.concept[j, i] <- c[2,1], silent = T)
        
  }
}

corrplot(corr.concept, method = "color")
