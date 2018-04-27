library(dplyr)
library(maptools)
library(ggplot2)

testoutcome <- read.csv("Testoutcome.csv", header = T, encoding = "UTF-8", na.strings = c("missing", "缺失"))
testitem <- read.csv("Testitem.csv", header = T, encoding = "UTF-8")
prerequisite <- read.csv("Prerequisite.csv", header = T)

colnames(testoutcome) <- c("city", "userId", "topicId", "topic", "userReportId", "conceptId", "concept", "master_char", "master")
colnames(testitem) <- c("city", "userId", "topicId", "topic", "userReportId", "examId", "series", "conceptId", "concept", "questionId", "question", "correct_char", "correct", "skip", "difficulty", "startTime", "endTime", "timespent")

attach(testoutcome)
testoutcome$userId <- as.factor(userId)
testoutcome$topicId <- as.factor(topicId)
testoutcome$userReportId <- as.factor(userReportId)
testoutcome$conceptId <- as.factor(conceptId)
detach()

attach(testitem)
testitem$userId <- as.factor(userId)
testitem$topicId <- as.factor(topicId)
testitem$userReportId <- as.factor(userReportId)
testitem$examId <- as.factor(examId)
testitem$conceptId <- as.factor(conceptId)
testitem$questionId <- as.factor(questionId)
detach()

# Users & tests statistics
length(unique(testoutcome$userId))
length(unique(testoutcome$userReportId))

t <- unique(testoutcome[, c(2,5)])
t.user <- group_by(t, userId)
t.user.report <- summarise(t.user, n = n())
summary(t.user.report$n)
sum(t.user.report$n==1)

ggplot(t.user.report, aes(x  = n)) + 
  geom_bar(stat = 'count', fill="darkorange2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  labs(x = "Number of Report", title = "Distribution of Reports by Users")
  
# Demographics of users
t <- unique(testoutcome[, c(1,2)])
sum(t$city=="NULL")

t <- t[t$city!="NULL", ]
t.city <- group_by(t, city)
t.user.city <- summarise(t.city, n = n())

summary(t.user.city$n)

ggplot(t.user.city, aes(x  = n)) + 
  geom_histogram(fill="darkorange2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  labs(x = "Number of Users", title = "Distribution of Users across Citys")
  
province <- read.csv("province.csv", header = T, encoding = "UTF-8")
colnames(province) <- c("province", "city")
t.province <- left_join(t, province, by = "city")
t.province[which(t.province$city=="吉林"), 3] <- "吉林省"
t.province <- t.province[!is.na(t.province$province), ]

t.province <- group_by(t.province, province)
t.user.prov <- summarise(t.province, n = n())

summary(t.user.prov$n)

ggplot(t.user.prov, aes(x  = n)) + 
  geom_histogram(fill="darkorange2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  labs(x = "Number of Users", title = "Distribution of Users across Provinces")

ggplot(data = t.user.prov, mapping = aes(x = factor(province), y = n)) + 
  geom_bar(stat= 'identity', position = 'dodge',fill="darkorange2") + 
  labs(x = "Province", y = "count", title = "Distribution of Users across Provinces") +
  theme(plot.title = element_text(hjust= 0.5))  
  
china_map = readShapePoly('bou2_4p.shp')
china_map@data$NAME <- as.character(iconv(china_map@data$NAME,"GBK","UTF-8"))

data1<- china_map@data
data2<- data.frame(id=row.names(data1), data1)
china_map1 <- fortify(china_map)
china_map_data <- full_join(china_map1, data2)

prov <- t.user.prov
colnames(prov)[1]<-"NAME"
china_data <- full_join(china_map_data, prov)
ggplot(china_data, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(colour="grey40") + 
  scale_fill_gradient(low="white",high="darkorange2") +
  coord_map("polyconic") +
  labs(x = "", y = "" , title = "The distribution of users across Country") +
  theme(plot.title = element_text(hjust= 0.5))
  
# Description of questions among the concepts
unique(testitem$conceptId)

ggplot(testitem, aes(x  = conceptId)) + 
  geom_bar(stat = 'count', fill="darkorange2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  ggtitle('Distribution of Questions per Concept')
 
# Description of concept mastery
ind <- which(!is.na(testoutcome$master))
t.master <- testoutcome[ind, c(6, 9)]
t.master <- group_by(t.master, conceptId)
t.user.master <- summarise(t.master, n = n(), sumMaster = sum(master))
t.user.master <- mutate(t.user.master, rate = sumMaster/n)
t.user.master

ggplot(data = t.user.master, mapping = aes(x = factor(conceptId), y = rate)) + 
  geom_bar(stat= 'identity', position = 'dodge',fill="darkorange2") + 
  labs(x = "conceptId", y = "rate", title = "Distribution of Concept Mastery") +
  theme(plot.title = element_text(hjust= 0.5))

# Accuracy of test
length(unique(testitem$questionId))

t.correct <- testitem[, c(10, 13)]
t.correct <- group_by(t.correct, questionId)
t.user.correct <- summarise(t.correct, n = n(), sumCorrect = sum(correct))
t.user.correct <- mutate(t.user.correct, rate = sumCorrect/n)
summary(t.user.correct$rate)

ggplot(t.user.correct, aes(x  = rate)) + 
  geom_histogram(fill="darkorange2") + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  labs(x = "Correct Rate", title = "Distribution of Correct Rate")

difficult <- unique(testitem[, c(10, 15)])
t.ques.diff <- full_join(t.user.correct, difficult)
t.ques.diff$difficulty <- as.factor(t.ques.diff$difficulty)

bartlett.test(rate ~ difficulty, data = t.ques.diff)
rate.diff <- aov(rate ~ difficulty, data = t.ques.diff)
summary(rate.diff)
TukeyHSD(rate.diff)

# Analysis of time spent on questions
summary(testitem$timespent)
startTime <- as.POSIXct(as.character(testitem$startTime), format = '%m/%d/%Y %H:%M:%S')
endTime <- as.POSIXct(as.character(testitem$endTime), format = '%m/%d/%Y %H:%M:%S')
timespent <- as.numeric(difftime(endTime, startTime, units="secs"))
summary(timespent)
testitem$startTime <- startTime
testitem$endTime <- endTime
testitem$timespent <- timespent

sum(testitem$correct[timespent < 5]) / sum(timespent < 5)
summary(timespent[timespent >= 5])
27 + 1.5 * (27 - 5)
sum(testitem$correct[timespent >= 5 & timespent < 1000]) / sum(timespent >= 5 & timespent < 1000)
ggplot(testitem[timespent >= 5 & timespent < 1000,], aes(x = timespent)) +
  stat_bin(fill="darkorange2", binwidth = 20) + 
  ggtitle('Distribution of Time Spent on Questions')
ggplot(testitem[timespent >= 5 & timespent <= 60,], aes(x = timespent)) +
  geom_bar(fill="darkorange2") + 
  ggtitle('Distribution of Time Spent on Questions')
  
# Analysis of time spent on tests
t <- testitem[, c(6, 16, 17)]
t.test <- group_by(t, examId)
t.testtime <- summarise(t.test, start = min(startTime), end = max(endTime))
t.testtime <- mutate(t.testtime, timeused = as.numeric(difftime(end, start, units="secs")))
summary(t.testtime$timeused)
207 + 1.5 * (207 - 63)
sum(t.testtime$timeused > 423)
ggplot(t.testtime[t.testtime$timeused <= 423,], aes(x = timeused)) +
  geom_bar(binwidth = 5, fill="darkorange2") + 
  ggtitle('Distribution of Time Spent on Test')
  
# The pattern of concepts
library(reshape2)
t.test.concept <- testitem[,c(5,7,8)]
t.test.concept <- dcast(t.test.concept, userReportId ~ series + conceptId)
dim(unique(t.test.concept[,-1]))

# The pattern of questions
t.test.question <- testitem[,c(5,7,10)]
t.test.question <- dcast(t.test.question, userReportId ~ series + questionId)
dim(unique(t.test.question[,-1]))
