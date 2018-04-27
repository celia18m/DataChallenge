studentitem <- testitem[which(testitem$userId=="4511"), ]
length(unique(studentitem$questionId))

sum(studentitem$correct) / dim(studentitem)[1]

studentoutcome <- testoutcome[which(testoutcome$userId=="4511"), ]

length(unique(studentoutcome$conceptId))

ggplot(studentoutcome, aes(x  = userReportId, fill = master_char)) + 
  geom_bar() + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  ggtitle('Concepts Mastery Condition')
  
t.student <- group_by(studentitem, userReportId)
t.stu.concept <- summarise(t.student, n = length(unique(conceptId)))
summary(t.stu.concept$n)
t.stu.wrong <- studentitem[studentitem$correct == 0, c(5, 8)]

t.stu.repeat <- group_by(studentitem, userReportId, conceptId)
t.stu.rep <- summarise(t.stu.repeat, n = n())
t.stu.rep[t.stu.rep$n>1, ]
length(unique(t.stu.rep$conceptId))

# Knowledge Map
library(networkD3)
x1 <- prerequisite[which(!is.na(prerequisite[,2])), c(2, 1)]
x2 <- prerequisite[which(!is.na(prerequisite[,3])), c(3, 1)]
x3 <- prerequisite[which(!is.na(prerequisite[,4])), c(4, 1)]
names(x1) <- c("concept", "prerequisite")
names(x2) <- c("concept", "prerequisite")
names(x3) <- c("concept", "prerequisite")
networkData <- bind_rows(x1, x2, x3)

simpleNetwork(networkData, zoom = TRUE)

library(igraph)
concept <- unique(studentoutcome[,c(6,7)])
names(networkData) <- c("to", "from")
g <- graph_from_data_frame(networkData, directed=TRUE, vertices=concept)
plot(g)

ggplot(studentitem, aes(x = as.factor(series), y = conceptId, colour = userReportId, group=userReportId)) +
  geom_line(size=2, alpha = 0.5) + 
  geom_point(size=4)
  
base_size <- 12
outcome_concept <- ggplot(studentoutcome, aes(userReportId, conceptId))+
  geom_tile(aes(fill = master), colour = "white")+
  scale_fill_gradient(low = "coral4", high = "white")+
  theme_grey(base_size = base_size)+
  labs(x = "userReportId", y = "conceptId" , title = "The condition of concept mastery of outcome")+
  theme(plot.title = element_text(hjust = 0.5))
print(outcome_concept)
