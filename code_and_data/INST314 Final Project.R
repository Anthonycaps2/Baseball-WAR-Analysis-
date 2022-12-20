MLB <- read.csv("C:\\School Stuff\\314 Final Project\\FanGraphs Leaderboard.csv",stringsAsFactors = TRUE)

aggregate(MLB$play_WAR, list(MLB$Team), sum)
aggregate(MLB$Clutch, list(MLB$Team), sum)

WIN <- read.csv("C:\\School Stuff\\314 Final Project\\2022_MLB_Winning Percentage - Sheet1.csv",stringsAsFactors = TRUE)

Playoffs <- read.csv("C:\\School Stuff\\314 Final Project\\Playoff_teams.csv",stringsAsFactors = TRUE)

df <- data.frame(MLB$ï..Name[MLB$play_WAR >= 1], MLB$Team[MLB$play_WAR >= 1], MLB$Age[MLB$play_WAR >= 1], MLB$play_WAR[MLB$play_WAR >= 1]) 

cor_matrix <- data.frame(Playoffs$WAR, Playoffs$CLUTCH, Playoffs$WINS)

summary(MLB)
summary(WIN)
summary(Playoffs)
head(df)

mean(MLB$Age)

mean(WIN$WAR)

max(MLB$play_WAR)

min(MLB$play_WAR)
#Correlation Coefficients 
cor(WIN$WAR, WIN$Wins)

#Correlation matrix
cor(cor_matrix)

#f-test 
sd(MLB$play_WAR[MLB$Age > 28])
sd(MLB$play_WAR[MLB$Age <= 28])
var.test(MLB$play_WAR[MLB$Age > 28], MLB$play_WAR[MLB$Age <= 28], alternative = "greater")

#t-test 

t.test(WIN$Wins[WIN$WAR > 20], WIN$Wins[WIN$WAR <= 20],alternative = "greater")

#wilcox 

median(MLB$play_WAR[MLB$Age <= 28])
median(MLB$play_WAR[MLB$Age > 28])

wilcox.test(MLB$play_WAR[MLB$Age <= 28], MLB$play_WAR[MLB$Age > 28])

#chi-sq 
chisq.test(table(df$MLB.Team.MLB.play_WAR....1.), p=c(1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30,1/30))

#regression 
lm(formula = WAR ~ Wins + CLUTCH, data = WIN)

#ANOVA
sus <-aov(MLB$play_WAR~MLB$Team)
summary(sus)

help("aov")
#Tukey's Post-hoc test
TukeyHSD(sus)

#Shapiro Wilkes test
shapiro.test(WIN$WAR)

#bar chart
barplot(WIN$CLUTCH, space = 10, names.arg = WIN$Team)

help("barplot")
#dot charts 
dotchart(WIN$WAR, groups = WIN$Team, xlab = "WAR", ylab = "Team")

#Individual Scatterplots
plot(MLB$Clutch, MLB$WAR)

plot(Playoffs$WINS, Playoffs$WAR)
plot(WIN$Wins, WIN$WAR)
#Scatterplot matrix
plot(WIN)

#regression line
abline(lm(WAR ~ Wins, data = WIN), col = "RED")

abline(lm(WAR ~ WINS, data = Playoffs), col = "RED")

#Qqplot 
qqnorm(MLB$play_WAR)

qqnorm(WIN$WAR)
#Qqline
qqline(MLB$play_WAR)

qqline(WIN$WAR)
#Density Plot 
d <- density(MLB$Age)

plot(d, main = "Density of MLB ages")

#Histogram
hist(WIN$WAR)

mean(MLB$Age)
#Boxplot
boxplot(formula = WIN$WAR ~ WIN$X0.5, horizontal = TRUE)

#Line Chart 
m20<-mean(MLB$play_WAR[MLB$Age == 20])
m21<-mean(MLB$play_WAR[MLB$Age == 21])
m22<-mean(MLB$play_WAR[MLB$Age == 22])
m23<-mean(MLB$play_WAR[MLB$Age == 23])
m24<-mean(MLB$play_WAR[MLB$Age == 24])
m25<-mean(MLB$play_WAR[MLB$Age == 25])
m26<-mean(MLB$play_WAR[MLB$Age == 26])
m27<-mean(MLB$play_WAR[MLB$Age == 27])
m28<-mean(MLB$play_WAR[MLB$Age == 28])
m29<-mean(MLB$play_WAR[MLB$Age == 29])
m30<-mean(MLB$play_WAR[MLB$Age == 30])
m31<-mean(MLB$play_WAR[MLB$Age == 31])
m32<-mean(MLB$play_WAR[MLB$Age == 32])
m33<-mean(MLB$play_WAR[MLB$Age == 33])
m34<-mean(MLB$play_WAR[MLB$Age == 34])
m35<-mean(MLB$play_WAR[MLB$Age == 35])
m36<-mean(MLB$play_WAR[MLB$Age == 36])
m37<-mean(MLB$play_WAR[MLB$Age == 37])
m38<-mean(MLB$play_WAR[MLB$Age == 38])
m39<-mean(MLB$play_WAR[MLB$Age == 39])
mean(MLB$play_WAR[MLB$Age == 40])

line <- c(m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39)

plot(x = c(20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39), y = line, type = "l", xlab = "Age", ylab = "Average WAR")

#Barchart 

barplot(Playoffs$WAR, names.arg = Playoffs$ï..TEAM, xlab = "Teams", ylab = "Total WAR")

barplot(Playoffs$CLUTCH, names.arg = Playoffs$ï..TEAM, xlab = "Teams", ylab = "Total Clutch")

#Logistic Regression
help("glm")

reg_WAR <- glm(Playoffs$WINS~Playoffs$WAR)

summary(reg)

exp(cbind(OR = coef(reg), confint(reg)))

REG_CLUTCH <- glm(Playoffs$WINS~Playoffs$CLUTCH)

summary(REG_CLUTCH)

exp(cbind(OR = coef(REG_CLUTCH), confint(REG_CLUTCH)))