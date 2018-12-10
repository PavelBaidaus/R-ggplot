Branch <- read.csv("Branch.csv", header=TRUE,  sep = ";", dec = ",") 

str(Branch)
summary(Branch)

library(ggplot2)

plot <- qplot(NPSperc, NBIcumulativåmUAH, data=Branch, color=RU, xlab= "NPS, %", ylab="NBIcumulat, mUAH", main="Branch order to employee feed")
plot + geom_text(aes(label=Branch), size=4, vjust=-1)

plot <- qplot(Vacanciesperc, NBIcumulativåmUAH, data=Branch, color=RU, xlab= "Vacancies, %", ylab="NBIcumulat, mUAH", main="Branch order to employee feed")
plot + geom_text(aes(label=Branch), size=4, vjust=-1)

install.packages("rgl")
library(rgl)
plot3d(Branch$NPSperc, Branch$NBIcumulativåmUAH, Branch$Vacanciesperc, type="s", size=0.75, lit=FALSE)



Branch1 <- read.csv("Branch1.csv", header=TRUE,  sep = ";", dec = ",") 
str(Branch1)
summary(Branch1)
data <- data.frame(Branch1$NPSperc, Branch1$NBIcumulativåmUAH, Branch1$Staffloadperc)
corr <- cor(data, use="pairwise.complete.obs", method="pearson")
round(corr, digits=2)

plot <- qplot(NPSperc, NBIcumulativåmUAH, data=Branch1, color=RU, xlab= "NPS, %", ylab="NBIcumulat, mUAH", main="Branch order to employee feed")
plot + geom_text(aes(label=Branch), size=4, vjust=-1)

plot <- qplot(Staffloadperc, NBIcumulativåmUAH, data=Branch1, color=RU, xlab= "Staffload, %", ylab="NBIcumulat, mUAH", main="Branch order to employee feed")
plot + geom_text(aes(label=Branch), size=4, vjust=-1)

plot <- qplot(Staffloadperc, NPSperc, data=Branch1, color=RU, xlab= "Staffload, %", ylab="NPS, %", main="Branch order to employee feed")
plot + geom_text(aes(label=Branch), size=4, vjust=-1)


install.packages("corrplot")
library(corrplot)
corrplot(corr, method = "shade")


install.packages("psych")
library(psych)
corr.test(corr, use="complete")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")


install.packages("qtlcharts")
library(qtlcharts)
iplotCorr(corr, reorder=TRUE)


Regression

fit <- lm(NBIcumulativåmUAH ~ Branch1$Staffloadperc + NPSperc, data=data)
summary(fit)
fit1 <- lm(NBIcumulativåmUAH ~ Branch1$Staffloadperc, data=data)
summary(fit1)
fit2 <- lm(NBIcumulativåmUAH ~ Branch1$NPSperc, data=data)
summary(fit2)
fit3 <- lm(NPSperc ~ Branch1$Staffloadperc, data=data)
summary(fit3)



Íîðìàëèçàöèÿ íå ïîìîãëà 
data.scale <- scale(data)
summary(data.scale)
fit <- lm(NBIcumulativåmUAH ~ Branch1$Staffloadperc + NPSperc, data=data.scale)
summary(fit)

Branch3 <- read.csv("Branch3.csv", header=TRUE,  sep = ";", dec = ",") 
str(Branch3)
summary(Branch3)
data3 <- data.frame(Branch3$NPSperc, Branch3$NBIcumulativåmUAH, Branch3$Staffloadperc)
fit <- lm(NBIcumulativåmUAH ~ Branch1$Staffloadperc + NPSperc, data=data3)

summary(fit)

corr <- cor(data3, use="pairwise.complete.obs", method="kendall")
corrplot(corr, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")

library(corrgram)
