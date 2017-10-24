require(plyr)
require(zoo)
require(ggplot2)
require(reshape2)

# Import:
# - Thumbtack Economic Sentiment Survey data
# - FRED datasets
sentiment<-read.csv("ESSsentiment.csv")
unemp <- read.csv("unemployment-FRED.csv")
consum <- read.csv("PCEC96.csv")
payrolls <- read.csv("PAYEMS.csv")
cons.sentiment <- read.csv("UMCSENT.csv")

# Convert date columns to Date format on all data
sentiment$start_date <- as.Date(sentiment$start_date, "%m/%d/%y")
sentiment$end_date <- as.Date(sentiment$end_date, "%m/%d/%y")
unemp$DATE <- as.Date(unemp$DATE, "%Y-%m-%d")
consum$DATE <- as.Date(consum$DATE, "%Y-%m-%d")
payrolls$DATE <- as.Date(payrolls$DATE, "%Y-%m-%d")
cons.sentiment$DATE <- as.Date(cons.sentiment$DATE, "%Y-%m-%d")

# Group data in the same date ranges:
# The result is grouped by (start_date, end_date) and computes count and mean of scores for
# each group
sentiment <- ddply(sentiment,.(start_date,end_date),summarise,count=sum(count), score=mean(score))

# Convert quarterly data to monthly by interpolation:
# 1. Convert sentiment data to zoo format and only keep score for the following 'spline'
sentiment.scores <- read.zoo(sentiment)[,"score"]
# 2. Compute the range of interpolation. [first start_date..last end_date]
DateSeq <-seq(sentiment$start_date[1],tail(sentiment$end_date,1),by="month")
# 3. Create a new data frame which contains start_date and interpolated values
sentimen.scores.interpolated <- data.frame(start_date=DateSeq, score=na.spline(sentiment.scores, xout = DateSeq))
# 4. Merge the above data frame with sentiment data and keep all the values from interpolated data frame.
final.sentiment <- (merge(sentiment, sentimen.scores.interpolated, by='start_date', all.y = T))

# Merge the Economic sentiment data with FRED datasets
sentimentUNRATE <- merge(unemp,final.sentiment, by.x="DATE", by.y="start_date")
sentimentCONSUM <- merge(consum,final.sentiment, by.x="DATE", by.y="start_date")
sentimentPAYROLL<- merge(payrolls,final.sentiment, by.x="DATE", by.y="start_date")
sentimentCONSUMERsen<- merge(cons.sentiment,final.sentiment, by.x="DATE", by.y="start_date")

# Make sure all columns with floats are numeric not categorical
sentimentUNRATE$score.y<-as.numeric(as.matrix(sentimentUNRATE$score.y))
sentimentCONSUM$score.y<-as.numeric(as.matrix(sentimentCONSUM$score.y))
sentimentPAYROLL$score.y<-as.numeric(as.matrix(sentimentPAYROLL$score.y))
sentimentCONSUMERsen$score.y<-as.numeric(as.matrix(sentimentCONSUMERsen$score.y))

# Thumbtack sentiment scores's prediction of unemployment rate using linear regression
# --------------------------------------------------------------------------------------
ols <-lm(sentimentUNRATE$score.y ~ sentimentUNRATE$UNRATE)
summary(ols)
plot(sentimentUNRATE$score.y ~ sentimentUNRATE$UNRATE, xlab="Monthly Unemployment rate" , ylab="Monthly economic sentiment")

# Compute the correlation between score and Unemployment rate
cor(sentimentUNRATE$score.y , sentimentUNRATE$UNRATE)

# Compare the change of scores and Unemployment rate from their starting points
x<-sentimentUNRATE$DATE
y1<-(sentimentUNRATE$score.y-sentimentUNRATE$score.y[1])/sentimentUNRATE$score.y[1]
y2<-(sentimentUNRATE$UNRATE-sentimentUNRATE$UNRATE[1])/sentimentUNRATE$UNRATE[1]
df<- data.frame(x,y1,y2)

# Convert them to long format and plot
df2<-melt(data=df, id.vars = "x")
ggplot(data=df2, aes(x = x, y =  value , colour = variable)) +
  geom_line()+
  xlab("Date") +
  ylab("y1=sentiment score, y2=UnmRate")

# Thumbtack sentiment prediction of consumer expenditures
# --------------------------------------------------------------------------------------
ols2 <-lm(sentimentCONSUM$score.y ~ sentimentCONSUM$PCEC96)
summary(ols2)
plot(sentimentCONSUM$score.y ~ sentimentCONSUM$PCEC96, xlab="Monthly consumer expenditures" ,
     ylab="Monthly economic sentiment")

# Compare the correlation of sentiment score and consumer expenditures(PCE96)
cor(sentimentCONSUM$score.y , sentimentCONSUM$PCEC96)

# Compare the change of scores and consumer expenditures(PCE96) from their starting points
x<-sentimentCONSUM$DATE
y1<-(sentimentCONSUM$score.y-sentimentCONSUM$score.y[1])/sentimentCONSUM$score.y[1]
y2<-(sentimentCONSUM$PCEC96-sentimentCONSUM$PCEC96[1])/sentimentCONSUM$PCEC96[1]
df<- data.frame(x,y1,y2)

# Convert them to long format and plot
df2<-melt(data=df, id.vars = "x")
ggplot(data=df2, aes(x = x, y =  value , colour = variable)) +
  geom_line()+
  xlab("Date") +
  ylab("y1=sentiment score, y2=Consumption")

# Thumbtack sentiment scores prediction of changes in nonfarm pyarolls
# --------------------------------------------------------------------------------------
ols3 <-lm(sentimentPAYROLL$score.y ~ sentimentPAYROLL$PAYEMS)
summary(ols3)
plot(sentimentPAYROLL$score.y ~ sentimentPAYROLL$PAYEMS,xlab="changes in nonfarm pyarolls" ,
     ylab="Monthly economic sentiment")

# Compare the correlation of sentiment score and nonfarm pyarolls(PAYEMS)
cor(sentimentPAYROLL$score.y , sentimentPAYROLL$PAYEMS)

# Compare the change of sentiment scores and nonfarm pyarolls(PAYEMS) from their starting points
x<-sentimentPAYROLL$DATE
y1<-(sentimentPAYROLL$score.y-sentimentPAYROLL$score.y[1])/sentimentPAYROLL$score.y[1]
y2<-(sentimentPAYROLL$PAYEMS-sentimentPAYROLL$PAYEMS[1])/sentimentPAYROLL$PAYEMS[1]
df<- data.frame(x,y1,y2)

# Convert them to long format and plot
df2<-melt(data=df, id.vars = "x")
ggplot(data=df2, aes(x = x, y =  value , colour = variable)) +
  geom_line()+
  xlab("Date") +
  ylab("y1=sentiment score, y2=payrolls")

#Thumbtack sentiment prediction of CONSUMER SENTIMENT
# --------------------------------------------------------------------------------------
ols4 <-lm(sentimentCONSUMERsen$score.y ~ sentimentCONSUMERsen$UMCSENT)
summary(ols4)
plot(sentimentCONSUMERsen$score.y ~ sentimentCONSUMERsen$UMCSENT, xlab="Consumer sentiment" ,
     ylab="Monthly economic sentiment")

# Compare the correlation of score and CONSUMER SENTIMENT(UMSCENT)
cor(sentimentCONSUMERsen$score.y , sentimentCONSUMERsen$UMCSENT)

# Compare the change of sentiment scores and CONSUMER SENTIMENT(UMSCENT) from their starting points
x<-sentimentCONSUMERsen$DATE
y1<-(sentimentCONSUMERsen$score.y-sentimentCONSUMERsen$score.y[1])/sentimentCONSUMERsen$score.y[1]
y2<-(sentimentCONSUMERsen$UMCSENT-sentimentCONSUMERsen$UMCSENT[1])/sentimentCONSUMERsen$UMCSENT[1]
df<- data.frame(x,y1,y2)

# Convert them to long format and plot
df2<-melt(data=df, id.vars = "x")
ggplot(data=df2, aes(x = x, y =  value , colour = variable)) +
  geom_line()+
  xlab("Date") +
  ylab("y1=sentiment score, y2=consumer sentiment")
