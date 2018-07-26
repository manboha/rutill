
# ============
# Bar graph
# ============

## 3.1. Making a Basic Bar Graph

library(ggplot2)
library(gcookbook) # for data sets

# x축에 group, y(막대의 높이)에 weight, 
# stat="identity"는 막대의 높이를 y값에 동일하게 맞춤
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity")

# x축이 연속형 변수일 경우
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_bar(stat="identity")
# 연속형 변수를 범주형 변수로 변환 factor
ggplot(BOD, aes(x=factor(Time), y=demand)) +
  geom_bar(stat="identity")

# 막대에 색을
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity", fill="lightblue", colour="black")

## 3.2. Grouping Bars Together

cabbage_exp
# dodge는 피하기, 겹치지 않게 배치
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat = "identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat = "identity", colour="black") +
  scale_fill_brewer(palette="Pastel1")

ce <- cabbage_exp[1:5, ]
ce
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat = "identity", colour="black") +
  scale_fill_brewer(palette="Pastel1")

## 3.3. Making a Bar Graph of Counts

# the variable on the x-axis is discrete
ggplot(diamonds, aes(x=cut)) +
  geom_bar() # defalut stat="bin"

# the variable on the x-axis is continuous
ggplot(diamonds, aes(x=carat)) +
  geom_bar()
ggplot(diamonds, aes(x=carat)) +
  geom_histogram()

## 3.4. Using Colors in a Bar Graph

library(gcookbook) # For the data set
upc <- subset(uspopchange, rank(Change)>40) 
upc

ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values = c("#669933", "#FFCC66")) +
  xlab("State")


## 3.5. Coloring Negative and Positive Bars Differently

library(gcookbook) # For the data set
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
csub

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", size=0.25, color="gray") +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide=FALSE)


## 3.6. Adjusting Bar Width and Spacing

library(gcookbook) # For the data set
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")  # default width=0.9

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", width=0.5)  # narrower bars

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", width=1)  # wider bars (maximun width = 1)

# grouped bars : default no space bwtween bars
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.6))


## 3.7. Making a Stacked Bar Graph

library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse = TRUE))

library(plyr)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar))) +
  geom_bar(stat="identity")


