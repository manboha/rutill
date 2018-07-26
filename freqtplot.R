# ----------------------------------------------------
# title: freqt_plot() 
# description: Printing Frequency Table of Data Frame
# usage: freqt(df, sep = "\t")
# arguments: df = data frame, sep = seperator 

# author : Bohak Maeng
# creation date : 2018.07.14
# last save date : 2018.07.21
# ----------------------------------------------------

# 인수 : 
# label : 라벨
# count : 빈도 수
# title : 타이틀(원 가운데 위치)
# label.position : 1=원밖(default), 0=원안
# orfer : 1=original, 2=reverse order, 3=descending order by count size

freqt_plot <- function(label, count, title = NULL, label.position = 1, 
                       label.size = 3, order = 1) {

  # ggplot2
  library(ggplot2)
  if (!"package:ggplot2" %in% search()) {
    stop('The package ggplot2 was not loaded')
  }

  # check length(count) == length(label)
  if (!length(count) == length(label)) {
    stop("The length of count is not equal to the length of label")
  }
  
  title <- gsub("(.{16})", "\\1\n", title)  # 16글자마다 줄바꿈
  
  if (label.position != 0) label.position = 1
  
  df <- data.frame(label, count)
  
  if (order == 2) {
    df <- df[rev(rownames(df)),]  # reverse order (2)
  } else if (order == 3) {
    df <- df[order(-df$count),]  # descending order by count size (3)
  }

  df$prop <- round(df$count/sum(df$count)*100, 1) # 구성비율
  df$ymin <- cumsum(df$prop) - df$prop   # 시작위치, 이전 최대값
  df$ymax <- cumsum(df$prop)             # 종료위치, 누적 합계값
  df$ypos <- df$ymin + df$prop/2         # 라벨위치, 중간위치
  df$hjust <- ifelse(df$ypos < 50, 0, 1)    # 라벨정렬, 0-왼쪽정렬, 1-오른쪽 정렬
  
  blank_theme <- theme_gray(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length=unit(0,"cm"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.margin=unit(c(0,0,0,0),"lines"),
      complete=TRUE
    )
  
  fillcolor <- topo.colors(nrow(df))  # 갯수 만큼 색 자동 출력
  
  p <- ggplot(df) +
    geom_rect(aes(xmin=3, xmax=4, ymin=ymin, ymax=ymax),
              fill=fillcolor, colour="white", alpha=0.5) +
    coord_polar(theta = "y") +     # 막대 그래프를 둥글게 -> pie 차트
    xlim(0, 4+label.position) +    # x축은 0 ~ 4, 막대는 xmin 3 ~ xmax 4, label.position
    blank_theme
  
  showlabel <- paste0(df$label, "\n", format(df$prop, nsmall=1), "%")  # 라벨 모양 꾸미기
  
  if(label.position == 1) {
    p + geom_text(aes(label=showlabel, x=4.5, y=ypos, hjust=hjust), size=label.size) +
      geom_segment(aes(x=4, xend=4.3, y=ypos, yend=ypos)) +
      annotate("text", x = 0, y = 0, label = title)
  } else {
    p + geom_text(aes(label=showlabel, x=3.5, y=ypos), size=label.size) +
      annotate("text", x = 0, y = 0, label = title)
  }
    
}



### 테스트
label=c("매우 만족","만족","보통","불만족","매우 불만족")
count=c(50,21,10,6,2)
title <- "Q1. 현재 진행되고 있거나 경험하신 출원절차 분야에 대해 어떻게 생각하십니까?"

freqt_plot(label, count, title = title, order=4)
freqt_plot(label, count, title = "질문 1", label.position = 0, label.size = 3)

#==========
# test

library(readxl)

# 테스트 데이터 불러오기
df <- read_excel("RawData.xlsx", sheet = "업무효능감")

xc <- df$class  # chr data type
x1 <- df$v1  # no missing, num data type
x8 <- df$v8  # missing, num data type 


values <- c(1:5)
valuenames <- c("매우 부정", "약간 부정", "보통", "약간 긍정", "매우 긍정")

t1 <- freqt(x8)
t2 <- freqt(x8, sep=",")
t3 <- freqt(x8, values, valuenames)
t4 <- freqt(x8, values, valuenames, sep=",")
t5 <- freqt(xc)


label <- t5$State
count <- t5$Frequency
end <- which(label=="Total")-1
label <- label[1:end]
count <- count[1:end]
label
count

title <- "Q1. 현재 진행되고 있거나 경험하신 분야에 대해 만족하십니까?"

freqt_plot(label, count, title = title, order=1)
freqt_plot(label, count, title = title, order=2)
freqt_plot(label, count, title = title, label.position=0, order=3)


