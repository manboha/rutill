# ----------------------------------------------------
# title: freqt() 
# description: Printing Frequency Table of Vector or Factor
# usage: freqt(bin, dim = sort(unique(bin)), dimnames = NULL, sep = "\t")
# arguments: bin = Vector or Factor, dim = values, dimnames = value names, sep = seperator 

# author : Bohak Maeng
# creation date : 2018.07.10
# last save date : 2018.07.15
# ----------------------------------------------------

freqt <- function(bin, dim = sort(unique(bin)), dimnames = NULL, sep = "\t")
{
  if (!is.vector(bin) && !is.factor(bin)) 
    stop("'bin' must be a vector or a factor")
  
  grossTotal <- length(bin)  # 전체 Total 빈도수 산출(결측치 포함)
  validTotal <- length(which(!is.na(bin)))  # Valid Total 빈도수 산출
  missingCount <- length(which(is.na(bin)))  # NA의 빈도수 산출, 없으면 0
  
  valueFreq <- NULL
  for (value in dim) {
    valueFreq <- c(valueFreq, length(which(bin==value))) # 해당 값의 빈도수 산출  
  }
  
  percent <- round(valueFreq/grossTotal*100, 1)
  Valid_Pct <- round(valueFreq/validTotal*100, 1)
  Cumul_Pct <- cumsum(Valid_Pct)
  
  # df output
  # rbind(x,list(1,16,"Paul"))
  output <- data.frame(Type=character(),
                       State=character(),
                       Frequency=integer(),
                       Percent=double(),
                       Valid_Pct=double(),
                       Cumul_Pct=double(),
                       stringsAsFactors=FALSE)
  
  for (i in 1:length(dim)) 
  {
    output <- rbind(output, data.frame(Type=ifelse(i==1, "valid", ""),
                                       State=paste(dim[i], dimnames[i]), 
                                       Frequency=valueFreq[i], 
                                       Percent=format(percent[i], nsmall=1), 
                                       Valid_Pct=format(Valid_Pct[i], nsmall=1), 
                                       Cumul_Pct=format(Cumul_Pct[i], nsmall=1),
                                       stringsAsFactors=FALSE))
  }
  
  output <- rbind(output, data.frame(Type="", 
                                     State="Total", 
                                     Frequency=validTotal, 
                                     Percent=format(round(validTotal/grossTotal*100, 1), nsmall=1), 
                                     Valid_Pct=format(round(100, 1), nsmall=1), 
                                     Cumul_Pct="",
                                     stringsAsFactors=FALSE))
  
  if (missingCount != 0) 
  {
    output <- rbind(output, data.frame(Type="missig", 
                                       State="", 
                                       Frequency=missingCount, 
                                       Percent=format(round(missingCount/grossTotal*100, 1), nsmall=1), 
                                       Valid_Pct="", 
                                       Cumul_Pct=""))
    
    output <- rbind(output, data.frame(Type="Total", 
                                       State="", 
                                       Frequency=grossTotal, 
                                       Percent=format(round(100, 1), nsmall=1), 
                                       Valid_Pct="", 
                                       Cumul_Pct=""))
  }
  
  if (sep != "\t") 
  {
    write.table(output, row.names=FALSE, sep=sep, quote=FALSE)
    invisible(output)  # invisible return
  } else
  {
    print(output, row.names=F)   # default, print & return(output)
  }

}  


# ----------------
# 테스트
# ----------------

library(readxl)

# 테스트 데이터 불러오기
df <- read_excel("RawData.xlsx", sheet = "업무효능감")

xc <- df$class  # chr data type
x1 <- df$v1  # no missing, num data type
x8 <- df$v8  # missing, num data type 


# test
values <- c(1:5)
valuenames <- c("매우 그렇지 않다", "그렇지 않다", "보통이다", "약간 그렇다", "매우 그렇다")

freqt(factor(x1), values, valuenames)
freqt(x1)
freqt(xc)
freqt(x8)
freqt(x8, values, valuenames)

freqt(x8, sep=",")

freqt(x8, values, valuenames, sep=",")

t1 <- freqt(x8)
t2 <- freqt(x8, sep=",")
t3 <- freqt(x8, values, valuenames)
t4 <- freqt(x8, values, valuenames, sep=",")

t1
t2
t3
t4

freqt(x8)
freqt(x8, values, valuenames)
freqt(x8, sep=",")
freqt(x8, values, valuenames, sep=",")
