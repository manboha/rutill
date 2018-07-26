# ----------------------------------------------------
# title: freqdf() 
# description: Printing Frequency Table of Data Frame
# usage: freqt(df, sep = "\t")
# arguments: df = data frame, sep = seperator 

# author : Bohak Maeng
# creation date : 2018.07.14
# last save date : 2018.07.14
# ----------------------------------------------------


freqdf <- function(df, sep = "\t")
{
  if (!is.data.frame(df)) 
    stop("'df' must be a data frame")
  
  # options(stringsAsFactors=FALSE)

  # df output
  output <- data.frame(Type=character(),
                       State=character(),
                       Frequency=double(),
                       Percent=double(),
                       Valid_Pct=double(),
                       Cumul_Pct=double(),
                       stringsAsFactors=FALSE)
  
  for (colno in 1:length(df)) 
  {
    bin <- df[[colno]] # to a vector data
    dim = sort(unique(bin))
    # dimnames = NULL

    # add variable name
    output <- rbind(output, data.frame(Type="Variable:", 
                                       State=names(df[colno]), 
                                       Frequency="", 
                                       Percent="", 
                                       Valid_Pct="", 
                                       Cumul_Pct="",
                                       stringsAsFactors=FALSE))
    
    # length of bins > 50, next
    if (length(unique(df[[colno]])) > 50)
    {
      output <- rbind(output, data.frame(Type="Skip", 
                                         State="N(bins)>50", 
                                         Frequency="", 
                                         Percent="", 
                                         Valid_Pct="", 
                                         Cumul_Pct=""))
      # print blank row
      output <- rbind(output, data.frame(Type="", 
                                         State="", 
                                         Frequency="", 
                                         Percent="", 
                                         Valid_Pct="", 
                                         Cumul_Pct=""))
      next
    }
    
    # Add Column name
    output <- rbind(output, data.frame(Type=names(output)[1], 
                                       State=names(output)[2], 
                                       Frequency=names(output)[3], 
                                       Percent=names(output)[4], 
                                       Valid_Pct=names(output)[5], 
                                       Cumul_Pct=names(output)[6],
                                       stringsAsFactors=FALSE))
    
    grossTotal <- length(bin)  # 전체 Total 빈도수 산출(결측치 포함)
    validTotal <- length(which(!is.na(bin)))  # Valid Total 빈도수 산출
    missingCount <- length(which(is.na(bin)))  # NA의 빈도수 산출, 없으면 0
    
    valueFreq <- integer()
    for (value in dim) 
    {
      valueFreq <- c(valueFreq, length(which(bin==value))) # 해당 값의 빈도수 산출  
    }
    
    percent <- round(valueFreq/grossTotal*100, 1)
    Valid_Pct <- round(valueFreq/validTotal*100, 1)
    Cumul_Pct <- cumsum(Valid_Pct)

    for (i in 1:length(dim)) 
    {
      output <- rbind(output, data.frame(Type=ifelse(i==1, "valid", ""),
                                         State=paste(dim[i]), 
                                         Frequency=valueFreq[i], 
                                         Percent=format(percent[i], nsmall=1), 
                                         Valid_Pct=format(Valid_Pct[i], nsmall=1), 
                                         Cumul_Pct=format(Cumul_Pct[i], nsmall=1)))
    }
    
    output <- rbind(output, data.frame(Type="", 
                                       State="Total", 
                                       Frequency=validTotal, 
                                       Percent=format(round(validTotal/grossTotal*100, 1), nsmall=1), 
                                       Valid_Pct=format(round(100, 1), nsmall=1), 
                                       Cumul_Pct=""))
    
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

    # print blank row
    output <- rbind(output, data.frame(Type="", 
                                       State="", 
                                       Frequency="", 
                                       Percent="", 
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

# test
d1 <- freqdf(df)
d2 <- freqdf(df, sep=",")
freqdf(df)
freqdf(df, sep=",")

str(d1)
str(d2)

