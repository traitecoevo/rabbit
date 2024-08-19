
## Original function from old code provided to the project
## Written around 10 years ago. We're keeping it here for comaprsion against newer results

doAccloop <- function(dat1) {

  ##  require(e1071,quietly = TRUE)
  ## These functions were being sourced from package e1071. They have been simplified and added here to reduce dependcies
  
  skewness <- function(x) {
    n <- length(x)
    x <- x - mean(x)
    y <- sqrt(n) * sum(x ^ 3) / (sum(x ^ 2) ^ (3/2))
    y <- y * ((1 - 1 / n)) ^ (3/2)
    
    return(y)
  }

  kurtosis <- function(x) {
    
    n <- length(x)
    x <- x - mean(x)
    r <- n * sum(x ^ 4) / (sum(x ^ 2) ^ 2)
    y <- r * (1 - 1 / n) ^ 2 - 3

    return(y)
  }

  meanX=mean(dat1[, 2])
  meany=mean(dat1[, 3])
  meanz=mean(dat1[, 4])
  
  maxx=max(dat1[, 2])
  maxy=max(dat1[, 3])
  maxz=max(dat1[, 4])
  
  minx=min(dat1[, 2])
  miny=min(dat1[, 3])
  minz=min(dat1[, 4])
  
  sdx=sd(dat1[, 2])
  sdy=sd(dat1[, 3])
  sdz=sd(dat1[, 4])
  
  ##Signal Magnitude Area
  SMA<-(sum(abs(dat1[, 2]))+sum(abs(dat1[, 3]))+sum(abs(dat1[, 4])))/nrow(dat1)
  
  ##Overall and Vectorial Dynamic Body Acceleration
  ODBA<-abs(dat1[, 2])+abs(dat1[, 3])+abs(dat1[, 4])#vector
  VDBA<-sqrt(dat1[, 2]^2+dat1[, 3]^2+dat1[, 4]^2)#vector
  
  minODBA<-min(ODBA)
  maxODBA<-max(ODBA)
  
  minVDBA<-min(VDBA)
  maxVDBA<-max(VDBA)
  
  sumODBA<-sum(ODBA)
  sumVDBA<-sum(VDBA)
  
  ##Correlation between axes
  corXY<-cor(dat1[, 2],dat1[, 3])
  corXZ<-cor(dat1[, 2],dat1[, 4])
  corYZ<-cor(dat1[, 3],dat1[, 4])
  
  ##Skewness in each axis
  skx<-skewness(dat1[, 2])
  sky<-skewness(dat1[, 3])
  skz<-skewness(dat1[, 4])
  
  ##kurtosis in each axis
  kux<-kurtosis(dat1[, 2])
  kuy<-kurtosis(dat1[, 3])
  kuz<-kurtosis(dat1[, 4])
  
  #time is V1
  time<-mean(dat1[, 1])
  
  #Time of epoch - this works for AX3 time input only
  
  dat_temp_matrix <- dplyr::tibble(
    time=time, meanX=meanX, meanY=meany, meanZ=meanz,
    maxx=maxx, maxy=maxy, maxz=maxz, 
    minx=minx, miny=miny, minz=minz,
    sdx=sdx,  sdy=sdy, sdz=sdz, 
    SMA=SMA,  minODBA=minODBA, maxODBA=maxODBA, minVDBA=minVDBA, maxVDBA=maxVDBA, 
    sumODBA=sumODBA, sumVDBA=sumVDBA, 
    corXY=corXY, corXZ=corXZ, corYZ=corYZ, 
    skx=skx,  sky=sky, skz=skz, 
    kux=kux,  kuy=kuy, kuz=kuz
  )
  
    return(dat_temp_matrix)
  }