doAccloop <- function(jj, data) { # Creates a training or test matrix, from data frame x, using a sample of size n (default is all rows)			
  
  dat1=data[jj:(jj+50),]
  
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
  
  dat_temp_matrix <- cbind(
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
  
  # Ensure it is a matrix
  dat_temp <- as.matrix(dat_temp_matrix)
  
  return(dat_temp)
}