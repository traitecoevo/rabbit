
## doAccloop is the Original function from old code provided to the project
## Written around 10 years ago. We're keeping it here for comaprsion against newer results
## doAccloop_all is a wrapepr toe enabl clauclations over sliding windows, similar to what was implemented previously

doAccloop_all <- function(dat1, window_size = 50) {

  i <- seq(nrow(dat1))
  dfs <- purrr::map(i, ~ dplyr::slice(dat1, .x + seq_len(window_size) - 1))

  f <- function(x){
    doAccloop(x)
  }

  out <-  purrr::map(dfs, f) |> purrr::list_rbind()

}

doAccloop <- function(dat1) {

  meanX=mean(dat1[, "x"])
  meany=mean(dat1[, "y"])
  meanz=mean(dat1[, "z"])
  
  maxx=max(dat1[, "x"])
  maxy=max(dat1[, "y"])
  maxz=max(dat1[, "z"])
  
  minx=min(dat1[, "x"])
  miny=min(dat1[, "y"])
  minz=min(dat1[, "z"])
  
  sdx=stats::sd(dat1[, "x"])
  sdy=stats::sd(dat1[, "y"])
  sdz=stats::sd(dat1[, "z"])
  
  ##Signal Magnitude Area
  SMA<-(sum(abs(dat1[, "x"]))+sum(abs(dat1[, "y"]))+sum(abs(dat1[, "z"])))/nrow(dat1)
  
  ##Overall and Vectorial Dynamic Body Acceleration
  ODBA<-abs(dat1[, "x"])+abs(dat1[, "y"])+abs(dat1[, "z"])#vector
  VDBA<-sqrt(dat1[, "x"]^2+dat1[, "y"]^2+dat1[, "z"]^2)#vector
  
  minODBA<-min(ODBA)
  maxODBA<-max(ODBA)
  
  minVDBA<-min(VDBA)
  maxVDBA<-max(VDBA)
  
  sumODBA<-sum(ODBA)
  sumVDBA<-sum(VDBA)
  
  ##Correlation between axes
  corXY<-stats::cor(dat1[, "x"], dat1[, "y"])
  corXZ<-stats::cor(dat1[, "x"], dat1[, "z"])
  corYZ<-stats::cor(dat1[, "y"], dat1[, "z"])
  
  ##Skewness in each axis
  ## require(e1071,quietly = TRUE)
  ## skewness functions was being sourced from package e1071. It has been simplified and added here to reduce dependencies
  skewness <- function(x) {
    n <- length(x)
    x <- x - mean(x)
    y <- sqrt(n) * sum(x^3) / (sum(x^2)^(3 / 2))
    y <- y * ((1 - 1 / n))^(3 / 2)

    return(y)
  }
  
  skx<-skewness(dat1[, "x"])
  sky<-skewness(dat1[, "y"])
  skz<-skewness(dat1[, "z"])
  
  #time is V1
  time<-mean(dat1[, "time"])
  
  #Time of epoch - this works for AX3 time input only
  
  dat_temp_matrix <- dplyr::tibble(
    time=time, meanX=meanX, meanY=meany, meanZ=meanz,
    maxx=maxx, maxy=maxy, maxz=maxz, 
    minx=minx, miny=miny, minz=minz,
    sdx=sdx,  sdy=sdy, sdz=sdz, 
    SMA=SMA,  minODBA=minODBA, maxODBA=maxODBA, minVDBA=minVDBA, maxVDBA=maxVDBA, 
    sumODBA=sumODBA, sumVDBA=sumVDBA, 
    corXY=corXY, corXZ=corXZ, corYZ=corYZ, 
    skx=skx,  sky=sky, skz=skz
  )
  
    return(dat_temp_matrix)
  }


