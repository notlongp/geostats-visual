devtools::install_github("notlongp/rgeostats-package")
library(corrplot)
require(RGeostats)
library(stringr)
library(shiny)
library(PerformanceAnalytics)
library(ggplot2)
library(GGally)

file.name <- "data.csv"
df <- read.csv(file.name, dec=",", na.strings=c("","NA"))

for (i in 1:ncol(df)){
  if (startsWith(names(df)[i], "D_") == TRUE ||
      startsWith(names(df)[i], "P_") == TRUE ||
      startsWith(names(df)[i], "C_") == TRUE){
    df[,i] <- as.numeric(paste(df[,i]))
  }
  if (startsWith(names(df)[i], "L_") == TRUE){
    df[,i] <- as.factor(paste(df[,i]))
  }
}

# df <- na.omit(df)
newdf <- data.frame(df[startsWith(names(df), "P_")],
                    df[startsWith(names(df), "D_")],
                    df[startsWith(names(df), "L_")])

coord <- subset(names(df), startsWith(names(df), "C_") == TRUE)

# Round-Up Function ====
roundUp <- function(x, round = c(1:10)){
  increment <- round[[which(x <= 10^floor(log10(x)) * round)[[1]]]]
  10^floor(log10(x)) * increment
}

# Round-Down Function ====
roundDown <- function(x, y, round = c(1:10)){
  if (floor(log10(x)) < floor(log10(y))) {
    increment = 0
  } else {
    increment <- tail(round[which(x >= 10^floor(log10(y)) * round)],1)
    10^floor(log10(x)) * increment
  }
}

# Rounding function
rounding <- function(df, x, y){
  
  x_ceil <- roundUp(max(df[,x]))
  y_ceil <- roundUp(max(df[,y]))
  x_floor <- roundDown(min(df[,x]), x_ceil)
  y_floor <- roundDown(min(df[,y]), y_ceil)
  
  value_list = list(x_ceil, x_floor, y_ceil, y_floor)
  
  return(value_list)
}

# Creating variograms
create_vario <- function(df, s, x, y, p){
  formation <- subset(df, df$F_Top == s)
  
  db <- db.create(formation[-1])
  db <- db.locate(db,c(x, y), "x")
  db <- db.locate(db, names = p, loctype = "z")
  vario_omni = vario.calc(db, nlag=10)
  vario_2dir = vario.calc(db, nlag=10, dir=c(0,90))
  vario_3dir = vario.calc(db, nlag=10, dir=c(0,45,90))
  vario_4dir = vario.calc(db, nlag=10, dir=c(0,30,60,90))
  
  vario_list = list(vario_omni, vario_2dir, vario_3dir, vario_4dir)
  
  return(vario_list)
}

# Choose a direction
select_direction <- function(s){
  if (s == "2dir"){
    return(2)
  } else if (s == "3dir"){
    return(3)
  } else if (s == "4dir"){
    return(4)
  } else {
    return(1)
  }
}

# Outliers filter
outlier_filter <- function(df, p, var){
  if (length(boxplot(df[,p])$out) == 0){
    newdf <- df
  } else if(var == FALSE){
    newdf <- df
  } else {
    outliers <- boxplot(df[,p])$out
    newdf <- df[-c(which(df[,p] %in% outliers)),]
  }
  return(newdf)
}

# Log transform filter
log_transformation <- function(df, p, var){
  if (var == FALSE){
    newdf <- df
  } else {
    newdf <- df
    newdf[,p] <- log10(newdf[,p])
  }
  return(newdf)
}
