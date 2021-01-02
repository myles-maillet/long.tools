#' Calculate Descriptive Statistics for a Level-1 Variable
#'
#' Calculates several descriptive statistics for a level-1 variable. Can be grouped by time or person id (i.e., level-2 grouping variable).
#' @param data dataset stored as dataframe
#' @param user_id the name of the level-2 grouping variable (e.g., person id)
#' @param var_id the name of the level-1 variable you want to examine; should be stored as numeric or integer within a dataframe
#' @param time_id if longitudinal, the name of the level-1 time variable (i.e., measurement occasion); should be stored as numeric or integer within a dataframe; if ignored, descriptive statistics are calculated by person id
#' @param dec the number of decimals used to display descriptive statistics; defaults to 2
#' @keywords descriptive statistics, describe
#' @return returns a matrix with descriptive statistics for each time point or user
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @import dplyr psych
#' @export
#' @examples
#' l.describe(data = ds, user_id = 'id', var_id = 'l1_var')
#' l.describe(data = ds, user_id = 'id', var_id = 'l1_var', time_id = 'time')

l.describe <- function(data, user_id, var_id, time_id = NULL, dec = 2){

  require(dplyr)
  require(psych)

  if(is.data.frame(data)==FALSE) stop('data is not a dataframe')
  if(length(data[,user_id]) <= 1) stop("incorrect id input; try 'user_id' in quotations")

  user_id <- data[, user_id]
  #var_id <- data[, var_id]

  if(is.factor(user_id)==TRUE){
    user_id <- as.numeric(as.character(user_id))
  }

if(is.null(time_id)==FALSE){

  time_id <- data[, time_id]

  n_time_points <- length(unique(time_id))
  time_points <- unique(time_id)

  output <- matrix(nrow = n_time_points, ncol = 9)

  for(i in time_points){
    data_sub <- subset(data, time_id == i)
    n <- length(data_sub[,var_id]) - sum(is.na(data_sub[,var_id]))
    mn <- mean(data_sub[,var_id], na.rm=T)
    mdn <- median(data_sub[,var_id], na.rm=T)
    stdv <- sd(data_sub[,var_id], na.rm=T)
    minm <- min(data_sub[,var_id], na.rm = T)
    maxm <- max(data_sub[,var_id], na.rm = T)
    skewv <- skew(data_sub[,var_id], na.rm = T)
    kurtv <- kurtosi(data_sub[,var_id], na.rm = T)

    output[which(time_points==i),1] <- i
    output[which(time_points==i),2] <- n
    output[which(time_points==i),3] <- round(mn,dec)
    output[which(time_points==i),4] <- round(mdn,dec)
    output[which(time_points==i),5] <- round(stdv,dec)
    output[which(time_points==i),6] <- round(minm,dec)
    output[which(time_points==i),7] <- round(maxm,dec)
    output[which(time_points==i),8] <- round(skewv,dec)
    output[which(time_points==i),9] <- round(kurtv,dec)
  }

  colnames(output) <- c("time","n","mean","median","sd","min","max","skew","kurt")
  return(output)

}

if(is.null(time_id)==TRUE){

  n_users <- length(unique(user_id))
  users <- unique(user_id)

  output <- matrix(nrow = n_users, ncol = 9)

  for(i in users){
    data_sub <- subset(data, user_id == i)
    n <- length(data_sub[,var_id]) - sum(is.na(data_sub[,var_id]))
    mn <- mean(data_sub[,var_id], na.rm=T)
    mdn <- median(data_sub[,var_id], na.rm=T)
    stdv <- sd(data_sub[,var_id], na.rm=T)
    minm <- min(data_sub[,var_id], na.rm = T)
    maxm <- max(data_sub[,var_id], na.rm = T)
    skewv <- skew(data_sub[,var_id], na.rm = T)
    kurtv <- kurtosi(data_sub[,var_id], na.rm = T)

    output[which(users==i),1] <- i
    output[which(users==i),2] <- n
    output[which(users==i),3] <- round(mn,dec)
    output[which(users==i),4] <- round(mdn,dec)
    output[which(users==i),5] <- round(stdv,dec)
    output[which(users==i),6] <- round(minm,dec)
    output[which(users==i),7] <- round(maxm,dec)
    output[which(users==i),8] <- round(skewv,dec)
    output[which(users==i),9] <- round(kurtv,dec)
  }

  colnames(output) <- c("id","n","mean","median","sd","min","max","skew","kurt")
  return(output)

}

}

