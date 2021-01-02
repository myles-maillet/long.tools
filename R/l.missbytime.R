#' Examine Missing Data for a Variable by Time
#'
#' Tabulates a matrix summary of the number (or percent) or missing data for each variable in a dataset for each time point (i.e., measurement occasion). Assumes all participants are expected to provide data for each time point. Requires missing data to be stored as \code{NA}.
#' @param data dataset stored as dataframe; missingness for all variables will be printed, so it is recommended to only include variables of interest in the dataset
#' @param user_id the name of the level-2 grouping variable (i.e., person id); can be stored as a factor, numeric, or integer within a dataframe
#' @param time_id the name of the \strong{time} variable (i.e., measurement occasion); should be stored as numeric or integer within a dataframe
#' @param missing missing data presented as either a \code{count} or \code{percent}; defaults to \code{count}
#' @keywords missing data
#' @return returns a matrix summary of the number (or percent) of missing observations by measurement occasion
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @seealso \code{\link{l.missbyid}}
#' @seealso \code{\link{l.naconvert}}
#' @export
#' @examples
#' new_ds <- l.missbytime(data = ds, user_id = 'id', time_id = 'time', missing = 'count')


l.missbytime <- function(data, user_id, time_id, missing = "count"){

  require(dplyr)

  data_filled <- l.missfill(data, user_id, time_id)

  time_id <- data_filled[,time_id]

  n_time_points <- length(unique(time_id))
  time_points <- unique(time_id)
  n_vars <- ncol(data_filled)

  output <- matrix(nrow = n_time_points, ncol = n_vars)

  for(i in time_points){
    data_sub <- subset(data_filled, time_id == i)
    #data_sub <- cbind(data_sub[,user_id], data_sub[,-user_id])

    if(missing == "count"){
      m <- colSums(is.na(data_sub))
    }

    if(missing == "percent"){
      m <- colSums(is.na(data_sub))
      m <- round(m/nrow(data_sub),2)
    }

    output[which(time_points==i),] <- m
    output[which(time_points==i),2] <- i

  }

  colnames(output) <- colnames(data_filled)

  data_out <- as.data.frame(output)

  print(data_out[,-1], row.names = F)

}



