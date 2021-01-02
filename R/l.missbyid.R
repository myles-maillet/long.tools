#' Examine Missing Data for a Variable by User
#'
#' Tabulates a matrix summary of the number (or percent) or missing data for each variable in a dataset for each participant. Assumes all participants are expected to provide data for each time point (i.e., measurement occasion). Requires missing data to be stored as \code{NA}.
#' @param data dataset stored as dataframe; missingness for all variables will be printed, so it is recommended to only include variables of interest in the dataset
#' @param user_id the name of the level-2 grouping variable (e.g., person id)
#' @param time_id the name of the \strong{time} variable (i.e., measurement occasion); should be stored as numeric or integer within a dataframe
#' @param missing missing data presented as either a \code{count} or \code{percent}; defaults to \code{count}
#' @keywords missing data
#' @return returns a matrix summary of the number (or percent) of missing observations by person id
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @seealso \code{\link{l.missbytime}}
#' @seealso \code{\link{l.naconvert}}
#' @export
#' @examples
#' new_ds <- l.missbyid(data = ds, user_id = 'id', time_id = 'time', missing = 'count')


l.missbyid <- function(data, user_id, time_id, missing = "count"){

  require(dplyr)

  data_filled <- l.missfill(data, user_id, time_id)

  user_id <- data_filled[,user_id]

  n_users <- length(unique(user_id))
  users <- unique(user_id)
  n_vars <- ncol(data_filled)

  output <- matrix(nrow = n_users, ncol = n_vars)

  for(i in users){
    data_sub <- subset(data_filled, user_id == i)

    if(missing == "count"){
      m <- colSums(is.na(data_sub))
    }

    if(missing == "percent"){
      m <- colSums(is.na(data_sub))
      m <- round(m/nrow(data_sub),2)
    }

    output[which(users==i),] <- m
    output[which(users==i),1] <- i

  }

  colnames(output) <- colnames(data_filled)

  data_out <- as.data.frame(output)

  print(data_out[,-2], row.names = F)

}

