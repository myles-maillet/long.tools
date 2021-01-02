#' Impute NA Values for Missing Observations in a Longitudinal Design
#'
#' Imputes NA values for missing (but expected) observations for each participant at each time point in a longitudinal design. In other words, creates a dataframe that includes all possible measurement occasions for each user. Limited use as-is, but a dependency for other missing data functions in this package. Requires missing data to be stored as \code{NA}.
#' @param data dataset stored as dataframe
#' @param user_id the name of the level-2 grouping variable (e.g., person id)
#' @param time_id if included, will calculate the ICC using an unconditional growth model (i.e., time included in the model as a predictor); should be stored as numeric or integer within a dataframe
#' @keywords missing data, imputation
#' @return returns an imputed dataset with \code{NA} values for missing observations
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @seealso \code{\link{l.missbytime}}
#' @seealso \code{\link{l.missbyid}}
#' @seealso \code{\link{l.naconvert}}
#' @export
#' @examples
#' new_ds <- l.missfill(data = ds, user_id = 'id', time_id = 'time')


l.missfill <- function(data, user_id, time_id){

  require(dplyr)

  x1 <- which(names(data) == user_id)
  x2 <- which(names(data) == time_id)

  if(is.factor(data[,user_id])==TRUE){
    data[,user_id] <- as.numeric(as.character(data[,user_id]))
  }

  user_id <- data[,user_id]
  time_id <- data[,time_id]

  users <- unique(user_id)
  time_points <- unique(time_id)

  fill_ids <- sort(rep(users, length(time_points)))
  fill_times <- rep(time_points, length(users))

  fds <- as.data.frame(cbind(fill_ids,fill_times))

  colnames(fds)[1] <- colnames(data)[x1]
  colnames(fds)[2] <- colnames(data)[x2]

  data_full <- full_join(fds, data, by = colnames(fds))

  return(data_full)

}


