#' Calculate the ICC for a Level-1 Variable
#'
#' Calculates an ICC for a level-1 variable by fitting a two-level unconditional model or unconditional growth model to determine the proportion of variance at each level. The ICC reflects the proportion of variance that is attributed to the higher level (e.g., ICC = 0.40 indicates that 40% of the variance in a variable is at level-2).
#' @param data dataset stored as dataframe
#' @param user_id the name of the level-2 grouping variable (i.e., person id)
#' @param var_id the name of the level-1 variable you want to calculate an ICC for; should be stored as numeric or integer within a dataframe
#' @param var_type the type of level-1 variable you are using, either \code{continuous} or \code{binary}; defaults to \code{continuous}; see Austin & Merlo (2017) for details on how the ICC is calculated with a binary outcome variable
#' @param time_id if included, will calculate the ICC using an unconditional growth model (i.e., time included in the model as a predictor); should be stored as numeric or integer within a dataframe
#' @param verbose if \code{TRUE}, outputs a summary statement along with the ICC; defaults to \code{FALSE}
#' @keywords icc, intraclass correlation coefficient
#' @return returns a value for the ICC
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @seealso \code{\link{l.describe}}
#' @import dplyr lme4
#' @export
#' @references Austin, P.C. & Merlo, J. (2017). Intermediate and advanced topics in multilevel logistic regression analysis. \emph{Statistics in medicine, 36}(20), 3257-3277.
#' @examples
#' l.icc(data = ds, user_id = 'id', var_id = 'l1_var')
#' l.icc(data = ds, user_id = 'id', var_id = 'l1_var', time_id = 'time')
#' l.icc(data = ds, user_id = 'id', var_id = 'l1_var', var_type = 'binary', verbose = TRUE)


#function

l.icc <- function(data, user_id, var_id, var_type="continuous",
                     time_id=NULL, verbose = FALSE){

  require(dplyr)
  require(lme4)

  if(is.data.frame(data)==FALSE) stop('data is not a dataframe')
  if(length(data[,user_id]) <= 1) stop("incorrect id input; try 'user_id' in quotations")
  if(length(data[,var_id]) <= 1) stop("incorrect variable input; try 'var_id' in quotations")

  user_id <- data[, user_id]
  var_id <- data[, var_id]


if(is.null(time_id)==TRUE){

  if (var_type == "continuous"){
    model <- lmer(var_id ~ 1 + (1 | user_id), data, REML = F)
    out1 <- as.data.frame(VarCorr(model))
    l2v <- out1$vcov[1]
    rsv <- out1$vcov[2]
    icc <- l2v / (l2v + rsv)
  }

  if (var_type == "binary"){
    model <- glmer(var_id ~ 1 + (1 | user_id), data, family = "binomial")
    out1 <- as.data.frame(VarCorr(model))
    l2v <- out1$vcov[1]
    rsv <- (pi^2)/3
    icc <- l2v / (l2v + rsv)
  }

}

if(is.null(time_id)==FALSE){

  time_id <- data[, time_id]

    if (var_type == "continuous"){
      model <- lmer(var_id ~ time_id + (1 | user_id), data, REML = F)
      out1 <- as.data.frame(VarCorr(model))
      l2v <- out1$vcov[1]
      rsv <- out1$vcov[2]
      icc <- l2v / (l2v + rsv)
    }

    if (var_type == "binary"){
      model <- glmer(var_id ~ time_id + (1 | user_id), data, family = "binomial")
      out1 <- as.data.frame(VarCorr(model))
      l2v <- out1$vcov[1]
      rsv <- (pi^2)/3
      icc <- l2v / (l2v + rsv)

    }

  }

if(verbose == FALSE){
  return(icc)
}

if(verbose == TRUE){

  statement <- paste0(round(icc*100,digits=1),"% of the variance is at level-2 (i.e., person-level, group-level)")
  return(statement)

}

}
