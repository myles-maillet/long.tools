#' Center a Level-1 Variable
#'
#' For centering level-1 variables, either with person-mean (i.e., within-person) or grand-mean centering. Using the function will output a new dataset with both the centered and group mean variables.
#' @param data dataset stored as dataframe
#' @param user_id the name of the level-2 grouping variable (e.g., person id); can be stored as a factor, numeric, or integer within a dataframe
#' @param var_id the name of the level-1 variable you want to center; should be stored as numeric or integer within a dataframe
#' @param type the type of centering you want to use, either person-mean centering \code{pmc} (i.e., centering around the means of level-2 grouping variables) or grand-mean centering \code{gmc} (i.e., centering around the grand mean); defaults to \code{pmc}; see Hoffman & Stawksi (2009) for a discussion of centering approaches
#' @keywords centering variables, within-person
#' @return a dataframe with a centered variable appended to it; if using person-mean centering, the person-mean variable will also be appended
#' @author Myles A. Maillet, \email{myles.a.maillet@gmail.com}
#' @seealso \code{\link{l.describe}}
#' @import dplyr
#' @export
#' @references Hoffman, L., & Stawski, R. S. (2009). Persons as contexts: Evaluating between-person and within-person effects in longitudinal analysis. \emph{Research in Human Development, 6}(2-3), 97-120.
#' @examples
#' new_ds <- l.center(data = ds, user_id = 'id', var_id = 'l1_var', type = 'pmc')

l.center <- function(data, user_id, var_id, type = "pmc"){

  require(dplyr)

  if(is.data.frame(data)==FALSE) stop('data is not a dataframe')
  if(length(data[,user_id]) <= 1) stop("incorrect id input; try 'user_id' in quotations")
  if(length(data[,var_id]) <= 1) stop("incorrect variable input; try 'var_id' in quotations")

if(type == "pmc"){

  pm_var <- aggregate(data[,var_id], data = data, by=list(data[,user_id]), mean, na.rm=T)
  colnames(pm_var) <- c(user_id,paste0(var_id,"_pm"))
  df <- full_join(data, pm_var, by=user_id)
  wpv <- df[,var_id] - df[,paste0(var_id,"_pm")]
  wpv <- data.frame(wpv)
  colnames(wpv) <- paste0(var_id,"_pmc")
  df <- cbind(df, wpv)

}

if(type == "gmc"){

  gm_value <- rep(mean(data[,var_id], na.rm = T), nrow(data))
  df <- cbind(data, gm_value)
  colnames(df) <- c(colnames(data),paste0(var_id,"_gm"))
  wpv <- df[,var_id] - df[,paste0(var_id,"_gm")]
  wpv <- data.frame(wpv)
  colnames(wpv) <- paste0(var_id,"_gmc")
  df <- cbind(df[,-ncol(df)], wpv)

}

  return(df)

}
