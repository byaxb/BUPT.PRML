#' calculate the Value Difference Metric
#'
#' @param x, a string or factor
#' @param y, the corresponding class label
#' @param norm, the norm to use
#' @details
#' VDM Calculation Demonstration
#'
#' More details about VDM can be found at:
#' https://en.wikipedia.org/wiki/Value_difference_metric
#'
#' Part of the code is adapted from the following source:
#' https://github.com/esmondhkchu/rvdm/tree/master
#'
#' @return the vdm value between pairs of the levels of x
#' @examples
#' x <- xigua3.0$根蒂
#' y <- xigua3.0$好瓜
#' vdm_demo(x, y)
#'
#' @export
vdm_demo <- function(x, y, norm = 2) {
  append_table <- function(x, y, i) {
    subset <- y[x == i]
    proba <- sapply(sort(unique(y)), function(j) sum(subset == j) / length(subset))
    return(proba)
  }

  # get vdm probabilities for a single dimension
  vdm_proba_single <- function(x, y) {
    proba <- lapply(unique(x), function(i) append_table(x, y, i))
    names(proba) <- unique(x)
    return(proba)
  }

  # get delta
  vdm_delta_single <- function(proba, val_1, val_2, norm = 2) {
    delta <- sum(abs(proba[[val_1]] - proba[[val_2]])^norm)
    return(delta)
  }

  x <- as.character(x)
  x_levels <- unique(x)
  level_paris <- expand.grid(x_levels, x_levels)
  level_paris <- level_paris[level_paris$Var1 != level_paris$Var2, ]
  proba <- vdm_proba_single(x, y)
  vdm_values <- apply(level_paris, 1, function(i) vdm_delta_single(proba, i[1], i[2], norm))
  level_paris$vdm_values <- vdm_values
  level_paris_with_vdm <- level_paris
  names(level_paris_with_vdm) <- c("level_1", "level_2", "vdm_value")
  return(level_paris_with_vdm)
}
