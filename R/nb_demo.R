#' calculate the Value Difference Metric
#'
#' @param train_X, a data.frame, the feature variables
#' @param train_y, string or factor, the target variable
#' @param test_x, a list or a data.frame including only one row
#' @details
#' This function calculates the naive bayes probability for each level of the target variable.
#' The detailed calculation is shown in the print method.
#'
#' The output of print.nb_demo is the latex version of the calculation process,
#' which can be pasted to a latex document.
#'
#' @return a list including all the components of the naive bayes
#' @examples
#' results <- nb_demo(xigua3.0[, -c(1, 10)], xigua3.0[, 10], list("青绿", "蜷缩", "浊响", "清晰", "凹陷", "硬滑", 0.697, 0.460))
#' results
#'
#' @export
nb_demo <- function(train_X, train_y, test_x) {
  if (is.data.frame(train_y)) {
    y_name <- names(train_y)
    train_y <- train_y[, y_name]
  } else {
    y_name <- "y"
  }
  if (!is.factor(train_y)) {
    train_y <- as.factor(train_y)
  }
  if (is.list(test_x) && !is.data.frame(test_x) && is.null(names(test_x))) {
    test_x <- as.data.frame(test_x)
    names(test_x) <- names(train_X)
  }
  y_levels <- levels(train_y)
  y_nlevels <- nlevels(train_y)
  x_vars <- colnames(train_X)
  test_x <- test_x[, x_vars]
  results <- list()

  for (i in 1:y_nlevels) {
    product <- 1
    product_expr <- "\\[\n\\begin{array}{ll}\n"
    cur_result <- list()
    y_level <- y_levels[i]
    cur_prior <- sum(train_y == y_level) / length(train_y)
    cur_result[["prior"]] <- cur_prior
    product <- product * cur_prior
    product_expr <- paste(
      product_expr,
      sprintf(
        "\\hspace{0.75em} P\\left(\\text{%s} = \\text{%s} \\right) & \\quad \\hspace{0.5em}  \\frac{%d}{%d} \\\\ \n",
        y_name, y_level, sum(train_y == y_level), length(train_y)
      )
    )
    train_X_sub <- train_X[train_y == y_level, ]
    for (cur_x_var in x_vars) {
      cur_x <- train_X_sub[, cur_x_var]
      if (is.numeric(cur_x)) {
        cur_x_mean <- mean(cur_x)
        cur_x_sd <- sd(cur_x)
        cur_P <- dnorm(test_x[, cur_x_var], mean = cur_x_mean, sd = cur_x_sd)
        cur_result[[cur_x_var]] <- list(mean = cur_x_mean, sd = cur_x_sd, p = cur_P)
        product_expr <- paste(product_expr,
          sprintf(
            "\\times p\\left( {\\text{%s} = \\text{%.3f}|\\text{%s} = \\text{%s}} \\right)  & \\quad \\times \\frac{1}{{\\sqrt {2\\pi } \\times %.3f }} \\exp \\left( { - \\frac{{{{\\left( {%.3f - %.3f } \\right)}^2}}}{{2 \\times {{%.3f} ^2}}}} \\right) \\\\ \n",
            cur_x_var, test_x[, cur_x_var], y_name, y_level, cur_x_sd, test_x[, cur_x_var], cur_x_mean, cur_x_sd
          ),
          sep = " "
        )
      } else {
        numerator <- sum(cur_x == test_x[, cur_x_var])
        denominator <- length(cur_x)
        cur_P <- numerator / denominator
        cur_result[[cur_x_var]] <- list(numerator = numerator, denominator = denominator, P = cur_P)
        product_expr <- paste(product_expr,
          sprintf(" \\times P\\left( {\\text{%s} = \\text{%s}|\\text{%s} = \\text{%s}} \\right) & \\quad \\times \\frac{%d}{%d} \\\\ \n", cur_x_var, test_x[, cur_x_var], y_name, y_level, numerator, denominator),
          sep = ""
        )
      }
      product <- product * cur_P
    }
    cur_result[["product"]] <- product
    product_expr <- paste(product_expr,
      sprintf("& \\quad  = %f", product),
      "\n \\end{array}\n \\]",
      sep = ""
    )
    cur_result[["product_expr"]] <- product_expr
    results[[y_level]] <- cur_result
  }
  class(results) <- "nb_demo"
  return(results)
}
#' @exportS3Method BUPT.PRML::print
print.nb_demo <- function(x, ...) {
  cat("Naive bayes demo:\n")
  for (i in 1:length(x)) {
    cat("\n\n------For", names(x)[i], "------\n\n", x[[i]]$product_expr, sep = "")
  }
}
