#' Explain difference between predictions from a model
#'
#' @param mod Model
#' @param x1 Input 1. Single row data frame or matrix.
#' @param x2 Input 2. Single row data frame or matrix.
#' @param d Max number of input dimensions to use.
#' @param predictfunc Func to use to predict. Defaults to `predict`.
#' Takes in model and data.
#' @param predictfuncwithmod Func to use to predict. Only takes in data.
#' @param name1 Name x1
#' @param name2 Name for x2
#' @param namesfromcol Column of x1/x2 to get name from
#'
#' @return ggplot object showing effect
#' @export
#' @importFrom stats predict
#'
#' @examples
#' # gbm
#' m1 <- gbm::gbm(Petal.Width ~ Sepal.Length + Sepal.Width, data=iris, distribution='gaussian')
#' x1 <- iris[1,]
#' x2 <- iris[2,]
#' epdiff1(m1, x1, x2)
#' epdiff1(m1, iris[12,], iris[20,])
#' epdiff1(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
#' epdiff1(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),], predictfunc=predict)
#' epdiff1(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),],
#'     predictfuncwithmod=function(xx){suppressMessages(predict(m1, xx))})
#' epdiff1(NA, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),],
#'     predictfuncwithmod=function(xx){suppressMessages(predict(m1, xx))})
#' epdiff1(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),], name1='aa', name2='bb')
#' epdiff1(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),], namesfromcol="Species")
#'
#' # Linear model
#' mod_lm <- lm(Petal.Width ~ Sepal.Length + Sepal.Width, data=iris)
#' epdiff1(mod_lm, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
#' epdiff1(mod_lm, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),], d=3)
#' mod_lm2 <- lm(Petal.Width ~ Petal.Length + Sepal.Width, data=iris)
#' epdiff1(mod_lm2, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
epdiff1 <- function(mod, x1, x2, d, predictfunc, predictfuncwithmod,
                    name1, name2, namesfromcol) {
  stopifnot(nrow(x1)==1, nrow(x2)==1, ncol(x1)==ncol(x2))

  if (missing(predictfunc)) {
    predictfunc <- predict
  }
  if (missing(predictfuncwithmod)) {
    predictfuncwithmod <- function(x) {suppressMessages(predictfunc(mod, x))}
  }
  if (missing(name1)) {
    if (missing(namesfromcol)) {
      name1 <- "y1"
    } else {
      name1 <- x1[[namesfromcol]][[1]]
    }
  }
  if (missing(name2)) {
    if (missing(namesfromcol)) {
      name2 <- "y2"
    } else {
      name2 <- x2[[namesfromcol]][[1]]
    }
  }

  # predict(m1, x1)
  # predict(m1, x2)
  # y1 <- suppressMessages(predictfunc(mod, x1))
  # y2 <- suppressMessages(predictfunc(mod, x2))
  y1 <- predictfuncwithmod(x1)
  y2 <- predictfuncwithmod(x2)
  # varuse <- rep(NA, ncol(x1))
  # xt <- x1
  # indexorder <- c()
  # yorder <- c()
  if (missing(d)) {
    d <- ncol(x1)
  } else {
    stopifnot(d <= ncol(x1), d>=2)
  }
  # df <- data.frame(ord=1:d, ind=NA, y=NA, name=NA)
  df <- NULL
  for (i in 1:ncol(x1)) {
    xi_1to2 <- x1
    xi_1to2[i] <- x2[i]
    xi_2to1 <- x2
    xi_2to1[i] <- x1[i]
    # yi_1to2 <- suppressMessages(predictfunc(mod, xi_1to2))
    # yi_2to1 <- suppressMessages(predictfunc(mod, xi_2to1))
    yi_1to2 <- predictfuncwithmod(xi_1to2)
    yi_2to1 <- predictfuncwithmod(xi_2to1)
    df <- rbind(df, data.frame(i=i, name=colnames(x1)[i],
                               y1to2 = yi_1to2,
                               y2to1 = yi_2to1
    ))
  }

  # df2 <- df[order(abs(df$y1to2 - y1) + abs(df$y2to1 - y2), decreasing = TRUE),]
  # Order them by most significant
  df$i2 <- order(order(abs(df$y1to2 - y1) + abs(df$y2to1 - y2), decreasing = TRUE))
  df2 <- df[df$i2<=d, ]
  ggplot2::ggplot(df2) +
    ggplot2::geom_segment(ggplot2::aes_string(xend="y1to2", y="i2+.1", yend="i2+.1"), x=y1) +
    ggplot2::geom_segment(ggplot2::aes_string(xend="y2to1", y="i2", yend="i2"), x=y2) +
    ggplot2::geom_vline(xintercept=y1, color='red') +
    ggplot2::geom_vline(xintercept=y2, color='red') +
    ggplot2::geom_text(ggplot2::aes_string(label="name", y="i2"),  x=.5*(y1+y2), vjust='bottom') +
    ggplot2::geom_point(ggplot2::aes_string(x="y1to2", y="i2+.1")) +
    ggplot2::geom_point(ggplot2::aes_string(x="y2to1", y="i2")) +
    ggplot2::annotate("text", x=(y1+y2)/2 + 1.2*(c(y1, y2) - (y1+y2)/2), y=1.5,label=c(name1, name2), color="red") +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("Predicted value") +
    ggplot2::ylab("Most important inputs")
}
