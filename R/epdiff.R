#' Explain difference between predictions from a model
#'
#' @param mod Model
#' @param x1 Input 1. Single row data frame or matrix.
#' @param x2 Input 2. Single row data frame or matrix.
#' @param d Max number of input dimensions to use.
#' @param predictfunc Func to use to predict. Defaults to `predict`.
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
#' epdiff(m1, x1, x2)
#' epdiff(m1, iris[12,], iris[20,])
#' epdiff(m1, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
#'
#' # Linear model
#' mod_lm <- lm(Petal.Width ~ Sepal.Length + Sepal.Width, data=iris)
#' epdiff(mod_lm, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
#' epdiff(mod_lm, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),], d=3)
#' mod_lm2 <- lm(Petal.Width ~ Petal.Length + Sepal.Width, data=iris)
#' epdiff(mod_lm2, iris[sample(1:nrow(iris), 1),], iris[sample(1:nrow(iris), 1),])
epdiff <- function(mod, x1, x2, d, predictfunc) {
  stopifnot(nrow(x1)==1, nrow(x2)==1, ncol(x1)==ncol(x2))

  if (missing(predictfunc)) {
    predictfunc <- predict
  }

  # predict(m1, x1)
  # predict(m1, x2)
  y1 <- suppressMessages(predictfunc(mod, x1))
  y2 <- suppressMessages(predictfunc(mod, x2))
  varuse <- rep(NA, ncol(x1))
  xt <- x1
  indexorder <- c()
  yorder <- c()
  if (missing(d)) {
    d <- ncol(x1)
  } else {
    stopifnot(d <= ncol(x1), d>=2)
  }
  df <- data.frame(ord=1:d, ind=NA, y=NA, name=NA)
  for (i in 1:(d-1)) {
    xt <- x1
    xt[which(!is.na(varuse))] <- x2[which(!is.na(varuse))]
    yi <- suppressMessages(predictfunc(mod, xt))
    yabsdiff <- Inf
    yabsdiffind <- NA
    for (j in which(is.na(varuse))) {
      # print(c(i,j))
      xj <- xt
      xj[j] <- x2[j]
      yj <- suppressMessages(predictfunc(mod, xj))
      if (abs(yj - y2) < yabsdiff) {
        ybest <- yj
        yabsdiff <- abs(yj-y2)
        yabsdiffind <- j
      }
    }
    varuse[yabsdiffind] <- i
    indexorder[i] <- yabsdiffind
    yorder[i] <- ybest
    df$ind[i] <- yabsdiffind
    df$y[i] <- ybest
  }
  df$y[d] <- y2
  df$ind[d] <- which(is.na(varuse))[1]
  df$name <- colnames(x1)[df$ind]
  # varuse
  # yorder
  # c(y1, y2)
  # df
  # df %>% ggplot(aes(y, ord)) + geom_point()
  # Left and right endpoints of the line
  df$yp1 <- c(y1, df$y[-d])
  df$yp2 <- c(df$y[-1], y2)
  ggplot2::ggplot(df, ggplot2::aes_string(y="ord", yend="ord", x="yp1", xend="y")) +
    ggplot2::geom_segment() +
    ggplot2::geom_point(ggplot2::aes_string(x="y", y="ord")) +
    ggplot2::geom_vline(xintercept=y1, color='red') +
    ggplot2::geom_vline(xintercept=y2, color='red') +
    ggplot2::geom_text(ggplot2::aes_string(label="name", y="ord"),  x=.5*(y1+y2), vjust='bottom') +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("Predicted value") +
    ggplot2::ylab("Most important inputs")

}
