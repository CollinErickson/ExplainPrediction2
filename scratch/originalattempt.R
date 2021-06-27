library(dplyr)
iris
iris %>% head
m1 <- gbm::gbm(Petal.Width ~ Sepal.Length + Sepal.Width, data=iris)
m1
m1 %>% summary

x1 <- iris[1,]
x2 <- iris[2,]
predict(m1, x1)
predict(m1, x2)
y1 <- predict(m1, x1)
y2 <- predict(m1, x2)
varuse <- rep(NA, ncol(x1))
xt <- x1
indexorder <- c()
yorder <- c()
d <- ncol(x1)
df <- data.frame(ord=1:d, ind=NA, y=NA, name=colnames(x1))
for (i in 1:(ncol(x1)-1)) {
  xt <- x1
  xt[which(!is.na(varuse))] <- x2[which(!is.na(varuse))]
  yi <- predict(m1, xt)
  yabsdiff <- Inf
  yabsdiffind <- NA
  for (j in which(is.na(varuse))) {
    print(c(i,j))
    xj <- xt
    xj[j] <- x2[j]
    yj <- predict(m1, xj)
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
varuse
yorder
c(y1, y2)
df
df %>% ggplot(aes(y, ord)) + geom_point()
df$y1 <- c(y1, df$y[-d])
df$y2 <- c(df$y[-1], y2)
df %>% ggplot(aes(y=ord, yend=ord, x=y1, xend=y)) +
  geom_segment() +
  geom_point(aes(x=y, y=ord)) +
  geom_vline(xintercept=y1, color='red') +
  geom_vline(xintercept=y2, color='red') +
  geom_text(aes(label=name, x=.3, y=ord), vjust='bottom') + scale_y_reverse() +
  xlab("Predicted value") +
  ylab("Most important inputs")
