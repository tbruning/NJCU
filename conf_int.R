require(dplyr)
require(tidyr)
require(ggplot2)

x <- c(1,2,3,4,5,6)

xbar <- 0
xsamp <- 0
for(i in 1:5000){
  xsamp[i] <- sample(x,1,replace = TRUE)
}
mean(xsamp)
sd_xsamp <- sd(xsamp)

for(i in 1:10) {
  xbar[i] <- mean(sample(xsamp,30, replace = TRUE))
}
mean_xbar <- mean(xbar)
sd_xbar<- sd(xbar)
sd(x)
sd(x) / sqrt(50)
x_count <- as.data.frame(c(1:10))
xbar <- as.data.frame(xbar)
# x_count <- tbl_df(x_count)
x_dt <- bind_cols(x_count,xbar)
colnames(x_dt) <- c("seq", "mean")
x_dt <- mutate(x_dt, ul = x_dt$mean +  2 * sd_xsamp / sqrt(50))
x_dt <- mutate(x_dt, ll = x_dt$mean -  2 * sd_xsamp / sqrt(50))
x_dt <- mutate(x_dt, ok = ifelse(ll >= mean_xbar | ul <= mean_xbar, 1, 0))
ggplot(x_dt, aes(mean, seq, xlim(0,5))) + 
  geom_point(size = 2, colour = "blue")  +
  geom_point(x = x_dt$ul, y = x_dt$seq, colour = "red") + 
  geom_point(x = x_dt$ll, y = x_dt$seq, colour = "red") +
  geom_vline(xintercept = mean(xsamp), colour ="gray") +
  geom_segment(aes( x= ll, y = seq, 
                    xend = ul, yend= seq, colour= ok )) +
   theme(legend.position="none") + labs(y = NULL) + theme(
     axis.text.y = element_blank()) 


seq <- c(1:3)
cfmean <- c(3.1,3.1,3.1)
ul95 <-  3.1 + 2 * sd_xsamp / sqrt(50)
ll95 <-  3.1  - 2 * sd_xsamp / sqrt(50)
ul90 <-  3.1 + 1.645 * sd_xsamp / sqrt(50)
ll90 <-  3.1  - 1.645 * sd_xsamp / sqrt(50)
ul99 <-  3.1 + 2.575 * sd_xsamp / sqrt(50)
ll99 <-  3.1  - 2.575 * sd_xsamp / sqrt(50)
