require(dplyr)
require(tidyr)
require(ggplot2)
x <- c(1,2,3,4,5,6)
set.seed(17)
xbar <- 0
xsamp <- 0
for(i in 1:5000){
    xsamp[i] <- sample(x,1,replace = TRUE)
}
mean_xsamp <- mean(xsamp)
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
colnames(x_dt) <- c("seq", "Mean")
x_dt <- mutate(x_dt, ul = x_dt$Mean +  1.645 * sd_xsamp / sqrt(50))
x_dt <- mutate(x_dt, ll = x_dt$Mean -  1.645 * sd_xsamp / sqrt(50))
x_dt <- mutate(x_dt, ok = ifelse(ll >= mean_xsamp | ul <= mean_xsamp, 1, 0))
annot <- read.table(text=
                        "seq|ul|just|text
                    4.1|6|5|CI does not include<br><br>population mean",
                    sep="|", header=TRUE, stringsAsFactors=FALSE)
annot$text <- gsub("<br>", "\n", annot$text)
ggplot(x_dt, aes(Mean, seq, xlim(0,5))) +
    geom_point(size = 2, colour = "blue")  +
    geom_point(x = x_dt$ul, y = x_dt$seq, colour = "red") +
    geom_point(x = x_dt$ll, y = x_dt$seq, colour = "red") +
    geom_vline(xintercept = mean(xsamp), colour ="gray") +
    geom_segment(aes( x= ll, y = seq,
                      xend = ul, yend= seq, colour= ok )) +
    theme(legend.position="none") + labs(y = NULL) + theme(
        axis.text.y = element_blank()) + labs(title="90% Confidence Interval (CI)", subtitle="Population Mean(3.5) vs. Sample Means +/- CI") +  geom_label(data=annot, aes(x=seq, y=ul, label=text), family="OpenSans-CondensedLight", lineheight=0.95, size=3, label.size=0, color="#2b2b2b", bg="transparent")

## 90, 95, 99 ci's
cl <- c(1:3)
cfmean <- c(3.1,3.1,3.1)
ul95 <-  3.1 + 2 * sd_xsamp / sqrt(50)
ll95 <-  3.1 - 2 * sd_xsamp / sqrt(50)
ul90 <-  3.1 + 1.645 * sd_xsamp / sqrt(50)
ll90 <-  3.1 - 1.645 * sd_xsamp / sqrt(50)
ul99 <-  3.1 + 2.575 * sd_xsamp / sqrt(50)
ll99 <-  3.1 - 2.575 * sd_xsamp / sqrt(50)
ci95 <- cbind(95,3.1, ll95, ul95)
ci99 <- cbind(99,3.1, ll99, ul99)
ci90 <- cbind(90,3.1, ll90, ul90)
cnames <- c("CL","Mean", "LL", "UL")
colnames(ci90) <- cnames
colnames(ci95) <- cnames
colnames(ci99) <- cnames
ci <- rbind(ci90, ci95, ci99)
ci <- as.data.frame(ci)
ggplot(ci, aes(Mean, CL)) + xlim(2,4) +
    geom_point(size = 2, colour = "blue")  +
    geom_point(x = ci$UL, y = ci$CL, colour = "red") +
    geom_point(x = ci$LL, y = ci$CL, colour = "red") +
    geom_segment(aes( x= LL, y = CL,
                      xend = UL, yend= CL, colour= "red" )) +
    theme(legend.position="none") + labs(y = NULL) + theme(
        axis.text.y = element_blank(),axis.text.x = element_blank()) + labs(title="90%, 95%, 99% Confidence Intervals (CI)") +   geom_label(data=ci, aes(x=LL, y=CL, label=paste(CL, "% CI")), hjust= 1.5, bg = "transparent" ) +
    geom_vline(xintercept = 2.65, colour ="gray")