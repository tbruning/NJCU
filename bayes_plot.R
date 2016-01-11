## ADd documentation
require(dplyr)
require(ggplot2)
require(tidyr)
set.seed(43)
x <- c(1,1,1,1,1,1,-1,-1,-1,-1)
cum_01 <- as.numeric(data.frame())
cum_50 <- as.numeric(data.frame())
cum_99 <- as.numeric(data.frame())
prior_01 <- .01
prior_50 <- .5
prior_99 <- .99
cum_01[1] <- prior_01
cum_50[1] <- prior_50
cum_99[1] <- prior_99
for(i in 1:26) {
    s <- sample(x, 1, replace = TRUE)
    if(s > 0) {
        hypo_true <- .7
        hypo_false <- .2
        
    } else {
        hypo_true <- .2
        hypo_false <- .7
        
    }
    z_01 <- bayes(cum_01[i], hypo_true,hypo_false)
    cum_01[i+1] <-  z_01
    z_50 <- bayes(cum_50[i], hypo_true,hypo_false)
    cum_50[i+1] <-  z_50
    z_99 <- bayes(cum_99[i], hypo_true,hypo_false)
    cum_99[i+1] <-  z_99
    
    
}
w <- c(0:26)
prio_01 <-c(rep(1,27))
w <- as.data.frame(w)
cum_01 <- as.data.frame(cum_01)
prio_01 <- as.data.frame(prio_01)
prio_50 <-c(rep(2,27))
cum_50 <- as.data.frame(cum_50)
prio_50 <- as.data.frame(prio_50)
prio_99 <-c(rep(3,27))
cum_99 <- as.data.frame(cum_99)
prio_99 <- as.data.frame(prio_99)
final_cumcum  <- bind_cols(w,prio_01,cum_01, prio_50, cum_50, prio_99, cum_99)
x1 <- rep("01", 27)
x50 <- rep("50", 27)
x99 <- rep("99", 27)
x1 <- as.data.frame(x1)
x50 <- as.data.frame(x50)
x99 <- as.data.frame(x99)
cum01 <- bind_cols(w, x1, cum_01)
cum50 <- bind_cols(w, x50, cum_50)
cum99 <- bind_cols(w, x99, cum_99)


colnames(cum01) <- c("Week", "Prior", "pct")
colnames(cum50) <- c("Week", "Prior", "pct")
colnames(cum99) <- c("Week", "Prior", "pct")
finalcum <- bind_rows(cum01, cum50, cum99)
finalcum <- mutate(finalcum, Pct = round(pct * 100, 0))
plot <- ggplot(data = finalcum) + aes(x=Week, y = Pct, group = Prior, colour = Prior) + geom_line() + geom_point() +
    xlab("Week") + ylab("Pct Belief\n in Bull Mkt.") +
    ggtitle("Baynesian Revision\n of Belief of Bull Market\n (Bull Market)")

plot

####################################################
## Bear Market
#################################################3#3
set.seed(43)
x <- c(-1,-1,-1,-1,-1,-1,1,1,1,1)
cum_01 <- as.numeric(data.frame())
cum_50 <- as.numeric(data.frame())
cum_99 <- as.numeric(data.frame())
prior_01 <- .01
prior_50 <- .5
prior_99 <- .99
cum_01[1] <- prior_01
cum_50[1] <- prior_50
cum_99[1] <- prior_99
for(i in 1:26) {
    s <- sample(x, 1, replace = TRUE)
    if(s > 0) {
        hypo_true <- .7
        hypo_false <- .2
        
    } else {
        hypo_true <- .2
        hypo_false <- .7
        
    }
    z_01 <- bayes(cum_01[i], hypo_true,hypo_false)
    cum_01[i+1] <-  z_01
    z_50 <- bayes(cum_50[i], hypo_true,hypo_false)
    cum_50[i+1] <-  z_50
    z_99 <- bayes(cum_99[i], hypo_true,hypo_false)
    cum_99[i+1] <-  z_99
    
    
}
w <- c(0:26)
prio_01 <-c(rep(1,27))
w <- as.data.frame(w)
cum_01 <- as.data.frame(cum_01)
prio_01 <- as.data.frame(prio_01)
prio_50 <-c(rep(2,27))
cum_50 <- as.data.frame(cum_50)
prio_50 <- as.data.frame(prio_50)
prio_99 <-c(rep(3,27))
cum_99 <- as.data.frame(cum_99)
prio_99 <- as.data.frame(prio_99)
final_cumcum  <- bind_cols(w,prio_01,cum_01, prio_50, cum_50, prio_99, cum_99)
x1 <- rep("01", 27)
x50 <- rep("50", 27)
x99 <- rep("99", 27)
x1 <- as.data.frame(x1)
x50 <- as.data.frame(x50)
x99 <- as.data.frame(x99)
cum01 <- bind_cols(w, x1, cum_01)
cum50 <- bind_cols(w, x50, cum_50)
cum99 <- bind_cols(w, x99, cum_99)


colnames(cum01) <- c("Week", "Prior", "pct")
colnames(cum50) <- c("Week", "Prior", "pct")
colnames(cum99) <- c("Week", "Prior", "pct")
finalcum <- bind_rows(cum01, cum50, cum99)
finalcum <- mutate(finalcum, Pct = round(pct * 100, 0))
plot <- ggplot(data = finalcum) + aes(x=Week, y = Pct, group = Prior, colour = Prior) + geom_line() + geom_point() +
    xlab("Week") + ylab("Pct Belief\n in Bull Mkt.") +
    ggtitle("Baynesian Revision\n of Belief of Bull Market\n (Bear Market)")

plot