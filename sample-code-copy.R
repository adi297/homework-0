library(dplyr)
library(tidyverse)
library(gtools)

library(ggrepel)
library(ggthemes)

library(dslabs)

#Monte-Carlo
B <-10000
set.seed(1, sample.kind = "Rounding")
monte_carlo <- function(n,p,l,g,e) {
  S <- replicate(B, {
    s <- sample(c(1,0), n, replace = TRUE, prob = c(p, (1 - p)))
    total <- sum(s)
    avg <- mean(s)
    stdev <- sd(s)
    summary(Total = total, Mean = avg, Standard_Deviation = stdev)
    less_than <- mean(S < l)
    greater_than <- mean(S > g)
    equal <- mean(S == e)
  })
}

#CLT
expected_value <- function(a,b,p){a*p + b*(1 - p)}
sum_expected_value <- function(a,b,p,n){n*(a*p + b*(1 - p))}
two_val_sd <- function(a,b,p){abs(b-a)*sqrt(p*(1-p))}
two_val_se <- function(a,b,p){abs(b-a)*sqrt(p*(1-p))}
sum_two_val_se <- function(a,b,p,n){sqrt(n)*(abs(b-a)*sqrt(p*(1-p)))}


check_over_range <- function(a,b,p,n,l) {avgr <- sum_expected_value(a,b,p,n)
sdr <- sum_two_val_se(a,b,p,n)
pnorm(l, avg, sd)
}

se_odds <- function(a,c,b,d){sqrt(1/a + 1/b + 1/c + 1/d)}
ci_upper_odds_95 <- function(a,c,b,d){log(a*d/b*c) + qnorm(0.975)*sqrt(1/a + 1/b + 1/c + 1/d)}
ci_lower_odds_95 <- function(a,c,b,d){log(a*d/b*c) - qnorm(0.975)*sqrt(1/a + 1/b + 1/c + 1/d)}
