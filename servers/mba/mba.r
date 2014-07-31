library(arules)
library(arulesViz)
library(ggplot2)

baskets   <- read.transactions("baskets.csv",  format="basket", sep=",")
unbaskets <- read.transactions("removals.csv", format="basket", sep=",")

installs <- apriori(baskets,   parameter= list(supp=0.001, conf=0.01, minlen=2, maxlen=2));
removals <- apriori(unbaskets, parameter= list(supp=0.001, conf=0.3, minlen=2, maxlen=2));

installs.df <- as(installs, 'data.frame')
removals.df <- as(removals, 'data.frame')

install.support <- function(from, to) {
    as(subset(installs, subset = lhs %pin% from & rhs %pin% to), 'data.frame')$support
}

install.confidence <- function(from, to) {
    as(subset(installs, subset = lhs %pin% from & rhs %pin% to), 'data.frame')$confidence
}

install.lift <- function(from, to) {
    as(subset(installs, subset = lhs %pin% from & rhs %pin% to), 'data.frame')$lift
}
