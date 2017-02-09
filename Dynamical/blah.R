data(ToothGrowth)
str(ToothGrowth)

ToothGrowth$supp <- as.factor(ToothGrowth$supp)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

d <- melt(ToothGrowth)
ggplot(d,aes(x = value)) + 
    facet_wrap(~variable) + 
    geom_histogram()

ggplot(ToothGrowth, aes(x=len)) +
    geom_histogram(aes(weights=len)) +
    facet_wrap( ~ supp, ncol=1)

ggplot(ToothGrowth, aes(x=len)) +
    geom_histogram(aes(weights=len)) +
    facet_wrap( ~ dose, ncol=1)

qhist(ToothGrowth, aes(supp, len)) + geom_point(color="royalblue")
ggplot(ToothGrowth, aes(dose, len)) + geom_point(color="royalblue")

out <- t.test(len ~ supp, data = ToothGrowth) 


TG12 <- ToothGrowth[ToothGrowth$dose == .5 | ToothGrowth$dose == 1,]
TG13 <- ToothGrowth[ToothGrowth$dose == 1 | ToothGrowth$dose == 2,]
TG23 <- ToothGrowth[ToothGrowth$dose == .5 | ToothGrowth$dose == 2,]



t.test(len ~ dose, data = TG12)
t.test(len ~ dose, data = TG13)
t.test(len ~ dose, data = TG23)
