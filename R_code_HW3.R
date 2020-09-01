#Problem1(2.22 a-d)
hours = c(159,280,101,212,224,379,179,264,222,362,168,250,149,260,485,170)
mean(hours)
sd(hours)
t.test(hours, mu=225, altern="greater", conf=0.95, paired=F, var.equ=T) 

t.test(hours, mu=225, altern="greater",conf=0.95, paired=F, var.equ=F)

#Problem2(2.26 a-b)
t1 = c(65,82,81,67,57,59,66,75,82,70)
t2 = c(64,56,71,69,83,74,59,82,65,79)
sd(t1)
sd(t2)
var.test(t1,t2)

t.test(t1, t2, altern="two.sided", conf=.95, paired=F, var.equ=T) 

#Problem3(2.33)
bo1 = c(6.08,6.22,7.99,7.4,6.48,7.99,6.32,7.60,6.03,7.52) 
bo2 = c(5.73,5.80,8.42,6.84,6.43,8.76,6.32,7.62,6.59,7.67)
t.test(bo1, bo2, altern="two.sided", conf=.95, paired=T, var.equ=F)
wilcox.test(bo1, bo2, paired=T)


#Problem4(3.6)
sh = c(4.51,7.95,4.97,3.00,7.97,2.23,3.95,5.64,9.35,6.52,4.96,6.10,7.19,4.03,2.72,9.19,5.17,5.70,5.85,6.45)
h1 = c(5.32,6,5.12,7.08,5.48,6.52,4.09,6.28,7.77,5.68,8.47,4.58,4.11,5.72,5.91,6.89,6.99,4.98,9.94,6.38)
h2 = c(4.73,5.82,5.69,3.86,4.06,6.56,8.34,3.01,6.71,6.51,1.7,5.89,6.55,5.34,5.88,7.50,3.28,5.38,7.30,5.46)
h4 = c(7.03,4.65,6.65,5.49,6.98,4.85,7.26,5.92,5.58,7.91,4.90,4.54,8.18,5.42,6.03,7.04,5.17,7.60,7.90,7.91)
data = data.frame(cbind(sh,h1,h2,h4))
data_st <- stack(data)
data_st
anova_res <- aov(values ~ ind, data = data_st)
summary(anova_res)
TukeyHSD(anova_res)
plot(anova_res)
#Problem5(3.16)
temp_100 = c(21.8, 21.9,21.7,21.6, 21.7)
temp_125 = c(21.7,21.4,21.5,21.4)
temp_150 = c(21.9,21.8,21.8,21.6,21.5)
temp_175 = c(21.9,21.7,21.8,21.4)


#Problem6
#Problem7
79.4 - 2.179*sqrt(15.6/5)
79.4 + 2.179*sqrt(15.6/5)

79.4 - 100.4 - 3.055*sqrt(2*15.6/5)
79.4 - 100.4 + 3.055*sqrt(2*15.6/5)
