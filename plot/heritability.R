sds    = seq(0.005,0.250,0.035)
#,0.130,0.150,0.18,0.2
#sds[1] = 0.005
herits = NULL
for(s in 1:length(sds)){
  parent = NULL
  offspring = NULL
  ss = sds[s]
  for(i in 1:10000){
    p1 = rnorm(1, 0.5, 0.1)
    p2 = rnorm(1, 0.5, 0.1)
    parent = c(parent, mean(p1, p2))
    x = rnorm(1, mean(c(p1, p2)), ss) 
    offspring = c(offspring, x)
  }
  plot(parent, offspring, main = ss, xlim=c(0,1), ylim=c(0,1))
  herits = c(herits, coef(lm(parent~offspring))[2])
}
cbind(sds, round(herits, 2))

#default
parent = NULL
offspring = NULL
for(i in 1:1000){
p1 = rnorm(1, 0.5, 0.1)
p2 = rnorm(1, 0.5, 0.1)
parent = c(parent, mean(p1, p2))
x = rnorm(1, mean(c(p1, p2)), 0.005) 
offspring = c(offspring, x)
}
coef(lm(parent~offspring))[2]
