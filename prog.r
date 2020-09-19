require(rgl);

data = read.csv('data.csv')
plot(data)

d = data[c("GFSI", "GPI", "GDPPC")]

#for(i in 1:3){
#	d[,i] = ( d[,i] - mean(d[,i]) ) / var(d[,i]);
#}

vec = eigen(cov(d))$vectors
d2 = as.matrix(d) %*% t(vec)
plot3d(d2, col=as.integer(data$"Income.group"), size=10)
d2 = as.matrix(d) %*% vec
plot3d(d2, col=as.integer(data$"Income.group"), size=10)

plot3d(d, col=as.integer(data$"Income.group"), size=10)
