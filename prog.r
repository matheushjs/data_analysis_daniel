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

model = lm(d[,1] ~ d[,2])
print(summary(model));

model = lm(d[,1] ~ d[,2] + d[,3])
print(summary(model));

llog = log(d[,3]);
model = lm(d[,1] ~ llog)
print(summary(model));

model = lm(d[,1] ~ d[,2] + llog)
print(summary(model));

plot3d(data$GTI, data$GFSI, data$GPI, type="p", col="#BBBBBB", size=10, xlab="GTI", ylab="GFSI", zlab="GPI", xlim=c(0, 9), ylim=c(0, 90), zlim=c(0, 3.8));
model = lm(data$GPI ~ data$GTI + data$GFSI);
coefs <- coef(model)
a <- coefs["data$GTI"]
b <- coefs["data$GFSI"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha=0.5, col="#0000FF");
