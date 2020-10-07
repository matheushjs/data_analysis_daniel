require(rgl);
require(viridis);
require(colorspace);

data = read.csv('data.csv')
data.orig = data;
#plot(data)

data[,"GDPPC"] = sqrt(data$GDPPC);
for(i in c("GPI", "GFSI", "GDPPC", "GTI", "HDI", "SDG")){
	data[,i] = ( data[,i] - mean(data[,i]) ) / sd(data[,i]);
}

normalize = function(data, name, min, max){
	data[,name] = (data[,name] - min) / (max - min);
	data;
}
#data = normalize(data, "GPI", 1, 5);
#data = normalize(data, "GFSI", 0, 100);
#data = normalize(data, "GDPPC", 0, sqrt(1607023));
#data = normalize(data, "GTI", 0, 10);
#data = normalize(data, "HDI", 0, 1);
#data = normalize(data, "SDG", 0, 100);

#vec = eigen(cov(d))$vectors
#d2 = as.matrix(d) %*% t(vec)
#plot3d(d2, col=as.integer(data$"Income.group"), size=10)
#d2 = as.matrix(d) %*% vec
#plot3d(d2, col=as.integer(data$"Income.group"), size=10)

#plot3d(d, col=as.integer(data$"Income.group"), size=10)

#model = lm(d[,1] ~ d[,2])
#print(summary(model));

#model = lm(d[,1] ~ d[,2] + d[,3])
#print(summary(model));

#llog = log(d[,3]);
#model = lm(d[,1] ~ llog)
#print(summary(model));

#model = lm(d[,1] ~ d[,2] + llog)
#print(summary(model));

#plot3d(data$GTI, data$GFSI, data$GPI, type="p", col="#BBBBBB", size=10, xlab="GTI", ylab="GFSI", zlab="GPI", xlim=c(0, 9), ylim=c(0, 90), zlim=c(0, 3.8));
#model = lm(data$GPI ~ data$GTI + data$GFSI);
#coefs <- coef(model)
#a <- coefs["data$GTI"]
#b <- coefs["data$GFSI"]
#c <- -1
#d <- coefs["(Intercept)"]
#planes3d(a, b, c, d, alpha=0.5, col="#0000FF");

col = qualitative_hcl("Dark 2", n=5)[4];
dev.new(width=0.7*20, height=0.7*4);
par(mfrow=c(1, 5));

#data[,"GDPPC"] = sqrt(data[,"GDPPC"]);
for(s in c("GFSI", "HDI", "SDG", "GDPPC", "GTI")){
	model = lm(data[,"GPI"] ~ data[,s]);
	plot(data[,s],  data$GPI, pch=19, col=col, xlab=s, ylab="GPI");

	idx = sort.list(data[,s]);
	lines(data[,s][idx], model$fitted.values[idx]);

	axis(side=3, at=(max(data[,s]) + min(data[,s]))/2, labels=paste("Adjusted R^2: ", round(summary(model)$adj.r.squared, 3)), tick=F);

	beta = coef(model)[2];
	beta.var = vcov(model)[2,2];
	delta = sqrt(beta.var) * sqrt( 2 * qf(0.95, lower.tail=F, df1=2, df2=nrow(data) - 1 - 1) );
	print(s);
	print(round(c(beta - delta, beta, beta + delta), 5));
	cat("\n");
}
#savePlot("gpi-vs-all-scatterplot.png");
#dev.off();

cols = qualitative_hcl("Set 2", n=5);
mydata = data.orig$GDPPC;
likelihood = function(p){
	print(p);
	-sum(dgamma(mydata, shape=p[1], scale=p[2]));
}
#result = optim(c(1, 801245), likelihood, method="L-BFGS", lower=c(0,0));
#print(result);
dev.new(width=9, height=7);
hist(mydata, prob=T, ylab="Density", xlab="GDPPC", col=cols[1], main="");
x = seq(0, max(mydata)*1.1, length=1000);
lines(x, dgamma(x, shape=1.628821e-01, scale=8.012450e+05), col=4, lwd=3);
savePlot("histogram-fit.png");
