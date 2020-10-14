require(rgl);
require(viridis);
require(colorspace);

data = read.csv('data.csv')
data.orig = data;
#plot(data)

#for(i in c("GPI", "GFSI", "GDPPC", "GTI", "HDI", "SDG")){
#	data[,i] = ( data[,i] - mean(data[,i]) ) / sd(data[,i]);
#}

plot3d(data$GTI, data$GFSI, data$GPI, type="s", col=as.numeric(data$Income.group), size=1, xlab="GTI", ylab="GFSI", zlab="GPI", axes=F, ylim=c(0, 100), zlim=c(0, 4));
axes3d("x-");
axes3d("y-");
axes3d("z-");
model = lm(data$GPI ~ data$GTI + data$GFSI);
coefs <- coef(model)
a <- coefs["data$GTI"]
b <- coefs["data$GFSI"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha=0.5, col="#FFFFFF");
