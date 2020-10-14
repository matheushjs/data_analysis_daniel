require(rgl);
require(viridis);
require(colorspace);

data = read.csv('data2.csv')

#for(i in c("GPI", "GFSI", "GFSIAff","GFSIAva","GFSIQS","GFSINat")){
#	data[,i] = ( data[,i] - mean(data[,i]) ) / sd(data[,i]);
#}

col = qualitative_hcl("Dark 2", n=5)[4];
dev.new(width=0.7*20, height=0.7*4);
par(mfrow=c(1, 5));

#data[,"GDPPC"] = sqrt(data[,"GDPPC"]);
for(s in c("GFSI", "GFSIAff","GFSIAva","GFSIQS","GFSINat")){
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
