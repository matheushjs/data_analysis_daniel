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

col.names = c("GFSI","HDI","SDG","GDPPC","GDP","GINI","GTI","GFSIAff","GFSIAva","GFSIQS","GFSINat");
result = matrix(0.0, nrow=length(col.names), ncol=length(col.names));

#data[,"GDPPC"] = sqrt(data[,"GDPPC"]);
for(i in col.names){
	for(j in col.names){
		model = lm(data[,"GPI"] ~ data[,i] + data[,j]);
		
		result[which(col.names == i), which(col.names==j)] = summary(model)$adj.r.squared;
	}
}

df = data.frame(result);
colnames(df) = col.names;
rownames(df) = col.names;
print(df);
#savePlot("gpi-vs-all-scatterplot.png");
#dev.off();
