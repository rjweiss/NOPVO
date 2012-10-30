#starting from nopvo.prop_dfs

lapply(nopvo.prop_dfs, function(x){
	print(names(x))
	#temp = x
	#temp$id = rownames(temp)
	#temp = melt(temp)
	
})

for(x in 1:length(names(nopvo.prop_dfs))){
	temp.name = names(nopvo.prop_dfs[x])
	temp.df = eval(parse(text = paste("nopvo.prop_dfs$", temp.name, sep = "")))
	temp.df$id = rownames(temp.df)
	temp.df = melt(temp.df, variable_name = "categories")
#	temp.df$var = temp.name
	temp.df = t(cast(temp.df, id ~ categories))
	temp.df$variable = temp.name
	print(temp.df)
}