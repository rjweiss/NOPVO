str<-rep('FALSE',1000)
str[1000]='TRUE'
str=table(str)/1000

str<-rep('FALSE',1000)
str[990:1000]='TRUE'
str=table(str)/1000

str<-rep('FALSE',1000)
str[950:1000]='TRUE'
str=table(str)/1000

str<-rep('FALSE',1000)
str[900:1000]='TRUE'
str=table(str)/1000

star=function(x){
if(as.numeric(str[names(x)=="FALSE"])<.95) ""
if(as.numeric(str[names(x)=="FALSE"])>.94) "*"
if(as.numeric(str[names(x)=="FALSE"])>.98) "**"
if(as.numeric(str[names(x)=="FALSE"])>.99) "***" 
}

stars_bootstrapped.males<- as.matrix(sapply(bootstrapped.males, function(x)star(x)))