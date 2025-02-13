# socds.R
# R to get permit data from SOCDS
# Tom Davidoff
# 06/17/24

library(data.table)
library(fixest)
library(ggplot2)
library("openxlsx")


# get permit data from SOCDS
# mdb-export Downloads/bpdata4web.mdb Place_annual | grep "All Permits" > onedrive/shift/data/raw/Place_annual.csv
# head -n 1
#state,county,juris,year,msa,cbsa,series,sertxt,permits,geo,period,footer

# inconsistent assignment of counties to CBSAs 


# read in permit data
dp <- fread("data/raw/Place_annual.csv",header=FALSE,na.strings=c("-"))
names(dp) <- c("STATE","COUNTY","JURIS","year","MSA","CBSA","series","sertxt","ALL_PERMITS_","geo","period","footer")
dp[,maxYear:=max(year,na.rm=TRUE),by=.(STATE,COUNTY)]
dp[,CBSA:=max(CBSA*(year==maxYear),na.rm=TRUE),by=.(STATE,COUNTY)]
dp <- dp[CBSA!=99999,.(ALL_PERMITS_=sum(ALL_PERMITS_,na.rm=TRUE)),by=.(CBSA,year)]
print(dp)

# metro division to cbsa
dm <- fread("data/raw/geocorr2022countyCBSA.csv",skip=1,select=c("Core-based statistical area code","Metropolitan division code"))
dm <- unique(dm)
names(dm) <- c("CBSA","metroFHFA")

# real HPI
hpi <- fread("data/raw/hpi_at_metro.csv",header=FALSE,na.strings=c("-"))
names(hpi) <- c("metroNAME","metroFHFA","year","quarter","hpi","zz")
hpi <- merge(hpi,dm,by="metroFHFA")
cpi <- fread("data/raw/CPIAUCNS.csv") # from FRED
cpi[,quarter:=quarter(cpi$DATE)]
cpi[,year:=year(cpi$DATE)]
cpi <- cpi[,.(cpi=mean(CPIAUCNS)),by=.(year,quarter)]
hpi <- merge(hpi,cpi,by=c("year","quarter"))
hpi[,hpi:=hpi/cpi]
print(hpi[,.N])
hpi <- unique(hpi[,.(hpi=mean(hpi)),by=.(CBSA,year)])
print(hpi[,.N])
print(summary(hpi))

dp <- merge(dp,hpi,by=c("CBSA","year"))

dp[,hpiMaxRatio:=NULL]
dp[,hpiMax:=1]
for (y in 1983:2023) {
	print(y)
	dp[,tempvar:=max(hpi*(year<y),na.rm=TRUE),by=CBSA]
	dp[year==y,hpiMax:=max(tempvar,na.rm=TRUE),by=CBSA]
	dp[year==y,hpiMaxRatio:=hpi/hpiMax]
}


print(summary(feols(log(ALL_PERMITS_)~log(hpi)+i(year)|CBSA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~i(year)|CBSA[log(hpi)],data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~log(hpi)+log(hpiMax)+i(year)|CBSA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~log(hpiMaxRatio)+i(year)|CBSA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~i(year)|CBSA[log(hpiMaxRatio)],data=dp[year>1983])))

# create log hpi difference by CBSA year from prior year
dp[,growHPI:=log(hpi/shift(hpi,1)),by=CBSA]
dp[,growPermits:=log(ALL_PERMITS_/shift(ALL_PERMITS_,1)),by=CBSA]

print(summary(feols(log(growPermits)~growHPI+i(year)|CBSA,data=dp[year>1983])))
print(summary(feols(log(growPermits)~growHPI+log(hpiMaxRatio) + i(year)|CBSA,data=dp[year>1983])))

dfdfddf



# housing units 1980
d80 <- fread("data/raw/nhgis0069_csv/nhgis0069_ds104_1980_county.csv",select=c("STATEA","COUNTYA","C8Y001"))
d80[,county80:=STATEA*1000+COUNTYA]
d80[,STATEA:=NULL]
d80[,COUNTYA:=NULL]
d80 <- merge(d80,dm,by.x="county80",by.y="County code",all.x=TRUE)
d80[,isDivision:=(`Metropolitan division code`<99999)]
d80[,metroFHFA:=isDivision*`Metropolitan division code`+(1-isDivision)*`Core-based statistical area code`]
d80 <- unique(d80[,.(units1980=sum(C8Y001,na.rm=TRUE)),by=metroFHFA])
# metro FHFA units in 1980
# merge with permits
d80 <- merge(dp,d80,by="metroFHFA",all.x=TRUE) # permits and units for plotting
dg1 <- d80[,.(hpi83 = max(hpi*(year==1983),na.rm=TRUE),hpi90 = max(hpi*(year==1990),na.rm=TRUE),units1980,permits=sum(ALL_PERMITS_*(year>1982) *(year<2000))),by=metroFHFA]
print(dg1)
dfdfdf


dp[,hpi1983:=max(hpi*(year==1983),na.rm=TRUE),by=metroFHFA]
dp[,hpi1990:=max(hpi*(year==1990),na.rm=TRUE),by=metroFHFA]
dp <- dp[(hpi1983>0) & (hpi1990>0)]
dp[,permits8390:=sum(ALL_PERMITS_*(year>=1983 & year<=1990),na.rm=TRUE),by=metroFHFA]
dp <- dp[permits8390>0]
print(dp)
dp[,quasiElasticity:=log(1+permits8390/u80)-log(hpi1990/hpi1983),by=metroFHFA]
print(unique(dp[order(quasiElasticity),.(cbsaNAME,quasiElasticity)]))
print(dp[metroFHFA==35614])
dp <- dp[quasiElasticity>0]
print(dp[order(quasiElasticity),.(cbsaNAME,quasiElasticity,permits8390,hpi1983,hpi1990)])
dp[,growHPI:=log(hpi/shift(hpi,1)),by=metroFHFA]
dp[,growHPILONG:=log(hpi/hpi1983)]
print(dp)
dp[,growElasticity:=growHPI*quasiElasticity]
dp[,growElasticityLong:=growHPILONG*quasiElasticity]
print(summary(dp))
fwrite(dp,"Downloads/elasticitySOCDS.csv")


dplot <- data.table(year=numeric(),coefSHORT=numeric(),coefLONG=numeric())
for (y in 1984:2022) {
	print(y)
	regSHORT <- feols(log(ALL_PERMITS_/u80)~growElasticity,data=dp[year==y])
	print(coef(regSHORT)["growElasticity"])
	regSHORTLevel <- feols((ALL_PERMITS_/u80)~growElasticity,data=dp[year==y])
	print("level")
	print(coef(regSHORTLevel)["growElasticity"])
	regLONG <- feols(log(ALL_PERMITS_/u80)~growElasticityLong,data=dp[year==y])
	dplot <- rbind(dplot,data.table(year=y,coefSHORT=coef(regSHORT)["growElasticity"],coefLONG=coef(regLONG)["growElasticityLong"]))
} 
ggplot(dplot,aes(x=year,y=coefSHORT))+geom_line()+geom_point()
ggsave("docs/usprice/text/elasticitySOCDS.png")


dp[,hpiMaxRatio:=NULL]
dp[,hpiMax:=1]
dplot <- data.table(year=numeric(),permits=numeric(),hpiMaxRatio=numeric())
for (y in 1983:2022) {
	print(y)
	dp[,tempvar:=max(hpi*(year<y),na.rm=TRUE),by=metroFHFA]
	dp[year==y,hpiMax:=max(tempvar,na.rm=TRUE),by=metroFHFA]
	dp[year==y,hpiMaxRatio:=hpi/hpiMax]
	dplot <- rbind(dplot,data.table(year=y,permits=log(median(dp[year==y,ALL_PERMITS_/u80],na.rm=TRUE)),hpiMaxRatio=log(median(dp[year==y,hpi/hpiMax],na.rm=TRUE))))
}

METROPLOTTED <- 0
if (METROPLOTTED==0) {
	for (m in unique(dp[,cbsaNAME])) {
		dx <- dp[cbsaNAME==m,.(year,permits=log(ALL_PERMITS_/1000),hpi=log(hpi),hpiMaxRatio)]
		dx <- melt(dx,id.vars=c("year"),variable.name="var",value.name="value")
		ggplot(dx,aes(x=year,y=value,color=var))+geom_line()+geom_point()+ggtitle(m)
		texter <- paste("docs/usprice/text/timeSeries/timeSeries",gsub("/","",m),".png",sep="")
		print(texter)
		ggsave(texter)
	}
}


print(summary(dp))
print(summary(feols(log(ALL_PERMITS_)~log(hpi)+i(year)|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~log(hpi)+log(hpiMax)+i(year)|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~log(hpiMaxRatio)+i(year)|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~log(hpi)+log(hpiMax)+i(year)*quasiElasticity|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~hpiMaxRatio+i(year)|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(ALL_PERMITS_)~hpiMaxRatio+hpiMaxRatio^2+hpiMaxRatio^3+i(year)|metroFHFA,data=dp[year>1983])))
print(summary(feols(log(hpi) ~ i(year>2012)*quasiElasticity|metroFHFA+year,data=dp[(year>1990) & (year<2020)])))

ggplot(dplot,aes(x=hpiMaxRatio,y=permits))+geom_line()+geom_point()
ggsave("docs/usprice/text/permitsHPIMaxxy.png")


dplot <- melt(dplot,id.vars=c("year"),variable.name="var",value.name="value")
ggplot(dplot,aes(x=year,y=value,color=var))+geom_line()+geom_point()
ggsave("docs/usprice/text/permitsHPIMax.png")

print(names(dp))
print(summary(dp))



# old 
if (FALSE) {
# SOCDS total permits by county
selectVars <- c("GEOID","STATE","COUNTY",paste("ALL_PERMITS_",1980:2022,sep=""))                                  
df <- fread("data/raw/Residential_Construction_Permits_by_County_5026727375813176131.csv",select=selectVars)

# county to CBSA 2022
dm <- fread("data/raw/geocorr2022countyCBSA.csv",skip=1,select=c("County code","Core-based statistical area code","Metropolitan division code"))
df[,countyCode:=STATE*1000+COUNTY]
df <- merge(df,dm,by.x="countyCode",by.y="County code",all.x=TRUE)
df[,isDivision:=(`Metropolitan division code`<99999)]
df[,metroFHFA:=isDivision*`Metropolitan division code`+(1-isDivision)*`Core-based statistical area code`]

# get annual permits by metro
dp <- unique(df[,.(metroFHFA=metroFHFA,cbsa=`Core-based statistical area code`,STATE)])
dp[,sumNA:=0]
dp[,lastNA:=0]
for (y in 1980:2022) {
	v <- paste("ALL_PERMITS_",y,sep="")
	newdata <- df[,.(sum(get(v),na.rm=TRUE),sum(is.na(get(v)))),by=metroFHFA]
	setnames(newdata,old="V1",new=v)
	setnames(newdata,old="V2",new="thisNA")
	dp <- merge(dp,newdata,by="metroFHFA")
	dp[,sumNA:=sumNA+thisNA]
	# drop the NA variable
	dp[thisNA>0,lastNA:=y]
	dp[,thisNA:=NULL]
	print(dp)
}
dp <- melt(dp,id.vars=c("metroFHFA","STATE"),variable.name="year",value.name="ALL_PERMITS_")
# rename year only from form ALL_PERMITS_1980 to 1980
dp[,year:=as.numeric(substr(year,13,16))]

}