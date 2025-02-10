# socds.R
# R to get permit data from SOCDS
# Tom Davidoff
# 06/17/24

library(data.table)
library(fixest)
library(ggplot2)
library("openxlsx")

selectVars <- c("GEOID","STATE","COUNTY",paste("ALL_PERMITS_",1980:2022,sep=""))                                  
df <- fread("docs/data/usprice/Residential_Construction_Permits_By_County.csv",select=selectVars)
print(summary(df))
# STATE COUNTY

# county to metro
#dm <- data.table(read.xlsx("docs/data/usprice/list1_2023.xlsx",startRow=3))
#dm <- dm[,.(Metropolitan.Division.Code,Metropolitan.Division.Title,CSA.Code,CBSA.Title,FIPS.State.Code,FIPS.County.Code)]
dm <- fread("docs/data/usprice/geocorr2022countyCBSA.csv",skip=1,select=c("County code","Core-based statistical area code","Metropolitan division code"))
print(summary(dm))
du <- fread("docs/data/usprice/cbsaUnits.csv")
print(du)
print(nrow(dm))
dm <- merge(dm,du,by.x="Core-based statistical area code",by.y="cbsa20",all.x=TRUE)
print(nrow(dm))

df[,countyCode:=STATE*1000+COUNTY]
df <- merge(df,dm,by.x="countyCode",by.y="County code",all.x=TRUE)
df[,isDivision:=(`Metropolitan division code`<99999)]
df[,metroFHFA:=isDivision*`Metropolitan division code`+(1-isDivision)*`Core-based statistical area code`]

dp <- unique(df[,.(metroFHFA=metroFHFA,cbsa=`Core-based statistical area code`,u80)])
for (v in paste("ALL_PERMITS_",1980:2022,sep="")) {
	print(v)
	newdata <- df[,(sum(get(v),na.rm=TRUE)),by=metroFHFA]
	setnames(newdata,old="V1",new=v)
	print(newdata)
	dp <- merge(dp,newdata,by="metroFHFA")
}
print(summary(dp))
dp <- melt(dp,id.vars=c("metroFHFA","u80"),variable.name="year",value.name="ALL_PERMITS_")
# rename year only from form ALL_PERMITS_1980 to 1980
dp[,year:=as.numeric(substr(year,13,16))]

hpi <- fread("docs/williams/data/HPI_AT_metro_2024.csv",header=FALSE,na.strings=c("-"))
names(hpi) <- c("cbsaNAME","metroFHFA","year","quarter","hpi","zz")
cpi <- fread("docs/data/usprice/CPIAUCNS.csv") # seasonally adjusted
cpi[,quarter:=quarter(cpi$DATE)]
cpi[,year:=year(cpi$DATE)]
cpi <- cpi[,.(cpi=mean(CPIAUCNS)),by=.(year,quarter)]
hpi <- merge(hpi,cpi,by=c("year","quarter"))
hpi[,hpi:=hpi/cpi]
print(hpi[,.N])
hpi <- unique(hpi[,.(cbsaNAME,hpi=mean(hpi)),by=.(metroFHFA,year)])
print(hpi[,.N])
print(summary(hpi))

dp <- unique(dp)
dp <- merge(dp,hpi,by=c("metroFHFA","year"))
print(dp)
print(unique(dp$CBSA.Title))

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



