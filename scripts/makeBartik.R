# make Bartik.R
# R to make Bartik weights
# Tom Davidoff
# Febrary 14, 2025

library(data.table)
# get county to cbsa mapping

dm <- fread("data/raw/geocorr2022countyCBSA.csv",skip=1,select=c("County code","Core-based statistical area code")) #),"Metropolitan division code"))
#dm[,metroFHFA:=ifelse(`Metropolitan division code`==99999,`Core-based statistical area code`,`Metropolitan division code`)]
#dm <- dm[metroFHFA!=99999,.(area_fips,metroFHFA)]
setnames(dm,c("County code","Core-based statistical area code"),c("area_fips","CBSA"))

den <- data.table(NULL)

dx <- data.table(year = numeric(),bartikGrow=numeric(),CBSA=numeric())
for (y in 1990:2023) { #start with 1990 base
    df <- fread(paste("data/raw/cew_",as.character(y),".csv",sep=""),select=c("area_fips","industry_code","agglvl_code","annual_avg_emplvl","own_code"))
    dfu <- df[,.(sum(annual_avg_emplvl)),by="industry_code"]
    setnames(dfu,"V1",paste("employmentNational",y,sep=""))
    if (nrow(den)<10) {
        den <- dfu
    }
    else {
        den <- merge(den,dfu,by="industry_code")
    }
    df[,area_fips:=as.numeric(area_fips)]
    df <- merge(df,dm,by="area_fips")
    df <- df[agglvl_code==75]
    print(table(df[,own_code]))
    db <- df[,.(employment=sum(annual_avg_emplvl)),by=c("CBSA","industry_code")]
    print(db)
    db[,employmentTotal:=sum(employment),by=.(CBSA)]
    db[,bartikWeight:=employment/employmentTotal]
    db$employment <- db$employmentTotal <- NULL
    db <- merge(db,den,by="industry_code")
    print(summary(db))
    if (y>1990) {
        db[,grow:=get(paste("employmentNational",y,sep=""))/get(paste("employmentNational",y-1,sep=""))]
        print(round(c(y,mean(db$grow),cor(db[,.(grow,bartikWeight)])),2))
        metroGrow <- db[,.(year=y,bartikGrow=sum(bartikWeight*grow)),by="CBSA"]
        dx <- rbind(dx,metroGrow)
    }
}
print(dx)
fwrite(dx,"data/derived/bartikGrow.csv")