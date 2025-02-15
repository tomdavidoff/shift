# make Bartik.R
# R to make Bartik weights
# Tom Davidoff
# Febrary 14, 2025

library(data.table)
# get county to cbsa mapping

dm <- fread("data/raw/geocorr2022countyCBSA.csv",skip=1,select=c("County code","Core-based statistical area code","Metropolitan division code"))
dm[,metroFHFA:=ifelse(`Metropolitan division code`==99999,`Core-based statistical area code`,`Metropolitan division code`)]
setnames(dm,"County code","area_fips")
dm <- dm[metroFHFA!=99999,.(area_fips,metroFHFA)]

# start with 1990 base
df <- fread("data/raw/cew_1990.csv")
df[,area_fips:=as.numeric(area_fips)]
df <- merge(df,dm,by="area_fips")
df <- df[,agglvl_code:=77]
db <- df[,.(employment=sum(annual_avg_emplvl)),by=c("year","metroFHFA","industry_code")]
print(db)
db[,employmentTotal:=sum(employment),by=.(year,metroFHFA)]
db[,bartikWeight:=employment/employmentTotal]

# now get growth 1991-2023 based on US growth times weights
for (y in 1991:2023) {
    print(y)
    df <- fread(paste0("data/raw/cew_", y, ".csv"),select=c("area_fips","industry_code","agglvl_code","annual_avg_emplvl"))
    df <- df[agglvl_code==17 & area_fips=="US000"]
    df[,area_fips:=as.numeric(area_fips)]
    df <- merge(df,dm,by="area_fips")
    df <- df[,agglvl_code:=77]
    db <- rbind(db,df[,.(employment=sum(annual_avg_emplvl)),by=c("year","metroFHFA","industry_code")])
    db[,employmentTotal:=sum(employment),by=.(year,metroFHFA)]
    db[,bartikWeight:=employment/employmentTotal]
}