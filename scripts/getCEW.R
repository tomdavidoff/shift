# getCEW.R
# R to loop through CEW files by year
# Tom Davidoff

library(data.table)

# typical URL https://data.bls.gov/cew/data/files/2023/csv/2023_annual_singlefile.zip
for (y in 1990:2023) {
    print(y)
    # See if file already exists
    if (file.exists(paste0("data/raw/cew_", y, ".csv"))) {
        print(paste0("data/raw/cew_", y, ".csv already exists"))
        next
    }
    dirname <- paste0("https://data.bls.gov/cew/data/files/", y, "/csv/")
    filename <- paste(y, "_annual_singlefile.zip", sep="")
    
    df <- fread(paste0(dirname, filename,sep=""),select=c("area_fips","own_code","industry_code","agglvl_code","year","annual_avg_emplvl"))
    print(table(df[,agglvl_code]))
    df <- df[agglvl_code==77 | agglvl_code==17] # county by NAICS 5 (and ownership) 77 or national 17 -- confirm same
    fwrite(df, paste0("data/raw/cew_", y, ".csv"))

}
