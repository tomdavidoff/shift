# getCEW.R
# R to loop through CEW files by year
# Tom Davidoff

library(data.table)

# files taken from getCEW.sh as download speeds very slow. Will delete raw files after processing
for (y in 1990:2023) {
    filename <- paste("data/raw/",y, ".annual.singlefile.csv", sep="")
    
    df <- fread(paste0(filename,sep=""),select=c("area_fips","own_code","industry_code","agglvl_code","year","annual_avg_emplvl"))
    print(table(df[,agglvl_code]))
    df <- df[agglvl_code==75 | agglvl_code==15 ] # county by NAICS 5 (and ownership) 77 or national 17 too many zeros, screw ups later
    fwrite(df, paste0("data/raw/cew_", y, ".csv"))

}
