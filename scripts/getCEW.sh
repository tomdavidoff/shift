# getCEW.sh
# bash to get CEW as not working via R
# Tom Davidoff
# Note files moved to data/raw ex-post

# loop through files with names https://data.bls.gov/cew/data/files/1992/csv/1992_annual_singlefile.zip but 1992 substituted with 1990:2023
# curl, unzip and move to data/cew

for i in {1990..2023}
do
	filename=$i"_annual_singlefile.zip"
	url="https://data.bls.gov/cew/data/files/"$i"/csv/"$filename
	  curl -o dummy.zip $url
	  unzip dummy.zip
done	
