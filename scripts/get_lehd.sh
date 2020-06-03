
cd $(git rev-parse --show-toplevel)
cd data

# JTOO files: main and auxiliary
wget "https://lehd.ces.census.gov/data/lodes/LODES7/il/od/il_od_main_JT00_2017.csv.gz"
wget "https://lehd.ces.census.gov/data/lodes/LODES7/il/od/il_od_aux_JT00_2017.csv.gz"
gunzip il_od_main_JT00_2017.csv.gz
gunzip il_od_aux_JT00_2017.csv.gz


cd ..
