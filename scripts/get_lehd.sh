
cd $(git rev-parse --show-toplevel)
cd data

# JTOO file
wget "https://lehd.ces.census.gov/data/lodes/LODES7/il/od/il_od_aux_JT00_2017.csv.gz"
gunzip il_od_aux_JT00_2017.csv.gz

cd ..
