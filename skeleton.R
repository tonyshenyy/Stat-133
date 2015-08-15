# =============================================================================
# Skeleton.R
# Description: This file contains all the commands to create folders and files
#              as well as commands to download resource files.
# Name: Yiyang Shen and Caylie Marie Connelly
# =============================================================================

# commands create folders code, rawdata, data, resources, report and images
dir.create('./code')
dir.create('./rawdata')
dir.create('./data')
dir.create('./resources')
dir.create('./report')
dir.create('./images')

# command to create file README.md
file.create('./README.md')

# command to download the raw data files
    # For storms.csv and tracks.csv
download.file(
  'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat',
  './rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat')
download.file(
  'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.EP.ibtracs_hurdat.v03r06.hdat',
  './rawdata/Basin.EP.ibtracs_hurdat.v03r06.hdat')

    # For Visualization
download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv',
  './rawdata/Basin.EP.ibtracs_wmo.v03r06.csv')
download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv',
  './rawdata/Basin.NA.ibtracs_wmo.v03r06.csv')
