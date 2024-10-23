## Shell script to generate the web-tiled rasters of for all time-series using gdal
# Jakob J. Assmann jakob.assmann@uzh.ch 13 May 2024
# *do not include "cbh_2019", "tlb_2019_a", "tlb_2019_b"

# This scripts takes all drone rasters for a given year, scales them to Byte, bunches them into a VRT and then generates web-tiles

# 2014
echo Generating tiles for 2014
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2014_byte.tif ../../data/web_data/tiles/stage/cbh_2014.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2014.tif ../../data/web_data/tiles/stage/tlb_2014.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2014.tif ../../data/web_data/tiles/stage/rdg_2014.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2014.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2014.vrt ../../data/web_data/tiles/2014
rm ../../data/web_data/tiles/stage/cbh_2014.tif
rm ../../data/web_data/tiles/stage/tlb_2014.tif
rm ../../data/web_data/tiles/stage/rdg_2014.tif
rm ../../data/web_data/tiles/stage/2014.vrt

# 2016
echo Generating tiles for 2016
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2016.tif ../../data/web_data/tiles/stage/cbh_2016.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2016.tif ../../data/web_data/tiles/stage/tlb_2016.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2016.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2016.vrt ../../data/web_data/tiles/2016
rm ../../data/web_data/tiles/stage/cbh_2016.tif
rm ../../data/web_data/tiles/stage/tlb_2016.tif
rm ../../data/web_data/tiles/stage/2016.vrt

# 2017
echo Generating tiles for 2017
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2017.tif ../../data/web_data/tiles/stage/cbh_2017.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2017.tif ../../data/web_data/tiles/stage/tlb_2017.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2017_b.tif ../../data/web_data/tiles/stage/rdg_2017.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2017.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2017.vrt ../../data/web_data/tiles/2017
rm ../../data/web_data/tiles/stage/cbh_2017.tif
rm ../../data/web_data/tiles/stage/tlb_2017.tif
rm ../../data/web_data/tiles/stage/rdg_2017.tif
rm ../../data/web_data/tiles/stage/2017.vrt

# 2018
echo Generating tiles for 2018
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2018.tif ../../data/web_data/tiles/stage/cbh_2018.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2018.tif ../../data/web_data/tiles/stage/tlb_2018.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2018.tif ../../data/web_data/tiles/stage/rdg_2018.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2018.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2018.vrt ../../data/web_data/tiles/2018
rm ../../data/web_data/tiles/stage/cbh_2018.tif
rm ../../data/web_data/tiles/stage/tlb_2018.tif
rm ../../data/web_data/tiles/stage/rdg_2018.tif
rm ../../data/web_data/tiles/stage/2018.vrt

# 2019
echo Generating tiles for 2019
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2019_b.tif ../../data/web_data/tiles/stage/cbh_2019.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2019_c.tif ../../data/web_data/tiles/stage/tlb_2019.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2019_a.tif ../../data/web_data/tiles/stage/rdg_2019.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2019.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2019.vrt ../../data/web_data/tiles/2019
rm ../../data/web_data/tiles/stage/cbh_2019.tif
rm ../../data/web_data/tiles/stage/tlb_2019.tif
rm ../../data/web_data/tiles/stage/rdg_2019.tif
rm ../../data/web_data/tiles/stage/2019.vrt

# 2020
echo Generating tiles for 2020
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2020.tif ../../data/web_data/tiles/stage/cbh_2020.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2020.tif ../../data/web_data/tiles/stage/tlb_2020.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2020.tif ../../data/web_data/tiles/stage/rdg_2020.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2020.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2020.vrt ../../data/web_data/tiles/2020
rm ../../data/web_data/tiles/stage/cbh_2020.tif
rm ../../data/web_data/tiles/stage/tlb_2020.tif
rm ../../data/web_data/tiles/stage/rdg_2020.tif
rm ../../data/web_data/tiles/stage/2020.vrt

# 2021
echo Generating tiles for 2021
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/cbh/norm/cbh_2021.tif ../../data/web_data/tiles/stage/cbh_2021.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/tlb/norm/tlb_2021.tif ../../data/web_data/tiles/stage/tlb_2021.tif
gdal_translate -ot Byte -scale 0 65535 0 255 -co PHOTOMETRIC=RGB -co ALPHA=YES ../../data/drone_data/rdg/web_gis/rdg_2021.tif ../../data/web_data/tiles/stage/rdg_2021.tif
gdalbuildvrt ../../data/web_data/tiles/stage/2021.vrt ../../data/web_data/tiles/stage/*
gdal2tiles.py --zoom 15-20 --processes=15 ../../data/web_data/tiles/stage/2021.vrt ../../data/web_data/tiles/2021
rm ../../data/web_data/tiles/stage/cbh_2021.tif
rm ../../data/web_data/tiles/stage/tlb_2021.tif
rm ../../data/web_data/tiles/stage/rdg_2021.tif
rm ../../data/web_data/tiles/stage/2021.vrt
