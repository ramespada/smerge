#!/bin/bash

year=2025
n=10

station_list_file="station_list_file.txt"

case $1 in 
-download)

  if [ ! -f $station_list_file ] 
  then 

     echo "Downloading station lists.."
     wget https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt
     grep "${year}0[12345]" isd-history.txt > $station_list_file

  fi;

  stations=(`shuf $station_list_file | awk '{print $1}' | head -n 10`)

  echo "Downloading ISHD files.."
  for id_sfc in ${stations[@]};
  do 
  	echo "  Station ID: $id_sfc"
  	wget https://www.ncei.noaa.gov/pub/data/noaa/${year}/${id_sfc}-99999-${year}.gz -P met/.
  	gzip -d met/${id_sfc}*.gz
  done
  
;;
-test)
echo "" > temporal.txt

  files=($(ls met))
  
  for f in ${files[@]}
  do 
     id=${f//-*/}
     STR="      @SFCMET=./met/${f}@ @IFSTN = ${id} @ @ASTZ = UTC-0000@ @END@"
     echo "${STR//@/!}" >> temporal.txt
  
  done
  
  sed "/__FILE LIST__/r temporal.txt" smerge_template.inp > smerge.inp
  sed -i "s/! NFF *=.*/!NFF = ${#files[@]} !/" smerge.inp
  sed -i "s/! IBYR *=.*/! IBYR = ${year} !/" smerge.inp
  sed -i "s/! IEYR *=.*/! IEYR = ${year} !/" smerge.inp
  
  rm temporal.txt
;;
esac
