#!/bin/csh

# to run in your shell; not on compute node:
set dummy_  = "yes" 
# to run not on compute node:
#set dummy_  = "no" 


set mm_    = 01
set yyyy_  = 2018

set data_path_arch  = /archive/u/sakella/sst_data/USGODAE/Y${yyyy_}/M${mm_}/
set data_path_nbk   = /discover/nobackup/$USER/people/ANA_group/JongK/argoBUFR/decoder/from_MattT/wrkDir

set type_  = profile
set sType_ = 4
set oFmt_  = netcdf

set nDays = `cal ${mm_} ${yyyy_} | awk 'NF {DAYS = $NF}; END {print DAYS}'`
echo 'Num of Days in Month: '${mm_}', Year: '${yyyy_} '= ' ${nDays}

if (! -e ${data_path_nbk} ) /bin/mkdir -p $data_path_nbk

set iDay = 1
while ( ${iDay} <= ${nDays} )

   set iDay_ = `echo ${iDay} | awk '{printf "%02d\n", $0;}'`
   set file_in = ${yyyy_}${mm_}${iDay_}'00.'${type_}

   if (${dummy_} == "yes") then
     echo "Copying " ${file_in} "to " ${data_path_nbk}
     /bin/cp ${data_path_arch}/${file_in} ${data_path_nbk}/${file_in}
   else
     echo "Processing for day ${iDay}..."
     get_ocn_obs.csh ${data_path_nbk} ${type_} ${sType_} ${oFmt_} ${yyyy_} ${mm_} ${iDay_} 00
   endif

   @ iDay++
 end

if (${dummy_} == "no") then
  if ( -e ${data_path_nbk} ) /bin/rm -rf $data_path_nbk
endif
# ---------------------------

#foreach i ( 1 2 )
#   echo $i 
#end
