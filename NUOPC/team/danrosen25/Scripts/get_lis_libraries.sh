#!/bin/bash

# Darwin (OSX) Support
if [[ $(uname) == "Darwin" ]]; then
   MD5='md5 -r'
   TAROPT=''
else
   MD5='md5sum'
   TAROPT='--skip-old-files'
fi

# Shell Options
set -u
set -o pipefail

# Script Defaults
usage="$(basename $0) [-h] [-p compiler] [-q] [-c] [-l]"
RED='' # Red
GRN='' # Green
YLW='' # Dark Gray
BLU='' # Blue
NCL='' # No Color
quiet=''
directory='downloads'
clean=false
scnt=0
ecnt=0
compiler='default'

# Script Options
while getopts ":hp:qcl" opt; do
  case ${opt} in
    h ) printf "script usage: ${usage}\n"
        printf "\t-p\tcompiler\tset the compiler\n"
        printf "\t-h\thelp\tprints this help information and exits\n"
        printf "\t-q\tquiet\tsupress wget, unzip, and tar output\n"
        printf "\t-c\tclean\tdelete downloaded and extracted files\n"
        printf "\t-l\tcolor\tadds color to output\n"
        exit 0
      ;;
    p ) compiler=$OPTARG
      ;;
    q ) quiet='-q'
      ;;
    c ) clean=true
      ;;
    l ) RED='\033[0;31m' # Red
        GRN='\033[0;32m' # Green
        YLW='\033[0;33m' # Dark Gray
        BLU='\033[0;34m' # Blue
        NCL='\033[0;00m' # No Color
      ;;
    ? )
      printf "ERROR: script usage: ${usage}\n" 1>&2
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

# Determine Compiler
function set_compiler()
{
   local icc=false
   local ifort=false
   local gcc=false
   local gfotran=false
   icc --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then icc=true; fi 
   ifort --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then ifort=true; fi
   gcc --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then gcc=true; fi
   gfortran --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then gfortran=true; else gfortran=false ; fi
   if [[ "${compiler}" == "default" ]]; then 
      if [ "${icc}" = true ] && [ "${ifort}" = true ] ; then
         compiler='intel'
      elif [ "${gcc}" = true ] && [ "${gfortran}" = true ] ; then
         compiler='gnu'
      else
         compiler='none'
      fi
   fi
   if [[ "${compiler}" == "intel" ]]; then
      if [ "${icc}" = false ] || [ "${ifort}" = false ] ; then
         CC_VERS="not found"
         FC_VERS="not found"
         printf "${RED}ERROR  :${NCL} intel compilers not found\n" 1>&2
         return 1
      fi
      export CC="icc"
      export FC="ifort"
      export F77="ifort"
      export F90="ifort"
      CC_VERS=`${CC} --version | head -n 1`
      FC_VERS=`${FC} --version | head -n 1`
   elif [[ "${compiler}" == "gnu" ]]; then
      if [ "${gcc}" = false ] || [ "${gfortran}" = false ] ; then
         CC_VERS="not found"
         FC_VERS="not found"
         printf "${RED}ERROR  :${NCL} gnu compilers not found\n" 1>&2
         return 1
      fi
      export CC="gcc"
      export FC="gfortran"
      export F77="gfortran"
      export F90="gfortran"
      CC_VERS=`${CC} --version | head -n 1`
      FC_VERS=`${FC} --version | head -n 1`
   else
      CC_VERS="not found"
      FC_VERS="not found"
      printf "${RED}ERROR  :${NCL} compiler not supported [${compiler}]\n" 1>&2
      return 1
   fi
}

# Get File Using wget
function get_file()
{
   local g_fpth=$1
   printf "${YLW}GET    :${NCL} [${g_fpth}]\n" 
   mkdir -p ${directory}
   wget ${quiet} -P ${directory} -nc ${g_fpth}
   if [ "$?" -ne 0 ]; then return 1; fi
}

# Check File Using md5sum
function check_file()
{
   local c_file=$1
   local c_md5h=$2
   local c_fpth=${directory}/${c_file}
   printf "${YLW}CHECK  :${NCL} [${c_file}]\n"
   c_md5s=`${MD5} ${c_fpth} | sed 's/ .*$//'`
   if [[ "${c_md5s}" != "${c_md5h}" ]]; then return 1; fi
}

# Extract File Using unzip, gzip, tar
function extract_file()
{
   local e_libr=$1
   local e_file=$2
   local e_extn=$3
   local e_fpth=${directory}/${e_file}
   printf "${YLW}EXTRACT:${NCL} [${e_file}]\n"
   if [[ "${e_extn}" == "zip" ]]; then
      unzip ${quiet} -n -d ${e_libr} ${e_fpth}
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${e_extn} == "tar.gz" ]]; then
      mkdir -p ${e_libr}
      tar ${TAROPT} -zxf ${e_fpth} -C ${e_libr}
      if [ "$?" -ne 0 ]; then return 1; fi
      rm -f ${e_fpth/.gz/}
   elif [[ ${e_extn} == "tar.Z" ]]; then
      mkdir -p ${e_libr}
      tar ${TAROPT} -zxf ${e_fpth} -C ${e_libr}
      if [ "$?" -ne 0 ]; then return 1; fi
      rm -f ${e_fpth/.Z/}
   elif [[ ${e_extn} == "tar" ]]; then
      mkdir -p ${e_libr}
      tar ${TAROPT} -xf ${e_fpth} -C ${e_libr}
      if [ "$?" -ne 0 ]; then return 1; fi
   else
      printf "${RED}ERROR  :${NCL} extension unknown [${e_extn}]\n" 1>&2
      return 1
   fi
}

# Clean Downloads and Extracted Files
function clean_lib()
{
   local c_libr=$1
   local c_file=$2
   local c_fpth=${directory}/${c_file}
   rm -f ${c_fpth}
   if [ "$?" -ne 0 ]; then return 1; fi
   rm -rf ${c_libr}
   if [ "$?" -ne 0 ]; then return 1; fi
   printf "${GRN}SUCCESS:${NCL} clean [${c_libr}]\n"
}

# Download, Check, and Extract Files
function get_lib()
{
   local g_libr=$1
   local g_file=$2
   local g_extn=$3
   local g_site=$4
   local g_md5h=$5

   get_file "${g_site}/${g_file}"
   if [ "$?" -ne 0 ]; then
      printf "${RED}ERROR  :${NCL} get_file failed [${g_fpth}]\n" 1>&2
      return 1
   fi
   check_file ${g_file} ${g_md5h}
   if [ "$?" -ne 0 ]; then
      printf "${RED}ERROR  :${NCL} check_file failed [${g_file}]\n" 1>&2
      return 1
   fi
   extract_file ${g_libr} ${g_file} ${g_extn}
   if [ "$?" -ne 0 ]; then
      printf "${RED}ERROR  :${NCL} extract_file failed [${g_file}]\n" 1>&2
      return 1
   fi
   printf "${GRN}SUCCESS:${NCL} get [${g_libr}]\n"
}

set_compiler

# hdf5
libr="hdf5"; vrsn="1.8.14"; extn="tar.gz"
file="hdf5-1.8.14.tar.gz"
site="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.14/src"
md5h="a482686e733514a51cde12d6fe5c5d95"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# netcdf
libr="netcdf"; vrsn="v4.3.3.1"; extn="tar.gz"
file="v4.3.3.1.tar.gz"
site="https://github.com/Unidata/netcdf-c/archive"
md5h="41fe6758d46cccb1675693d155ee7001"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# netcdf-fortran
libr="netcdf-fortran"; vrsn="4.2"; extn="tar.gz"
file="netcdf-fortran-4.2.tar.gz"
site="https://github.com/Unidata/netcdf-fortran/archive"
md5h="2f9df26da58c68bd0baaab1ca961aee3"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# jasper
libr="jasper"; vrsn="1.900.1"; extn="zip"
file="jasper-1.900.1.zip"
site="http://www.ece.uvic.ca/~frodo/jasper/software"
md5h="a342b2b4495b3e1394e161eb5d85d754"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# grib_api
libr="grib_api"; vrsn="1.12.3"; extn="tar.gz"
file="grib_api-1.12.3.tar.gz"
site="https://confluence.ecmwf.int/download/attachments/3473437"
md5h="584f60702aeed70330cca42d13b96889"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# esmf
libr="esmf"; vrsn="7_1_0r"; extn="tar.gz"
file="esmf_7_1_0r_src.tar.gz"
site="https://sourceforge.net/projects/esmf/files/ESMF_7_1_0r"
md5h="9e455bc36a0aaa9b87e0bdedc78a47f5"
export ESMF_DIR="<ESMF Directory>"
export ESMF_INSTALL_PREFIX="<ESMF Installation Directory>"
export ESMF_COMM="mpi"
export ESMF_BOPT="O"
if [[ "${compiler}" == "intel" ]]; then
   export ESMF_COMPILER="intel"
elif [[ "${compiler}" == "gnu" ]]; then
   export ESMF_COMPILER="gfortran"
else
   printf "${RED}ERROR  :${NCL} compiler not supported [${compiler}]\n" 1>&2
fi
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# hdf4
libr="hdf"; vrsn="4.2.11"; extn="tar.gz"
file="hdf-4.2.11.tar.gz"
site="https://support.hdfgroup.org/ftp/HDF/releases/HDF4.2.11/src"
md5h="063f9928f3a19cc21367b71c3b8bbf19"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# hdfeos
libr="HDF-EOS"; vrsn="2.19v1.00"; extn="tar.Z"
file="HDF-EOS2.19v1.00.tar.Z"
site="ftp://edhs1.gsfc.nasa.gov/edhs/hdfeos/previous_releases"
md5h="b8648484fc78a2db7073dd603f3fb251"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
else
   get_lib ${libr} ${file} ${extn} ${site} ${md5h}
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# Print Summary Information
printf "\n"
printf "${BLU}###################################################${NCL}\n"
printf "${BLU}#                     SUMMARY                     #${NCL}\n"
printf "${BLU}###################################################${NCL}\n"
printf "\n"
printf "\t${YLW}C Compiler:${NCL} ${CC_VERS}\n"
printf "\t${YLW}Fortran Compiler:${NCL} ${FC_VERS}\n"
printf "\n"
printf "\t${GRN}SUCCESS :${NCL} ${scnt}\n"
printf "\t${RED}FAILURES:${NCL} ${ecnt}\n"
printf "\n"

exit 0
