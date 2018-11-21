#!/bin/bash

alias md5='md5sum'
quiet=''
directory='downloads'
clean=false

while getopts ":qc" opt; do
  case ${opt} in
    q ) quiet='-q'
      ;;
    c ) clean=true
      ;;
    \? )
      echo "Invalid Option: -$OPTARG" 1>&2
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

function get_file()
{
   local g_fpth=$1
   echo "GET    : [${g_fpth}]" 
   mkdir -p ${directory}
   wget ${quiet} -P ${directory} -nc ${g_fpth}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: get_file failed [${g_fpth}]"
      return 1
   fi
}

function check_file()
{
   local c_file=$1
   local c_md5h=$2
   local c_fpth=${directory}/${c_file}
   echo "CHECK  : [${c_file}]"
   c_md5s=`md5sum ${c_fpth} | sed 's/\s.*$//'`
   if [[ "${c_md5s}" != "${c_md5h}" ]]; then
      echo "ERROR: check_file failed [${c_file}]"
      return 1
   fi
}

function extract_file()
{
   local e_libr=$1
   local e_file=$2
   local e_extn=$3
   local e_fpth=${directory}/${e_file}
   echo "EXTRACT: [${e_file}]"
   if [[ "${e_extn}" == "zip" ]]; then
      unzip ${quiet} -n -d ${e_libr} ${e_fpth}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file unzip failed [${e_file}]"
         return 1
      fi
   elif [[ ${e_extn} == "tar.gz" ]]; then
      gzip ${quiet} -k -d -f ${e_fpth}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file gzip failed [${e_file}]"
         return 1
      fi
      mkdir -p ${e_libr}
      tar --skip-old-files -xf ${e_fpth/.gz/} -C ${e_libr}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file tar failed [${e_file}]"
         return 1
      fi
      rm -f ${e_fpth/.gz/}
   elif [[ ${e_extn} == "tar.Z" ]]; then
      gzip ${quiet} -k -d -f ${e_fpth}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file gzip failed [${e_file}]"
         return 1
      fi
      mkdir -p ${e_libr}
      tar --skip-old-files -xf ${e_fpth/.Z/} -C ${e_libr}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file tar failed [${e_file}]"
         return 1
      fi
      rm -f ${e_fpth/.Z/}
   elif [[ ${e_extn} == "tar" ]]; then
      mkdir -p ${e_libr}
      tar --skip-old-files -xf ${e_fpth} -C ${e_libr}
      if [ "$?" -ne 0 ]; then
         echo "ERROR: extract_file tar failed [${e_file}]"
         return 1
      fi
   else
      echo "ERROR: extract_file failed [${e_file}]"
      return 1
   fi
}

function clean_lib()
{
   local c_libr=$1
   local c_file=$2
   local c_fpth=${directory}/${c_file}
   rm -f ${c_fpth}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: clean_lib failed [${c_fpth}]"
      return 1
   fi
   rm -rf ${c_libr}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: clean_lib failed [${c_libr}]"
      return 1
   fi
   echo "SUCCESS: clean [${c_libr}]"
}

function get_lib()
{
   local g_libr=$1
   local g_file=$2
   local g_extn=$3
   local g_fpth=$4
   local g_md5h=$5

   get_file ${g_fpth}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: get_file failed [${g_fpth}]"
      return 1
   fi
   check_file ${g_file} ${g_md5h}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: check_file failed [${g_file}]"
      return 1
   fi
   extract_file ${g_libr} ${g_file} ${g_extn}
   if [ "$?" -ne 0 ]; then
      echo "ERROR: extract_file failed [${g_file}]"
      return 1
   fi
   echo "SUCCESS: get [${g_libr}]"
}

# jasper
libr="jasper"; vrsn="1.900.1"; extn="zip"
file="${libr}-${vrsn}.${extn}"
site="http://www.ece.uvic.ca/~frodo/jasper/software"
fpth="${site}/${file}"
md5h="a342b2b4495b3e1394e161eb5d85d754"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# grib_api
libr="grib_api"; vrsn="1.12.3"; extn="tar.gz"
file="${libr}-${vrsn}.${extn}"
site="https://confluence.ecmwf.int/download/attachments/3473437"
fpth="${site}/${file}"
md5h="584f60702aeed70330cca42d13b96889"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# hdf4
libr="hdf"; vrsn="4.2.11"; extn="tar.gz"
file="${libr}-${vrsn}.${extn}"
site="https://support.hdfgroup.org/ftp/HDF/releases/HDF4.2.11/src"
fpth="${site}/${file}"
md5h="063f9928f3a19cc21367b71c3b8bbf19"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# hdf5
libr="hdf5"; vrsn="1.8.14"; extn="tar.gz"
file="${libr}-${vrsn}.${extn}"
site="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.14/src"
fpth="${site}/${file}"
md5h="a482686e733514a51cde12d6fe5c5d95"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# hdfeos
libr="HDF-EOS"; vrsn="2.19v1.00"; extn="tar.Z"
file="${libr}${vrsn}.${extn}"
site="ftp://edhs1.gsfc.nasa.gov/edhs/hdfeos/previous_releases"
fpth="${site}/${file}"
md5h="b8648484fc78a2db7073dd603f3fb251"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# netcdf
libr="netcdf"; vrsn="v4.3.3.1"; extn="tar.gz"
file="${vrsn}.${extn}"
site="https://github.com/Unidata/netcdf-c/archive"
fpth="${site}/${file}"
md5h="41fe6758d46cccb1675693d155ee7001"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# netcdf-fortran
libr="netcdf-fortran"; vrsn="4.2"; extn="tar.gz"
file="${libr}-${vrsn}.${extn}"
site="https://github.com/Unidata/netcdf-fortran/archive"
fpth="${site}/${file}"
md5h="2f9df26da58c68bd0baaab1ca961aee3"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

# esmf
libr="esmf"; vrsn="esmf-e3ac95b2e25a36d6cbfa68ed93eb1645fea29fcd"; extn="zip"
file="${libr}-${vrsn}.${extn}"
site="https://sourceforge.net/code-snapshots/git/e/es/esmf/esmf.git"
fpth="${site}/${file}"
md5h="578250d13ebde1efe869eba2fb2f516f"
if [ "${clean}" = true ] ; then
   clean_lib ${libr} ${file}
else
   get_lib ${libr} ${file} ${extn} ${fpth} ${md5h}
fi

exit 0
