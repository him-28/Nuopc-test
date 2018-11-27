#!/bin/bash

# Darwin (OSX) Support
if [[ $(uname) == "Darwin" ]]; then
   MD5="md5 -r"
   TAROPT=""
else
   MD5="md5sum"
   TAROPT="--skip-old-files"
fi

# Script Dev Settings
# set -u
# set -o pipefail

# Script Defaults
RED="" # Red
GRN="" # Green
YLW="" # Dark Gray
BLU="" # Blue
NCL="" # No Color
quiet=""
output="/dev/stdout"
declare -A libraries=( 
   [hdf5]=true
   [netcdf]=true
   [netcdf-fortran]=true
   [jasper]=true
   [grib_api]=true
   [esmf]=true
   [hdf]=true
   [hdf-eos]=true
)
get=true
check=true
extract=true
config=true
build=true
scpdir="${PWD}"
libdir="${PWD}/LIBRARIES"
achdir="${libdir}/achives"
srcdir="${libdir}/src"
logdir="${libdir}"
insdir="${libdir}"
clean=false
scnt=0
ecnt=0
compiler="default"

# Script Options
usage="$(basename $0) [-h] [-l list] [-s #] [-p compiler] [-q] [-c] [-o #] [-r]"
while getopts ":hl:s:p:qco:r" opt; do
  case ${opt} in
    h ) printf "script usage: ${usage}\n"
        printf "\t-h\thelp\t\tprints this help information and exits\n"
        printf "\t-l\tlibraries\tinclude listed libraries, comma delimited\n"
        printf "\t-s\tstep\t\t1=get, 2=check, 3=extract, 4=config, 5=build\n"
        printf "\t-p\tcompiler\tset the compiler\n"
        printf "\t-q\tquiet\t\tsupress wget, unzip, and tar output\n"
        printf "\t-c\tclean\t\tdelete downloaded and extracted files\n"
        printf "\t-o\toutput\t\t0=none, 1=screen, 2=log\n"
        printf "\t-r\tcolor\t\tadds color to output\n"
        exit 0
      ;;
    l ) list=${OPTARG//,/ }
        unset libraries
        declare -A libraries=( 
           [hdf5]=false
           [netcdf]=false
           [netcdf-fortran]=false
           [jasper]=false
           [grib_api]=false
           [esmf]=false
           [hdf]=false
           [hdf-eos]=false
        )
        for lib in ${list}; do
           libraries[${lib}]=true
        done
      ;;
    s ) case ${OPTARG} in
          0 ) get=false; check=false; extract=false; config=false; build=false
            ;;
          1 ) get=true; check=false; extract=false; config=false; build=false
            ;;
          2 ) get=false; check=true; extract=false; config=false; build=false
            ;;
          3 ) get=false; check=false; extract=true; config=false; build=false
            ;;
          4 ) get=false; check=false; extract=false; config=true; build=false
            ;;
          5 ) get=false; check=false; extract=false; config=false; build=true
            ;;
          ? ) printf "ERROR: no step option: ${OPTARG}\n" 1>&2
              exit 1
            ;;
        esac
      ;;
    p ) compiler=${OPTARG}
      ;;
    q ) quiet='-q'
      ;;
    c ) clean=true
      ;;
    o ) case ${OPTARG} in
          0 ) output="/dev/null"
            ;;
          1 ) output="/dev/stdout"
            ;;
          2 ) output="${logdir}/getlibs.log"
              rm -f ${output}
              mkdir -p ${logdir}
            ;;
          ? ) printf "ERROR: no output option: ${OPTARG}\n" 1>&2
              exit 1
            ;;
        esac
      ;;
    r ) RED='\033[0;31m' # Red
        GRN='\033[0;32m' # Green
        YLW='\033[0;33m' # Dark Gray
        BLU='\033[0;34m' # Blue
        NCL='\033[0;00m' # No Color
      ;;
    ? ) printf "ERROR: script usage: ${usage}\n" 1>&2
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
   return 0
}

# Get File Using wget
function get_file()
{
   if [ "${get}" = false ] ; then
      return 0
   fi
   local g_fpth="$1"
   printf "${YLW}GET    :${NCL} [${g_fpth}]\n" 
   mkdir -p ${achdir}
   wget ${quiet} -P ${achdir} -nc ${g_fpth} >> ${output} 2>&1
   if [ "$?" -ne 0 ]; then return 1; fi
   return 0
}

# Check File Using md5sum
function check_file()
{
   if [ "${check}" = false ] ; then
      return 0
   fi
   local c_file="$1"
   local c_md5h="$2"
   local c_fpth="${achdir}/${c_file}"
   printf "${YLW}CHECK  :${NCL} [${c_file}]\n"
   c_md5s=`${MD5} ${c_fpth} | sed 's/ .*$//'`
   if [[ "${c_md5s}" != "${c_md5h}" ]]; then return 1; fi
   return 0
}

# Extract File Using unzip, gzip, tar
function extract_file()
{
   if [ "${extract}" = false ] ; then
      return 0
   fi
   local e_file="$1"
   local e_extn="$2"
   local e_fpth="${achdir}/${e_file}"
   printf "${YLW}EXTRACT:${NCL} [${e_file}]\n"
   mkdir -p ${srcdir}
   if [[ "${e_extn}" == "zip" ]]; then
      unzip ${quiet} -n -d ${srcdir} ${e_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${e_extn} == "tar.gz" ]]; then
      tar ${TAROPT} -C ${srcdir} -zxf ${e_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${e_extn} == "tar.Z" ]]; then
      tar ${TAROPT} -C ${srcdir} -zxf ${e_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${e_extn} == "tar" ]]; then
      tar ${TAROPT} -C ${srcdir} -xf ${e_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   else
      printf "${RED}ERROR  :${NCL} extension unknown [${e_extn}]\n" 1>&2
      return 1
   fi
   return 0
}

function config_lib()
{
   if [ "${config}" = false ]; then
      return 0
   fi
   local c_edir="$1"
   local c_copt="$2"
   printf "${YLW}CONFIG :${NCL} [${c_edir}]\n"
   cd ${srcdir}/${c_edir}
   if [ ! -x configure ]; then
      printf "${RED}WARNING:${NCL} no configure script [${c_edir}]\n" 1>&2
   else
      ./configure ${quiet} ${c_copt} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   fi
   cd ${scpdir}
   return 0
}

function build_lib()
{
   if [ "${build}" = false ] ; then
      return 0
   fi
   local b_edir="$1"
   printf "${YLW}BUILD  :${NCL} [${b_edir}]\n"
   cd ${srcdir}/${b_edir}
   make >> ${output} 2>&1
   make install >> ${output} 2>&1
   if [ "$?" -ne 0 ]; then return 1; fi
   cd ${scpdir}
   return 0
}

# Clean Downloads and Extracted Files
function clean_lib()
{
   if [ "${clean}" = false ] ; then
      return 0
   fi
   local c_file="$1"
   local c_edir="$2"
   rm -f "${achdir}/${c_file}"
   if [ "$?" -ne 0 ]; then return 1; fi
   rm -rf "${srcdir}/${c_edir}"
   if [ "$?" -ne 0 ]; then return 1; fi
   return 0
}

# Download, Check, and Extract Files
function install_lib()
{
   local i_file="$1"
   local i_extn="$2"
   local i_site="$3"
   local i_md5h="$4"
   local i_edir="$5"
   local i_copt="$6"
   if [ "${clean}" = false ] ; then
      get_file "${i_site}/${i_file}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} get failed [${i_file}]\n" 1>&2
         return 1
      fi
      check_file "${i_file}" "${i_md5h}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} check failed [${i_file}]\n" 1>&2
         return 1
      fi
      extract_file "${i_file}" "${i_extn}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} extract failed [${i_file}]\n" 1>&2
         return 1
      fi
      config_lib "${i_edir}" "${i_copt}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} config failed [${i_edir}]\n" 1>&2
         return 1
      fi
      build_lib "${i_edir}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} build failed [${i_edir}]\n" 1>&2
         return 1
      fi
   else
      clean_lib "${i_file}" "${i_edir}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} clean failed [${i_edir}]\n" 1>&2
         return 1
      fi
   fi
   printf "${GRN}SUCCESS:${NCL} [${i_edir}]\n"
}

function summary()
{
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
   exit ${ecnt}
}

# set compiler default
set_compiler

# hdf5
libr="hdf5"; vrsn="1.8.14"; extn="tar.gz"
file="hdf5-1.8.14.tar.gz"
edir="hdf5-1.8.14"
copt="--enable-fortran --prefix=${insdir}"
site="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.14/src"
md5h="a482686e733514a51cde12d6fe5c5d95"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# netcdf
libr="netcdf"; vrsn="v4.3.3.1"; extn="tar.gz"
file="v4.3.3.1.tar.gz"
edir="netcdf-c-4.3.3.1"
copt="--enable-netcdf-4 --disable-dap-remote-tests --prefix=${insdir}"
site="https://github.com/Unidata/netcdf-c/archive"
md5h="41fe6758d46cccb1675693d155ee7001"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CPPFLAGS="-I${insdir}/include"
      export LDFLAGS="-L${insdir}/lib"
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# netcdf-fortran
libr="netcdf-fortran"; vrsn="4.2"; extn="tar.gz"
file="netcdf-fortran-4.2.tar.gz"
edir="netcdf-fortran-4.2"
copt="--prefix=${insdir}"
site="ftp://ftp.unidata.ucar.edu/pub/netcdf"
md5h="cc3bf530223e8f4aff93793b9f197bf3"
if [ "${libraries[$libr]}" = true ]; then
   (
      export LD_LIBRARY_PATH="${insdir}/lib:${LD_LIBRARY_PATH}"
      export CPPFLAGS="-I${insdir}/include -DgFortran"
      export LDFLAGS="-L${insdir}/lib"
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# jasper
libr="jasper"; vrsn="1.900.1"; extn="zip"
file="jasper-1.900.1.zip"
edir="jasper-1.900.1"
copt="--enable-shared --prefix=${insdir}"
site="http://www.ece.uvic.ca/~frodo/jasper/software"
md5h="a342b2b4495b3e1394e161eb5d85d754"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# grib_api
libr="grib_api"; vrsn="1.12.3"; extn="tar.gz"
file="grib_api-1.12.3.tar.gz"
edir="grib_api-1.12.3"
copt="--with-jasper=${insdir}/lib --with-netcdf=${insdir}/lib --prefix=${insdir}"
site="https://confluence.ecmwf.int/download/attachments/3473437"
md5h="584f60702aeed70330cca42d13b96889"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# esmf
libr="esmf"; vrsn="7_1_0r"; extn="tar.gz"
file="esmf_7_1_0r_src.tar.gz"
edir="esmf"
copt=""
site="https://sourceforge.net/projects/esmf/files/ESMF_7_1_0r"
md5h="9e455bc36a0aaa9b87e0bdedc78a47f5"
if [ "${libraries[$libr]}" = true ]; then
   (
      if [[ "${compiler}" == "intel" ]]; then
         export ESMF_COMPILER="intel"
      elif [[ "${compiler}" == "gnu" ]]; then
         export ESMF_COMPILER="gfortran"
      else
         printf "${RED}ERROR  :${NCL} compiler unknown [${compiler}]\n" 1>&2
      fi
      export ESMF_DIR="${srcdir}/${edir}"
      export ESMF_INSTALL_PREFIX="${insdir}"
      export ESMF_COMM="mpi"
      export ESMF_BOPT="O"
      export ESMF_NETCDF="split"
      export ESMF_NETCDF_INCLUDE="${insdir}/include"
      export ESMF_NETCDF_LIBPATH="${insdir}/lib"
      export ESMF_INSTALL_HEADERDIR="${insdir}/include"
      export ESMF_INSTALL_MODDIR="${insdir}/mod"
      export ESMF_INSTALL_LIBDIR="${insdir}/lib"
      export ESMF_INSTALL_BINDIR="${insdir}/bin"
      export ESMF_INSTALL_DOCDIR="${insdir}/doc"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# hdf4
libr="hdf"; vrsn="4.2.11"; extn="tar.gz"
file="hdf-4.2.11.tar.gz"
edir="hdf-4.2.11"
copt="--enable-fortran --disable-netcdf --prefix=${insdir}"
site="https://support.hdfgroup.org/ftp/HDF/releases/HDF4.2.11/src"
md5h="063f9928f3a19cc21367b71c3b8bbf19"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

# hdfeos
libr="hdf-eos"; vrsn="2.19v1.00"; extn="tar.Z"
file="HDF-EOS2.19v1.00.tar.Z"
edir="hdfeos"
copt="--prefix=${insdir}"
site="ftp://edhs1.gsfc.nasa.gov/edhs/hdfeos/previous_releases"
md5h="b8648484fc78a2db7073dd603f3fb251"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CC="${insdir}/bin/h4cc -Df2cFortran"
      export FC="${insdir}/bin/h4fc"
      export F77="${insdir}/bin/h4fc"
      export F90="${insdir}/bin/h4fc"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); else scnt=$((scnt+1)); fi
fi

summary

