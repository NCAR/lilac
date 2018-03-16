#!/usr/bin/env bash

#
# Temporary build script to prototype out what is necessary to collect
# all TPL dependancies for lilac. Eventually want to convert to
# make/cmake based system.
#
# Note that the initial version of lilac is only expected to work with
# clm. But there is desire to make it more generic and work with other
# science components. To facilitate that, we need to break up the
# dependancies into the direct lilac dependancies and the science
# component dependancies.
#

#
# Build dependancies
#
#  - gmake
#  - cmake
#  - fortran 2003 compiler

#
# lilac dependancies
#
# lilac has explicit dependancies (direct function calls) on
#   - mpi
#   - cime
#     - mct
#     - csm_share (logging, ESMF_wrf_time_manager)
#     - ?pio?
#     - ?gptl?
#
# lilac has implicit dependancies (not called directly) on:
#   - netcdf (i/o)
#   - esmf - may use real library instead of csm_share!
#
LILAC_TPLS=( mct csm_share )

#
# ctsm dependancies
#   - cime
#
#

#
# build info - eventually dynamically generate
#
MACHINE=`uname -n`
OS=`uname -s`
COMPILER=gnu
BUILD_TYPE=debug
THREADING=none
GMAKE=gmake
GMAKE_OPTS='-j 2'

#CMAKE_VERBOSE=0
CMAKE_VERBOSE=1

MPIEXEC=mpiexec
MPIFC=mpifort
MPICC=mpicc
MPIHEADER=/usr/local/Cellar/mpich/3.2.1_1/include
MPILIBS=/usr/local/Cellar/mpich/3.2.1_1/lib

FC=${MPIFC}
CC=${MPICC}

export MPICC MPIFC MPILIBS MPIHEADER CC FC
#
# define the directories we are working with
#
LILAC_ROOT=${PWD}
BUILD_DIR=${LILAC_ROOT}/_build/${MACHINE}-${COMPILER}-${BUILD_TYPE}-${THREADING}
CIME_DIR=${LILAC_ROOT}/externals/cime
INSTALL_DIR=${BUILD_DIR}/install

LILAC_CMAKE_UTIL=${LILAC_ROOT}/CMake

#
# initialize the build
#
if [ ! -d ${CIME_DIR} ]; then
    echo 'cime directory does not exist!'
    exit 1
fi

mkdir -p ${BUILD_DIR}
mkdir -p ${INSTALL_DIR}

#
# mct
#
MCT_SRC_DIR=${CIME_DIR}/src/externals/mct
MCT_BUILD_DIR=${BUILD_DIR}/mct

mkdir -p ${MCT_BUILD_DIR}
mkdir -p ${MCT_BUILD_DIR}/mct
cp ${MCT_SRC_DIR}/mct/Makefile ${MCT_BUILD_DIR}/mct/Makefile
mkdir -p ${MCT_BUILD_DIR}/mpeu
cp ${MCT_SRC_DIR}/mpeu/Makefile ${MCT_BUILD_DIR}/mpeu/Makefile

cp ${MCT_SRC_DIR}/mkinstalldirs ${MCT_BUILD_DIR}/mkinstalldirs
cp ${MCT_SRC_DIR}/install-sh ${MCT_BUILD_DIR}/install-sh


pushd ${MCT_BUILD_DIR}
echo "Installing mct..."
${MCT_SRC_DIR}/configure --prefix ${INSTALL_DIR} SRCDIR=${MCT_SRC_DIR} --enable-debugging &> mct.config.log
${GMAKE} ${GMAKE_OPTS} -f ${MCT_SRC_DIR}/Makefile subdirs &> mct.build.log
${GMAKE} ${GMAKE_OPTS} -f ${MCT_SRC_DIR}/Makefile install &> mct.install.log
echo "    Finished installing mct."
popd


#
# cime cmake based libraries
#
CIME_SRC_DIR=${CIME_DIR}/src/share/util
CIME_BUILD_DIR=${BUILD_DIR}/cime

CIME_CMAKE_MODULE_DIRECTORY=${CIME_DIR}/src/CMake

mkdir -p ${CIME_BUILD_DIR}
cp ${LILAC_CMAKE_UTIL}/Macros.cmake ${CIME_BUILD_DIR}

pushd ${CIME_BUILD_DIR}
echo "Installing cime cmake libraries."

export COMPILER OS
cmake \
    -C ${LILAC_CMAKE_UTIL}/Macros.cmake \
    -DCIMEROOT=${CIME_DIR} \
    -DCIME_CMAKE_MODULE_DIRECTORY=${CIME_CMAKE_MODULE_DIRECTORY} \
    -DCMAKE_BUILD_TYPE="CESM_DEBUG", \
    -Wdev \
    -DENABLE_PFUNIT=OFF \
    -DENABLE_GENF90=ON \
    -DCMAKE_PROGRAM_PATH=${CIME_DIR}/src/externals/genf90 \
    -DCMAKE_INCLUDE_PATH=${INSTALL_DIR}/include \
    ${LILAC_CMAKE_UTIL}/cime

if [ $? != '0' ]; then
    echo "Error running cmake for cime libraries!"
    exit
fi

make VERBOSE=${CMAKE_VERBOSE} -j 1

echo "    Finished installing cime cmake libraries."
popd
