cmake_minimum_required(VERSION 2.8)

#
# Based off the top level ctsm file for unit tests,
# ctsm/src/CMakeLists.txt, in git commit ec9c950
#

list(APPEND CMAKE_MODULE_PATH ${CIME_CMAKE_MODULE_DIRECTORY})
include(CIME_initial_setup)

project(lilac_ext_build Fortran C)

include(CIME_utils)

# This definition is needed to avoid having ESMF depend on mpi
# add_definitions(-DHIDE_MPI)

# mct, built externally
include_directories(${CMAKE_INCLUDE_PATH})
#link_directories(${CMAKE_CURRENT_BINARY_DIR})

# Add source directories from other share code (csm_share, etc.). This should be
# done first, so that in case of name collisions, the CLM versions take
# precedence (when there are two files with the same name, the one added later
# wins).
add_subdirectory(${CIMEROOT}/src/share/util csm_share)
add_subdirectory(${CIMEROOT}/src/share/esmf_wrf_timemgr esmf_wrf_timemgr)
add_subdirectory(${CIMEROOT}/src/drivers/mct/shr drv_share)

# Extract just the files we need from drv_share
set (drv_sources_needed_base
  glc_elevclass_mod.F90
#X#  seq_comm_mct.F90
  )
extract_sources("${drv_sources_needed_base}" "${drv_sources}" drv_sources_needed)

# Build libraries containing stuff needed for the unit tests.
# Eventually, these add_library calls should probably be distributed into the correct location, rather than being in this top-level CMakeLists.txt file.
add_library(csm_share ${share_sources} ${share_mct_sources} ${share_pio_sources} ${drv_sources_needed} ${esmf_wrf_timemgr_sources})
declare_generated_dependencies(csm_share "${share_genf90_sources}")
#X# add_library(esmf_wrf_timemgr ${esmf_wrf_timemgr_sources})
#X# add_dependencies(esmf_wrf_timemgr csm_share)
 
# We need to look for header files here, in order to pick up shr_assert.h
include_directories(${CIMEROOT}/src/share/include)

# And we need to look for header files here, for some include files needed by
# the esmf_wrf_timemgr code
include_directories(${CIMEROOT}/src/share/esmf_wrf_timemgr)

# Tell cmake to look for libraries & mod files here, because this is where we built libraries
include_directories(${CMAKE_CURRENT_BINARY_DIR})
link_directories(${CMAKE_CURRENT_BINARY_DIR})


install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ DESTINATION include
  FILES_MATCHING PATTERN "*.mod")
install(TARGETS csm_share
  ARCHIVE DESTINATION lib)
      
