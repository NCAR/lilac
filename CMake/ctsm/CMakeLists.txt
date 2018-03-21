cmake_minimum_required(VERSION 2.8)

list(APPEND CMAKE_MODULE_PATH ${CIME_CMAKE_MODULE_DIRECTORY})
include(CIME_initial_setup)

project(ctsm Fortran C)

include(CIME_utils)

# cime share, mct, etc
include_directories(${CMAKE_INCLUDE_PATH})


# This definition is needed to avoid having ESMF depend on mpi
# add_definitions(-DHIDE_MPI)

# Add ctsm source directories (these add their own test directories)
add_subdirectory(${CTSM_ROOT}/src/utils clm_utils)
add_subdirectory(${CTSM_ROOT}/src/biogeochem clm_biogeochem)
add_subdirectory(${CTSM_ROOT}/src/soilbiogeochem clm_soilbiogeochem)
add_subdirectory(${CTSM_ROOT}/src/biogeophys clm_biogeophys)
add_subdirectory(${CTSM_ROOT}/src/dyn_subgrid clm_dyn_subgrid)
add_subdirectory(${CTSM_ROOT}/src/main clm_main)
add_subdirectory(${CTSM_ROOT}/src/init_interp clm_init_interp)
add_subdirectory(${CTSM_ROOT}/src/fates/main fates_main)

# Build libraries containing stuff needed for the unit tests.
# Eventually, these add_library calls should probably be distributed into the correct location, rather than being in this top-level CMakeLists.txt file.
add_library(ctsm ${clm_sources})
declare_generated_dependencies(ctsm "${clm_genf90_sources}")

# We need to look for header files here, in order to pick up shr_assert.h
include_directories(${CIMEROOT}/src/share/include)

# And we need to look for header files here, for some include files needed by
# the esmf_wrf_timemgr code
include_directories(${CIMEROOT}/src/share/esmf_wrf_timemgr)

# Tell cmake to look for libraries & mod files here, because this is where we built libraries
include_directories(${CMAKE_CURRENT_BINARY_DIR})
link_directories(${CMAKE_CURRENT_BINARY_DIR})

