# Note that files just used for unit testing should be added to the
# initial list. All remaining files need to be added in the if not
# enable_pfunit block.

set(genf90_files
  ncdio_pio.F90.in
  )

process_genf90_source_list("${genf90_files}" ${CMAKE_CURRENT_BINARY_DIR} clm_genf90_sources)

sourcelist_to_parent(clm_genf90_sources)

list(APPEND clm_sources "${clm_genf90_sources}")

list(APPEND clm_sources
  ColumnType.F90
  pftconMod.F90
  FuncPedotransferMod.F90
  GridcellType.F90
  LandunitType.F90
  PatchType.F90
  TopoMod.F90
  abortutils.F90
  atm2lndMod.F90
  atm2lndType.F90
  accumulMod.F90
  clm_varcon.F90
  clm_varctl.F90
  clm_varpar.F90
  clm_varsur.F90 
  column_varcon.F90
  decompMod.F90
  filterColMod.F90
  glc2lndMod.F90
  glcBehaviorMod.F90
  initSubgridMod.F90
  landunit_varcon.F90
  lnd2glcMod.F90
  ncdio_utils.F90
  organicFileMod.F90
  subgridAveMod.F90
  subgridWeightsMod.F90
  surfrdUtilsMod.F90
  )

if(NOT ENABLE_PFUNIT)
  list(APPEND clm_sources
    GetGlobalValuesMod.F90
     )
endif()

sourcelist_to_parent(clm_sources)
