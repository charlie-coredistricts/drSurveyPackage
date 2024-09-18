clean_to_process_dr_panorama_long <- function(district,period,raw_file_xwalk) {
  # print(raw_file_xwalk$CORE_Value)
  if(staticValues$clean_files$dr_panorama_long_survey %in% raw_file_xwalk$CORE_Value){

    writeLines("\n Calling DR Panorama Long - clean_to_process")

    dr_panorama_long_df <- open_file(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_clean,
      file_type = staticValues$clean_files$dr_panorama_long_survey
    )

    # print(dr_panorama_long_df)

    ################ TO DO: clean 06_roster_panorama to work here ################
    dr_panorama_long_formatted <- cleanPanoramaLong(dr_panorama_long_df)

    dr_panorama_long_surveyed <- dr_panorama_long_formatted %>%
      select(local_id,cds,timestamp_utc)

    dr_panorama_long_sheet <- paste0('https://docs.google.com/spreadsheets/d/', staticValues$raw_results_sheet[[district]], '/edit?usp=sharing')

    range_write(dr_panorama_long_sheet,dr_panorama_long_surveyed,col_names = FALSE,range = "a2")

    # lbusd_surveyed <- pano_all %>%
    #   filter(district_name == "Long Beach Unified") %>%
    #   select(local_id,cds,timestamp_utc)
    #
    # sausd_surveyed <- pano_all %>%
    #   filter(district_name == "Santa Ana Unified") %>%
    #   select(local_id,cds,timestamp_utc)
    #
    # # already updated for Fall 2024
    # # lbusd_sheet <- 'https://docs.google.com/spreadsheets/d/1lEbvTCZqruNCy53jufvM6q1-Owf3UUM14XArZeM_Lmw/edit?usp=sharing'
    # # sausd_sheet <- 'https://docs.google.com/spreadsheets/d/1B4cPN50ozlyvw2Vd9N16fcseuqeozzmZhhz3XCXFmls/edit?usp=sharing'
    #
    # lbusd_sheet <- paste0('https://docs.google.com/spreadsheets/d/', staticValues$raw_results_sheet[["long_beach_unified"]], '/edit?usp=sharing')
    # sausd_sheet <- paste0('https://docs.google.com/spreadsheets/d/', staticValues$raw_results_sheet[["santa_ana_unified"]], '/edit?usp=sharing')
    #
    # range_write(lbusd_sheet,lbusd_surveyed,col_names = FALSE,range = "a2")
    # range_write(sausd_sheet,sausd_surveyed,col_names = FALSE,range = "a2")

    ################################################################################

    dr_panorama_long_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
     ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
   )
    # print(dr_panorama_long_df_process_path)
    # final path that goes to the process folder - the idea is that for the QC to work, the final file isn't specific to a survey format
    # final survey df output that in every data source/district/school needs to look the same
    #

    # this goes to dev > district > dr_fall > clean
   saveOrArchive(dr_panorama_long_formatted,dr_panorama_long_df_process_path)
  }
}
