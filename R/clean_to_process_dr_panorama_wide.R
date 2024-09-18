clean_to_process_dr_panorama_wide <- function(district,period,raw_file_xwalk) {
  # print(raw_file_xwalk$CORE_Value)
  if(staticValues$clean_files$dr_panorama_wide_survey %in% raw_file_xwalk$CORE_Value){

    writeLines("\n Calling DR Panorama Wide - clean_to_process")

    dr_panorama_wide_df <- open_file(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_clean,
      file_type = staticValues$clean_files$dr_panorama_wide_survey
    )

    # names(dr_panorama_wide_df)
    # print(dr_panorama_wide_df)

    ################ TO DO: clean 06_roster_panorama to work here ################
    dr_panorama_wide_df <- dr_panorama_wide_df %>%
      mutate(timestamp_utc = gsubfn('.',list('T' = ' '), timestamp_utc),
             timestamp_utc = str_remove_all(timestamp_utc, "\\+00:00")) %>%
      filter(timestamp_utc >= staticValues$survey_window$start)

    dr_panorama_wide_surveyed <- dr_panorama_wide_df %>%
      select(local_id,cds,timestamp_utc)

    # print(head(dr_panorama_wide_surveyed))

    dr_panorama_wide_sheet <- paste0('https://docs.google.com/spreadsheets/d/', staticValues$raw_results_sheet[[district]], '/edit?usp=sharing')

    range_write(dr_panorama_wide_sheet,dr_panorama_wide_surveyed,col_names = FALSE,range = "a2")

    ################################################################################

    dr_panorama_wide_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
    )
    saveOrArchive(dr_panorama_wide_df,dr_panorama_wide_df_process_path)
  }
}
