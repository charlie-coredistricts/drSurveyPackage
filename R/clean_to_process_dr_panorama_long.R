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

    print(dr_panorama_long_df)

    # clean here
    dr_panorama_long_df <- dr_panorama_long_df %>%
      mutate(converted_date = as.POSIXct(timestamp_utc)) %>%
      filter(converted_date > "2023-09-01")

    formatQuestions <- function(qNum) {
      if (strtoi(qNum) < 22) {
        return(paste("q",qNum,sep = ""))
      }
      else {
        return(paste("fr",qNum,sep = ""))
      }
    }

    format_pano <- function(dr_panorama_long_df){

      # dr_panorama_long_df <- dr_panorama_long_df

      dr_panorama_long_df$questions <- lapply(dr_panorama_long_df$question_order,formatQuestions)
      dr_panorama_long_df$select_one_score[is.na(dr_panorama_long_df$select_one_score)] <- ""
      dr_panorama_long_df$values <- paste(dr_panorama_long_df$free_response_text,na.omit(dr_panorama_long_df$select_one_score),sep = "")

      print(dr_panorama_long_df)
      names(dr_panorama_long_df)

      # pano_student_answered <- dr_panorama_long_df %>%
      #   filter(select_one_score != "") %>%
      #   group_by(student_id,converted_date) %>%
      #   count(student_id)

      # print(pano_student_answered)

      pano_wide_data <- dr_panorama_long_df %>%
        select(-question_id,-question_order,-question_text,-select_one_score,-free_response_text,-timestamp_utc,-converted_date) %>%
        # select(-question_order,-select_one_score,-free_response_text) %>%
        pivot_wider(names_from = questions,values_from = values) %>%
        unchop(everything())
        # left_join(pano_student_answered)

      # print(pano_wide_data)
      # names(pano_wide_data)

      return(pano_wide_data)

    }

    dr_panorama_long_formatted <- format_pano(dr_panorama_long_df)
    print("formatted:")
    print(dr_panorama_long_formatted)

    ################ TO DO: clean 06_roster_panorama to work here ################
    lbusd_surveyed <- pano_all %>%
      filter(district_name == "Long Beach Unified") %>%
      select(respondent_sis_id_value,school_names,submission_time)

    sausd_surveyed <- pano_all %>%
      filter(district_name == "Santa Ana Unified") %>%
      select(respondent_sis_id_value,school_names,submission_time)

    # already updated for Fall 2024
    lbusd_sheet <- 'https://docs.google.com/spreadsheets/d/1lEbvTCZqruNCy53jufvM6q1-Owf3UUM14XArZeM_Lmw/edit?usp=sharing'

    sausd_sheet <- 'https://docs.google.com/spreadsheets/d/1B4cPN50ozlyvw2Vd9N16fcseuqeozzmZhhz3XCXFmls/edit?usp=sharing'


    range_write(lbusd_sheet,lbusd_surveyed,col_names = FALSE,range = "a2")
    # write.csv(lbusd_surveyed,paste(path_to_save_directory,"/00_raw/02_clean_panorama/lbusd_surveyed.csv",sep=""))

    range_write(sausd_sheet,sausd_surveyed,col_names = FALSE,range = "a2")

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
