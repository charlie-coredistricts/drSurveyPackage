# currently in onTrack, but eventually move to its own DR package
# helper_load_google_survey -- in loading instructions, first load googlesheets4 API to load whole thing and run through process below
# output of this will be what goes to next phase

# might want to change cleanGoogle function ex. changing column names
# use Google Sheets templates to change values

# xwalk helper that copies xwalks -- in another helper script? not to worry about this extra step right now
# possible to copy from period to period, or district to district

clean_to_process_dr_google_forms <- function(district,period,raw_file_xwalk) {
  # print(raw_file_xwalk$CORE_Value)
  if(staticValues$clean_files$dr_google_forms_survey %in% raw_file_xwalk$CORE_Value){

    writeLines("\n Calling DR Google Forms - clean_to_process")

    dr_google_forms_df <- open_file(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_clean,
      file_type = staticValues$clean_files$dr_google_forms_survey
    )

    print(dr_google_forms_df)

    ################################################################################ clean here
    # Winter 2024 Google URLs
    google_sheet_urls <- list("https://docs.google.com/spreadsheets/d/1n9x5xVoyHuPjV0jstAkxFYw79P9uC0Re15Eey_Z8OaQ/edit?resourcekey#gid=265867015", # OUSD
                              "https://docs.google.com/spreadsheets/d/1L2PNPpa_0lv0Z8aAy8QEodVV194PI1UBAWjUxtAirTk/edit?usp=sharing", # GGUSD
                              "https://docs.google.com/spreadsheets/d/12mU9dI1qdP3bsqmP8Ct9Ys2bei1yqbvcoNfindJ4fUc/edit?resourcekey#gid=1601503859", # SDUSD
                              "https://docs.google.com/spreadsheets/d/1u17BuyUkKQKLNK7ddEMcHTuK-AqrXUm1mi0oUTMqKhw/edit?resourcekey#gid=85361515" # VUSD
    )



    # Read each of the Google sheets then stack all the resulting files
    google_sheets <- lapply(google_sheet_urls,read_sheet,col_types = "c") %>%
      Reduce(f = rbind)

    # call our cleaning function and merge the results with the district email domains
    district_email_domains <- read.csv("./crosswalk/email_domain_xwalk.csv", fileEncoding = 'UTF-8-BOM')

    cleaned_google_file <- cleanGoogle(google_sheets)
    sheet <- merge(cleaned_google_file,district_email_domains)
    ################################################################################

    dr_google_forms_df_process_path <- universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      ### TO DO across all clean_to_process
      file = staticValues$data_elements$dr_survey # data_elements instead of clean_files; dr_survey should be the only clean_files name
    )
    saveOrArchive(dr_google_forms_df,dr_google_forms_df_process_path)
  }
}
