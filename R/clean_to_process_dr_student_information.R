#' DR Student Information Clean to Process
#'
#' @param district
#' @param period
#' @param raw_file_xwalk
#'
#' @return
#' @export
#'
#' @examples
clean_to_process_dr_student_information <- function(district,period) {

  writeLines("--Calling Student Information Clean to Process--")

  stu_info_audit <- open_file(
    district = district,
    period = period,
    location = staticValues$locations$egnyte_clean,
    file_type = staticValues$clean_files$dr_student_information
  )

  if ("cds" %in% names(stu_info_audit)){
    stu_info_audit <- stu_info_audit %>%
      select(-cds)
  }

  if ("school_name" %in% names(stu_info_audit)){
    stu_info_audit <- stu_info_audit %>%
      select(-school_name)
  }



  if("hispanic_indicator" %in% names(stu_info_audit)){
    if("Y" %in% (unique(stu_info_audit$hispanic_indicator))){
      stu_info_audit <- stu_info_audit %>%
        mutate(race_ethnicity_code = ifelse(hispanic_indicator == "Y","LX",race_ethnicity_code))
    }
  }


  stu_info_audit <- stu_info_audit %>%
    left_join(el_status_code_to_description,by=c("english_learner_status_code" = "code")) %>%
    rename(english_learner_status_description = description) %>%
    left_join(gender_code_to_description, by=c("gender_code" = "code")) %>%
    rename(gender_description = description) %>%
    left_join(race_eth_code_to_description, by=c("race_ethnicity_code" = "code")) %>%
    rename(race_ethnicity_description = description) %>%
    distinct()

  stu_info_audit$duplicate_record <- duplicated(select(stu_info_audit,local_id))

  # Can remove the following 3 if statements when we can remove the columns from the enumerations

  final_required_columns <- c("ssid",
                              "local_id",
                              "student_last_name",
                              "student_first_name",
                              "student_middle_name",
                              "student_date_of_birth",
                              "student_email",
                              "gender_code",
                              "gender_description",
                              "race_ethnicity_code",
                              "race_ethnicity_description",
                              "sed_status",
                              "foster_youth",
                              "unhoused_status",
                              "english_learner_status_code",
                              "english_learner_status_description",
                              "special_education_status")

  for (col in final_required_columns) {
    # Check if the column exists in the data frame
    if (!(col %in% names(stu_info_audit))) {
      # If the column does not exist, add it with a null value
      stu_info_audit[[col]] <- NA
    }
  }


  student_information_final <- stu_info_audit %>%
    filter(duplicate_record == F) %>%
    select(
      ssid,
      local_id,
      student_last_name,
      student_first_name,
      student_middle_name,
      student_date_of_birth,
      student_email,
      gender_code,
      gender_description,
      race_ethnicity_code,
      race_ethnicity_description,
      sed_status,
      foster_youth,
      unhoused_status,
      english_learner_status_code,
      english_learner_status_description,
      special_education_status
    ) %>%
    distinct()


  # }

  egnyte_output_file <-
    universal_file_path_helper(
      district = district,
      period = period,
      location = staticValues$locations$egnyte_process,
      file = staticValues$data_elements$dr_student_information
    )

  saveOrArchive(student_information_final,egnyte_output_file)

}
