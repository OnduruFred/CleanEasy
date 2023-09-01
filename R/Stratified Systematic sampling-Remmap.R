#' Stratified Systematic Sample for sampling AIM 3 subjects from women who has gone through the screening process described in Aim 1  in REMAPP
#'
#' Function allows you to sample AIM3 cohort for each trimester cross-sectional
#'
#'
#'
#'
#' @export
#' @param  data - The REMAPP data set to be operated on
#' @param  k - sample interval
#' @param enrollment_month - Enrollment month(s) to reach trimester III e.g c("May","June")
#' @param enrollment_year - Enrollment Year e.g 2023
#' @param start_date - subset the data frame to suite when to start the sampling (only the sample frame of interest)


# --------------    Stratified Systematic Sampling    ---------------------- #
#         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

strat_syst_sampling = function(data ,k = 2,enrollment_month = c("May","June"),enrollment_year = 2023,start_date = "2023-05-01"){
  #          **********************************************************************************************************
  #-------------------      Require packages & install at all not installed
  #********************************************************************************************************************
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(lubridate)){install.packages(lubridate); library(lubridate)}
  if(!require(janitor)){install.packages(janitor); library(janitor)}
  if(!require(zoo)){install.packages(zoo); library(zoo)}


  # >>>>>>>            Systematic sample algorithm        <<<<<<<<< #
  #****************************************************************************************
  syst_sampling = function(N,k){
    set.seed(1234)
    # k = floor(N/n)
    n = (N/k)
    r = sample(1:k, 1)
    seq(r, r + k*(n-1), k)
  }
  # @@@@@ NOTE: k = interval, n = sample size, r = the first participant sampled, N = Sample space(Total enrollments in the period);


  #____________________________________________________________________________________________________________________
  #                  Subset for enrollment observations for the enrolled participants only from 03-April-2023
  #                              Generate/ Mutate new variables
  #--------------------------------------------------------------------------------------------------------------------
  Prisma.dt = data %>% clean_names() %>% drop_na(momid) %>% filter(type_visit == 1) %>%
    filter(con_dsstdat >= start_date) %>% filter(us_ga_wks_age_fts1 < 14) %>%
    mutate(enroll_month = months(con_dsstdat,abbreviate = FALSE),
           enroll_year = year(con_dsstdat)) %>%
    mutate(`US GA fts1 @ Enrollment` = str_c(us_ga_wks_age_fts1," Wks & ",us_ga_days_age_fts1," Days"))



  # ******************************************************************************************************************
  #-------    Sampling for different Trimesters
  #**************************************************************************
  #*
  #*      Sample Trimester III
  #*********************************
  trim3.data_sp = Prisma.dt %>% filter(enroll_month %in%c(enrollment_month) &
                                         enroll_year %in%c(enrollment_year)) %>%
    select(momid,type_visit,con_dsstdat,entry_date,enroll_month,enroll_year,`US GA fts1 @ Enrollment`,
           us_ga_wks_age_fts1,us_ga_days_age_fts1) %>%
    mutate(intv_time.early = (30 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Early window(anc-32)` = con_dsstdat + (intv_time.early*7),
           intv_time = (32 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Actual Visit(anc-32)` = con_dsstdat + (intv_time*7),
           intv_time.late = (34 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Late window(anc-32)` = con_dsstdat + (intv_time.late*7)) %>%
    select(momid,con_dsstdat,enroll_month,enroll_year,`US GA fts1 @ Enrollment`,
           `Early window(anc-32)`,`Actual Visit(anc-32)`,`Late window(anc-32)`,entry_date) %>%
    mutate_at(vars(`Early window(anc-32)`,`Actual Visit(anc-32)`,`Late window(anc-32)`),remove_var_label) %>%
    #  >>>>>>>>>>>> ----  Sort data by enrollment date  ------ <<<<<<<<<<<<<<<<  #
    #                   """""""""""""""""""""""""""""""""
    arrange(entry_date) %>%
    mutate(id = row_number()) %>% select(id,everything(),-entry_date)

  trim3.data = trim3.data_sp[syst_sampling(N = nrow(trim3.data_sp),k = k), ] %>% mutate(`Sampled Trimester` = "Trimester III")



  #*        Sample Trimester II
  #*  ******************************
  #*
  trim2.data_sp =  Prisma.dt %>%
    #   Remove the sample space/frame for participants already sampled for Trimester 3 (using anti join sql)
    #   ------------------------------------------------------------------------------------------------------
  anti_join(trim3.data_sp %>% select(momid)) %>%
    select(momid,type_visit,con_dsstdat,entry_date,enroll_month,enroll_year,`US GA fts1 @ Enrollment`,
           us_ga_wks_age_fts1,us_ga_days_age_fts1) %>%
    mutate(intv_time.early = (18 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Early window(anc-20)` = con_dsstdat + (intv_time.early*7),
           intv_time = (20 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Actual Visit(anc-20)` = con_dsstdat + (intv_time*7),
           intv_time.late = (22 - round((us_ga_wks_age_fts1*7 + us_ga_days_age_fts1)/7,1)),
           `Late window(anc-20)` = con_dsstdat + (intv_time.late*7)) %>%
    select(momid,con_dsstdat,enroll_month,enroll_year,`US GA fts1 @ Enrollment`,
           `Early window(anc-20)`,`Actual Visit(anc-20)`,`Late window(anc-20)`,entry_date) %>%
    mutate_at(vars(`Early window(anc-20)`,`Actual Visit(anc-20)`,`Late window(anc-20)`),remove_var_label) %>%

    #@@@@@@@@     Enforce Sampling Timeline based on Trimester III timelines   @@@@@@@@@@@@#
    #            """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
    filter(`Late window(anc-20)` <= last(trim3.data_sp$`Late window(anc-32)`,na_rm = TRUE)) %>%

    #  >>>>>>>>>>>> ----  Sort data by enrollment date  ------ <<<<<<<<<<<<<<<<  #
    #                   """""""""""""""""""""""""""""""""
    arrange(entry_date) %>%
    mutate(id = row_number()) %>% select(id,everything(),-entry_date)

  trim2.data = trim2.data_sp[syst_sampling(N = nrow(trim2.data_sp),k = k), ] %>% mutate(`Sampled Trimester` = "Trimester II")


  # $$$$$$$$$$$$  Returned final Sampled data sets  $$$$$$$$$$$$$$$$
  #              ====================================
  return(trim3.data %>% bind_rows(trim2.data) %>%
           rename(`Consent Date` = con_dsstdat,
                  `Month @ Enrollment` = enroll_month,
                  `Year @ Enrollment` = enroll_year))
}


