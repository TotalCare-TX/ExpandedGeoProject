# Number of days in a quarter  ---------------

days_in_quarter <- function(year, quarter) {
  start_date <- lubridate::ymd(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))
  end_date   <- start_date %m+% months(3) - days(1)
  as.integer(end_date - start_date + 1)
}


# scaled patient per day (ppd) for each FESD facility ------------------------
# this function reads in the Records data and the Facility Data from the DB and returns
# a scaled patient per day (ppd) for each FESD facility

scaled_ppd <- function(ed_base, ed_fac) { 
  
  # extract FSEDs THCICs across all quarters 
  unique_fac_fesd <- ed_fac %>% filter(FACILITY_TYPE == "FEMC") %>%  
    dplyr::pull(THCIC_ID) %>% unique()
  
  # number of patients per quarter per FSED excluding bookending quarters
  avg_ppq_2 <- ed_base |> filter(Year != 2022) |>
    filter(FIRST_PAYMENT_SRC %in% c("12", "14", "AM", "BL", "CI", "HM")) |>
    # left_join(unique_fac_fesd) |>
    filter(THCIC_ID %in% unique_fac_fesd) |>
    mutate(Year_Quarter = str_c(as.character(Year), as.character(Quarter), sep = "_")) |>
    dplyr::select(c(THCIC_ID, Year_Quarter)) |>
    group_by(Year_Quarter, THCIC_ID) |>
    summarize(n = n()) |> #Number of patients per facility per quarter
    ungroup() |>
    pivot_wider(names_from = Year_Quarter, values_from = n) |>
    pivot_longer(!THCIC_ID, names_to = "Year_Quarter", values_to = "n") |>
    mutate(Year = substr(gsub("\\D", "", Year_Quarter),1,4),
           Quarter = substr(gsub("\\D", "", Year_Quarter),5,5)) |>
    group_by(THCIC_ID) %>%
    mutate(seq_id = dplyr::row_number()) %>%
    filter(seq_id != 1) %>%
    filter(seq_id != max(seq_id)) %>% ungroup() %>%
    filter(!is.na(n))
  
  
  
  # number of days in each quarter in our data
  
  qtr_days <- tibble(Year_Quarter = unique(avg_ppq_2$Year_Quarter)) |>
    dplyr::mutate(Days = days_in_quarter(as.numeric(substr(gsub("\\D", "", Year_Quarter),1,4)), 
                                         as.numeric(substr(gsub("\\D", "", Year_Quarter),5,5))))
  
  
  # Now let's find the average daily patients for all quarters and facilities
  # Filters for facilities with all quarters present
  fsed_list_all_Qs <- avg_ppq_2 %>% group_by(THCIC_ID) %>% 
    summarise(n= n_distinct(Year_Quarter)) %>% 
    filter(n==max(n)) %>% pull(THCIC_ID) %>% unique()
  length(fsed_list_all_Qs)
  
  # average ppd for each quarter across all FSEDs(present in all quarters)
  fac_qtr_ppd <- avg_ppq_2 |>
    filter(THCIC_ID %in% fsed_list_all_Qs) |>
    left_join(qtr_days) |>
    mutate(Avg_PPD_Fac = n/Days) |>
    group_by(Year_Quarter) |>
    summarize(Avg_PPD = mean(Avg_PPD_Fac)) |>
    ungroup()
  
  # creating the scaling factor
  fac_scaling <- fac_qtr_ppd |>
    mutate(Scaling_Factor = mean(fac_qtr_ppd$Avg_PPD)/Avg_PPD) |>
    dplyr::select(c(Year_Quarter, Scaling_Factor))
  
  
  # Scaling the number of patients for each facility in each quarter, then computing the scaled ppd 
  # avg_ppq_scaled <- avg_ppq_2 %>% left_join(fac_scaling) %>% 
  #   mutate(scaled_n = n*Scaling_Factor,
  #          days = case_when(grepl("Q1", Year_Quarter, ignore.case =T) ~ ifelse(lubridate::leap_year(as.numeric(substr(gsub("\\D", "", Year_Quarter),1,4))), 91, 90),
  #                           grepl("Q2", Year_Quarter, ignore.case =T) ~ 91,
  #                           TRUE ~ 92)) %>% group_by(THCIC_ID) %>%
  #   summarise(scaled_ppd = sum(scaled_n)/sum(days))
  
  avg_ppq_scaled <- avg_ppq_2 %>% left_join(fac_scaling) %>% 
    left_join(qtr_days) %>% 
    mutate(scaled_n = n*Scaling_Factor) %>% group_by(THCIC_ID) %>%
    summarise(scaled_ppd = sum(scaled_n)/sum(days))
  
}
