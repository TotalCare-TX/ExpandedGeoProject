
# read Records 
ed_base <- readRDS("cleaned_data/ed_base.RDS")
# read Facility
ed_fac <- readRDS("cleaned_data/ed_facility.RDS")

source("helpers.R")


scaled_pdd_data <- scaled_ppd(ed_base, ed_fac)


scaled_pdd_data


