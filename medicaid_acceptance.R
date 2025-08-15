#This script will seek to determine which facilities are likely to accept Medicaid/Medicare.

#It should not be used very often - once calculated a single time, it the output can probably be joined up with stored ER data

#### Libraries ------------------------------

#Uses tidyverse

#### Computations ----------------------

#Now we will look for Medicaid/Medicare as a payment form and check the % of this

#This function will take the PUDF table as its input. It is presumed that this input will be a SQL query. We will need the THCIC_ID and the FIRST_PAYMENT_SRC columns. Each row will we a patient encounter.

#percent_medmed is the percent of patients with first payment source as medicaid/medicare. We can change this as needed, but based on prior research we will say this is likely 0.02.

#I have examined the log of the percent medicaid/medicare acceptance as well log(1+Per_MedMed) for use later as a numeric variable.

#The output will be the a table of THCIC_ID, Per_MedMed, and a Boolean column of Accepts_Med.

calculate_medicaid_acceptance <- function(pudf_table, percent_medmed_cutoff = 0.02){
    
    df <- pudf_table |>
        mutate(MedMed = str_detect(FIRST_PAYMENT_SRC, 'MA|MB|MC')) |>
        group_by(THCIC_ID) |>
        summarize(Per_MedMed = sum(MedMed, na.rm = TRUE)/n()) |>
        mutate(Accepts_Med = Per_MedMed >= percent_medmed_cutoff) |>
        dplyr::select(THCIC_ID, Per_MedMed, Accepts_Med)
    
    return(df)
    
}
