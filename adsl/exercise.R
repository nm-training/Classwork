# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)

load(url("https://tinyurl.com/2dzvyvd2"))


# Exercise 1: Write a program to merge: DM and SUPPDM

supp <- suppdm |> 
  pivot_wider(
    id_cols = "USUBJID", 
    names_from = QNAM, 
    values_from = QVAL
  )

comb_dm <- dm |> left_join(supp, by = "USUBJID")

# Exercise 2: Write a program to generate Numeric Variables for Sex, Race, Ethnic (ASEX/ARACE/AETHNIC) variable.

sex_fmt <- c(
  "M" = "Male", 
  "F" = "Female"
)

race_fmt <- c(
  "AMERICAN INDIAN OR ALASKA NATIVE" =  "American Indian or Alaska Native", 
  "ASIAN"   = "Asian"   , 
  "BLACK OR AFRICAN AMERICAN"   =  "Black or African American"    , 
  "WHITE" = "White"       , 
  "NOT REPORTED" = "Other"
)

ethnic_fmt <- c(
  "UNKNOWN" = "Unknown", 
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino"
)

dm2 <- comb_dm |> 
  mutate(
    ASEX = sex_fmt[SEX], 
    ARACE = race_fmt[RACE], 
    AETHNIC = ethnic_fmt[ETHNIC], 
    ASEXN = factor(ASEX, levels = unname(sex_fmt)) |> as.numeric(), 
    ARACEN = factor(ARACE, levels = unname(race_fmt)) |> as.numeric(), 
    AETHNICN = factor(AETHNIC, levels = unname(ethnic_fmt)) |> as.numeric(), 
  ) 

# Exercise 3: Write a program to generate population flags (SAFFL, ITTFL, ENRFL)

dm3 <- dm2 |> 
  mutate(
    SAFFL = "Y", 
    ITTFL = "Y", 
    ENRFL = "Y"
  )



ds1 <- ds |> 
  filter(DSSCAT == "END OF STUDY", 
         DSDECOD == "COMPLETED") |> 
  mutate(
    COMPLFL = "Y"
  ) |> 
  select(USUBJID, COMPLFL)


ds2 <- ds |> 
  filter(
         DSDECOD == "RANDOMIZED") |> 
  mutate(
    RANDFL = "Y"
  ) |> 
  select(USUBJID, RANDFL)


dm4 <- dm3 |> left_join(ds1, by = "USUBJID") |>  left_join(ds2, by = "USUBJID")

           
# Exercise 4: Write a program to generate Treatment Variables for planned treatment (TRTP) and actual treatment (TRTA). (Similar to above Exercise 2 of ADCM)





# Exercise 5: Write a program to generate Dates / time such as treatment start date / time (TRTSDT, TRTSDTM) and treatment end date / time (TRTEDT, TRTENTM). (Keep dates in numeric date9 format and e8601dt for datetime format) 

dm5 <- dm3 |> 
  mutate(
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC), 
    TRTSDTM = as.POSIXct(RFXSTDTC, format = "%Y-%m-%dT%H:%M") , 
    TRTENTM = as.POSIXct(RFXENDTC, format = "%Y-%m-%dT%H:%M") 
  ) |> 
  mutate(
    TRTSDTM2 = as.POSIXct(str_c(TRTEDT, "00:00")), 
    TRTENTM2 = as.POSIXct(str_c(TRTEDT, "23:59")), 
    TRTSDTMF = "H",
    TRTENTMF = "S"
  )


# Exercise 6: Write a program to generate ADSL dataset.


