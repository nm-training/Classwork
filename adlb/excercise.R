# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)
library(admiral)

# install.packages("admiral")


load(url("https://tinyurl.com/3bc8dcjv"))

# Exercise 1: Write a program to merge LB and ADSL

adlb1 <- lb |> 
  left_join(adsl, by = c("USUBJID", "STUDYID"))


# Exercise 2: Derive dates and sequence variable: 
# ADT, ADY, ASEQ

adlb2 <- adlb1 |> 
  mutate(
    ADT = as.Date(LBDTC), 
    ADY = LBDY, 
    ASEQ = LBSEQ
  )


# Exercise 3: Create Parameter: PARAM, PARAMCD, PARAMN, Parameter Category: PARCAT1, PARCAT1N, 

adlb3 <- adlb2 |> 
  mutate(
    PARAM = if_else(LBSTAT != "NOT DONE" & LBORRESU != "",  str_c(LBTEST, " (", LBSTRESU, ")"), LBTEST), 
    PARAMCD = LBTESTCD, 
    PARAMN = factor(PARAM) |> as.numeric(), 
    PARCAT1 = LBCAT, 
    PARCAT1N = factor(PARCAT1) |> as.numeric()
  )


# Exercise 4: Derive Results - Analysis Value: AVAL, AVALC, and ANRLO, ANRHI, ANRIND.

adlb4 <- adlb3 |> 
  mutate(
    AVAL = LBSTRESN, 
    AVALC = LBSTRESC, 
    ANRLO = LBSTNRLO, 
    ANRHI = LBSTNRHI, 
    ANRIND = LBNRIND
  )

# Exercise 5: Derive Timing Variables - Analysis Visit: AVISIT

adlb5 <- adlb4 |> 
  mutate(
    AVISIT = VISIT,
    AVISITN = VISITNUM
  )

# Exercise 6: Derive Timing Flag Variables (ONTRTFL)

adlb6 <- adlb5 |> 
  mutate(
    ONTRTFL = if_else(
      TRTSDT <= ADT & ADT <= TRTEDT, "Y", ""
    )
  )

# Exercise 7: Derive Baseline (ABLFL, BASE, BNRIND)

adlb7 <- adlb6 |> 
  mutate(
    ABLFL = LBBLFL, 
    BASE = if_else( ABLFL == "Y", AVAL, NA), 
    BNRIND = if_else(ABLFL == "Y", ANRIND, NA)
  ) |> 
  group_by(USUBJID, PARAMCD) |> 
  fill(BASE, .direction = "updown") |> 
  fill(BNRIND, .direction = "updown") |> 
  ungroup()

# Exercise 8: Analysis Variable flag (ANLxxFL) 




# Exercise 9: Derive Change from Baseline (CHG) and Percent Change from Baseline (PCHG), 





# Exercise 10: Creating the Lab Grade

adlb8 <- convert_blanks_to_na(adlb7) |> 
  select(-c(PARAMCD, PARAM, PARAMN))

param_lookup <- tribble(
  ~LBTESTCD, ~PARAMCD,  ~PARAM,                                             ~PARAMN,
  "ALP",     "ALP",   "Alkaline Phosphatase (mg/dL)",                       1,
  "ALT",     "ALT",     "Alanine Aminotransferase (mg/dL)",                   2,
  "AST",     "AST",     "Aspartate Aminotransferase (mmol/L)",                 3,
  "BILI",    "BILI",    "Bilirubin (mg/dL)",                               4,
  "CREAT",   "CREAT",    "Creatinine (mg/dL)",                             5,
  "PH",      "PH",      "pH",                                               6,
  "SODIUM",  "SODIUM",  "Sodium (mmol/L)",                                  7,
  "WBC",     "WBC",     "Leukocytes (10^9/L)",                              8
)

adlb9 <- adlb8 |> 
  derive_vars_merged_lookup(
    dataset_add = param_lookup, 
    by_vars = exprs(LBTESTCD), 
    new_vars = exprs(PARAMCD, PARAM, PARAMN)
  )

grade_lookup <- tibble::tribble(
  ~PARAMCD, ~ATOXDSCL,                    ~ATOXDSCH,
  "ALP",  NA_character_,                "Alkaline phosphatase increased",
  "ALT",    NA_character_,                "Alanine aminotransferase increased",
  "AST",    NA_character_,                "Aspartate aminotransferase increased",
  "BILI",   NA_character_,                "Blood bilirubin increased",
  "CREAT",  NA_character_,                "Creatinine increased",
  "PH",        "Acidosis",                "Alkalosis",
  "SODIUM", "Hyponatremia",               "Hypernatremia",
  "WBC",    "White blood cell decreased", "Leukocytosis",
)

library(admiral)


adlb10 <- adlb9 |> 
  derive_vars_merged(
    dataset_add = grade_lookup, 
    by_vars = exprs(PARAMCD)
  )

adlb11 <- adlb10 |> 
  derive_var_atoxgr_dir(
    new_var = ATOXGRL, 
    tox_description_var = ATOXDSCL,
    meta_criteria = atoxgr_criteria_ctcv5, 
    criteria_direction = "L", 
    abnormal_indicator = "LOW", 
    get_unit_expr = extract_unit(PARAM)
  ) |> 
  derive_var_atoxgr_dir(
    new_var = ATOXGRH, 
    tox_description_var = ATOXDSCH,
    meta_criteria = atoxgr_criteria_ctcv5, 
    criteria_direction = "H", 
    abnormal_indicator = "HIGH", 
    get_unit_expr = extract_unit(PARAM)
  )

adlb12 <- adlb11 |> 
  group_by(USUBJID, PARAMCD) |> 
  mutate(
    BTOXGRL = if_else(ABLFL == "Y", ATOXGRL, NA), 
    BTOXGRH = if_else(ABLFL == "Y", ATOXGRH, NA), 
  ) |> 
  fill(BTOXGRL, .direction = "updown") |> 
  fill(BTOXGRH, .direction = "updown") |> 
  ungroup()

adlb13 <- adlb12 |> 
  mutate(
    ATOXGRHN = ATOXGRH, 
    ATOXGRLN = ATOXGRL, 
    BTOXGRHN = BTOXGRH, 
    BTOXGRLN = BTOXGRL, 
    ATOXGRH = str_c("Grade ", ATOXGRH), 
    ATOXGRL = str_c("Grade ", ATOXGRL), 
    BTOXGRL = str_c("Grade ", BTOXGRL), 
    BTOXGRH = str_c("Grade ", BTOXGRH),
    
    WTOXGRL = if_else(is.na(ABLFL), "Y", NA),
    WTOXGRH = if_else(is.na(ABLFL), "Y", NA)
  )





































