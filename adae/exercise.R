# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)
load(url("https://tinyurl.com/43fv5v79"))

# Exercise 1: Write a program to merge AE and SUPPAE dataset.

supp <- suppae |>
  pivot_wider(
    id_cols = c("USUBJID", "IDVARVAL"),
    names_from = QNAM,
    values_from = QVAL
  ) |>
  mutate(AESEQ = as.numeric(IDVARVAL)) |>
  select(-IDVARVAL) 

comb_ae <- ae |>
  left_join(supp, by = c("USUBJID", "AESEQ"))

# Exercise 2: Write a program to create flags related to study drug.
# AESEVFL, AESERFL, AEACNFL, AERELFL, TRTEMFL

ae2 <- comb_ae |> 
  mutate(
    AESERFL = if_else(AESER == "Yes", "Y", ""), 
    AESEVFL = if_else(AESEV == "SEVERE", "Y", ""), 
    AERELFL = if_else(AEREL == "RELATED", "Y", ""), 
    AEACNFL = if_else(AEACN == "DRUG WITHDRAWN", "Y", ""), 
    TRTEMFL = if_else(AETRTEM == "Y", "Y", "")
  )

# Exercise 3: Write a program to create AESTDT, AEENDT and TRTP, TRTSDT, TRTEDT from DM/ADSL.

ae3 <- ae2 |> 
  left_join(select(dm, USUBJID, ARM, RFSTDTC, RFENDTC), by = "USUBJID") |> 
  mutate(
    TRTP = ARM,
    ASTDT = as.Date(AESTDTC),
    AENDT = as.Date(AEENDTC), 
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC)
  )

# Exercise 4: Write a program to create ADAE dataset.
