# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)


load(url("https://tinyurl.com/5y4zxwcc"))


# Exercise 1: Write a program to merge VS and ADSL

advs1 <- vs |> 
  left_join(adsl, by = c("USUBJID", "STUDYID"))


# Exercise 2: Derive dates if necessary: ADT, ADY, TRTSDT, TRTEDT

advs2 <- advs1 |> 
  mutate(
    ADT = as.Date(VSDTC), 
    ADY = VSDY
  )

# param_fmt <- c(
#   "Diastolic Blood Pressure (mmHg)" = 1, 
#   "Diastolic Blood Pressure (mmHg)" = 2
# )


# Exercise 3: Create Parameter: PARAM, PARAMCD, PARAMN, Parameter Category: PARCAT1, PARCAT1N

advs3 <- advs2 |> 
  mutate(
    PARAM = if_else(VSSTAT != "NOT DONE", str_c(VSTEST, " (", VSSTRESU, ")"), VSTEST) ,
    PARAMCD = VSTESTCD, 
    PARAMN = factor(VSTEST) |> as.numeric(), 
    PARCAT1 = if_else(PARAMCD %in% c("HEIGHT", "WEIGHT"), "Subject Characteristic", "Vital Sign"), 
    PARCAT1N = factor(PARCAT1) |> as.numeric(), 
  ) 


# Exercise 4: Derive Results - Analysis Value: AVAL, AVALC

advs4 <- advs3 |> 
  mutate(
    AVAL = VSSTRESN, 
    AVALC = VSSTRESC
  )

# Exercise 5: Derive Timing Variables - Analysis Visit: AVISIT

advs5 <- advs4 |> 
  mutate(
    AVISIT = VISIT, 
    AVISITN = VISITNUM, 
    ATPT = VSTPT, 
    ATPTN = VSTPTNUM
    
  ) 


# Exercise 6: Derive Timing Flag Variables (ONTRTFL)

advs6 <- advs5 |> 
  mutate(
    ONTRTFL = if_else(
      TRTSDT <= ADT & ADT <= TRTEDT , "Y", ""
    )
  ) 



# Exercise 7: Derive Baseline (BASETYPE, ABLFL, BASE)

advs7 <- advs6 |> 
  mutate(
    
    BASETYPE = case_when(
      ATPTN == 1 ~ "LAST: Pre-infusion", 
      ATPTN == 2 ~ "LAST: Post-infusion", 
      TRUE ~ "LAST"
      
    ), 
    ABLFL = VSBLFL
  ) |> 
  group_by(USUBJID, PARAMCD) |> 
  mutate(
    BASE = if_else(ABLFL == "Y", AVAL, NA)
  ) |> 
  fill(BASE, .direction = "updown") |>
  ungroup() 


# Exercise 8: Derive Change from Baseline (CHG) and Percent Change from Baseline (PCHG)

advs8 <- advs7 |> 
  mutate(
    CHG = AVAL - BASE, 
    PCHG = (CHG / BASE) * 100
  ) 


# Exercise 9: Analysis Variable flag (ANLxxFL)

advs9 <- advs8 |> 
  mutate(
    ANL01FL = if_else(
      ADT >= as.Date(RFICDTC), "Y", ""
    ), 
    ANL02FL = if_else(
      ADT >= TRTSDT & ABLFL != "Y", "Y", ""
    )
  ) 


# Exercise 10:  Assign Treatment if necessary (TRTA, TRTP) and ASEQ

advs10 <- advs9 |> 
  mutate(
    ASEQ = VSSEQ
  )



# Exercise 11: Write a program to create ADVS dataset.




