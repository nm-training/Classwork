# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)


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

