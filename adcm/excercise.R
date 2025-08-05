# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)
load(url("https://tinyurl.com/25v8tkrh"))


# Exercise 1: Write a program to merge CM and SUPPCM dataset.

left_join(cm, suppcm, by = c("USUBJID", "CMSEQ"))

supp <- suppcm |> 
  pivot_wider(
    id_cols = c("USUBJID", "IDVARVAL"), 
    names_from = QNAM, 
    values_from = QVAL
  ) |> 
  mutate(CMSEQ = as.numeric(IDVARVAL)) |> 
  select(-IDVARVAL)

comb_cm <- left_join(cm, supp, by = c("USUBJID", "CMSEQ"))

# Exercise 2: Write a program to merge above data with Treatment groups (TRTP) and Treatment start date (TRTSDT) and Treatment end date (TRTEDT) from DM/ADSL.

dm1 <- select(dm, USUBJID, RFXSTDTC, RFXENDTC, ARM)
cm2 <- left_join(comb_cm, dm1, by = c("USUBJID")) |> 
  mutate(
    TRTP = ARM, 
    TRTSDT = as.Date(RFXSTDTC), 
    TRTEDT = as.Date(RFXENDTC)
  )

# Exercise 3: In CM dataset, impute the partial dates in CMSTDTC and CMENDTC and create respective imputation flag.

cm3 <- cm2 |> 
  mutate(
    ASTDT = case_when(
      str_length(CMSTDTC) == 4 ~ str_c(CMSTDTC, "-01-01") |> as.Date(), 
      TRUE ~ as.Date(CMSTDTC)
    ), 
    AENDT = case_when(
      str_length(CMENDTC) == 7 ~ str_c(CMENDTC, "-01") |> as.Date(), 
      CMENDTC == "" ~ NA, 
      TRUE ~ as.Date(CMENDTC)
    ), 
    
    ASTDTF = case_when(
      str_length(CMSTDTC) == 4 ~ "M",
      TRUE ~ ""
    ), 
    
    AENDTF = case_when(
      str_length(CMENDTC) == 7 ~ "D",
      TRUE ~ ""
    )
  )
  
# Exercise 4: Create a PREFL (Prior Flag) and ONTRTFL (On Treatment Flag).

cm4 <- cm3 |> 
  mutate(
   PREFL = if_else(ASTDT < TRTSDT, "Y", ""), 
   ONTRTFL = if_else(ASTDT >= TRTSDT & ASTDT <= TRTEDT, "Y", ""), 
   FUPFL = if_else(ASTDT > TRTEDT, "Y", "")
  )


cm5 <- cm4 |> 
  mutate(
    ASTDY = CMSTDY, 
    AENDY = CMENDY, 
  )


# Exercise 5: Write a full program to generate ADCM dataset.





