# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.


library(tidyverse)

load(url("https://bit.ly/3TZT7Ae"))


# Exercise 1: Write a program to map proper formats for AESTDTC and AEENDTC from raw AE dataset. 

raw_ae |> 
  mutate(
    AESTDTC = str_c(dmy(AESTDT_RAW), "T", AESTTM),
    AEENDTC = dmy(AEENDT_RAW) |> as.character(), 
    AEST = str_c(as.Date(AESTDT_RAW, format = "%d %b %Y"), "T", AESTTM)
    # AESTDTC = str_c(AESTDT_YYYY, AESTDT_MM, AESTDT_DD, sep="-") |> ymd() |> as.character()
  ) |> pull(AEST) |> unique()



# Exercise 2: Write a program to create Unique Subject Identifier variable and Sequence variable. 




# Exercise 3: Write a program to calculate Study Day variable. (AESTDTC, AEENDTC) 




# Exercise 4: Write a program to calculate Adverse Event Treatment Emergent (AETRTEM).

raw_ae |> 
  mutate(
    AETRTEM = if_else(AESTDY >= 1, "Y", "N")
  )


# Exercise 5: See the Mapping specs to get required information to create AE and SUPPAE datasets.


# Exercise 6: After that, validate the AE & SUPPAE datasets with provided dataset (upon request).
