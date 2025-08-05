# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

# load
library(tidyverse)

load(url("https://bit.ly/4kUqn6V"))


# Exercise 1: Write a program to get screening failures (SCFAIL) from raw Inclusion/Exclusion (ie) dataset.

SCFAIL <- raw_ie |> 
  filter(IESFYN == "Yes")

# Exercise 2: Write a program to get first and last exposure date of each subject from raw Exposure (ex) dataset.

raw_ex |>
  mutate(
    first = str_c(EXDT, "T", EXSTTM), 
    last = str_c(EXDT, "T", EXENTM)
  ) |> 
  summarise(
    first_ex = min(first), 
    last_ex = max(last), 
    .by = SUBJECT
  ) |> select(SUBJECT, first_ex, last_ex)

# Exercise 3: Write a program to get death date using raw / Death details (DD) dataset.

raw_dd |> 
  mutate(DTHDT = dthdt, 
         DTHFL = ""
  )

# Exercise 4: Write a program to get the End of participation date (RFPENDTC) from raw Disposition (DS) Dataset. 

raw_ds |> 
  mutate(RFPENDTC = DSENDT)

# Exercise 5: Write a program to derive race information (as per SDTM CT) from raw demographic (DM) dataset.

raw_dm |>
  mutate(
    RACE = case_when( 
      RACE1 == 1 ~ "American Indian Alaksa Native",
      RACE2 == 1 ~ "Asian",
      RACE3 == 1 ~ "Black or African American",
      RACE4 == 1 ~ "Native Haiwan",
      RACE5 == 1 ~ "White",
      RACE6 == 1 ~ "Not reported"
    )
  )

# Exercise 6: Write a program to create planned treatment arms (ARM/ARMCD) from randomization (rand ) dataset and Actual Treatment arms (ACTARM/ACTARMCD) from exposure (ex).

raw_rand |> 
  mutate(ARM = RNDOSE,
         ARMCD = if_else(RNDOSE == "Placebo", "PLAC", RNDOSE)) |> 
  select(SUBJECT, ARM, ARMCD)

raw_ex |> 
  mutate(ACTARM = EXTRT,
         ACTARMCD = if_else(EXTRT == "Placebo", "PLAC", EXTRT)) |> 
  select(SUBJECT, ACTARM, ACTARMCD)


# Exercise 7: Write a program to map information to DM and SUPPDM dataset (DM and SUPPDM). You can reuse the above programs to get desired output.