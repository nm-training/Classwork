# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.


library(tidyverse)
# load(url("https://bit.ly/4596rqZ"))
load(url("https://tinyurl.com/2m36ce2s"))

# study day
cal_days <- function(date, ref_date) {
  case_when(
    is.na(date) | is.na(ref_date) ~ NA_integer_, # Handle missing dates
    as.Date(date) < as.Date(ref_date) ~ as.numeric(as.Date(date) - as.Date(ref_date)),
    TRUE ~ as.numeric(as.Date(date) - as.Date(ref_date)) + 1L
  )
}


# Exercise 1: Use raw VS datsets and append them to create:
# USUBJID, VSTESTCD, VSTEST, VSORRES, VSSPID, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM.

vs1 <- raw_vs |>
  pivot_longer(
    cols = c(VSHR, VSRESP, VSSYS, VSDIA, VSTEMP),
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |>
  mutate(
    STUDYID = "TRA-025",
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"),
    VSTESTCD = case_when(
      VSTESTCD == "VSHR" ~ "PULSE",
      VSTESTCD == "VSRESP" ~ "RESP",
      VSTESTCD == "VSSYS" ~ "SYSBP",
      VSTESTCD == "VSDIA" ~ "DIABP",
      VSTESTCD == "VSTEMP" ~ "TEMP",
    ),
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate",
      VSTESTCD == "RESP" ~ "Respiration",
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure",
      VSTESTCD == "TEMP" ~ "Temperature",
    ),
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm",
      VSTESTCD == "RESP" ~ "breaths/min)",
      VSTESTCD == "SYSBP" ~ "mmHg",
      VSTESTCD == "DIABP" ~ "mmHg",
      VSTESTCD == "TEMP" ~ "C",
    ),
    VSDTC = as.character(VSDT),
    VISIT = str_to_title(FOLDERNAME),
    VISITNUM = if_else(VISIT == "Screening", 1, 7),
  ) |>
  select(STUDYID, USUBJID, VSTESTCD, VSTEST, VSORRES, VSDTC, VISIT, VISITNUM)

vs2 <- raw_vs1 |>
  pivot_longer(
    cols = c(VS1HR, VS1RESP, VS1SYS, VS1DIA, VS1TEMP),
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |>
  mutate(
    STUDYID = "TRA-025",
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"),
    VSTESTCD = case_when(
      VSTESTCD == "VS1HR" ~ "PULSE",
      VSTESTCD == "VS1RESP" ~ "RESP",
      VSTESTCD == "VS1SYS" ~ "SYSBP",
      VSTESTCD == "VS1DIA" ~ "DIABP",
      VSTESTCD == "VS1TEMP" ~ "TEMP",
    ),
    VSTEST = case_when(
      VSTESTCD == "PULSE" ~ "Pulse Rate",
      VSTESTCD == "RESP" ~ "Respiration",
      VSTESTCD == "SYSBP" ~ "Systolic Blood Pressure",
      VSTESTCD == "DIABP" ~ "Diastolic Blood Pressure",
      VSTESTCD == "TEMP" ~ "Temperature",
    ),
    VSORRESU = case_when(
      VSTESTCD == "PULSE" ~ "bpm",
      VSTESTCD == "RESP" ~ "breaths/min)",
      VSTESTCD == "SYSBP" ~ "mmHg",
      VSTESTCD == "DIABP" ~ "mmHg",
      VSTESTCD == "TEMP" ~ "C",
    ),
    VSDTC = if_else(!is.na(VS1DT) & VS1TM != "", str_c(VS1DT, VS1TM, sep = "T"), as.character(VS1DT)),
    VSTPT = VS1TPT,
    VSTPTNUM = VS1TPT_STD,
    VISIT = str_to_title(FOLDERNAME),
    VISITNUM = case_when(
      VISIT == "Day 1" ~ 2,
      VISIT == "Day 2" ~ 3,
      VISIT == "Day 3" ~ 4,
      VISIT == "Day 4" ~ 5,
      VISIT == "Day 5" ~ 6
    ),
  ) |>
  select(STUDYID, USUBJID, VSTESTCD, VSTEST, VSORRES, VSDTC, VISIT, VISITNUM, VSTPT, VSTPTNUM) |>
  arrange(STUDYID, USUBJID, VSTESTCD, VSDTC, VISIT, VSTPTNUM)


vs3 <- raw_vs2 |>
  pivot_longer(
    cols = c(VS2HGT, VS2WGT),
    names_to = "VSTESTCD",
    values_to = "VSORRES"
  ) |>
  mutate(
    STUDYID = "TRA-025",
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"),
    VSTESTCD = case_when(
      VSTESTCD == "VS2HGT" ~ "HEIGHT",
      VSTESTCD == "VS2WGT" ~ "WEIGHT"
    ),
    VSORRESU = case_when(
      VSTESTCD == "HEIGHT" ~ "cm",
      VSTESTCD == "WEIGHT" ~ "kg"
    ),
    VSTEST = str_to_title(VSTESTCD),
    VSDTC = as.character(VS2DT),
    VISIT = str_to_title(FOLDERNAME),
    VISITNUM = 1
  ) |>
  select(STUDYID, USUBJID, VSTESTCD, VSTEST, VSORRES, VSDTC, VISIT, VISITNUM)

vs4 <- bind_rows(
  vs1, vs2, vs3
)

# Exercise 2: Write the derivation of VSBLFL (Baseline Flag) using the datasets form exercise 1.


vs_dm <- vs4 |>
  left_join(
    select(dm, USUBJID, RFSTDTC),
    by = "USUBJID"
  )

blfl <- vs_dm |> 
  filter(
    !is.na(VSORRES) & VSORRES != "",
    !is.na(VSDTC) & VSDTC != "",
    VSDTC <= RFSTDTC
  ) |> 
  group_by(USUBJID, VSTESTCD) |>
  filter(VISITNUM == max(VISITNUM)) |>
  mutate(
    VSBLFL = "Y"
  ) |> 
  ungroup() |>
  select(USUBJID, VSDTC, VSTESTCD, VISITNUM, VSBLFL)

# Exercise 3: Derive VSSTAT using the dataset.

stats <- vs4 |> 
  mutate(VSSTAT = if_else( is.na(VSORRES), "NOT DONE", ""  ))

# Exercise 4: Derive VSSEQ and study day variable.


vs5 <- vs4 |> 
  mutate(VSSTAT = if_else( is.na(VSORRES), "NOT DONE", ""  )) |> 
  group_by(USUBJID) |> 
  arrange(STUDYID, USUBJID, VSTESTCD, VSDTC, VISIT, VSTPTNUM) |> 
  mutate(
    VSSEQ = row_number()
  ) |> 
  ungroup()

vs6 <- vs5 |> 
  left_join(blfl, by = c("USUBJID", "VSDTC", "VSTESTCD", "VISITNUM")) |> 
  left_join(
    select(dm, USUBJID, RFSTDTC), 
    by = "USUBJID") |>
  mutate(
    VSDY = cal_days(VSDTC, RFSTDTC),
    DOMAIN = "VS"
  ) |> 
  select(-RFSTDTC)





