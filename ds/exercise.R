# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

load(url("https://bit.ly/4faKUTm"))




# study day
cal_days <- function(date, ref_date) {
  case_when(
    is.na(date) | is.na(ref_date) ~ NA_integer_, # Handle missing dates
    as.Date(date) < as.Date(ref_date) ~ as.numeric(as.Date(date) - as.Date(ref_date)),
    TRUE ~ as.numeric(as.Date(date) - as.Date(ref_date)) + 1L
  )
}

# Exercise 1. Use the RAW_VISIT dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

visit <- raw_visit |> 
  filter(FOLDER == "SCN") |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "SCREENING", 
    DSDECOD = "SCREENING", 
    DSCAT = "DISPOSITION EVENT", 
    EPOCH = "SCREENING", 
    DSSTDTC = as.character(VISDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)


# Exercise 2. Use the RAW_IE dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

scrnfl <- raw_ie |> 
  filter(IESFYN != "No") |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "SCREEN FAILURE", 
    DSDECOD = "SCREEN FAILURE", 
    DSCAT = "DISPOSITION EVENT", 
    EPOCH = "SCREENING", 
    DSSTDTC = as.character(IESFDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

ic <- raw_ie |> 
  filter(IESFYN == "No") |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "INFORMED CONSENT OBTAINED", 
    DSDECOD = "INFORMED CONSENT OBTAINED", 
    DSCAT = "PROTOCOL MILESTONE", 
    EPOCH = "SCREENING", 
    DSSTDTC = as.character(DSSTDT)
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

# Exercise 3. Use the RAW_RAND dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC. 

rand <- raw_rand |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "RANDOMIZED", 
    DSDECOD = "RANDOMIZED", 
    DSCAT = "PROTOCOL MILESTONE", 
    EPOCH = "SCREENING", 
    DSSTDTC = str_c(RNDT, RNTM, sep = "T")
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, EPOCH, DSSTDTC)

# Exercise 4. Use the RAW_EX dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

eot <- raw_ex |> 
  summarise(
    last_ex = str_c(EXDT, "T", EXENTM) |> max(),  .by = SUBJECT
  ) |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "END OF TREATMENT", 
    EPOCH = "TREATMENT", 
    DSSTDTC = last_ex
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)


# Exercise 5. Use the RAW_DS dataset to derive USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC.

eos <- raw_ds |> 
  mutate(
    USUBJID = str_c("TRA-025", SUBJECT, sep = "-"), 
    DSTERM = "COMPLETED", 
    DSDECOD = "COMPLETED", 
    DSCAT = "DISPOSITION EVENT", 
    DSSCAT = "END OF STUDY", 
    EPOCH = "FOLLOW-UP", 
    DSSTDTC = as.character(DSENDT) 
  ) |> 
  select(USUBJID, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC)



# Exercise 6. Append all the datasets to create ds dataset.

ds_data <- bind_rows(
  visit, scrnfl, ic, rand, eot, eos
) |> 
  mutate(STUDYID = "TRA-025") |>
  arrange(STUDYID, USUBJID, DSSTDTC, DSCAT, DSDECOD) |>
  group_by(USUBJID) |>
  mutate(DSSEQ = row_number()) |>
  ungroup()


ds2 <- ds_data |> 
  left_join(dm, by = "USUBJID") |>
  mutate(
    DSSTDY = cal_days(DSSTDTC, RFSTDTC),
    DOMAIN = "DS"
  ) |> 
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    DSSEQ,
    DSTERM,
    DSDECOD,
    DSCAT,
    DSSCAT,
    EPOCH,
    DSSTDTC,
    DSSTDY
  )











