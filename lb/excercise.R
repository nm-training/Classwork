# R Trainer: Binod Jung Bogati (linkedin.com/in/bjungbogati)
# copyright: © 2025. Unauthorized distribution or reuse prohibited.

library(tidyverse)

load(url("https://tinyurl.com/c54cyt9y"))

# Exercise 1: Transpose required values in respective columns (test results/units/high/low/flags, + high, - low and 0 normal). 

lb1 <- raw_lb1 |> 
  mutate(
    LBTESTCD = "WBC", 
    LBORRES = WBC, 
    LBORRESU = WBC_UN, 
    LBORNRLO = WBC_LOW,
    LBORNRHI = WBC_HIGH,
    LBNRIND = case_when(
      WBC_FLAG == "+" ~ "HIGH", 
      WBC_FLAG == "-" ~ "LOW", 
      WBC_FLAG == "0" ~ "NORMAL"
  ), 
    LBDTC = LBDT
  ) |> 
  select(
    SUBJECT, FOLDERNA, DATAPAGE, LBTESTCD, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBDTC, LBNRIND
  )


### Break Time Over

# result <- raw_lb2 |>
#   pivot_longer(
#     cols = c(CREAT, SODIUM, AST, ALT, BILI, ALP),
#     names_to = "LBTESTCD",
#     values_to = "LBORRES"
#   ) |> 
#   select(SUBJECT, LBTESTCD, LBORRES, LBDT, FOLDERNA, DATAPAGE)
# 
# lbunits <- raw_lb2 |>
#   pivot_longer(
#     cols = c(CREAT_UN, SODIUM_U, AST_UN, ALT_UN, BILI_UN, ALP_UN),
#     names_to = "LBTESTCD",
#     values_to = "LBORRESU"
#   ) |> 
#   mutate(LBTESTCD = str_extract(LBTESTCD, "[^_]+")) |> 
#   select(SUBJECT, LBTESTCD, LBORRESU, LBDT, FOLDERNA, DATAPAGE)
# 
# lblow <- raw_lb2 |>
#   pivot_longer(
#     cols = c(CREAT_LO, SODIUM_L, AST_LOW, ALT_LOW, BILI_LOW, ALP_LOW),
#     names_to = "LBTESTCD",
#     values_to = "LBORNRLO"
#   ) |> 
#   mutate(LBTESTCD = str_extract(LBTESTCD, "[^_]+")) |> 
#   select(SUBJECT, LBTESTCD, LBORNRLO, LBDT, FOLDERNA, DATAPAGE)
# 
# lbhigh <- raw_lb2 |>
#   pivot_longer(
#     cols = c(CREAT_HI, SODIUM_H, AST_HIGH, ALT_HIGH, BILI_HIG, ALP_HIGH),
#     names_to = "LBTESTCD",
#     values_to = "LBORNRHI"
#   ) |> 
#   mutate(LBTESTCD = str_extract(LBTESTCD, "[^_]+")) |> 
#   select(SUBJECT, LBTESTCD, LBORNRHI, LBDT, FOLDERNA, DATAPAGE)
# 
# refind <- raw_lb2 |>
#   pivot_longer(
#     cols = c(CREAT_FL, SODIUM_F, AST_FLAG, ALT_FLAG, BILI_FLA, ALP_FLAG),
#     names_to = "LBTESTCD",
#     values_to = "LBNRIND"
#   ) |> 
#   mutate(LBTESTCD = str_extract(LBTESTCD, "[^_]+"), 
#          LBNRIND = case_when(
#            LBNRIND == "+" ~ "HIGH", 
#            LBNRIND == "-" ~ "LOW", 
#            LBNRIND == "0" ~ "NORMAL"
#          )
#          ) |> 
#   select(SUBJECT, LBTESTCD, LBNRIND, LBDT, FOLDERNA, DATAPAGE)
# 
# 
# keyvar <- c("SUBJECT", "LBTESTCD", "LBDT", "FOLDERNA", "DATAPAGE")
# 
# 
# lb_dataset <- left_join(result, lbunits, by = keyvar) |> 
#   left_join(lblow, by = keyvar) |> 
#   left_join(lbhigh, by = keyvar) |>
#   left_join(refind, by = keyvar) 

# lb_data2 <- bind_cols(result, lblow, lbhigh, refind)


lb2_spec <- data.frame(
  .name = names(raw_lb2)[21:56],
  .value = c("LBORRES", "LBORRESC", "LBORRESU", "LBORNRLO", "LBORNRHI", "LBNRIND"),
  LBTESTCD = c("CREAT", "SODIUM", "AST", "ALT", "BILI", "ALP") |> rep(each = 6)
)

lb2_data <- raw_lb2 |> 
  pivot_longer_spec(
    lb2_spec
  ) |> 
  mutate(
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH", 
      LBNRIND == "-" ~ "LOW", 
      LBNRIND == "0" ~ "NORMAL"
    ), 
    LBDTC = LBDT
  ) |> 
  select(
    SUBJECT, FOLDERNA, DATAPAGE, LBTESTCD, LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBDTC, LBNRIND
  )


lb3_spec <- data.frame(
  .name = names(raw_lb3)[21:25],
  .value = c("LBORRES", "LBORRESC", "LBORNRLO", "LBORNRHI", "LBNRIND"),
  LBTESTCD = c("UPH") |> rep (each=5)
)

lb3_data <-  raw_lb3 |> 
  pivot_longer_spec(
    lb3_spec
  ) |> 
  mutate(
    LBNRIND = case_when(
      LBNRIND == "+" ~ "HIGH", 
      LBNRIND == "-" ~ "LOW", 
      LBNRIND == "0" ~ "NORMAL"
    ), 
    LBDTC = LBDT
  ) |> 
  select(
    SUBJECT, FOLDERNA, DATAPAGE, LBTESTCD, LBORRES, LBORNRLO, LBORNRHI, LBDTC, LBNRIND
  )


lb_data <- bind_rows(
  lb1, lb2_data, lb3_data
)

# Excercise 2: For Lab test (LBTEST/LBTESTCD) and Lab category (LBCAT), change it as per LOINC LB Mapping document. 

lb_data2 <- lb_data |> 
  mutate(
    LBTEST = case_when(
      LBTESTCD == "WBC" ~ "Leukocytes", 
      LBTESTCD == "CREAT" ~ "Creatinine", 
      LBTESTCD == "SODIUM" ~ "Sodium", 
      LBTESTCD == "AST" ~ "Aspartate Aminotransferase", 
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase", 
      LBTESTCD == "BILI" ~ "Total Bilirubin", 
      LBTESTCD == "ALP" ~ "Alkaline Phosphatase"
    )
    
    # LBTESTCD = case_when(
    #   LBTESTCD == "WBC" ~ "WBC", 
    #   LBTESTCD == "CREAT" ~ "CREAT", 
    #   LBTESTCD == "SODIUM" ~ "SODIUM", 
    #   LBTESTCD == "AST" ~ "AST", 
    #   LBTESTCD == "ALT" ~ "ALT", 
    #   LBTESTCD == "BILI" ~ "BILI", 
    #   LBTESTCD ==  "ALP" ~ "ALP"
    #   LBTESTCD == "UPH" ~ "UPH"
    # )
  )

# Exercise 3: Convert the original values/units (LBORRES/LBORRESU) into standard units (LBSTRESN /LBSTRESU)


lb3_data <- lb_data2  |> 
  mutate(
    LBSTRESN = case_when(
      LBTEST == "Leukocytes" & LBORRESU == "10^3/mm^3" ~ as.numeric(LBORRES) * 1.5, 
      TRUE ~ LBORRES
    ), 
    
    LBSTRESU = case_when(
      LBTEST == "Leukocytes" & LBORRESU == "10^3/mm^3" ~ "10^9/L", 
      LBTEST == "Leukocytes" & LBORRESU == "10^3/uL" ~ "10^9/L", 
      LBTEST == "Leukocytes" & LBORRESU == "K/mm^3" ~ "10^9/L", 
      LBTEST == "Leukocytes" & LBORRESU == "K/uL" ~ "10^9/L", 
      
      
      TRUE ~ LBORRESU
    )
  ) |> 
  select(SUBJECT, LBTEST, LBORRESU, LBORRES, LBSTRESN, LBSTRESU)



# Exercise 4: Map all the dataset to final lb dataset.

# LBSTAT
# VISITNUM
# VISIT
# LBDTC
# LBBLFL
# LBDY
