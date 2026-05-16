library(dplyr)
library(lubridate)
library(readr)

set.seed(42)

N <- 80
study <- "ABCD-123"
treatments <- c("Placebo", "Low Dose", "High Dose")
countries <- c("USA", "CAN", "GBR", "DEU", "FRA")
sexes <- c("M", "F")
races <- c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN",
           "AMERICAN INDIAN OR ALASKA NATIVE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")
ethnics <- c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO")
discon_reasons <- c("ADVERSE EVENT", "WITHDRAWAL BY SUBJECT",
                    "PROGRESSIVE DISEASE", "PROTOCOL VIOLATION", "")

base_date <- ymd("2023-01-15")

rows <- list()
for (i in 1:N) {
  subjid <- sprintf("%03d", i)
  usubjid <- paste0(study, "-", subjid)

  site <- sample(1:10, 1)
  country <- sample(countries, 1)

  trt <- sample(treatments, 1)
  trt_code <- switch(trt, "Placebo" = 0, "Low Dose" = 1, "High Dose" = 2)

  age <- round(runif(1, 35, 85), 1)
  age_grp <- ifelse(age < 65, "<65", ">=65")

  sex <- sample(sexes, 1)
  race <- sample(races, 1)
  ethnic <- sample(ethnics, 1)

  arm <- trt
  saffl <- sample(c("Y", "N"), 1, prob = c(0.95, 0.05))
  ittfl <- "Y"
  efffl <- sample(c("Y", "N"), 1, prob = c(0.9, 0.1))
  compfl <- sample(c("Y", "N"), 1, prob = c(0.7, 0.3))

  trtsdt <- base_date + days(sample(0:180, 1))
  trtedt <- trtsdt + days(sample(1:365, 1))
  trtedy <- as.integer(difftime(trtedt, trtsdt, units = "days")) + 1

  params <- list(
    c("Time to Death", "TTDEATH"),
    c("Time to Disease Progression", "TTPROGR"),
    c("Time to Response", "TTRESP")
  )

  for (p in params) {
    param <- p[1]
    paramcd <- p[2]

    if (paramcd == "TTDEATH") {
      censor <- sample(c(0, 1), 1, prob = c(0.3, 0.7))
      evntdesc <- ifelse(censor == 1, "", "DEATH")
      cnsdtdsc <- ifelse(censor == 1, "LAST KNOWN ALIVE", "")
      days_to_event <- sample(30:365, 1)
      adt <- min(trtsdt + days(days_to_event), trtsdt + days(365))
    } else if (paramcd == "TTPROGR") {
      censor <- sample(c(0, 1), 1, prob = c(0.45, 0.55))
      evntdesc <- ifelse(censor == 1, "", "DISEASE PROGRESSION")
      cnsdtdsc <- ifelse(censor == 1, "NO PROGRESSION", "")
      days_to_event <- sample(14:365, 1)
      adt <- min(trtsdt + days(days_to_event), trtsdt + days(365))
    } else {
      censor <- sample(c(0, 1), 1, prob = c(0.35, 0.65))
      evntdesc <- ifelse(censor == 1, "", "RESPONSE")
      cnsdtdsc <- ifelse(censor == 1, "NO RESPONSE", "")
      days_to_event <- sample(28:365, 1)
      adt <- min(trtsdt + days(days_to_event), trtsdt + days(365))
    }

    aval <- as.integer(difftime(adt, trtsdt, units = "days")) + 1
    startdt <- trtsdt
    ady <- as.integer(difftime(adt, trtsdt, units = "days")) + 1

    discont <- sample(c("Y", "N"), 1, prob = c(0.3, 0.7))
    dcsreas <- ifelse(discont == "Y", sample(discon_reasons, 1), "")

    rows[[length(rows) + 1]] <- data.frame(
      STUDYID = study,
      USUBJID = usubjid,
      SUBJID = subjid,
      SITEID = sprintf("%02d", site),
      COUNTRY = country,
      TRTSDT = format(trtsdt, "%Y-%m-%d"),
      TRTEDT = format(trtedt, "%Y-%m-%d"),
      TRT01PN = trt_code,
      TRT01P = trt,
      TRT01AN = trt_code,
      TRT01A = trt,
      ARMCD = paste0("ARM", trt_code),
      ARM = arm,
      ACTARMCD = paste0("ARM", trt_code),
      ACTARM = arm,
      AGE = age,
      AGEU = "YEARS",
      AGEGR1 = age_grp,
      SEX = sex,
      RACE = race,
      ETHNIC = ethnic,
      SAFFL = saffl,
      ITTFL = ittfl,
      EFFFL = efffl,
      COMPLFL = compfl,
      PARAM = param,
      PARAMCD = paramcd,
      AVAL = aval,
      CNSR = censor,
      EVNTDESC = evntdesc,
      CNSDTDSC = cnsdtdsc,
      STARTDT = format(startdt, "%Y-%m-%d"),
      ADT = format(adt, "%Y-%m-%d"),
      ADY = ady,
      SRCDOM = "ADSL",
      SRCVAR = paramcd,
      DISCONT = discont,
      DCSREAS = dcsreas,
      DOSAGE = trt_code * 50,
      DOSGRP = trt,
      DOSDUR = trtedy,
      stringsAsFactors = FALSE
    )
  }
}

adtte <- bind_rows(rows)
adtte <- adtte %>%
  arrange(USUBJID, PARAMCD) %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY,
    TRTSDT, TRTEDT, TRT01PN, TRT01P, TRT01AN, TRT01A,
    ARMCD, ARM, ACTARMCD, ACTARM,
    AGE, AGEU, AGEGR1, SEX, RACE, ETHNIC,
    SAFFL, ITTFL, EFFFL, COMPLFL,
    PARAM, PARAMCD,
    AVAL, CNSR, EVNTDESC, CNSDTDSC,
    STARTDT, ADT, ADY,
    SRCDOM, SRCVAR,
    DISCONT, DCSREAS,
    DOSAGE, DOSGRP, DOSDUR
  )

outdir <- "data"
write_csv(adtte, file.path(outdir, "adtte.csv"))

cat(sprintf("Generated %d rows across %d subjects with %d parameters.\n",
            nrow(adtte), N, n_distinct(adtte$PARAMCD)))
cat(paste0("File saved: ", outdir, "/adtte.csv\n"))
cat("\nFirst 5 rows:\n")
print(head(adtte, 5))
cat("\nParameter breakdown:\n")
print(table(adtte$PARAMCD))
