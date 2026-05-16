/* ==============================================
   generate_adtte.sas
   Generate ADaM ADTTE sample dataset (CDISC ADaMIG v1.3)
   
   Output:
     - data/adtte.csv
     - data/adtte.xpt (SAS Transport v5)
   
   Equivalent of: generate_adtte.R
   ============================================== */

/* Set seed for reproducibility */
%let seed = 42;

/* Define library for output */
libname out "data";

/* ---------- ADSL-level data ---------- */
data adsl;
  call streaminit(&seed);
  length STUDYID $8 USUBJID $12 SUBJID $3 SITEID $2 COUNTRY $3
         TRT01P $20 TRT01A $20 ARMCD $6 ARM $20 ACTARMCD $6 ACTARM $20
         AGEU $6 AGEGR1 $3 SEX $1 RACE $60 ETHNIC $50
         SAFFL $1 ITTFL $1 EFFFL $1 COMPLFL $1
         DISCONT $1 DCSREAS $30 dosgrp $20;
  retain STUDYID "ABCD-123";

  array trtlist[3] $20 _temporary_ ("Placebo","Low Dose","High Dose");
  array cntrylist[5] $3 _temporary_ ("USA","CAN","GBR","DEU","FRA");
  array racelist[5] $60 _temporary_
    ("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN",
     "AMERICAN INDIAN OR ALASKA NATIVE","NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER");
  array ethniclist[2] $50 _temporary_ ("HISPANIC OR LATINO","NOT HISPANIC OR LATINO");
  array dcsreaslist[4] $30 _temporary_
    ("ADVERSE EVENT","WITHDRAWAL BY SUBJECT","PROGRESSIVE DISEASE","PROTOCOL VIOLATION");

  do i = 1 to 80;
    SUBJID = put(i, z3.);
    USUBJID = cats(STUDYID, "-", SUBJID);
    SITEID = put(rand("integer", 1, 10), z2.);
    COUNTRY = cntrylist[rand("integer", 1, 5)];

    trt_idx = rand("integer", 1, 3);
    TRT01P = trtlist[trt_idx];
    TRT01A = TRT01P;
    ARMCD = cats("ARM", trt_idx - 1);
    ARM = TRT01P;
    ACTARMCD = ARMCD;
    ACTARM = ARM;

    AGE = round(rand("uniform") * 50 + 35, 0.1);
    if AGE < 65 then AGEGR1 = "<65";
    else AGEGR1 = ">=65";
    AGEU = "YEARS";

    if rand("uniform") < 0.5 then SEX = "M"; else SEX = "F";
    RACE  = racelist[rand("integer", 1, 5)];
    ETHNIC = ethniclist[rand("integer", 1, 2)];

    if rand("uniform") < 0.95 then SAFFL = "Y"; else SAFFL = "N";
    ITTFL = "Y";
    if rand("uniform") < 0.90 then EFFFL = "Y"; else EFFFL = "N";
    if rand("uniform") < 0.70 then COMPLFL = "Y"; else COMPLFL = "N";

    /* Treatment start/end dates */
    TRTSDT = "15JAN2023"D + rand("integer", 0, 180);
    TRTEDT = TRTSDT + rand("integer", 1, 365);
    TRTEDUR = TRTEDT - TRTSDT + 1;

    trt_code = trt_idx - 1;
    dosage = trt_code * 50;
    dosgrp = TRT01P;

    if rand("uniform") < 0.30 then DISCONT = "Y";
    else DISCONT = "N";
    if DISCONT = "Y" then DCSREAS = dcsreaslist[rand("integer", 1, 4)];
    else DCSREAS = "";

    output;
  end;
  drop i trt_idx trt_code;
run;

/* ---------- ADTTE: expand ADSL × 3 endpoints ---------- */
data adtte;
  set adsl;
  length PARAM $50 PARAMCD $8 EVNTDESC $25 CNSDTDSC $30
         SRCDOM $6 SRCVAR $8;
  retain SRCDOM "ADSL";

  do p = 1 to 3;
    if p = 1 then do;
      PARAM  = "Time to Death";
      PARAMCD = "TTDEATH";
      if rand("uniform") < 0.30 then do;
        CNSR = 0;  /* event */
        EVNTDESC = "DEATH";
        CNSDTDSC = "";
        days = rand("integer", 30, 365);
      end;
      else do;
        CNSR = 1;  /* censored */
        EVNTDESC = "";
        CNSDTDSC = "LAST KNOWN ALIVE";
        days = rand("integer", 30, 365);
      end;
      ADT = min(TRTSDT + days, TRTSDT + 365);
    end;

    else if p = 2 then do;
      PARAM  = "Time to Disease Progression";
      PARAMCD = "TTPROGR";
      if rand("uniform") < 0.45 then do;
        CNSR = 0;
        EVNTDESC = "DISEASE PROGRESSION";
        CNSDTDSC = "";
        days = rand("integer", 14, 365);
      end;
      else do;
        CNSR = 1;
        EVNTDESC = "";
        CNSDTDSC = "NO PROGRESSION";
        days = rand("integer", 14, 365);
      end;
      ADT = min(TRTSDT + days, TRTSDT + 365);
    end;

    else do;
      PARAM  = "Time to Response";
      PARAMCD = "TTRESP";
      if rand("uniform") < 0.35 then do;
        CNSR = 0;
        EVNTDESC = "RESPONSE";
        CNSDTDSC = "";
        days = rand("integer", 28, 365);
      end;
      else do;
        CNSR = 1;
        EVNTDESC = "";
        CNSDTDSC = "NO RESPONSE";
        days = rand("integer", 28, 365);
      end;
      ADT = min(TRTSDT + days, TRTSDT + 365);
    end;

    AVAL = ADT - TRTSDT + 1;
    STARTDT = TRTSDT;
    ADY = AVAL;
    SRCVAR = PARAMCD;

    output;
  end;
  drop p days;
run;

/* ---------- Reorder variables (ADaM standard order) ---------- */
data adtte;
  retain STUDYID USUBJID SUBJID SITEID COUNTRY
         TRTSDT TRTEDT TRT01PN TRT01P TRT01AN TRT01A
         ARMCD ARM ACTARMCD ACTARM
         AGE AGEU AGEGR1 SEX RACE ETHNIC
         SAFFL ITTFL EFFFL COMPLFL
         PARAM PARAMCD
         AVAL CNSR EVNTDESC CNSDTDSC
         STARTDT ADT ADY
         SRCDOM SRCVAR
         DISCONT DCSREAS
         DOSAGE DOSGRP TRTEDUR;
  set adtte;
  format TRTSDT TRTEDT STARTDT ADT yymmdd10.;
run;

/* ---------- Add numeric treatment code variables ---------- */
data adtte;
  set adtte;
  if TRT01P = "Placebo"  then do; TRT01PN = 0; TRT01AN = 0; end;
  if TRT01P = "Low Dose" then do; TRT01PN = 1; TRT01AN = 1; end;
  if TRT01P = "High Dose" then do; TRT01PN = 2; TRT01AN = 2; end;
run;

/* ---------- Sort ---------- */
proc sort data=adtte;
  by USUBJID PARAMCD;
run;

/* ---------- Export CSV ---------- */
proc export data=adtte
  outfile="data/adtte.csv"
  dbms=csv
  replace;
run;

/* ---------- Export XPT (SAS Transport v5) ---------- */
libname xptout xport "data/adtte.xpt";
data xptout.adtte; set adtte; run;
libname xptout clear;

/* ---------- Summary ---------- */
proc sql;
  select count(*) as Total_Rows, count(distinct USUBJID) as N_Subjects,
         count(distinct PARAMCD) as N_Parameters
  from adtte;
quit;

title "Parameter Breakdown";
proc freq data=adtte;
  tables PARAMCD / nocum nopercent;
run;
title;
