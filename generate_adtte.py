import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random

random.seed(42)
np.random.seed(42)

N = 80

study = "ABCD-123"
treatments = ["Placebo", "Low Dose", "High Dose"]
countries = ["USA", "CAN", "GBR", "DEU", "FRA"]
sexes = ["M", "F"]
races = ["WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "AMERICAN INDIAN OR ALASKA NATIVE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"]
ethnics = ["HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"]
discon_reasons = ["ADVERSE EVENT", "WITHDRAWAL BY SUBJECT", "PROGRESSIVE DISEASE", "PROTOCOL VIOLATION", ""]

base_date = datetime(2023, 1, 15)

rows = []
for i in range(1, N + 1):
    subjid = f"{i:03d}"
    usubjid = f"{study}-{subjid}"

    site = random.randint(1, 10)
    country = random.choice(countries)

    trt = random.choice(treatments)
    if trt == "Placebo":
        trt_code = 0
    elif trt == "Low Dose":
        trt_code = 1
    else:
        trt_code = 2

    age = round(random.uniform(35, 85), 1)
    age_grp = f"<65" if age < 65 else ">=65"

    sex = random.choice(sexes)
    race = random.choice(races)
    ethnic = random.choice(ethnics)

    arm = trt
    armcd = trt_code

    saffl = random.choices(["Y", "N"], weights=[0.95, 0.05])[0]
    ittfl = "Y"
    efffl = random.choices(["Y", "N"], weights=[0.9, 0.1])[0]
    compfl = random.choices(["Y", "N"], weights=[0.7, 0.3])[0]

    trtsdt = base_date + timedelta(days=random.randint(0, 180))
    # Treatment duration 1-365 days
    trtedt = trtsdt + timedelta(days=random.randint(1, 365))

    # Study day of last dose: 1 = day of first dose
    trtedy = (trtedt - trtsdt).days + 1

    # Event/parameters
    params = [
        ("Time to Death", "TTDEATH"),
        ("Time to Disease Progression", "TTPROGR"),
        ("Time to Response", "TTRESP"),
    ]

    for param, paramcd in params:
        if paramcd == "TTDEATH":
            censor = random.choices([0, 1], weights=[0.3, 0.7])[0]
            evntdesc = "" if censor == 1 else "DEATH"
            cnsdtdsc = "LAST KNOWN ALIVE" if censor == 1 else ""
            if censor == 1:
                # censored: start to last known alive (before or at study end)
                adt = min(trtsdt + timedelta(days=random.randint(30, 365)), trtsdt + timedelta(days=365))
            else:
                # event occurred
                adt = trtsdt + timedelta(days=random.randint(30, 365))
        elif paramcd == "TTPROGR":
            censor = random.choices([0, 1], weights=[0.45, 0.55])[0]
            evntdesc = "" if censor == 1 else "DISEASE PROGRESSION"
            cnsdtdsc = "NO PROGRESSION" if censor == 1 else ""
            if censor == 1:
                adt = min(trtsdt + timedelta(days=random.randint(14, 365)), trtsdt + timedelta(days=365))
            else:
                adt = trtsdt + timedelta(days=random.randint(14, 365))
        else:  # TTRESP
            censor = random.choices([0, 1], weights=[0.35, 0.65])[0]
            evntdesc = "" if censor == 1 else "RESPONSE"
            cnsdtdsc = "NO RESPONSE" if censor == 1 else ""
            if censor == 1:
                adt = min(trtsdt + timedelta(days=random.randint(28, 365)), trtsdt + timedelta(days=365))
            else:
                adt = trtsdt + timedelta(days=random.randint(28, 365))

        aval = (adt - trtsdt).days + 1 if censor == 0 else (adt - trtsdt).days + 1
        startdt = trtsdt
        ady = (adt - trtsdt).days + 1

        discont = random.choices(["Y", "N"], weights=[0.3, 0.7])[0]
        dcsreas = random.choice(discon_reasons) if discont == "Y" else ""

        row = {
            "STUDYID": study,
            "USUBJID": usubjid,
            "SUBJID": subjid,
            "SITEID": f"{site:02d}",
            "COUNTRY": country,
            "TRTSDT": trtsdt.strftime("%Y-%m-%d"),
            "TRTEDT": trtedt.strftime("%Y-%m-%d"),
            "TRT01PN": trt_code,
            "TRT01P": trt,
            "TRT01AN": trt_code,
            "TRT01A": trt,
            "ARMCD": f"ARM{trt_code}",
            "ARM": arm,
            "ACTARMCD": f"ARM{trt_code}",
            "ACTARM": arm,
            "AGE": age,
            "AGEU": "YEARS",
            "AGEGR1": age_grp,
            "SEX": sex,
            "RACE": race,
            "ETHNIC": ethnic,
            "SAFFL": saffl,
            "ITTFL": ittfl,
            "EFFFL": efffl,
            "COMPLFL": compfl,
            "PARAM": param,
            "PARAMCD": paramcd,
            "AVAL": aval,
            "CNSR": censor,
            "EVNTDESC": evntdesc,
            "CNSDTDSC": cnsdtdsc,
            "STARTDT": startdt.strftime("%Y-%m-%d"),
            "ADT": adt.strftime("%Y-%m-%d"),
            "ADY": ady,
            "SRCDOM": "ADSL",
            "SRCVAR": paramcd,
            "DISCONT": discont,
            "DCSREAS": dcsreas,
            "DOSAGE": trt_code * 50,
            "DOSGRP": trt,
            "DOSDUR": trtedy,
        }
        rows.append(row)

adtte = pd.DataFrame(rows)

cols = [
    "STUDYID", "USUBJID", "SUBJID", "SITEID", "COUNTRY",
    "TRTSDT", "TRTEDT", "TRT01PN", "TRT01P", "TRT01AN", "TRT01A",
    "ARMCD", "ARM", "ACTARMCD", "ACTARM",
    "AGE", "AGEU", "AGEGR1", "SEX", "RACE", "ETHNIC",
    "SAFFL", "ITTFL", "EFFFL", "COMPLFL",
    "PARAM", "PARAMCD",
    "AVAL", "CNSR", "EVNTDESC", "CNSDTDSC",
    "STARTDT", "ADT", "ADY",
    "SRCDOM", "SRCVAR",
    "DISCONT", "DCSREAS",
    "DOSAGE", "DOSGRP", "DOSDUR",
]
adtte = adtte[cols]
adtte = adtte.sort_values(["USUBJID", "PARAMCD"]).reset_index(drop=True)

outdir = "data"
adtte.to_csv(f"{outdir}/adtte.csv", index=False)

print(f"Generated {len(adtte)} rows across {N} subjects with {adtte['PARAMCD'].nunique()} parameters.")
print(f"File saved: {outdir}/adtte.csv")
print("\\nFirst 5 rows:")
print(adtte.head().to_string())
print("\\nParameter breakdown:")
print(adtte["PARAMCD"].value_counts().to_string())
