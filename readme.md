
@author: Miguel Garcia-Duch

## HFCS USAGE FOR CAPITAL INCOME ANALISIS

- Project implementing distinct statistical methods on HFCS Survey data. Datasets are not available for privacy reasons but are available for free for research purposes under request. Link: <https://www.ecb.europa.eu/home/pdf/research/hfcn/access_form_leadresearchersurname_researchersurname.pdf>

-----------------------------------------------------------------------------------------------------------------------

## JOINT COLNAMES (common for all countries and waves)

Needed or important variables and their original names:

```r
# new set names
old <- c(
    "hg0510", "hg0610", "dhaq01ea", "dhiq01ea",
    "dhageh1", "dh0001", "dheduh1", "dhgenderh1", "dhemph1", "dhhst",
    "hg0310", "di1400", "di1520", "di1700", "di2000",
    "dn3001", "da2100", "da1120", "da1110", "da1400", "da1200", "da1000",
    "hd0210", "hb2900", "hb2410", "pe0200", "pe0300", "pe0400", "fpe0200", "fpe0300"
),
```

```r
# new set names
new <-  c(
        "profit", "Kgains", "quintile.gwealth", "quintile.gincome",
        "age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan",
        "rental", "financ", "pvpens", "pvtran", "income",
        "net_we", "net_fi", "other", "main", "real", "bussiness", "total_real",
        "num_bs", "val_op", "num_op", "status", "d_isco", "d_nace", "retired_status", "retired_isco08"
    )
```

-----------------------------------------------------------------------------------------------------------------------

## CONSTRUCTION OF RENTSBI VARIABLE

- CURRENT:
"rental", "financ", "profit", "pvpens", "kgains"
"hg0310", "di1400", "hg0510", "", "hg0610", di1520

HG0610=DI1800
gross income from other income sources
What was the total gross amount received over (the last 12 months / the last
calendar year)?

### HG0310=DI1300

gross rental income from real estate property

### DI1420 = HG0510

Income from private business other than self-employment

### DI1410 = HG0410

Income from financial assets, gross of interest payments

### DI1400 = DI1410 + DI1420

Income from financial assets

### DI1420 = HG0510

Income from private business other than self-employment

### DI1520 = Sum of PG0410 for household members

Income from occupational and private pensions

### PG0410

It includes:

- Income received from occupational pension schemes
Old age, survivors, sickness, disability and unemployment pensions received as
interest or dividends from individual insurance private plans.
It excludes:
- Pensions from public pension schemes (under PG0300)

## MISSING CATEGORIES TO INTERPRET

### ISCO

2 character ISCO-08 code
dataset[employm == 1 & d_isco %in% c(1,10:19)], employm := 5] # manager
dataset[employm %in% c(4, 5)], employm := 4] # retired/other
dataset[employm %in% c(1, 3)], employm := 4] # employee
dataset[status == 2  & employm == 3 , employm := 2] # self-employed
dataset[status == 2  & employm == 2 , employm := 3] # capitalist

-> PE0200 (status in employment)
1 - Employee
2 - Self-employed - with employees
3 - Self-employed - without employees
4 - Unpaid family worker

### DNACE
