
# HFCS USAGE

## JOINT COLNAMES

Needed or important variables:

- RA0100 -> reference of household (1 = head)
- HW0010 -> weights
- IM0100 -> implicate ID
- HID -> household id A (personal file)
- SA0010 -> household id B
- SA0100 -> country
- RA0400 -> country birth
- RA0200 -> gender
- RAO300 -> age
- PA0100 -> martial status
- PA0200 -> education
- HB0100 -> household size
- PE0100x -> labour status
- PE0200 -> status in employment -> (nº of workers: 1, employee 2, Self-employed 3, with employee)
- PE0300 -> job description (ISCO)
- PE0300 -> main employment (NACE)
- #########################################
- HD0210 -> nº of businesses
- PG0100 -> employee income
- HG0200 -> income from private transfers (group 7 income)
- DI2000 -> total household income (aggregated in D category)

## COMPLETE COUNTRIES

panel_countries <- ("AT","DE","BE","ES","IT","CY","MT")

## INCOME CATEGORIES

    setnames(survey_ecv,
        old = c("PB190", "PL051", "PE041", "RB090", "HH021", "PL040C", "AGE", "RB290", "PB140", "HX040", "HY040N", "HY090N"),
        new = c("civil", "ocup", "educ", "sex", "tenancy", "class", "bage", "country", "birth", "members", "housrent", "profit")
    )

    models[[i]] <- svyglm(rentsbi ~ bage + country + educ + members + civil + sex + class + tenancy + educ, design = survey_total, family = "quasibinomial")

### Personal

- PG0100received employee income
- PG0110gross cash employee income
- PG0200received self-employment income
- PG0210 gross self-employment income (profit/losses of unincorporated enterprises)
- PG0300received income from public pensions
- PG0310gross income from public pensions
- PG0400 received income from private and occupational pension plans
- PG0410gross income from occupational and private pension plans
- PG0500received income from unemployment benefits
- PG0510gross income from unemployment benefits

### Household

- HG0100received income from public transfers
- HG0110gross income from regular social transfers
- HG0200received income from regular private transfers
- HG0210income from regular private transfers133
- HG0250received income from other private transfers
- HG0260financial assistance received from relatives and friends

## INHERITANCE

- HH0100any substantial gift or inheritance received140
- HH0110no of gifts/inheritances received
- HH020$xgift/inheritance $x: year gift/inheritance received
- HH030$xgift/inheritance $x: what kind of assets received
- HH040$xgift/inheritance $x: value
- HH050$xgift/inheritance $x: type of transfer (gift/inheritance)
- HH060$xgift/inheritance $x: from whom received

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------

## DERIVED VARIABLES

### DERIVED TABLES GENERAL

c("DHAGEH1", "DH0001", "DHEDUH1", "DHGENDERH1", "DHEMPH1", "DHHST")
c("age_ref", "hsize", "edu_ref", "head_gendr", "employm", "tenan")

### DERIVED TABLES INCOME

c("DI1300", "DI1400", "DI1520", "DI1700", "DI2000")
c("rental", "financ", "pvpens", "pvtran", "income")

### DERIVED TABLES WEALTH

c("DN3001", "DA2100", "DA1120", "DA1110", "DA1400", "DA1200", "DA1000")
c("net_we", "net_fi", "other", "main", "real", "bussiness", "total_real")

### OTHER PROPERTIES AND BUSINESS DERIVED VARIABLES

c("HD0210", "HB2900", "HB2410", "HB250$x", "HB260$x")
c("num_bs", "val_op", "num_op", "op_type", "op_use")

### PERSONAL VARIABLES EMPLOYMENT (For head -> RA0010=DHIDH1)

c("PE0200", "PE0300", "PE0400")
c("status", "d_isco", "d_nace")
