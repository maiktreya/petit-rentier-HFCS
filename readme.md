
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

(from personal composed into household variables):

- ID,ID
- Survey,Survey
- SA0010,Household number
- SA0100,Country
- IM0100,Implicate
- DN3001,DN3001 Net wealth
- DA1000i,DA1000i Has real assets
- DA2100i,DA2100i Has financial assets
- DA1110i,DA1110i Has HMR
- DA1120i,DA1120i Has other real estate property
- DA1121i,DA1121i Has other real estate property for business
- DA1130i,DA1130i Has vehicles
- DA1131i,DA1131i Has valuables
- DA1140i,DA1140i Has self-employment business wealth
- DA1400i,DA1400i Has real estate wealth
- DA1200i,DA1200i Has business wealth
- DA2101i,DA2101i Has deposits
- DA21011i,DA21011i Has sight accounts
- DA21012i,DA21012i Has saving accounts
- DA2102i,DA2102i Has mutual funds
- DA2103i,DA2103i Has bonds
- DA2104i,DA2104i Has non-SE private business wealth
- DA2105i,DA2105i Has shares (publicly traded)
- DA2106i,DA2106i Has managed accounts
- DA2107i,DA2107i Has money owed to household
- DA2108i,DA2108i Has other assets
- DA2109i,DA2109i Has voluntary pensions/whole life insurance
- DL1000i,DL1000i Has debt
- DL1100i,DL1100i Has mortgage debt
- DL1110i,DL1110i Has HMR mortgage
- DL1120i,DL1120i Has other property mortgage
- DL1200i,DL1200i Has non-mortgage debt
- DOFINASSETS,DOFINASSETS Household has financial assets excluding sight accounts
- DOHMRONLY,DOHMRONLY Value of HMR>=99 % of total assets
- DODARATIO,DODARATIO Debt to asset ratio of indebted households
- DODARATIO75P,DODARATIO75P Debt to asset ratio>=75%
- DOLTVRATIO,DOLTVRATIO Loan to value ratio of main residence
- DOLTVRATIO75P,DOLTVRATIO75P Loan to value of main residence >=75 %
- DODIRATIO,DODIRATIO Debt to income ratio of indebted households
- DODIRATIO300p,DODIRATIO300p Debt to income ratio>=3
- DODSTOTAL,DODSTOTAL Debt service to income ratio, all indebted households
- DODSTOTAL40P,DODSTOTAL40P Debt service to income ratio >= 40 %, all indebted households
- DODSMORTG,DODSMORTG Mortgage debt service to income ratio of households with mortgage debt
- DODSMORTG40P,DODSMORTG40P Mortgage debt service to income ratio >= 40 %
- DODSMORTGHMR,DODSMORTGHMR HMR Mortgage debt service to income ratio of households with HMR mortgage
- DOFOODC,DOFOODC Food expenditure at home/outside home, annual
- DOFOODCP,DOFOODCP Food expenditure as a share of income
- DOCREDITC,DOCREDITC Credit constrained household
- DOINHERIT,DOINHERIT Substantial inheritance/gift received
- DOEINHERIT,DOEINHERIT Expecting to receive inheritance in the future
- DHAQ01,DHAQ01 Country quintile, gross wealth, among households
- DHNQ01,DHNQ01 Country quintile, net wealth, among households
- DHLQ01,DHLQ01 Country quintile, total liabilities, among indebted households
- DHIQ01,DHIQ01 Country quintile, total gross income (DI2000), among households
- DA1122,DA1122 Value of other real estate property not for business activities
- DNTOP10,DNTOP10 Country top 10% net wealth
- DODIRATIOM,DODIRATIOM Mortgage debt to income ratio of households with mortgage debt
- DODIRATIOM300p,DODIRATIOM300p Mortgage debt to income ratio>=3
- DOFOODCH,DOFOODCH Food expenditure (at home), annual
- DL1110b,DL1110b Outstanding balance of fixed interest rate HMR mortgages
- DL1110c,DL1110c Outstanding balance of unknown interest rate regime HMR mortgages
- DL1120b,DL1120b Outstanding balance of fixed interest rate mortgages on other properties
- DL1120c,DL1120c Outstanding balance of unknown interest rate regime mortgages on other properties
- DL2210,DL2210 Payments for other non-mortgage loans (flow)
- DHRA0100H,DHRA0100H RA0100 of reference person
- DHchildrendependent,DHchildrendependent Number of dependent children
- DL1110ai,DL1110ai Has adjustable interest rate HMR mortgage
- DL1110bi,DL1110bi Has fixed interest rate HMR mortgage
- DL1110ci,DL1110ci Has unknown interest rate regime HMR mortgage
- DL1120ai,DL1120ai Has adjustable interest rate other property mortgage
- DL1120bi,DL1120bi Has fixed interest rate other property mortgage
- DL1120ci,DL1120ci Has unknown interest rate regime other property mortgage
- DL2000i,DL2000i Has debt payments
- DL2100i,DL2100i Has mortgage payments
- DL2110i,DL2110i Has HMR mortgage payments
- DL2120i,DL2120i Has other property mortgage payments
- DL2200i,DL2200i Has non-mortgage debt payments
- DL2210i,DL2210i Has other non-mortgage loans payments
- DOFINASSIST,DOFINASSIST Ability to get financial assistance from friends or relatives
- DOCREDITAPPL,DOCREDITAPPL Applied for credit within last 3 years
- DOCREDITREFUSED,DOCREDITREFUSED Refused or only reduced credit (among those applying in last 3 years)
- DI1800,DI1800 Income from other sources
- DI2000eq,DI2000eq Equivalised household gross income
- DA1000SH,DA1000SH Real assets as share of gross wealth
- DA2100SH,DA2100SH Financial assets as share of gross wealth
- DH0001,DH0001 Number of household members
- DH0006,DH0006 Number of household members 16+
- DH14P,DH14P Number of household members aged 14+
- DHN013,DHN013 Number of children in household (0-13)
- DH0003,DH0003 Number of economically active members in household
- DH0004,DH0004 Number of household members in employment
- DHHTYPE,DHHTYPE Household type
- DH0002,DH0002 Consumption units (OECD modified)
- DA1110,DA1110 Value of household's main residence
- DA1120,DA1120 Value of other real estate property (all)
- DA1121,DA1121 Value of other real estate property used for business activities
- DA1130,DA1130 Value of household's vehicles
- DA1131,DA1131 Valuables
- DA1140,DA1140 Value of self-employment businesses
- DA2101,DA2101 Deposits
- DA21011,DA21011 Deposits: sight accounts
- DA21012,DA21012 Deposits: saving accounts
- DA2102,DA2102 Mutual funds, total
- DA2103,DA2103 Bonds
- DA2104,DA2104 Value of non self-employment private business
- DA2105,DA2105 Shares, publicly traded
- DA2106,DA2106 Managed accounts
- DA2107,DA2107 Money owed to households
- DA2108,DA2108 Other assets
- DA2109,DA2109 Voluntary pension/whole life insurance
- DL1110,DL1110 Outstanding balance of HMR mortgages
- DL1120,DL1120 Outstanding balance of mortgages on other properties
- DL1200,DL1200 Outstanding balance of non-mortgage debt
- DL1100,DL1100 Outstanding balance of mortgage debt
- DL2100,DL2100 Payments for mortgages (flow)
- DL2110,DL2110 Payments for HMR mortgages (flow)
- DL2200,DL2200 Payments for non-mortgage debt (flow)
- DL2000,DL2000 Payments for household's total debt (flow)
- DI1412,DI1412 Interest payments
- DI1100,DI1100 Employee income
- DI1200,DI1200 Self-employment income
- DI1300,DI1300 Rental income from real estate property
- DI1410,DI1410 Income from financial assets, gross of interest payments
- DI1400,DI1400 Income from financial investments
- DI1420,DI1420 Income from private business other than self-employment
- DI1510,DI1510 Income from public pensions
- DI1520,DI1520 Income from occupational and private pensions
- DI1500,DI1500 Income from pensions
- DI1610,DI1610 Unemployment benefits
- DI1620,DI1620 Other social transfers
- DI1600,DI1600 Regular social transfers (except pensions)
- DI1700,DI1700 Regular private transfers
- DA1000,DA1000 Total real assets 1  (incl. business wealth, vehicles and valuables)
- DA1200,DA1200 Business wealth
- DA1400,DA1400 Real estate wealth
- DA2100,DA2100 Total financial assets 1 (excl. public and occupational pension plans)
- DA3001,DA3001 Total assets, excl. public and occupational pension plans
- DL1000,DL1000 Total outstanding balance of household's liabilities
- DI2000,DI2000 Total household gross income
- DA1122i,DA1122i Has other real estate property not for business
- DATOP10,DATOP10 Country top 10% gross wealth
- DHHST,DHHST Housing status
- DHaged65plus,DHaged65plus Household members aged 65 or more
- DITOP10,DITOP10 Country top 10% total gross income
- DL1110a,DL1110a Outstanding balance of adjustable interest rate HMR mortgages
- DL1120a,DL1120a Outstanding balance of adjustable interest rate mortgages on other properties
- DL1210,DL1210 Outstanding balance of credit line/overdraft
- DL1210i,DL1210i Has credit line/overdraft debt
- DL1220,DL1220 Outstanding balance of credit card debt
- DL1220i,DL1220i Has credit card debt
- DL1230,DL1230 Outstanding balance of other non-mortgage loans
- DL1230i,DL1230i Has non-mortgage loans
- DLTOP10,DLTOP10 Country top 10% total liabilities
- DNNLA,DNNLA Net liquid assets
- DNNLAi,DNNLAi Has net liquid assets
- DI1100i,DI1100i Has employee income
- DI1200i,DI1200i Has self-employment income
- DI1300i,DI1300i Has rental income from real estate property
- DI1400i,DI1400i Has income from financial investments
- DI1410i,DI1410i Has income from financial assets, gross of interest payments
- DI1420i,DI1420i Has income from private business other than self-employment
- DI1500i,DI1500i Has income from pensions
- DI1510i,DI1510i Has income from public pensions
- DI1520i,DI1520i Has income from occupational and private pensions
- DI1600i,DI1600i Has income from regular social transfers (except pensions)
- DI1610i,DI1610i Has income from unemployment benefits
- DI1620i,DI1620i Has income from other social transfers
- DI1700i,DI1700i Has income from regular private transfers
- DI1800i,DI1800i Has income from other sources
- DITOP10eq,DITOP10eq Country top 10% gross equalised income
- DL2120,DL2120 Payments for other property mortgages (flow)
- DOFOODCUC,DOFOODCUC Food expenditure at home/outside home per consumption unit, annual
- DOFOODCHUC,DOFOODCHUC Food expenditure at home per consumption unit, annual
- DA2199,DA2199 Other types of financial assets
- DA2199i,DA2199i Has other types of financial assets
- DATOP10EA,DATOP10EA EA top 10% gross wealth
- DHAQ01EA,DHAQ01EA EA quintile, gross wealth, among households
- DHIQ01EA,DHIQ01EA EA quintile, total gross income (DI2000), among households
- DHLQ01EA,DHLQ01EA EA quintile, total liabilities, among indebted households
- DHNQ01EA,DHNQ01EA EA quintile, net wealth, among households
- DITOP10EA,DITOP10EA EA top 10% total gross income
- DITOP10eqEA,DITOP10eqEA EA top 10% gross equalised income
- DLCC,DLCC Has credit card
- DLCL,DLCL Has credit line/overdraft
- DLTOP10EA,DLTOP10EA EA top 10% total liabilities
- DNTOP10EA,DNTOP10EA EA top 10% net wealth
- DOCREDITREFUSAL,DOCREDITREFUSAL Experienced credit refusal or reduction(among those applying in last 3 years)
- DODSTOTALp,DODSTOTALp Debt service to income ratio, households with debt payments
- DODSTOTAL40Pp,DODSTOTAL40Pp Debt service to income ratio >= 40 %, households with debt payments
- DOGIFTINHER,DOGIFTINHER Amount of received gifts and inheritances
- DHAGEH1,DHAGEH1 Age of reference person
- DHAGEH1B,DHAGEH1B Age of reference person - brackets
- DHEDUH1,DHEDUH1 Education of reference person
- DHEMPH1,DHEMPH1 Main labour status of reference person
- DHGENDERH1,DHGENDERH1 Gender of reference person
- DHIDH1,DHIDH1 Household head1 ID
- HW0010,HW0010 household weight
