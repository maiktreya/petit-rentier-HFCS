#

REAL ESTATE WEALTH INEQUALITY AND SOCIAL CLASS: A RIF-oriented distributional analysys for the spanish case (2004-2020)

## WORKING HYPOTHESIS

- Previous literature almost exclusively focusses on "wealth inequality" rather than more general an informative "wealth distribution".

- There is a generalized trend towards growing wealth distribution. this fact may be hidden if just focus on the mean but could be detected either clustering population or analysing alternative statistics (mean, GINI). Technically , the recent development of RIF regressions makes this kind of analysis suitable.

- Among the possible categorical clustering, beyond the obvious one like, homeownership, gender of income group, two additional classifications stand out:

- Class (as it expected to become growingly less significant).

- Age because (even considering the natural positive correlation between wealth and age), there is a generalised trend in the last decades, especially among western European countries, towards a growing gap between the wealth of young and other age groups. The preliminary interpretation suggests a growing importance of age clusters for wealth distribution and increasing negative effect on wealth associated to young citizens as compared to the gaps and influence of previous decades.

- In the empirical part, we build our Methodology I'm another important hypothesis: Standard OLS does not correctly grasp  the highly un linear nature of wealth empirical Distributions and RIF regressions on quantiles and Gini should be more robust. for completeness, we report regressions for these 3 main statistics in order to understand the nature of the highly skewed nature of wealth distributions. A nature which we would analyse to finalize our work through the reconstruction of empirical distribution functions through smoothing algorithms.

- A final hypothesis regards the implementation of ML techniques to identify influential factors in wealth distributions. In general, ML algorithms hasn't been yet widely adopted in wealth analysis literature as their inner working privileges "black-box" predictions and partial effects like the ones reported by coefficients in traditional linear regression cannot be normally computed. Nevertheless, ML techniques like gradient boosting can potentially constitute an important tool identifying influential factors on the variable of interest before estimating a traditional GLS model with the meaningful factors. This perspective could be especially interesting when working with multiple year independent surveys which don't allow to construct proper time series for panel analysis but can be pooled for the training dataset where influential factors are identified in the first instance.

## EMPIRICAL SECTION

1. Multiyear exploratory analysis of descriptive statistics

2. Analysis of empirical distributions

3. Mean OLS unconditional regression

4. Median and GINI: RIF GLS unconditional regression

5. Oaxaca-blinder decomposition (start and end of period change analysis

### Structure of the empirical section

Evolution of mean median and Gini of net wealth through 2002-2020.

Identify underlying factors on  wealth distribution through gradient-boost techniques. (Pooling all EFF survey issues in one pooled training set identifying the year).

Personal factors:

- Sex
- Age
- Education

Structural economic factors:

- Class
- Income level
- Homeownership
- Multiproperty
- Financial Portfolio
- Inheritance
- Rental income

---
CONSIDERATIONS:
>
> - Interaction between personal and structural factors (Age vs Class .i.e., young workers).
>
> - Perform RIF regression for GINI and median including the identified influential factors for all the annual issues 2002-2020.
>
> - Oaxaca-Blinder decomposition of GINI for workers, young people and both. Compare 2002 to 2020.

## MAIN BIBLIOGRAPHY

### HOUSING WEALTH

- (Wind,2016): The distribution of housing wealth in 16 EU countries: accounting for institutional differences.
- (Kaas et al., 2019): Homeownership and wealth inequality in Europe.
- (Boertien & López-Gay, 2023): The polarization of real estate ownership and increasing wealth inequality in Spain. ✅

### TOTAL WEALTH

- (Cowell et al.,2017): Wealth top incomes and inequality.
- (Azpirate, 2010): The Household Wealth Distribution in Spain: The Role of Housing and Financial Wealth.  ✅

#### CLASS

- (Duvoux, 2022): Class and relative wealth accumulation in 5 EU countries from HCFS.
- (Westhoff, 2021): Social class and earnings trajectori4es for 14 EU countries.

#### AGE

- (Hwang et al., 2021): Population aging and income inequality.
- (Ihle† & Siebert-Meyerhoff, 2017): The richer the older?  Adecomposition of wealth inequality by age sub-groups.

### WAGE INEQUALITY AND CLASS / TYPE

- (Giangregorio & Manta, 2021): The structure of the labour market and wage inequalitu using RIF-OLS: the italian case.
- (Gradin, 2021): Inequality by population groups and sources of income. Accounting for inequality changes in Spain during the recession.

### ML APPROACHES

- (Salas-Rojo & Rodríguez, 2022): Inheritances and wealth inequality: a machine learning approach. ✅

---
---

## DRAFT NOTES

  -> FIRST STEPS:

- Check how the distinct social clasess are constructed
  - Worker
  - Capitalist
  - Slf-Employed
  - Retired
  - Inactive
  - Manager

- Get the remaining variables needed to include:
  - Multiprop
  - Inheritance
  - Rental income
  - Education
