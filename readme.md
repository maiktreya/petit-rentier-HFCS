#

## WORKING HYPOTHESIS

- Previous literature almost exclusively focusses on "wealth inequality" rather than more general an informative "wealth distribution".

- There is a generalized trend towards growing wealth distribution. this fact may be hidden if just focus on the mean but could be detected either clustering population or analysing alternative statistics (mean, GINI). Technically , the recent development of RIF regressions makes this kind of analysis suitable.

- Among the possible categorical clustering, beyond the obvious one like, homeownership, gender of income group, two additional classifications stand out:

- Class (as it expected to become growingly less significant).

- Age (because even considering the natural positive correlation between wealth and age, there is a generalised trend in the last decades, especially among western European countries, towards a growing gap between the wealth of young and other age groups. The preliminary interpretation suggests a growing importance of age clusters for wealth distribution and increasing negative effect on wealth associated to young citizens as compared to the gaps and influence of previous decades.

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
- Financial Portfolio
- Inheritance

---
CONSIDERATIONS:
>
> - Interaction between personal and structural factors (Age vs Class .i.e., young workers).
>
> - Perform RIF regression for GINI and median including the identified influential factors for all the annual issues 2002-2020.
>
> - Oaxaca-Blinder decomposition of GINI for workers, young people and both. Compare 2002 to 2020.
