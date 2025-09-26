# 📖 Usage Guide: Model Performance Comparison

This document explains the purpose, methodology, and usage of the `model_comparison_cv.R` script.

---

### 1. Purpose of the Script

The primary goal of this script is to **quantitatively compare the predictive performance** of two different modeling approaches for identifying "petit-rentier" households:

1. **XGBoost (`xgboost`)**: A powerful, non-linear machine learning model optimized for **prediction**.
2. **Mixed-Effects Logistic Regression (`lme4::glmer`)**: A statistical model designed for **inference and explanation**, which accounts for the hierarchical structure of the data (households within countries).

While the main research relies on `glmer` for its interpretability (as seen in `sensitivity_Kgains.R`), this script answers a critical question: **How much predictive accuracy do we sacrifice for that interpretability?**

By benchmarking the two, we can:

- Validate the use of XGBoost for preliminary feature selection (as described in `doc/HSFC-2023-DOC/new-intro.md`).
- Understand the trade-off between a complex "black-box" predictor and a simpler, interpretable statistical model.

---

### 2. Methodology

To ensure a fair and robust comparison, the script employs the following methodology:

- **K-Fold Cross-Validation**: Instead of a single, potentially biased train/test split, the script uses **5-fold cross-validation**. The data is split into 5 "folds," and the models are trained 5 times, each time using a different fold as the test set. This provides a more stable and reliable estimate of model performance.

- **Standardized Task**: Both models are given the exact same objective: predict the `rents_dummy` variable (using the K-inclusive definition) based on the same set of predictor variables.

- **Comprehensive Metrics**: Performance is evaluated using a standard set of classification metrics, which are averaged across the 5 folds:
  - **AUC (Area Under the ROC Curve)**: The primary metric for overall predictive power, independent of any specific classification threshold. A value of 1.0 is a perfect model; 0.5 is a random guess.
  - **Accuracy**: The overall percentage of correct predictions.
  - **F1-Score**: The harmonic mean of Precision and Recall. This is especially useful for imbalanced datasets where the positive class (petit-rentiers) is rare.

---

### 3. How to Run the Script

1. Open your R console or RStudio.
2. Set your working directory to the root of the `petit-rentier-HFCS` project.
3. Execute the script using the `source()` command:

```r
source("prod/ml_methods/model_comparison_cv.R")
```

The script will print the final summary table to the console and save it to a CSV file.

---

### 4. Outputs

The script generates a single, clean summary file:

- **File**: `prod/ml_methods/output/model_comparison_summary.csv`
- **Content**: A table containing the average performance metrics (AUC, Accuracy, F1-Score) and their standard deviations for both the `glmer` and `xgboost` models.

**Example Output Table:**

| model   | auc   | auc_sd | accuracy | f1_score |
| :------ | :---- | :----- | :------- | :------- |
| glmer   | 0.851 | 0.010  | 0.912    | 0.453    |
| xgboost | 0.925 | 0.008  | 0.921    | 0.621    |

---

### 5. How to Interpret the Results

The output table allows for a direct comparison of the models' predictive capabilities.

- **Expected Result (XGBoost > glmer)**: You should typically expect the `xgboost` model to have a higher AUC and F1-Score. This is because it is a more flexible, non-linear model designed specifically to maximize predictive accuracy. This result confirms that for pure prediction, XGBoost is the superior tool.

- **An Interesting Result (glmer ≈ XGBoost)**: If the performance of `glmer` is very close to that of `xgboost`, it suggests that the simpler, linear relationships and the random effects structure of the `glmer` model are capturing the vast majority of the predictive signal in the data. In this scenario, the additional complexity of XGBoost provides little benefit, strengthening the case for using the more interpretable `glmer` model for the final analysis.

Ultimately, this comparison provides empirical evidence to justify the project's two-pronged methodological approach: using XGBoost for data-driven feature selection and using `glmer` for robust, interpretable, and theory-driven inference.
