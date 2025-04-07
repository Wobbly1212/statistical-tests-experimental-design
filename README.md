📊 Experimental Design with R
Comprehensive Statistical Testing and Visualization Toolkit

Author: Mohammad Hossein Darabi

This repository provides a complete R-based toolkit for analyzing experimental data using statistical tests. It walks through everything from visual exploration to assumptions checking, hypothesis testing, and post-hoc analysis — including both **parametric** and **non-parametric** methods.

It is perfect for students, researchers, or data enthusiasts working with group comparisons, repeated measures, or mixed designs.

---

## 📁 Datasets Used

This project utilizes both built-in and custom datasets, including:

| Dataset            | Description                                                   | Source               |
|--------------------|---------------------------------------------------------------|----------------------|
| ToothGrowth        | Tooth length by supplement and dose                           | `datasets` package   |
| PlantGrowth        | Weights of plants under different conditions                  | `datasets` package   |
| mice2              | Mouse weights before and after treatment                      | `datarium` package   |
| selfesteem         | Self-esteem scores at 3 time points                           | `datarium` package   |
| anxiety            | Anxiety scores across physical activity levels and time       | `datarium` package   |
| poison.data        | Survival time under different poisons and treatments          | `BHH2` package       |
| dental, rat, respiration | Experimental data for advanced non-parametric analysis | `nparLD` package     |

---

## 🔍 Key Topics Covered

### ✅ Parametric Tests
- One-sample t-test
- Two-sample t-test (independent and paired)
- One-way and two-way ANOVA
- Repeated measures ANOVA
- Mixed ANOVA (between and within factors)
- Post-hoc tests (Tukey HSD, Bonferroni-adjusted t-tests)

### ✅ Non-Parametric Alternatives
- Wilcoxon signed-rank test
- Mann-Whitney U test
- Kruskal-Wallis test
- Friedman test (non-parametric repeated measures)
- Non-parametric mixed ANOVA using the `nparLD` package

---

## 🧪 Assumptions Tested

Before each parametric test, the following assumptions are carefully verified:
- **Normality**: Shapiro-Wilk, Jarque-Bera, Anderson-Darling, Lilliefors
- **Homogeneity of variances**: Bartlett, Fligner-Killeen, Levene test
- **Outliers**: Detected using Z-scores and IQR
- **Sphericity** (for repeated measures): Mauchly's test

---

## 📊 Visualizations

The script includes high-quality visualizations using `ggplot2`, `ggpubr`, and base R:
- Histograms
- Boxplots (grouped and colored)
- Interaction plots
- Profile plots for paired data
- Density overlays

---

## 🧭 Project Structure

The R script is organized in clear, commented sections:
1. **Exploratory Data Analysis (EDA)**
2. **Outlier Detection**
3. **Parametric Tests (t-tests and ANOVA)**
4. **Non-Parametric Tests**
5. **Mixed and Repeated Measures Designs**
6. **Advanced Non-Parametric Models with `nparLD`**
7. **Post-Hoc Comparisons**

You can run it sequentially or jump to the section relevant to your data and design.

---

## ⚙️ Requirements

Install the following packages before running the script:

```r
install.packages(c(
  "ggplot2", "ggpubr", "tseries", "nortest", "DescTools", "multcompView",
  "car", "rstatix", "datarium", "PairedData", "reshape", "reshape2",
  "nlme", "BHH2", "tidyverse", "nparLD"
))
```

---

## ▶️ How to Use

1. Clone or download this repository.
2. Open `Comparison of Groups.R` in RStudio.
3. Run section by section based on your use case.
4. Ensure `sonno-long.csv` (if used) is in your working directory.

---

## 🎯 Use Cases

- Analyze experiments with multiple groups
- Run repeated measures designs
- Explore data visually and statistically
- Learn or teach applied statistics in R

---

## 🙏 Credits

This resource is built using:
- R core packages (`datasets`, `ggplot2`, `stats`)
- Educational data from `datarium`, `BHH2`, and `nparLD`
- Online materials from STHDA and CRAN documentation

Special thanks to the R and data science community for open resources and powerful libraries.

---

## 📘 License

MIT License. You are free to use, share, and modify.
