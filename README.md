# 📊 Statistical Modeling and Analysis of Telemarketing Campaign for Bank Term Deposits  

## 📝 Project Overview  
This project analyzes a telemarketing campaign by a Portuguese bank that aimed to promote **term deposits**, a fixed-interest investment product providing consistent income for the bank. The objective is to:  
- Predict customers most likely to subscribe to a term deposit.  
- Optimize outreach strategies for cost-effective and targeted telemarketing efforts.  
- Provide actionable insights for future campaigns.  

The analysis leverages **CRISP-DM methodology** and uses statistical models to explore key research questions about customer behavior and campaign effectiveness.  

---

## 📂 Project Structure  

The repository contains the following files:  

- **`Dataset/`**  
  - `train.csv` – Training dataset for model development.  
  - `test.csv` – Test dataset for model validation.  

- **`Research-Analysis-1.R`** – Logistic regression analysis to identify key factors affecting subscription likelihood.  
- **`Research-Analysis-2.R`** – Chi-square test to evaluate demographic differences in subscription rates.  
- **`Research-Analysis-3.R`** – Survival analysis to assess the impact of previous campaign results.  
- **`Research-Analysis-4.R`** – Random Forest model for optimizing contact strategy.  
- **`Research-Analysis-5.R`** – Cluster analysis for customer segmentation based on subscription likelihood.  

- **`FINAL PROJECT REPORT - Statistical Modeling and Analysis of Bank’s Telemarketing Campaigns.pdf`** – Comprehensive report of findings and recommendations.  
- **`Project Proposal.pdf`** – Initial project proposal outlining goals, methods, and significance.  

---

## 📊 Dataset  

The dataset, sourced from the **UCI Machine Learning Repository**, includes customer demographic, financial, and engagement information. Enhancements such as added columns and random sampling have been applied to improve flexibility for research purposes.  

- **Source:** [Bank Marketing Dataset](https://archive.ics.uci.edu/ml/datasets/Bank+Marketing)  
- **Citation:** S. Moro, P. Cortez, and P. Rita, *A Data-Driven Approach to Predict the Success of Bank Telemarketing*, Decision Support Systems, Elsevier, 62:22-31, June 2014.  

---

## 🔍 Research Questions and Models  

### 1. What factors significantly affect a customer’s likelihood of subscribing to a term deposit?  
**Model:** Logistic Regression  
**Method:** Analyze predictor variables (e.g., age, job, balance) to understand their impact on subscription probability.  

### 2. Is there a statistically significant difference in subscription rates across demographic groups?  
**Model:** Chi-Square Test for Independence  
**Method:** Evaluate categorical variables like job, education, and marital status to identify receptive demographic segments.  

### 3. How do previous campaign results impact current subscription rates?  
**Model:** Survival Analysis (Cox Proportional Hazards Model & Kaplan-Meier Analysis)  
**Method:** Investigate how variables like `pdays`: no_of_days_since_last_contact and `previous`: previous_no_of_contacts influence subscription rates over time.  

### 4. What is the optimal contact strategy in terms of frequency and timing?  
**Model:** Random Forest model  
**Method:** Assess the effect of variables like campaign timing and frequency on conversion likelihood.  

### 5. How can customers be segmented based on subscription likelihood?  
**Model:** Cluster Analysis (K-means & Hierarchical Clustering)  
**Method:** Group clients into clusters to design targeted marketing strategies.  

---

## 🌟 Significance  

This project provides a **data-driven foundation** for optimizing telemarketing strategies, enabling the bank to:  
- Improve customer targeting.  
- Reduce marketing costs.  
- Increase subscription rates.  

Moreover, the use of statistical methods offers a replicable framework for analyzing customer behavior, contributing to the field of data-driven marketing.  

---

## 🛠️ Technology Stack  

- **Programming Language:** R  
- **Libraries:** dplyr, reticulate, caret, tidyverse, pROC, ggplot2, car, randomForest, factoextra, NbClust, fpc, survival, survminer, cluster
- **Methodology:** CRISP-DM  

---

## 🚀 How to Run  

1. **Clone the repository:**  
   ```bash
   git clone https://github.com/Praveena1109/Statistical-Analysis-of-Telemarketing-Campaign
   cd Statistical-Analysis-of-Telemarketing-Campaign

2. **Prepare the Environment:**
   Ensure R is installed on your system, and install the required libraries listed below.  
   ```R
   install.packages(c("dplyr", "reticulate", "caret", "tidyverse", "pROC", "ggplot2", "car", "randomForest", "factoextra", "NbClust", "fpc", "survival", "survminer", "cluster"))

3. **Run analyses:**
   Execute the R scripts in sequence and the output will be displayed in the console. 
# Statistical-Analysis-of-Telemarketing-Campaign
