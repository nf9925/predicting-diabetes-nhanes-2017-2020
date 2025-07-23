# End-to-end RStudio analysis of NHANES data to predict diabetes—SMOTE-balanced training, multiple ML models, and performance benchmarking.

# Predictive Modeling for Diagnosing Diabetes (NHANES 2017–March 2020)

This project compares classic statistical models and modern machine-learning methods to predict physician-diagnosed diabetes using pre-pandemic NHANES data. The entire workflow was built in **RStudio** for a graduate Data Mining in Healthcare course.

---

## 🎯 Objective
- **Task:** Binary classification — `DIQ010` (1 = doctor said you have diabetes, 0 = no)  
- **Goal:** Identify which modeling approach best predicts diabetes status and which predictors matter most.

---

## 📦 Data
- **Source:** National Health and Nutrition Examination Survey (NHANES) 2017–March 2020 pre-pandemic cycle (Centers for Disease Control and Prevention [CDC], 2024).  
- **Merge key:** `SEQN`  
- **Population:** Adults ≥ 20 years  
- **Outcome variable:** `DIQ010` (doctor-diagnosed diabetes)

### Predictor Domains (motivated by prior literature)
- **Demographic:** Age, sex, race/ethnicity, education (Andreoletti et al., 2015; Tao et al., 2022)  
- **Questionnaire:** Income, smoking status, physical activity (PaPathanasiou et al., 2013; Park & Kim, 2021)  
- **Examination:** BMI, systolic/diastolic blood pressure (Kamura et al., 2016; Hagberg et al., 1984)  
- **Laboratory:** Total cholesterol, fasting glucose, hs-CRP (Solano & Goldberg, 2006; Albosta & Bakke, 2021; Pan et al., 2017)

---

## 🧼 Preprocessing Pipeline
- Removed/recoded invalid or impossible codes; median imputation for selected continuous variables  
- **Handled class imbalance with SMOTE** on the training set  
- Train/test split: **70% / 30%** using `caret::createDataPartition`  
- Feature engineering (dummy variables / collapsed categories) where needed

---

## 🧪 Models Compared
- **Logistic Regression** (full, reduced, interaction, male-only, etc.)  
- **Shrinkage:** LASSO, Ridge  
- **Tree-Based:** Decision Tree, Random Forest, Gradient Boosting (GBM), XGBoost  
- **Ensemble:** Stacked model (tree/boosting outputs fed into a logistic meta-model)

---

## 📈 Performance Summary

### Logistic Regression Family (sorted by AIC)
| Model                         | AIC    | Accuracy | AUC_ROC | Sensitivity | Specificity |
|-------------------------------|--------|----------|---------|-------------|-------------|
| Log_Model_Male_Only           | 510.48 | 0.880    | 0.973   | 0.941       | 0.876       |
| Demo_Lab_Model                | 685.83 | 0.855    | 0.963   | 0.941       | 0.849       |
| Interaction_Model             | 770.59 | 0.865    | 0.969   | 0.941       | 0.860       |
| Lab_Exam_Model                | 874.12 | 0.891    | 0.953   | 0.941       | 0.888       |
| BP_Glu_Interaction_Model      | 886.12 | 0.876    | 0.938   | 0.882       | 0.876       |
| Biologically_Relevant_Model   | 911.54 | 0.902    | 0.941   | 0.882       | 0.903       |
| Demo_Only_Model               | 1166.46| 0.673    | 0.673   | 0.588       | 0.678       |
| Demo_AGR_Model                | 1277.60| 0.680    | 0.713   | 0.647       | 0.682       |
| Exam_Only_Model               | 1410.34| 0.662    | 0.651   | 0.529       | 0.671       |

### Shrinkage Methods
| Model | AIC   | Accuracy | AUC_ROC | Sensitivity | Specificity |
|-------|-------|----------|---------|-------------|-------------|
| LASSO | 40.37 | 1.000    | 1.000   | 1.000       | 1.000       |
| Ridge | 99.03 | 0.996    | 1.000   | 1.000       | 0.996       |

### Tree-Based Models
| Model          | AIC   | Accuracy | AUC_ROC | Sensitivity | Specificity |
|----------------|-------|----------|---------|-------------|-------------|
| Decision Tree  | 20.00 | 1.000    | 1.000   | 1.000       | 1.000       |
| GBM            | 60.44 | 1.000    | 1.000   | 1.000       | 1.000       |
| XGBoost        | 83.66 | 1.000    | 1.000   | 1.000       | 1.000       |
| Random Forest  |112.33 | 1.000    | 1.000   | 1.000       | 1.000       |

### Ensemble
| Model             | AIC   | Accuracy | AUC_ROC | Sensitivity | Specificity |
|-------------------|-------|----------|---------|-------------|-------------|
| Stacked_Ensemble  | 11.26 | 1.000    | 1.000   | 1.000       | 1.000       |

> *I’m aware the 1.000s likely reflect SMOTE-driven class balance and model complexity; results should be interpreted with caution and validated externally.*

---

## ✅ Conclusion

Across every modeling family, performance was consistently high—but a few patterns stood out:

1. **Stacked Ensemble = Top Performer.** It achieved the lowest AIC (11.26) and perfect test metrics (Accuracy/AUC = 1.00), suggesting exceptional fit—but also a red flag for overfitting without external validation.

2. **Shrinkage Wins on Parsimony.** LASSO delivered a perfect AUC with a dramatically lower AIC (40.37) than Ridge (99.03), indicating it trimmed noise predictors effectively while keeping signal strong.

3. **Tree-Based Models Were Also “Too Good.”** Decision Tree, GBM, XGBoost, and Random Forest all scored 1.00 on Accuracy/AUC, but with varying AICs (Decision Tree lowest at 20.00). Again: impressive, but suspiciously perfect.

4. **Logistic Variants Still Held Their Own.** Among traditional models, the male-only logistic model (AIC 510.48, AUC 0.973) led the pack, while simpler demographic-only specs performed poorly—confirming richer feature sets matter.

5. **Key Predictors Recurred.** Age, BMI, fasting glucose, and select demographic variables consistently carried weight across models, aligning with prior literature on diabetes risk.

**Bottom line:** Ensemble and shrinkage techniques outperformed plain logistic regression on this dataset, but the prevalence of perfect scores means future work should focus on:
- External/temporal validation (e.g., another NHANES wave or a longitudinal cohort),
- Model calibration and interpretability (e.g., SHAP values),
- Guarding against leakage or overfitting in preprocessing.

Until then, treat these “perfect” models as proof-of-concept, not production-ready tools.


![ROC for LR](https://github.com/user-attachments/assets/4dc0dacb-ba42-4be4-9097-7831d46b2355)

![ROC for SM](https://github.com/user-attachments/assets/e74834c9-3e67-4976-90f3-d348672bc160)

![ROC for TBM](https://github.com/user-attachments/assets/876db3dc-9b7a-46d9-9ec4-6f9c0955d099)

![ROC for SEM](https://github.com/user-attachments/assets/b93dcea8-a9be-45cd-b9b4-096646d346c8)

References (APA 7th ed.)

Albosta, M., & Bakke, J. (2021). Intermittent fasting: Is there a role in the treatment of diabetes? A review of the literature and guide for primary care physicians. Clinical Diabetes and Endocrinology, 7(1), 1–8. https://doi.org/10.1186/s40842-020-00116-1

Andreoletti, C., Leszczynski, J. P., & Disch, W. B. (2015). Gender, race, and age: The content of compound stereotypes across the life span. International Journal of Aging and Human Development, 81(1–2), 27–53. https://doi.org/10.1177/0091415015616395

Centers for Disease Control and Prevention. (2024). Diabetes: 38 million people have diabetes; Prediabetes. https://www.cdc.gov/diabetes-prevention/

Hagberg, J. M., Ehsani, A. A., Goldring, D., Hernandez, A., Sinacore, D. R., Holloszy, J. O., & Louis, S. (1984). Effect of weight training on blood pressure hemodynamics in hypertensive adolescents. Journal of Pediatrics, 104(1), 147–152. https://doi.org/10.1016/S0022-3476(84)80615-0

Kamura, Y., Iwata, M., Maeda, S., Shinmura, S., Koshimizu, Y., Honoki, H., Fukuda, K., Ishiki, M., Usui, I., Fukushima, Y., Takano, A., Kato, H., Murakami, S., Higuchi, K., Kobashi, C., & Tobe, K. (2016). FTO gene polymorphism is associated with type 2 diabetes through its effect on increasing the maximum BMI in Japanese men. PLOS ONE, 11(11), e0165523. https://doi.org/10.1371/journal.pone.0165523

Pan, A., Wang, Y., Yuan, J.-M., & Koh, W.-P. (2017). High-sensitive C-reactive protein and risk of incident type 2 diabetes: A case–control study nested within the Singapore Chinese Health Study. BMC Endocrine Disorders, 17(1), 1–9. https://doi.org/10.1186/s12902-017-0159-5

PaPathanasiou, G., Georgakopoulos, D., Papageorgiou, E., Zerva, E., Michalis, L., Kalfakakou, V., & Evangelou, A. (2013). Effects of smoking on heart rate at rest and during exercise, and on heart rate recovery, in young adults. Hellenic Journal of Cardiology, 54(3), 168–177.

Park, A. (2024, September 4). The weight loss drug that can prevent diabetes. TIME.

Park, S. H., & Kim, C. G. (2021). What types of exercise are more effective in reducing obesity and blood pressure for middle-aged women? A systematic review with meta-analysis. Biological Research for Nursing, 23(4), 658–675. https://doi.org/10.1177/10998004211015424

Riley, L. (2024). Mean fasting blood glucose. World Health Organization. https://www.who.int/data/gho/indicator-metadata-registry/imr-details/2380

Solano, M. P., & Goldberg, R. B. (2006). Lipid management in type 2 diabetes. Clinical Diabetes, 24(1), 27–32. https://doi.org/10.2337/diaclin.24.1.27

Tao, M. H., Liu, J. L., & Nguyen, U. S. D. T. (2022). Trends in diet quality by race/ethnicity among adults in the United States for 2011–2018. Nutrients, 14(19), 1–14. https://doi.org/10.3390/nu14194178

Zhai, Y., Zhang, Y., Chu, Z., Geng, B., Almaawali, M., Fulmer, R., Lin, Y. W. D., Xu, Z., Daniels, A. D., Liu, Y., Chen, Q., & Du, X. (2024). Machine learning predictive models to guide prevention and intervention allocation for anxiety and depressive disorders among college students. Journal of Counseling and Development. https://doi.org/10.1002/jcad.12543

Zhou, B., Rayner, A. W., Gregg, E. W., Sheffer, K. E., Carrillo-Larco, R. M., Bennett, J. E., Shaw, J. E., Paciorek, C. J., Singleton, R. K., Barradas Pires, A., Stevens, G. A., Danaei, G., Lhoste, V. P., Phelps, N. H., Heap, R. A., Jain, L., D’Ailhaud De Brisis, Y., Galeazzi, A., Kengne, A. P., … Ezzati, M. (2024). Worldwide trends in diabetes prevalence and treatment from 1990 to 2022: A pooled analysis of 1108 population-representative studies with 141 million participants. The Lancet, 404(10467), 2077–2093. https://doi.org/10.1016/S0140-6736(24)02317-1
