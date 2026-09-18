# 🍄 Mushroom Edibility Classification
**KNN & Random Forest with R-Shiny Dashboard**

> Data Mining & Visualization - 
> Institut Teknologi Sepuluh Nopember (ITS) Surabaya

---

## 📌 Project Overview
This project classifies mushrooms as **edible or poisonous** using the UCI Mushroom Classification dataset (8,124 samples, 23 categorical features). Two classification models — **K-Nearest Neighbor (KNN)** and **Random Forest** — were compared using Repeated Holdout and K-Fold Cross-Validation. Feature selection was performed using **Recursive Feature Elimination (RFE)** to identify the 10 most informative features. An interactive **R-Shiny dashboard** was also built for data exploration, visualization, and real-time prediction.


---

## 🔧 Tools & Libraries

**Python**
- `pandas`, `numpy` — data processing
- `scikit-learn` — KNN, Random Forest, RFE, cross-validation, evaluation metrics
- `matplotlib`, `seaborn` — visualization

**R**
- `shiny`, `bslib`, `shinyjs` — dashboard framework
- `randomForest` — Random Forest model
- `plotly`, `DT`, `tidyverse` — visualization and data table

---

## 📊 Results

| Model | Evaluation | Accuracy | Precision | Recall | AUC |
|---|---|---|---|---|---|
| KNN | Holdout | 1.00 | 1.00 | 1.00 | 1.00 |
| KNN | K-Fold CV | 1.00 | 1.00 | 0.9993 | 1.00 |
| Random Forest | Holdout | 1.00 | 1.00 | 0.99 | 1.00 |
| Random Forest | K-Fold CV | 0.9970 | 1.00 | 0.9993 | 1.00 |

Both models achieved near-perfect performance, attributed to high class separability in the dataset and effective feature selection via RFE.

---

## 🖥️ R-Shiny Dashboard
The dashboard includes 5 interactive modules:
- **Summary Statistics** — descriptive stats for all variables
- **Visual Charts** — distribution and correlation heatmap
- **Prediction** — real-time edibility prediction using top 10 features
- **Database** — full mushroom dataset with filters
- **Authors** — team information

🔗 [View Dashboard Here!](https://akbarrazan.shinyapps.io/mushrooms_dashboards/)

---

## 📂 Dataset
Source: [UCI Mushroom Classification — Kaggle](https://www.kaggle.com/datasets/uciml/mushroom-classification)
