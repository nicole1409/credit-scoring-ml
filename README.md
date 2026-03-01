# Credit Scoring with Machine Learning 🏦📊

**Comprehensive ML pipeline** for credit risk classification (Good/Standard/Poor) on 100,000 banking records.

**Team project**: Perrelli, Santero, Moretti – collaborative analysis.

## Project Overview
- **Dataset**: [Kaggle Credit Score Classification](https://www.kaggle.com/datasets/parisrohan/credit-score-classification) (28 vars, 100k instances)
- **Goal**: Predict credit profiles using unsupervised + supervised ML methods
- **Language**: R (libraries: mice, randomForest, kernlab, keras, dbscan, cluster)

## Pipeline
1. **Pre-processing**:
   - MICE imputation (non-parametric)
   - Outlier removal (IQR method)
   - Random Forest feature selection → 11 key variables
   - Standardization

2. **Unsupervised Clustering**:
   - K-Means, K-Medians (Euclidean/Manhattan)
   - Hierarchical Agglomerative (Ward + Manhattan)  
   - DBscan (eps=2.05, minPts=13)
   - **Results**: ~0.63 cluster purity across all methods

3. **Supervised Classification** (80/20 train/test split):
   | Model            | Test Accuracy | Notes                          |
   |------------------|---------------|--------------------------------|
   | **Radial SVM**   | **70.3%**     | C=1, sigma=0.1 (best performer)|
   | Random Forest    | 69.5%         | ntree=2000, mtry=2             |
   | Neural Network   | 68.7%         | 512 ReLU neurons, 50 epochs    |
   | KNN              | 65.4%         | k=31                           |

## Repository Contents
- `Codice.R` → Full R script (preprocessing, clustering, classification, evaluation)
- `Report.docx` → Detailed analysis with confusion matrices, plots, metrics
- `README.md` → This file

## How to Run
```r
# Install required packages
install.packages(c("openxlsx", "mice", "caret", "randomForest", 
                   "kernlab", "keras", "dbscan", "cluster"))

# Download dataset from Kaggle, save as "train.xlsx"  
# Run full pipeline
source("Codice.R")
