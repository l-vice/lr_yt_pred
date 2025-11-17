# Prediction of Engagement Metrics — Louis Rossmann YouTube Channel
## Machine Learning — Orange3 & R Analysis

Objective
---------
The goal of this project is to predict the **like-to-view ratio** of Louis Rossmann’s YouTube videos as a measure of audience engagement.  
The analysis combines **statistical modeling in R** with **machine learning implementation in Orange3**, integrating rigorous data transformation, diagnostics, and validation workflows.

Full Report
------------
The complete HTML analysis report can be viewed online here:  
[**HTML MIRROR**](https://l-vice.github.io/lr_yt_pred/R/r_markdown/analysis_summary_R.html)

Methodology
------------
Data Preparation:
- Source: YouTube Data API v3  
- Scope: 3,315 videos scraped with complete metadata  
- Cleaning: Removal of non-predictive metadata (tags, categories, location data)  
- Engineered variables:
  - `duration_seconds`, `age_days`, `weekday`, `hour`, `month`, `duration_bucket`, `weekend`
  - Engagement ratios: `like_view_ratio`, `comment_view_ratio`
- Transformations: Applied `log1p`, `sqrt`, and **Box–Cox transformations** to improve normality and reduce kurtosis.

Modeling Framework:
- Environments: R (for preprocessing and diagnostics) and Orange3 (for model fitting)
- Algorithms tested:
  - Linear Regression (OLS / WLS)
  - Random Forest
  - AdaBoost and XGBoost regressors
- Validation: 5-fold cross-validation with 80/20 train–test split
- Metrics: R², RMSE, MAE, MAPE

Diagnostics:
- Heteroskedasticity and residual Q–Q analysis  
- Influence diagnostics via Cook’s distance  
- Normalization through Box–Cox and rank-based weighting  

Results
-------
Two modeling environments were explored to ensure generalizability and feature robustness.

**Weighted Least Squares (WLS) — R Environment (Extended Validation)**  
- Achieved **R² = 0.862** after Box–Cox transformations and rank-based weighting.  
- Confirmed strong linear explanatory power of the transformed variables.  
- Used internally for validation only; not included in the official course submission due to Orange3-only modeling requirements.

**Random Forest Regressor — Orange3**  
- Achieved **R² = 0.70**, **RMSE = 0.014**, **MAE = 0.010**, **MAPE = 23.4%** on the test set.  
- Demonstrated the highest cross-validation stability and generalization among ensemble models.  
- Served as the official final model submitted in the coursework.

Insights
--------
- Engagement is **non-linearly dependent** on video duration and comment ratio.  
- **Box–Cox transformations** improved model normality and residual variance behavior.  
- The **Random Forest** model effectively captured non-linear interactions between predictors.  
- The WLS result (R² = 0.862) validated that feature engineering in R was statistically sound and transferable across modeling frameworks.  
- Short-to-medium videos (<8 minutes) showed the strongest normalized engagement levels, consistent with viewing behavior patterns.

Tools
-----
Languages: R, Python (Orange3 GUI)  
Platforms: RStudio and Orange3  
Version Control: Git and GitHub

References
----------
De Jonge, E., & Van Der Loo, M. (2013). *An introduction to data cleaning with R.* Heerlen: Statistics Netherlands.  

Phad, K., & Thaker, H. (2023). *Exploring Orange: A Comprehensive Study on Open-Source Data Mining for Visual Analytics.*  

Sanyour, R., Abdullah, M., & El Emary, I. M. (2024). *Assessing Sentiment in YouTube Video Content: A Title and Description Analysis Approach to Analyze User Reactions.* International Journal for Applied Information Management, 4(4), 229–245.  

Seidel, N. (2024). *Short, long, and segmented learning videos: From YouTube practice to enhanced video players.* Technology, Knowledge and Learning, 29(4), 1965–1991.  

Demšar, J., Curk, T., Erjavec, A., Gorup, C., Hocevar, T., Milutinovic, M., … Zupan, B. (2013). *Orange: Data mining toolbox in Python.* Journal of Machine Learning Research, 14, 2349–2353.  

Chen, T., & Guestrin, C. (2016). *XGBoost: A scalable tree boosting system.* In *Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining* (pp. 785–794). ACM. 

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning: With applications in R. Springer.

Author
------
Luka Bojovic  
Email: lbojovic2@gmail.com
