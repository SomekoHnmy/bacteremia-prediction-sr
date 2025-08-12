# bacteremia-prediction-sr
Python and R code for a systematic review of bacteremia prediction models.

## Repository Structure

### Meta-Analysis Code (bacteremia_sr_meta_analysis.r)

**Purpose**: Statistical meta-analysis of prediction model performance

**Key Features**:
- Data preprocessing and cleaning for systematic review datasets
- C-statistics meta-analysis using metamisc and valmeta
- O:E ratio (observed-to-expected events) meta-analysis
- Forest plot generation for discriminative and calibration performance
- PROBAST (Prediction model Risk Of Bias ASsessment Tool) risk assessment visualization
- Comprehensive summary statistics and model comparison tables

**Main Outputs**:
- Forest plots for C-statistics and O:E ratios by prediction model
- PROBAST risk of bias and applicability assessment charts
- Summary tables with pooled estimates and confidence intervals

### AI-Assisted Screening (bacteremia_prediction_sr_preprocess_for_screening_with_ai.ipynb)

**Purpose**: Full-text screening using large language models

**Key Features**:
- PDF text extraction using Adobe PDF Services API
- Full-text preprocessing and segmentation
- Claude 3.7 Sonnet integration for automated screening decisions
- Structured output parsing with inclusion/exclusion reasoning
- Progress tracking and checkpoint saving for large datasets

**Workflow**:
1. Extract text from PDF files using Adobe API
2. Clean and segment extracted text
3. Apply AI screening with structured prompts
4. Generate screening decisions with detailed justifications

### Machine Learning Filter (bacteremia_sr_machine_learning_filter_creation.ipynb)

**Purpose**: Iterative machine learning-assisted title/abstract screening

**Key Features**:
- BibTeX database processing and entry management
- TF-IDF vectorization for text preprocessing
- LightGBM classifier with hyperparameter optimization using Optuna
- High-recall optimization (F-beta score with β=4)
- Iterative training on manually screened batches
- PMID validation against known relevant studies

**Workflow**:
1. Process BibTeX databases and extract random samples
2. Manual screening in Rayyan for training data
3. Train LightGBM classifiers with optimized hyperparameters
4. Apply filters to remaining literature
5. Iterative refinement with additional manual screening rounds

## Technical Requirements

### R Dependencies

```r
# Meta-analysis packages
library(metafor)
library(metamisc)
library(meta)

# Data manipulation and visualization
library(dplyr)
library(ggplot2)
library(gtsummary)

# File I/O
library(readxl)
library(knitr)
```

### Python Dependencies

```python
# PDF processing
pdfservices-sdk

# AI integration
anthropic
openai

# Machine learning
scikit-learn
lightgbm
optuna

# Data processing
pandas
bibtexparser
rispy

# Text processing
tqdm
pydantic
```

## Getting Started

### Meta-Analysis

```r
# Load your cleaned dataset
cleaned_data <- readRDS("path/to/your/cleaned_data.rds")

# Run meta-analysis for C-statistics
create_forest_plot("qSOFA", meta_analysis_data)

# Generate PROBAST visualizations
rob_chart <- create_rob_chart(external_validation)
```

### AI-Assisted Screening

```python
# Configure AI client
client = OpenAI(
    base_url="https://openrouter.ai/api/v1",
    api_key="your_api_key"
)

# Process full-text screening
result_df = process_fulltext_screening(
    df=df_cleaned,
    system_prompt=system_prompt,
    client=client
)
```

### Machine Learning Filtering

```python
# Train ML classifier
model, vectorizer, threshold, params = train_final_model(
    rayyan_decided_df,
    beta_value=4,  # High recall optimization
    n_trials=50
)

# Apply to new literature
filtered_results = predict_and_summarize(
    new_df, create_json_text, vectorizer, model, threshold
)
```

## Key Outputs

### Meta-Analysis Results

- Forest Plots: Visual representation of pooled C-statistics and O:E ratios
- Summary Tables: Comprehensive model performance metrics
- Risk Assessment: PROBAST-based quality evaluation charts

### Screening Results

- AI Decisions: Structured inclusion/exclusion decisions with reasoning
- ML Predictions: Probability scores for literature relevance
- Performance Metrics: Sensitivity, specificity, and F-beta scores

## Methodology Highlights

### Statistical Approach

- Random-effects meta-analysis using REML estimation
- Prediction intervals for between-study heterogeneity
- Subgroup analysis by model type and validation approach

### AI Integration

- Structured prompting for consistent screening decisions
- Error handling with retry mechanisms and checkpoint saving
- Validation against known relevant studies (PMID matching)

### Machine Learning

- Iterative training with expanding datasets
- Hyperparameter optimization using Bayesian optimization
- High-recall design to minimize false negatives in screening

## Citation

If you use this code in your research, please cite our systematic review:

```
[Citation information will be added upon publication]
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contributing

We welcome contributions to improve the methodology and code quality. Please:
1. Fork the repository
2. Create a feature branch
3. Submit a pull request with detailed description

## Contact

For questions about the methodology or code implementation, please contact:
[Your contact information]

---

**Note**: This repository represents a complete computational workflow for systematic reviews in medical AI. Each component can be adapted for other systematic review topics with appropriate modifications to search strategies and inclusion criteria.
