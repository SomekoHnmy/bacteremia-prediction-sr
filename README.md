# bacteremia-prediction-sr
Python and R code for a systematic review of bacteremia prediction models.

This repository contains all code used in our systematic review of bacteremia prediction models:
- **01_ml_filter/**: Machine learning–assisted screening filter (Python; TF-IDF + LightGBM, Optuna)
- **02_ai_assist/**: AI-assisted full-text screening and data extraction (Python)
- **03_meta_analysis/**: Meta-analysis of model performance (R; `metamisc`, `metafor`, `meta`, etc.)

## Repository Structure

```

01\_ml\_filter/
bacteremia\_sr\_machine\_learning\_filter\_creation.ipynb   # ML filter training & application
02\_ai\_assist/
bacteremia\_prediction\_sr\_preprocess\_for\_screening\_with\_ai.ipynb  # AI-assisted screening/data extraction
03\_meta\_analysis/
meta\_analysis.R                                        # R analysis script
README.md

```

## Data Inputs

- **Deduplicated RIS** exported from Mendeley after uploading RIS files from **multiple databases** and removing duplicates.
- **Training labels**: 500 randomly sampled records (excluding the initial 50 trial records) screened by humans to create labeled data.

## Environment

### Python (for 01_ml_filter and 02_ai_assist)
- Python **3.13.2**
- Packages: `scikit-learn 1.5.2`, `optuna 4.1.0`, `lightgbm 4.5.0`, `pandas`, `numpy`, `scipy`, `python-dotenv`

### R (for 03\_meta\_analysis)

* R **4.4.2**
* CRAN packages used by `meta_analysis.R`:

  * `readxl`, `dplyr`, `ggplot2`, `tidyr`,
    `metamisc`, `pROC`, `gridExtra`, `gtsummary`, `lubridate`, `purrr`, `forcats`, `knitr`, `kableExtra`

Install (example):

```r
install.packages(c(
  "metafor","readxl","dplyr","ggplot2","tidyr","meta","forestplot",
  "metamisc","pROC","gridExtra","gtsummary","lubridate","purrr",
  "forcats","knitr","kableExtra"
))
```

---

## Reproducible Workflow

### 0) Pre-processing & Trial Screening (documentation)

1. Export RIS from MEDLINE/CENTRAL/EMBASE (and others).
2. Upload RIS files to **Mendeley** → **deduplicate** → export the **cleaned RIS**.
3. Randomly sample **50** records for a **trial screening** (to refine criteria); **exclude** these from training.
4. Randomly sample **500** records (excluding the initial 50) and perform human screening to create labels.

### 1) ML Filter (01\_ml\_filter)

Notebook: `01_ml_filter/bacteremia_sr_machine_learning_filter_creation.ipynb`

* Text features: **Title + Abstract → TF-IDF**
* Classifier: **LightGBM**
* Tuning: **Optuna**, recall-oriented (high sensitivity to minimize missed eligible studies)
* Inputs:

  * `data/input/deduplicated_references.ris`
  * `data/input/labels_training_500.csv`
* Outputs (example):

  * `data/output/predictions.csv` (record\_id, score, rank)
  * `data/output/model.pkl`

> Run the notebook cell-by-cell. 

### 2) AI-assisted Full-text Screening / Data Extraction (02\_ai\_assist)

Notebook: `02_ai_assist/bacteremia_prediction_sr_preprocess_for_screening_with_ai.ipynb`

* Requires API key (e.g., `OPENAI_API_KEY`) in `.env` (do **not** commit secrets).
* Typical steps:

  * Pre-rank using `predictions.csv` (e.g., take top-K)
  * Fetch/attach full texts (not included here)
  * Apply standardized prompts/templates to assist screening/extraction
* Outputs (example):

  * `data/output/extracted_items.csv`
  * Logs and prompt templates under `02_ai_assist/PROMPTS/` (if applicable)

### 3) Meta-analysis (03\_meta\_analysis)

Script: `03_meta_analysis/meta_analysis.R`

* Set your input Excel path near the top:

  ```r
  file_path <- "your_file_path_here.xlsx"  # Replace with actual
  ```

* The script:

  * Parses dates, coerces types, cleans categories/labels
  * Splits rows for studies that reported **multiple model types**
  * Builds summary tables (`gtsummary`) for **internal** and **external** validation subsets
  * Performs **meta-analysis of C-statistics** (`valmeta(measure="cstat")`)
  * Performs **meta-analysis of O\:E ratio** (`valmeta(measure="OE")`) using `oecalc`
  * Produces **forest plots** (built-in `plot()` and customized `metamisc::forest`)
  * Summarizes **PROBAST** Risk of Bias and Applicability
  * Saves tables/figures to disk

* Edit output paths in the script (replace placeholders like `your_output_path/` and `your/output/folder/path`) so results land under:

  ```
  data/output/
    table1.docx
    probast_rob_chart.png
    probast_applicability_chart.png
    forest_plots/*.png
    basic_summary_with_probast.csv
  ```

> The script prints summaries to console and uses `View()` for interactive inspection. When running headless (e.g., CI), you may comment out `View()` lines.

---

## Notes & Good Practices

* **Reproducibility**: fix random seeds (Python notebooks) and document TF-IDF settings (ngram range, stopwords, max\_features).
* **High-recall objective**: explicitly note thresholding strategy for screening (sensitivity vs. workload trade-off).
* **Do not commit secrets**: keep API keys in `.env` and add `.env` to `.gitignore`.
* **Large binary outputs**: avoid committing heavy artifacts (e.g., `.pkl`, `.docx`, `.png`) unless essential. Prefer writing to `data/output/` and ignoring via `.gitignore`.
* **Clearing notebook outputs**: before committing, clear outputs (VS Code: *Command Palette → Notebook: Clear All Outputs*).

Example `.gitignore`:

```
.venv/
.env
__pycache__/
.ipynb_checkpoints/
data/output/
*.pkl
*.docx
*.png
*.csv
```

---

## License

* **Code**: MIT (or your preference)
* **Text/Figures**: CC BY 4.0 (adjust as needed)
