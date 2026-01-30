# Household Expense Tracker

A local-first, privacy-focused [R Shiny](https://shiny.posit.co/) application for tracking household expenses, planning budgets, and analyzing financial habits.

This tool is designed to be simple, robust, and completely offline—your financial data never leaves your computer.

## Table of Contents
- [User Guide](#user-guide)
  - [Installation & Launch](#installation--launch)
  - [Using the Application](#using-the-application)
- [Technical Documentation](#technical-documentation)
  - [Architecture](#architecture)
  - [Data Model](#data-model)
  - [Desktop Application](#desktop-application)
- [Data Files](#data-files)
- [Acknowledgement](#acknowledgement)

---

## User Guide

### Installation & Launch

#### 1. Mac Desktop App (Recommended)
This project includes a standalone macOS application bundle.
1.  Locate `Household Budgeting.app` in the project root.
2.  Drag it to your **Applications** folder or **Dock**.
3.  Click the icon to launch! A Terminal window will open (handling the background process), and the app will open in your default browser.

#### 2. Manual Launch (Developer Mode)
If you prefer running it from the source or are on Windows/Linux:
1.  Install R (version 4.0+).
2.  Install dependencies:
    ```r
    install.packages(c("shiny", "DT", "readr", "dplyr", "lubridate", "ggplot2", "scales", "stringdist", "shinyjs"))
    ```
3.  Run the app:
    ```bash
    Rscript run_app.R
    ```

### Using the Application

#### **Expenses Tab**
- **Log an Expense:** Manually enter transaction details. Categories and Payers are saved automatically for future use.
- **Manage History:** The "Recorded expenses" table shows your history. You can edit invalid entries directly in the table or delete them.

#### **Settings Tab (CSV Import)**
- **Import Statements:** Drag and drop your Bank or Credit Card CSV files here.
- **Smart Staging:**
    - The app automatically parses common formats (e.g., Credit Card statements).
    - **Payer Assignment:** It intelligently maps "Member Name" columns (e.g., "CARSON SLATER" or "CHLOE SLATER") to the correct Payer.
    - **Duplicate Detection:** It flags transactions that match existing records by Date, Amount, and Description.
    - **Review:** You can edit or delete items in the staging area before finalizing the import.

#### **Budgeting Tab**
- **Income:** Set your expected monthly income.
- **Budget Lines:** Create budget limits for specific Categories/Subcategories.
- **Frequency:** set budgets as *Monthly*, *Quarterly*, or *Annually*.
- **Effective Dates:** Budgets are time-variant. A budget set with an Effective Date of "Jan 1, 2026" applies only to that month onwards, preserving historical budget accuracy.

#### **Reporting Tab**
- **Monthly Summary:** At the top, you'll see a high-level "Scorecard" for the selected month:
    - **Total Budget:** Sum of all active monthly limits.
    - **Total Spent:** Sum of all expenses.
    - **Result:** Green ("Under Budget") or Red ("Over Budget").
- **Performance Table:** Detailed breakdown of spending vs. budget per category.
- **Spending Trends:** Interactive charts showing your spending over time.

---

## Technical Documentation

This section is for developers modifying the codebase.

### Architecture
The app is a standard **R Shiny** application structured as follows:
- **`run_app.R`**: The entry point. Handles dependency checking, port selection (`SHINY_PORT`), and browser launching.
- **`app/app.R`**: Contains the monolithic Shiny UI and Server logic.
    - **UI**: Uses `navbarPage` for tabbed navigation. Uses `DT` for interactive tables.
    - **Server**: Uses `reactiveVal` for in-memory state management of Expenses and Budgets.

### Data Model
Data persistence is handled via CSV files in the `data/` directory. There is no database; the app loads CSVs into memory on startup and rewrites them on every save.

- **`expenses.csv`**: Flat list of transactions.
- **`budgets.csv`**: Stores budget definitions.
    - **SCD Type 2 Logic**: Budgets uses `EffectiveDate` to track changes over time. When generating reports, the app filters for budgets where `EffectiveDate <= ReportMonth` and takes the most recent entry for each Category.
    - **Normalization**: Annual budgets are divided by 12 dynamically for monthly reporting views.

### Desktop Application
The `Household Budgeting.app` is a standard macOS Bundle created via the `create_app.sh` script.

1.  **Structure**: Standard `.app` directory structure (`Contents/MacOS`, `Contents/Resources`).
2.  **Launcher**: `Contents/MacOS/launcher` is a Bash script.
    - It is generated dynamically to hardcode the project path.
    - It uses **AppleScript (`osascript`)** to mistakenly launch a Terminal window. This is a deliberate design choice to bypass macOS App Sandbox permissions, ensuring the app can read/write to the user's Documents folder without code signing.
3.  **Icon**: The script converts a source PNG into a multi-resolution `.icns` file using `sips` and `iconutil`.

---

## Data Files

- `data/expenses.csv`: Primary storage. Ignored by git.
- `data/expenses_backup.csv`: Created automatically before saving changes.
- `data/category_budget.csv`: Historical budget definitions.
- `data/income_sources.csv`: Income settings.

---

## File Structure

```text
.
├── Household Budgeting.app/  # macOS Application Bundle
├── README.md                 # Project Documentation
├── app/
│   ├── app.R                 # Main Shiny Application Code
│   └── www/                  # Static assets (icons)
├── create_app.sh             # Script to build the Mac App
├── data/                     # Local data storage (CSVs)
│   ├── budgets.csv
│   ├── expenses.csv
│   └── income_sources.csv
├── desktop_app.py            # Python wrapper (alternative launcher)
├── run_app.R                 # R Entry point
└── tools/
    └── install_shortcut.R    # Legacy shortcut installer
```

---

## Acknowledgement

This project was entirely done using OpenAI's **Codex** and Google DeepMind's **Antigravity**.
