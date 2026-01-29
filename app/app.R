library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(lubridate)


# Data configuration -----------------------------------------------------------

data_dir <- normalizePath(
  file.path("..", "data"),
  winslash = "/",
  mustWork = FALSE
)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}

expenses_path <- file.path(data_dir, "expenses.csv")
budgets_path <- file.path(data_dir, "category_budget.csv")
income_path <- file.path(data_dir, "income_sources.csv")

empty_expenses <- tibble::tibble(
  Date = as.Date(character()),
  Description = character(),
  Category = character(),
  Subcategory = character(),
  Amount = numeric(),
  Payer = character()
)

empty_budgets <- tibble::tibble(
  Category = character(),
  Subcategory = character(),
  Limit = numeric(),
  Frequency = character()
)

default_payers <- c("Joint", "Carson", "Chloe")

clean_subcategory <- function(x) {
  if (is.null(x)) {
    return("")
  }

  x <- tidyr::replace_na(x, "")
  trimws(x)
}

display_subcategory <- function(value) {
  value <- clean_subcategory(value)
  ifelse(nzchar(value), value, "")
}

load_expenses <- function() {
  if (!file.exists(expenses_path)) {
    return(empty_expenses)
  }

  df <- readr::read_csv(
    expenses_path,
    col_types = readr::cols(
      Date = col_date(),
      Description = col_character(),
      Category = col_character(),
      Subcategory = col_character(),
      Amount = col_double(),
      Payer = col_character()
    ),
    show_col_types = FALSE
  )

  if (!"Subcategory" %in% names(df)) {
    df$Subcategory <- ""
  }

  df %>%
    mutate(
      Description = tidyr::replace_na(Description, ""),
      Category = tidyr::replace_na(Category, ""),
      Subcategory = clean_subcategory(Subcategory),
      Payer = tidyr::replace_na(Payer, ""),
      Amount = replace_na(Amount, 0)
    ) %>%
    arrange(desc(Date))
}

load_budgets <- function() {
  if (!file.exists(budgets_path)) {
    return(empty_budgets)
  }

  df <- readr::read_csv(
    budgets_path,
    col_types = cols(
      Category = col_character(),
      Subcategory = col_character(),
      Limit = col_double(),
      Frequency = col_character(),
      EffectiveDate = col_date()
    ),
    show_col_types = FALSE
  )

  if (!"Limit" %in% names(df) && "Target" %in% names(df)) {
    df <- dplyr::rename(df, Limit = Target)
  }

  if (!"Frequency" %in% names(df)) {
    df$Frequency <- "Monthly"
  }

  if (!"EffectiveDate" %in% names(df)) {
    df$EffectiveDate <- as.Date("2020-01-01")
  }

  df %>%
    mutate(
      Category = tidyr::replace_na(Category, ""),
      Subcategory = clean_subcategory(Subcategory),
      Limit = replace_na(Limit, 0),
      Frequency = replace_na(Frequency, "Monthly"),
      EffectiveDate = replace_na(EffectiveDate, as.Date("2020-01-01"))
    ) %>%
    arrange(Category, Subcategory, desc(EffectiveDate))
}

load_monthly_income <- function() {
  if (!file.exists(income_path)) {
    return(NA_real_)
  }

  income <- readr::read_csv(
    income_path,
    col_types = cols(
      Source = col_character(),
      Amount = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(Amount = replace_na(Amount, 0)) %>%
    summarise(total = sum(Amount, na.rm = TRUE)) %>%
    pull(total)

  if (is.finite(income) && length(income) == 1) {
    income
  } else {
    NA_real_
  }
}

write_expenses <- function(df) {
  readr::write_csv(df, expenses_path, na = "")
}

write_budgets <- function(df) {
  readr::write_csv(df, budgets_path, na = "")
}

write_monthly_income <- function(amount) {
  tibble::tibble(Source = "Monthly", Amount = amount) %>%
    readr::write_csv(income_path, na = "")
}

format_subcategory <- function(value) {
  value <- clean_subcategory(value)
  ifelse(nzchar(value), value, "(Unspecified)")
}

format_expense_table_data <- function(df) {
  df %>% mutate(Subcategory = display_subcategory(Subcategory))
}

format_budget_table_data <- function(df) {
  df %>% mutate(Subcategory = display_subcategory(Subcategory))
}

get_monthly_limit <- function(limit, frequency) {
  case_when(
    frequency == "Monthly" ~ limit,
    frequency == "Quarterly" ~ limit / 3,
    frequency == "Bi-annually" ~ limit / 6,
    frequency == "Annually" ~ limit / 12,
    TRUE ~ limit
  )
}

# User interface --------------------------------------------------------------

ui <- navbarPage(
  title = "Household Budgeting",
  tabPanel(
    "Expenses",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Log an expense"),
          dateInput("expense_date", "Date", value = Sys.Date()),
          textInput("expense_description", "Description"),
          selectizeInput(
            "expense_category",
            "Category",
            choices = NULL,
            options = list(
              placeholder = "Select or add a category",
              create = TRUE
            )
          ),
          selectizeInput(
            "expense_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(
              placeholder = "Select or add a subcategory",
              create = TRUE,
              allowEmptyOption = TRUE
            )
          ),
          numericInput(
            "expense_amount",
            "Amount",
            value = NA,
            min = 0,
            step = 0.01
          ),
          selectizeInput(
            "expense_payer",
            "Payer",
            choices = NULL,
            options = list(placeholder = "Select or add a payer", create = TRUE)
          ),
          actionButton("add_expense", "Add expense", class = "btn-primary"),
          br(),
          br(),
          strong("Totals"),
          textOutput("expense_totals", inline = FALSE)
        ),
        column(
          width = 8,
          h3("Recorded expenses"),
          actionButton(
            "delete_expense",
            "Delete selected expense",
            class = "btn-danger mb-2"
          ),
          DTOutput("expense_table")
        )
      )
    )
  ),
  tabPanel(
    "Budgeting",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Plan budgets"),
          numericInput(
            "income",
            "Monthly income",
            value = NA,
            min = 0,
            step = 50
          ),
          actionButton("set_income", "Save income", class = "btn-secondary"),
          actionButton("set_income", "Save income", class = "btn-secondary"),
          br(),
          br(),
          h4("Add or update a budget line"),
          dateInput(
            "budget_start",
            "Effective Month",
            value = floor_date(Sys.Date(), "month"),
            format = "yyyy-mm-dd",
            startview = "year"
          ),
          selectizeInput(
            "budget_category",
            "Category",
            choices = NULL,
            options = list(
              placeholder = "Select or add a category",
              create = TRUE
            )
          ),
          selectizeInput(
            "budget_subcategory",
            "Subcategory",
            choices = NULL,
            options = list(
              placeholder = "Select or add a subcategory",
              create = TRUE,
              allowEmptyOption = TRUE
            )
          ),
          selectizeInput(
            "budget_frequency",
            "Frequency",
            choices = c("Monthly", "Quarterly", "Bi-annually", "Annually"),
            selected = "Monthly"
          ),
          numericInput(
            "budget_limit",
            "Limit (per frequency period)",
            value = NA,
            min = 0,
            step = 10
          ),
          actionButton("add_budget", "Save budget", class = "btn-primary")
        ),
        column(
          width = 8,
          h3("Budgets"),
          uiOutput("income_summary"),
          actionButton(
            "delete_budget",
            "Delete selected budget",
            class = "btn-danger mb-2"
          ),
          DTOutput("budget_table")
        )
      )
    )
  ),
  tabPanel(
    "Reporting",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          h3("Budget performance"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                "report_month",
                "Period",
                choices = c("All time" = "all"),
                selected = "all"
              )
            )
          ),
          DTOutput("report_table"),
          br(),
          h3("Spending trends"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                "spending_period",
                "Time aggregation",
                choices = c("Monthly" = "month", "Weekly" = "week"),
                selected = "month"
              )
            ),
            column(
              width = 4,
              selectInput(
                "spending_view",
                "View",
                choices = c(
                  "Total spending" = "total",
                  "By category" = "category"
                ),
                selected = "total"
              )
            ),
            column(
              width = 4,
              uiOutput("trend_category_filter")
            )
          ),
          plotlyOutput("spending_trend", height = "400px"),
          br(),
          h3("Spending by category"),
          plotOutput("category_plot", height = "350px"),
          br(),
          h3("Category and subcategory details"),
          DTOutput("category_table")
        )
      )
    )
  ),
  tabPanel(
    "Settings",
    fluidPage(
      column(
        width = 12,
        h3("Data Management"),
        p("Create a timestamped backup of your current expenses and budget data."),
        actionButton(
          "backup_data",
          "Backup Data",
          icon = icon("save"),
          class = "btn-success"
        )
      )
    )
  )
)

# Server logic ----------------------------------------------------------------

server <- function(input, output, session) {
  expenses <- reactiveVal(load_expenses())
  budgets <- reactiveVal(load_budgets())
  monthly_income <- reactiveVal(load_monthly_income())
  pending_expense_delete <- reactiveVal(NULL)
  pending_budget_delete <- reactiveVal(NULL)

  observeEvent(input$backup_data, {
    # Define backup directory (relative to the app folder)
    # The app is running in 'app/', so '..' goes to project root
    backup_dir <- normalizePath(file.path("..", "backups"), mustWork = FALSE)

    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, showWarnings = FALSE)
    }

    timestamp <- format(Sys.time(), "%Y%m%d")
    data_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

    if (length(data_files) == 0) {
      showNotification("No data files found to backup.", type = "warning")
      return()
    }

    success_count <- 0
    for (file in data_files) {
      base_name <- tools::file_path_sans_ext(basename(file))
      ext <- tools::file_ext(file)
      new_name <- paste0(base_name, "_", timestamp, "-backup.", ext)
      target_path <- file.path(backup_dir, new_name)

      if (file.copy(file, target_path, overwrite = TRUE)) {
        success_count <- success_count + 1
      }
    }

    if (success_count > 0) {
      showNotification(
        paste("Successfully backed up", success_count, "files to 'backups' folder."),
        type = "message"
      )
    } else {
      showNotification("Backup failed.", type = "error")
    }
  })

  observeEvent(
    TRUE,
    {
      updateNumericInput(session, "income", value = monthly_income())
    },
    once = TRUE
  )

  observe({
    expense_categories <- expenses() %>%
      filter(nzchar(Category)) %>%
      pull(Category)

    budget_categories <- budgets() %>%
      filter(nzchar(Category)) %>%
      pull(Category)

    categories <- sort(unique(c(expense_categories, budget_categories)))

    if (!is.null(input$expense_category) && nzchar(input$expense_category)) {
      categories <- unique(c(categories, input$expense_category))
    }

    if (!is.null(input$budget_category) && nzchar(input$budget_category)) {
      categories <- unique(c(categories, input$budget_category))
    }
    updateSelectizeInput(
      session,
      "expense_category",
      choices = categories,
      selected = input$expense_category,
      server = FALSE
    )

    updateSelectizeInput(
      session,
      "budget_category",
      choices = categories,
      selected = input$budget_category,
      server = FALSE
    )
  })

  observe({
    payers <- expenses() %>%
      filter(nzchar(Payer)) %>%
      pull(Payer)

    payers <- sort(unique(c(default_payers, payers)))

    if (!is.null(input$expense_payer) && nzchar(input$expense_payer)) {
      payers <- unique(c(payers, input$expense_payer))
    }

    updateSelectizeInput(
      session,
      "expense_payer",
      choices = payers,
      selected = input$expense_payer,
      server = FALSE
    )
  })

  observe({
    dates <- expenses()$Date

    # Always include current month in options, even if no expenses yet
    current_month <- floor_date(Sys.Date(), "month")

    if (length(dates) == 0) {
      months <- current_month
    } else {
      months <- sort(unique(c(floor_date(dates, "month"), current_month)), decreasing = TRUE)
    }

    month_names <- format(months, "%B %Y")
    month_values <- format(months, "%Y-%m-%d")
    choices <- setNames(month_values, month_names)
    choices <- c("All time" = "all", choices)

    # Default to current month if "all" is selected (initial state) or if current selection is invalid
    selected <- input$report_month
    current_month_val <- format(current_month, "%Y-%m-%d")

    if (is.null(selected) || selected == "all") {
      selected <- current_month_val
    }

    updateSelectInput(
      session,
      "report_month",
      choices = choices,
      selected = selected
    )
  })

  observe({
    category <- input$expense_category

    if (is.null(category) || !nzchar(category)) {
      updateSelectizeInput(
        session,
        "expense_subcategory",
        choices = "",
        selected = "",
        server = FALSE
      )
      return()
    }

    expense_subs <- expenses() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory)

    budget_subs <- budgets() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory)

    subchoices <- sort(unique(c(
      clean_subcategory(expense_subs),
      clean_subcategory(budget_subs)
    )))

    current <- clean_subcategory(input$expense_subcategory)
    choices <- unique(c("", subchoices))
    if (nzchar(current)) {
      choices <- unique(c(choices, current))
    }

    named_choices <- structure(choices, names = choices)

    updateSelectizeInput(
      session,
      "expense_subcategory",
      choices = named_choices,
      selected = if (nzchar(current)) current else "",
      server = FALSE
    )
  })

  observe({
    category <- input$budget_category

    if (is.null(category) || !nzchar(category)) {
      updateSelectizeInput(
        session,
        "budget_subcategory",
        choices = "",
        selected = "",
        server = FALSE
      )
      return()
    }

    subchoices <- budgets() %>%
      filter(Category == category, nzchar(Subcategory)) %>%
      pull(Subcategory) %>%
      clean_subcategory() %>%
      sort()

    current <- clean_subcategory(input$budget_subcategory)
    choices <- unique(c("", subchoices))
    if (nzchar(current)) {
      choices <- unique(c(choices, current))
    }

    named_choices <- structure(choices, names = choices)

    updateSelectizeInput(
      session,
      "budget_subcategory",
      choices = named_choices,
      selected = if (nzchar(current)) current else "",
      server = FALSE
    )
  })

  observeEvent(input$add_expense, {
    description <- trimws(input$expense_description)
    category <- trimws(input$expense_category)
    subcategory <- trimws(input$expense_subcategory)
    payer <- trimws(input$expense_payer)

    validate(
      need(
        !is.null(input$expense_date) && !is.na(input$expense_date),
        "Please supply a date."
      ),
      need(nzchar(description), "Describe the expense."),
      need(nzchar(category), "Choose a category."),
      need(
        !is.null(input$expense_amount) &&
          !is.na(input$expense_amount) &&
          input$expense_amount > 0,
        "Enter a positive amount."
      )
    )

    entry <- tibble::tibble(
      Date = as.Date(input$expense_date),
      Description = description,
      Category = category,
      Subcategory = clean_subcategory(subcategory),
      Amount = as.numeric(input$expense_amount),
      Payer = payer
    )

    updated <- bind_rows(expenses(), entry) %>% arrange(desc(Date))
    expenses(updated)
    write_expenses(updated)

    updateTextInput(session, "expense_description", value = "")
    updateNumericInput(session, "expense_amount", value = NA)
    updateSelectizeInput(
      session,
      "expense_subcategory",
      selected = NULL,
      server = FALSE
    )
    showNotification("Expense added.", type = "message")
  })

  observeEvent(input$delete_expense, {
    selected <- input$expense_table_rows_selected
    if (length(selected) == 0) {
      showNotification("Select an expense to delete.", type = "warning")
      return()
    }

    current <- expenses()
    if (nrow(current) == 0) {
      showNotification("No expenses to delete.", type = "warning")
      return()
    }

    valid <- selected[selected >= 1 & selected <= nrow(current)]
    if (length(valid) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    row_idx <- valid[1]
    record <- current[row_idx, , drop = FALSE]
    pending_expense_delete(list(row_data = record))

    showModal(modalDialog(
      title = "Delete expense",
      easyClose = FALSE,
      size = "m",
      tags$p("Are you sure you want to delete this expense?"),
      tags$ul(
        tags$li(strong("Date:"), format(record$Date)),
        tags$li(strong("Description:"), record$Description),
        tags$li(
          strong("Category:"),
          paste(
            record$Category,
            format_subcategory(record$Subcategory),
            sep = " › "
          )
        ),
        tags$li(strong("Amount:"), scales::dollar(record$Amount)),
        tags$li(
          strong("Payer:"),
          ifelse(nzchar(record$Payer), record$Payer, "--")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_expense",
          "Delete",
          class = "btn btn-danger"
        )
      )
    ))
  })

  observeEvent(input$add_budget, {
    category <- trimws(input$budget_category)
    subcategory <- clean_subcategory(input$budget_subcategory)
    frequency <- input$budget_frequency
    effective_date <- as.Date(input$budget_start)

    validate(
      need(!is.null(effective_date) && !is.na(effective_date), "Provide an effective date."),
      need(nzchar(category), "Provide a category."),
      need(
        !is.null(input$budget_limit) &&
          !is.na(input$budget_limit) &&
          input$budget_limit >= 0,
        "Enter a non-negative limit."
      )
    )

    new_budget <- tibble::tibble(
      Category = category,
      Subcategory = subcategory,
      Limit = as.numeric(input$budget_limit),
      Frequency = frequency,
      EffectiveDate = effective_date
    )

    current <- budgets()
    match_idx <- which(
      tolower(current$Category) == tolower(category) &
        tolower(clean_subcategory(current$Subcategory)) == tolower(subcategory) &
        current$EffectiveDate == effective_date
    )

    if (length(match_idx) > 0) {
      current$Limit[match_idx[1]] <- new_budget$Limit
      current$Frequency[match_idx[1]] <- new_budget$Frequency
      updated <- current
    } else {
      updated <- bind_rows(current, new_budget) %>%
        arrange(Category, Subcategory, desc(EffectiveDate))
    }

    budgets(updated)
    write_budgets(updated)

    updateSelectizeInput(
      session,
      "budget_subcategory",
      selected = NULL,
      server = FALSE
    )
    updateNumericInput(session, "budget_limit", value = NA)
    updateSelectizeInput(session, "budget_frequency", selected = "Monthly")
    showNotification("Budget saved.", type = "message")
  })

  observeEvent(input$delete_budget, {
    selected <- input$budget_table_rows_selected
    if (length(selected) == 0) {
      showNotification("Select a budget to delete.", type = "warning")
      return()
    }

    current <- budgets()
    if (nrow(current) == 0) {
      showNotification("No budgets to delete.", type = "warning")
      return()
    }

    valid <- selected[selected >= 1 & selected <= nrow(current)]
    if (length(valid) == 0) {
      showNotification(
        "Selected budget is no longer available.",
        type = "error"
      )
      return()
    }

    row_idx <- valid[1]
    record <- current[row_idx, , drop = FALSE]
    pending_budget_delete(list(row_data = record))

    showModal(modalDialog(
      title = "Remove Budget",
      easyClose = FALSE,
      size = "m",
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        "How would you like to remove this budget?"
      ),
      radioButtons(
        "delete_mode",
        label = NULL,
        choices = c(
          "Stop budgeting (Preserve history)" = "archive",
          "Delete record (Correction)" = "delete"
        ),
        selected = "archive"
      ),
      tags$div(
        style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
        tags$p(strong("Selected Item:")),
        tags$ul(
          tags$li(strong("Category:"), record$Category),
          tags$li(strong("Subcategory:"), format_subcategory(record$Subcategory)),
          tags$li(strong("Limit:"), scales::dollar(record$Limit)),
          tags$li(strong("Frequency:"), record$Frequency),
          tags$li(strong("Effective Date:"), format(record$EffectiveDate))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_delete_budget",
          "Confirm",
          class = "btn btn-primary"
        )
      )
    ))
  })

  observeEvent(input$confirm_delete_expense, {
    info <- pending_expense_delete()
    pending_expense_delete(NULL)
    removeModal()

    if (is.null(info)) {
      return()
    }

    current <- expenses()
    if (nrow(current) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    record <- info$row_data
    match_idx <- which(
      current$Date == record$Date &
        current$Description == record$Description &
        current$Category == record$Category &
        current$Subcategory == record$Subcategory &
        current$Amount == record$Amount &
        current$Payer == record$Payer
    )

    if (length(match_idx) == 0) {
      showNotification(
        "Selected expense is no longer available.",
        type = "error"
      )
      return()
    }

    updated <- current[-match_idx[1], , drop = FALSE]
    expenses(updated)
    write_expenses(updated)
    showNotification("Expense deleted.", type = "message")
  })

  observeEvent(input$confirm_delete_budget, {
    info <- pending_budget_delete()
    pending_budget_delete(NULL)
    removeModal()

    if (is.null(info)) {
      return()
    }

    current <- budgets()
    # Re-verify the data is still valid
    if (nrow(current) == 0) {
      return()
    }

    record <- info$row_data

    # Mode Handling
    mode <- input$delete_mode
    if (is.null(mode)) mode <- "archive" # Default safe fallback

    if (mode == "archive") {
      # ARCHIVE: Create a new 0-limit entry effective current month
      new_date <- floor_date(Sys.Date(), "month")

      # Use update logic to prevent duplicates for the same month
      match_idx <- which(
        tolower(current$Category) == tolower(record$Category) &
          tolower(clean_subcategory(current$Subcategory)) ==
            tolower(clean_subcategory(record$Subcategory)) &
          current$EffectiveDate == new_date
      )

      if (length(match_idx) > 0) {
        # Update existing entry for this month
        current$Limit[match_idx[1]] <- 0
        updated <- current
        msg <- "Budget stopped for this month (updated existing entry)."
      } else {
        # Create new entry
        new_entry <- tibble::tibble(
          Category = record$Category,
          Subcategory = record$Subcategory,
          Limit = 0,
          Frequency = record$Frequency,
          EffectiveDate = new_date
        )
        updated <- bind_rows(current, new_entry) %>%
          arrange(Category, Subcategory, desc(EffectiveDate))
        msg <- "Budget stopped effective this month."
      }

      budgets(updated)
      write_budgets(updated)
      showNotification(msg, type = "message")
    } else {
      # DELETE: Physically remove the record
      match_idx <- which(
        current$Category == record$Category &
          current$Subcategory == record$Subcategory &
          current$Limit == record$Limit &
          current$Frequency == record$Frequency &
          current$EffectiveDate == record$EffectiveDate
      )

      if (length(match_idx) == 0) {
        showNotification(
          "Selected budget is no longer available.",
          type = "error"
        )
        return()
      }

      updated <- current[-match_idx[1], , drop = FALSE]
      budgets(updated)
      write_budgets(updated)
      showNotification("Budget record deleted.", type = "message")
    }
  })

  observeEvent(input$set_income, {
    validate(
      need(
        !is.null(input$income) && !is.na(input$income) && input$income >= 0,
        "Enter a non-negative income."
      )
    )
    amount <- as.numeric(input$income)
    monthly_income(amount)
    write_monthly_income(amount)
    showNotification("Income updated.", type = "message")
  })

  expense_totals <- reactive({
    df <- expenses()
    tibble::tibble(
      Total = sum(df$Amount, na.rm = TRUE),
      Count = nrow(df)
    )
  })

  output$expense_totals <- renderText({
    totals <- expense_totals()
    paste0(
      "Logged expenses: ",
      totals$Count,
      " | Total spent: ",
      dollar(totals$Total)
    )
  })

  output$expense_table <- renderDT({
    data <- format_expense_table_data(expenses())

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20)),
      selection = "single",
      editable = list(target = "cell")
    ) %>%
      formatCurrency(
        "Amount",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      )
  })

  output$budget_table <- renderDT({
    data <- format_budget_table_data(budgets())

    validate(need(nrow(data) > 0, "Add budgets to track your plan."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20)),
      selection = "single",
      editable = list(target = "cell")
    ) %>%
      formatCurrency(
        "Limit",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      )
  })

  expense_proxy <- dataTableProxy("expense_table")
  budget_proxy <- dataTableProxy("budget_table")

  observeEvent(input$expense_table_cell_edit, {
    info <- input$expense_table_cell_edit
    df <- expenses()

    if (is.null(info$row) || is.null(info$col)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    row_idx <- as.integer(info$row)
    rows_all <- input$expense_table_rows_all
    if (!is.null(rows_all)) {
      if (row_idx < 1 || row_idx > length(rows_all)) {
        DT::replaceData(
          expense_proxy,
          format_expense_table_data(df),
          resetPaging = FALSE,
          rownames = FALSE
        )
        return()
      }
      row_idx <- as.integer(rows_all[row_idx])
    }

    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    col_idx <- as.integer(info$col)
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(df)) {
      DT::replaceData(
        expense_proxy,
        format_expense_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    column <- names(df)[col_idx]
    value <- info$value

    parse_amount <- function(x) {
      as.numeric(gsub("[^0-9.-]", "", x))
    }

    updated_value <- switch(column,
      Date = {
        parsed <- tryCatch(
          as.Date(value),
          error = function(e) NA
        )
        if (is.na(parsed)) {
          showNotification("Enter a valid date (YYYY-MM-DD).", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        parsed
      },
      Amount = {
        parsed <- parse_amount(value)
        if (is.na(parsed) || parsed <= 0) {
          showNotification("Enter a positive amount.", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        parsed
      },
      Subcategory = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned) || identical(cleaned, "(Unspecified)")) {
          ""
        } else {
          clean_subcategory(cleaned)
        }
      },
      Category = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned)) {
          showNotification("Category cannot be empty.", type = "error")
          return(DT::replaceData(
            expense_proxy,
            format_expense_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        cleaned
      },
      Description = trimws(value),
      Payer = trimws(value),
      value
    )

    if (is.null(updated_value)) {
      return()
    }

    df[row_idx, column] <- updated_value
    df <- df %>% arrange(desc(Date))
    expenses(df)
    write_expenses(df)
    DT::replaceData(
      expense_proxy,
      format_expense_table_data(df),
      resetPaging = FALSE,
      rownames = FALSE
    )
  })

  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- budgets()

    if (is.null(info$row) || is.null(info$col)) {
      DT::replaceData(
        budget_proxy,
        format_budget_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    row_idx <- as.integer(info$row)
    rows_all <- input$budget_table_rows_all
    if (!is.null(rows_all)) {
      if (row_idx < 1 || row_idx > length(rows_all)) {
        DT::replaceData(
          budget_proxy,
          format_budget_table_data(df),
          resetPaging = FALSE,
          rownames = FALSE
        )
        return()
      }
      row_idx <- as.integer(rows_all[row_idx])
    }

    if (is.na(row_idx) || row_idx < 1 || row_idx > nrow(df)) {
      DT::replaceData(
        budget_proxy,
        format_budget_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    col_idx <- as.integer(info$col)
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(df)) {
      DT::replaceData(
        budget_proxy,
        format_budget_table_data(df),
        resetPaging = FALSE,
        rownames = FALSE
      )
      return()
    }

    column <- names(df)[col_idx]
    value <- info$value

    parse_amount <- function(x) {
      as.numeric(gsub("[^0-9.-]", "", x))
    }

    updated_value <- switch(column,
      Limit = {
        parsed <- parse_amount(value)
        if (is.na(parsed) || parsed < 0) {
          showNotification("Enter a non-negative limit.", type = "error")
          return(DT::replaceData(
            budget_proxy,
            format_budget_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        parsed
      },
      Category = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned)) {
          showNotification("Category cannot be empty.", type = "error")
          return(DT::replaceData(
            budget_proxy,
            format_budget_table_data(df),
            resetPaging = FALSE,
            rownames = FALSE
          ))
        }
        cleaned
      },
      Subcategory = {
        cleaned <- trimws(value)
        if (!nzchar(cleaned) || identical(cleaned, "(Unspecified)")) {
          ""
        } else {
          clean_subcategory(cleaned)
        }
      },
      value
    )

    if (is.null(updated_value)) {
      return()
    }

    df[row_idx, column] <- updated_value
    df <- df %>% arrange(Category, Subcategory)
    budgets(df)
    write_budgets(df)
    DT::replaceData(
      budget_proxy,
      format_budget_table_data(df),
      resetPaging = FALSE,
      rownames = FALSE
    )
  })

  output$income_summary <- renderUI({
    income <- monthly_income()
    budget_total <- budgets() %>%
      mutate(MonthlyLimit = get_monthly_limit(Limit, Frequency)) %>%
      summarise(Total = sum(MonthlyLimit, na.rm = TRUE)) %>%
      pull(Total)
    remaining <- if (is.na(income)) NA_real_ else income - budget_total

    tags$div(
      tags$p(
        strong("Monthly income:"),
        if (is.na(income)) "Not set" else dollar(income)
      ),
      tags$p(
        strong("Budgeted total:"),
        dollar(budget_total)
      ),
      tags$p(
        strong("Unallocated:"),
        if (is.na(remaining)) "--" else dollar(remaining)
      )
    )
  })

  output$trend_category_filter <- renderUI({
    if (!identical(input$spending_view, "category")) {
      return(NULL)
    }

    categories <- expenses() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)")
      ) %>%
      pull(Category) %>%
      unique() %>%
      sort()

    if (length(categories) == 0) {
      return(tags$p("Add expenses to choose categories."))
    }

    selected <- input$trend_categories
    selected <- selected[selected %in% categories]
    if (length(selected) == 0) {
      selected <- categories
    }

    selectizeInput(
      "trend_categories",
      "Categories",
      choices = categories,
      selected = selected,
      multiple = TRUE,
      options = list(
        placeholder = "Filter categories",
        plugins = list("remove_button")
      )
    )
  })

  output$spending_trend <- renderPlotly({
    df <- expenses()
    validate(need(nrow(df) > 0, "Add expenses to see spending trends."))

    df <- df %>%
      filter(!is.na(Date)) %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)"),
        Amount = replace_na(Amount, 0)
      )

    validate(need(
      nrow(df) > 0,
      "Add expenses with dates to see spending trends."
    ))

    unit <- if (identical(input$spending_period, "week")) "week" else "month"
    df <- df %>%
      mutate(
        Period = if (unit == "week") {
          floor_date(Date, unit = "week", week_start = 1)
        } else {
          floor_date(Date, unit = "month")
        }
      )

    if (!identical(input$spending_view, "category")) {
      summary <- df %>%
        group_by(Period) %>%
        summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(Period)

      validate(need(nrow(summary) > 0, "Add expenses to see spending trends."))

      plot_ly(
        summary,
        x = ~Period,
        y = ~Total,
        type = "scatter",
        mode = "lines+markers",
        hovertemplate = paste0(
          "%{x|%b %d, %Y}<br>Total: $%{y:,.2f}<extra></extra>"
        ),
        name = "Total"
      ) %>%
        layout(
          xaxis = list(title = if (unit == "week") "Week" else "Month"),
          yaxis = list(
            title = "Spending",
            tickprefix = "$",
            separatethousands = TRUE
          ),
          legend = list(orientation = "h", x = 0, y = -0.2),
          title = "Spending over time"
        )
    } else {
      selected <- input$trend_categories
      if (!is.null(selected) && length(selected) > 0) {
        df <- df %>% filter(Category %in% selected)
      }

      summary <- df %>%
        group_by(Period, Category) %>%
        summarise(Total = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
        arrange(Period)

      validate(need(
        nrow(summary) > 0,
        "Adjust filters to see spending trends."
      ))

      plt <- plot_ly()
      categories <- unique(summary$Category)
      for (cat in categories) {
        cat_data <- summary %>% filter(Category == cat)
        plt <- plt %>%
          add_trace(
            data = cat_data,
            x = ~Period,
            y = ~Total,
            type = "scatter",
            mode = "lines+markers",
            name = cat,
            hovertemplate = paste0(
              "%{x|%b %d, %Y}<br>",
              cat,
              ": $%{y:,.2f}<extra></extra>"
            )
          )
      }

      plt %>%
        layout(
          xaxis = list(title = if (unit == "week") "Week" else "Month"),
          yaxis = list(
            title = "Spending",
            tickprefix = "$",
            separatethousands = TRUE
          ),
          legend = list(orientation = "h", x = 0, y = -0.2),
          title = "Spending over time"
        )
    }
  })

  category_summary <- reactive({
    df <- expenses()
    if (nrow(df) == 0) {
      return(tibble::tibble(
        Category = character(),
        Subcategory = character(),
        Total = numeric(),
        Transactions = integer()
      ))
    }

    if (!is.null(input$report_month) && input$report_month != "all") {
      month_start <- as.Date(input$report_month)
      month_end <- ceiling_date(month_start, "month") - days(1)
      df <- df %>% filter(Date >= month_start & Date <= month_end)

      if (nrow(df) == 0) {
        return(tibble::tibble(
          Category = character(),
          Subcategory = character(),
          Total = numeric(),
          Transactions = integer()
        ))
      }
    }

    df %>%
      mutate(Subcategory = format_subcategory(Subcategory)) %>%
      group_by(Category, Subcategory) %>%
      summarise(
        Total = sum(Amount, na.rm = TRUE),
        Transactions = dplyr::n(),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
  })

  report_data <- reactive({
    categories <- category_summary()
    if (nrow(categories) == 0 && nrow(budgets()) == 0) {
      return(tibble::tibble())
    }

    report_date <- if (is.null(input$report_month) || input$report_month == "all") {
      Sys.Date()
    } else {
      as.Date(input$report_month)
    }

    # Time-variant budget logic:
    # 1. Filter budgets that started on or before the report date
    # 2. For each Category/Subcategory, pick the most recent one (SCD Type 2 snapshot)
    active_budgets <- budgets() %>%
      filter(EffectiveDate <= report_date) %>%
      group_by(Category, Subcategory) %>%
      slice_max(order_by = EffectiveDate, n = 1, with_ties = FALSE) %>%
      ungroup()

    active_budgets %>%
      mutate(
        Subcategory = format_subcategory(Subcategory),
        # Show full annual limit for annual items, but monthly equivalent for others
        Limit = case_when(
          Frequency == "Annually" ~ Limit,
          TRUE ~ get_monthly_limit(Limit, Frequency)
        )
      ) %>%
      full_join(categories, by = c("Category", "Subcategory")) %>%
      mutate(
        Limit = replace_na(Limit, 0),
        Total = replace_na(Total, 0),
        Transactions = replace_na(Transactions, 0L),
        Remaining = Limit - Total,
        Status = case_when(
          Limit == 0 & Total == 0 ~ "No activity",
          Limit == 0 & Total > 0 ~ "Over (no budget)",
          Total < Limit ~ "Under budget",
          Total == Limit ~ "On budget",
          TRUE ~ "Over budget"
        )
      ) %>%
      arrange(desc(Total))
  })

  output$report_table <- renderDT({
    data <- report_data()
    validate(need(nrow(data) > 0, "Add expenses or budgets to see the report."))

    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency(
        "Total",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatCurrency(
        "Limit",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatCurrency(
        "Remaining",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      ) %>%
      formatStyle(
        "Status",
        color = styleEqual(
          c("Under budget", "On budget", "Over budget", "Over (no budget)"),
          c("green", "black", "red", "red")
        )
      )
  })

  output$category_table <- renderDT({
    summary <- category_summary()
    validate(need(nrow(summary) > 0, "Add expenses to see the summary."))

    datatable(
      summary,
      rownames = FALSE,
      options = list(pageLength = 10, lengthMenu = c(5, 10, 20))
    ) %>%
      formatCurrency(
        "Total",
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2
      )
  })

  output$category_plot <- renderPlot({
    expense_summary <- category_summary() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)")
      ) %>%
      group_by(Category) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")

    budget_summary <- budgets() %>%
      mutate(
        Category = ifelse(nzchar(Category), Category, "(Uncategorized)"),
        Limit = get_monthly_limit(Limit, Frequency)
      ) %>%
      group_by(Category) %>%
      summarise(Limit = sum(Limit, na.rm = TRUE), .groups = "drop")

    summary <- full_join(expense_summary, budget_summary, by = "Category") %>%
      mutate(
        Total = replace_na(Total, 0),
        Limit = replace_na(Limit, 0),
        Percent = if_else(
          Limit > 0,
          (Total / Limit) * 100,
          if_else(Total > 0, 100, 0)
        ),
        Fill = if_else(
          (Limit > 0 & Total > Limit) | (Limit == 0 & Total > 0),
          "#d73027",
          "#1b9e77"
        )
      ) %>%
      arrange(Percent)

    validate(need(
      nrow(summary) > 0,
      "Add expenses or budgets to see the plot."
    ))

    max_percent <- max(summary$Percent, na.rm = TRUE)
    if (!is.finite(max_percent)) {
      max_percent <- 0
    }
    upper_limit <- max(100, ceiling(max_percent / 10) * 10)

    ggplot(summary, aes(x = reorder(Category, Percent), y = Percent)) +
      geom_col(aes(fill = Fill), show.legend = FALSE) +
      geom_hline(
        yintercept = 100,
        linetype = "dashed",
        color = "#333333",
        linewidth = 0.8
      ) +
      labs(
        x = "Category",
        y = "Percent of budget",
        title = "Spending by category"
      ) +
      scale_y_continuous(
        labels = scales::label_percent(scale = 1),
        limits = c(0, upper_limit)
      ) +
      scale_fill_identity() +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
