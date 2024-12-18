library(xml2)
library(stringr)

# Function to process the HTML file
process_html_file <- function(file_path) {
  
  # Read the HTML file
  html <- read_html(file_path)
  
  # Format numbers to 3 decimal places
  format_numbers <- function(x) {
    # Check if the value is a number
    if (!is.na(suppressWarnings(as.numeric(x)))) {
      formatted_num <- format(round(as.numeric(x), 3), nsmall = 3)
      return(formatted_num)
    }
    return(x)
  }
  
  # Update all numeric cells
  td_nodes <- xml_find_all(html, "//td")
  td_texts <- xml_text(td_nodes)
  formatted_texts <- sapply(td_texts, format_numbers, USE.NAMES = FALSE)
  mapply(xml_set_text, td_nodes, formatted_texts, SIMPLIFY = FALSE)
  
  # Replace 'Standard deviation' with 'Std' in row titles
  title_nodes <- xml_find_all(html, "//td[@style='text-align:left;']")
  titles <- xml_text(title_nodes)
  updated_titles <- str_replace_all(titles, "Standard deviation", "Std")
  mapply(xml_set_text, title_nodes, updated_titles, SIMPLIFY = FALSE)
  
  # Change padding from right to left in style
  styles <- xml_find_all(html, "//style")
  style_text <- xml_text(styles)
  updated_style <- str_replace(style_text, "padding-right: 2em;", "padding-left: 2em;")
  xml_set_text(styles, updated_style)
  
  # Rename column titles
  column_titles <- c("Mean", "Min", "p25", "Median", "p75", "Max")
  th_nodes <- xml_find_all(html, "//th")
  current_titles <- xml_text(th_nodes)
  
  if (length(current_titles) == length(column_titles) + 1) {
    new_titles <- c("", column_titles)
  } else {
    new_titles <- column_titles
  }
  
  mapply(xml_set_text, th_nodes, new_titles, SIMPLIFY = FALSE)
  
  # Save the modified file
  write_html(html, file_path)
}

# Define the base directory as the current working directory
base_dir <- getwd()

# Define the pattern for files
pattern <- "summary_stats_table.html"

# Get the list of HTML files
html_files <- list.files(base_dir, recursive = TRUE, pattern = pattern, full.names = TRUE)

# Process each HTML file
lapply(html_files, process_html_file)