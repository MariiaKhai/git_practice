# Load necessary libraries
library(flextable)  # for creating customizable tables
library(officer)  # for creating and manipulating Microsoft Office documents
library(tidyverse)  # for data manipulation and analysis
library(safetyData)  # for accessing example clinical trial data sets

# Load example data from the safetyData package
data(adam_adsl)

# Set data.frame automatic printing as a flextable
use_df_printer()

# Set default styling for all flextables created in this script
set_flextable_defaults(
  border.color = "#AAAAAA", # Set the border color
  font.family = "Arial",    # Set the font family
  font.size = 10,           # Set the font size
  padding = 2,              # Set padding inside cells
  line_spacing = 1.5        # Set the line spacing
)

# Select specific columns from the adam_adsl dataset
adsl <- select(adam_adsl, AGE, SEX, BMIBLGR1, DURDIS, ARM)

# Summarize data by ARM group; summarizor() seems to be a custom or undefined function
dat <- summarizor(adsl, by = "ARM")
dat

# Convert the summarized data into a flextable
ft <- as_flextable(dat)
ft

# Customize the flextable
ft <- as_flextable(dat, spread_first_col = TRUE, separate_with = "variable") %>%
  bold(i = ~ !is.na(variable), j = 1, bold = TRUE) %>%
  set_caption(
    autonum = officer::run_autonum(seq_id = "tab", bkm = "demo_tab", bkm_all = FALSE),
    fp_p = officer::fp_par(text.align = "left", padding = 5),
    align_with_table = FALSE,
    caption = as_paragraph(
      "Demographic Characteristics",
      "\nx.x: Study Subject Data"
    )
  ) %>% 
  add_footer_lines("Source: ADaM adsl data frame from r package 'safetyData'") %>% 
  fix_border_issues() %>% 
  autofit()

ft


# Map column names to their labels
col_labels <- map_chr(adsl, function(x) attr(x, "label"))

# Apply labels to the 'stat' column of the flextable
ft <- labelizor(ft, j = "stat", labels = col_labels, part = "all")

# Add page numbers and save the table as a Word document
ft %>%
  add_header_lines("Page ") %>%
  append_chunks(i = 1, part = "header", j = 1, as_word_field(x = "Page")) %>% 
  save_as_docx(path = "C:/Mariia/tmp/R/Lessons/adsl2.docx")

