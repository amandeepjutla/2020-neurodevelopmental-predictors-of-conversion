# 1_import.r
# 20201207
# Import spreadsheet and recoded dataframes

spreadsheet <- read_xlsx(
    paste(RAW_PATH, RAW_FILE, sep = '/'),
    sheet=4,
    na = c('N/A','999'))

childhood_behavioral_problems <- read_csv(
    paste(RECODED_PATH, 'childhood_behavioral_problems.csv', sep = '/'),
    col_types = 'iiiiii')

maternal_age <- read_csv(
    paste(RECODED_PATH, 'maternal_age.csv', sep = '/'), col_types='ii')

paternal_age <- read_csv(
    paste(RECODED_PATH, 'paternal_age.csv', sep = '/'), col_types='ii')

psych_hx_sibling <- read_csv(
    paste(RECODED_PATH, 'psych_hx_sibling.csv', sep = '/'), col_types='iiiii')

psych_hx_parent <- read_csv(
    paste(RECODED_PATH, 'psych_hx_parent.csv', sep = '/'), col_types='ii')
