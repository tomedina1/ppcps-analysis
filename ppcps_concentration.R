 # ----- PPCPs Concentrations Analysis -----
        # Taylor Medina
 
 # --- PACKAGES ---
 
 library(tidyverse)
 library(here)
 library(readxl)
 library(openxlsx)
 
 # --- DATA WRANGLING ---
 
 
 # Load in raw excel file
 ppcps_raw <- read_excel(here('data/PPCP Data.xlsx')) 
 ppcps_raw <- ppcps_raw[-1, ]

 
 # Organize the data frame by site
 by_site <- function(dataset) {
   
   # create a blank excel workbook
   final_file <- createWorkbook()
  
   # loops through each column in the dataframe
   for (i in 2:ncol(dataset)){
     
    # selects the  specific site
    site <- dataset %>% 
      select(1, i) 
    
    # renames columns for ease
    colnames(site) <- c('x', 'y')
   
    # filters out data
    site_filter <- site %>% 
      filter(x != 'ng/L', !str_detect(x, 'Batch'), 
              !str_detect(x, 'unit'), !str_detect(y, '-'),
              y != 0) %>% 
      mutate(y = as.numeric(y))
   
    # statistical analysis
    site_stats <- site_filter %>% 
      group_by(x) %>% 
      summarize(mean_conc = mean(y, na.rm = TRUE),
                sd_conc = sd(y, na.rm = TRUE),
                count = n()) 
    
    # appends a new sheet and saves the final workbook
    ws <- addWorksheet(final_file, sheetName = i)
    writeData(final_file, ws, site_stats, startCol = 1, startRow = 1) 
    saveWorkbook(final_file, 'ppcps.xlsx', overwrite = TRUE)}

 }
 
 ppcps <- by_site(ppcps_raw)
