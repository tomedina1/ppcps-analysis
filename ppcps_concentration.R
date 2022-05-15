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
                max = max(y),
                min = min(y),
                count = n()) 
    
    # appends a new sheet and saves the final workbook
    ws <- addWorksheet(final_file, sheetName = i)
    writeData(final_file, ws, site_stats, startCol = 1, startRow = 1) 
    saveWorkbook(final_file, 'ppcps.xlsx', overwrite = TRUE)}

 }
 
 ppcps <- by_site(ppcps_raw)
 
 # Function to get the number of sites each compounds shows up in
 site_count <- function(dataset){
   
   # wrangle the dataset
   dataset_clean <- dataset %>% 
     janitor::clean_names() %>% 
     filter(!str_detect(batch_1, 'Compound'), !str_detect(batch_1, 'ng/L'),
            !str_detect(batch_1, 'Batch'), !str_detect(batch_1, 'Name')) %>% 
     na_if('-') %>% 
     mutate_at(vars(2:ncol(dataset)), as.numeric) %>% 
     mutate_if(is.numeric, ~1 * (. >= 0))
   
   # get the distinct chemicals 
   counts <- dataset_clean[1] %>% 
     distinct() 
   
   # rename columns for ease
   colnames(counts) <- 'x'
   
   # site-by-site analysis
   for (i in 2:ncol(dataset_clean)){
     
     # goes by each site
     site <- dataset_clean %>% 
       select(1, i)
     
     # rename for ease
     colnames(site) <- c('x', 'y')
     
     # filters for where there are chemicals and then removes duplicates
     site_filter <- site %>% 
       filter(y == 1) %>% 
       distinct(x, .keep_all = TRUE)
     
     # creates a dataframe by merging observances of the chemical for each site
     counts <- merge(counts, site_filter, by.x = 'x', by.y = 'x', all.x = TRUE)
   }
   
   # calculates the total amount of sites each chemical is in
   count_row <- rowSums(counts[, -1], na.rm = TRUE) %>% 
     cbind(counts[1]) %>% 
     rev()
  
   return(count_row)
 }

 site_counts <- site_count(ppcps_raw)
 
 # writes the excel sheet
 write.xlsx(site_counts, 'counts_by_site.xlsx')