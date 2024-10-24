

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rvest)
library(tidyverse)
library(RSelenium)

# Directories
outdir <- "data/vessel_ids/raw"

# Read PVNs
pvn_df <- read.csv("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/primary_vessel_ids_in_gillnet_logbook_data.csv")
pvns <- pvn_df$primary_id

# Loop through
################################################################################

# Firefox profile
fprof <- RSelenium::makeFirefoxProfile(list(
  # detail level for download (0 = Desktop, 1 = systems default downloads location, 2 = custom folder.)
  browser.download.folderList = 2L,
  # location for data download
  browser.download.dir = outdir ,
  # stores a comma-separated list of MIME types to save to disk without asking what to use to open the file
  browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  # disable PDF javascript so that PDFs are not displayed
  pdfjs.disabled = TRUE,
  # turn off scan and loading of any additionally added plug-ins
  plugin.scan.plid.all = FALSE,
  # high number defined for version of Adobe Acrobat
  plugin.scan.Acrobat = "99.0"))


# Launch RSelenium server and driver
rD <- RSelenium::rsDriver(browser="firefox",
                          # set which version of browser
                          version = "latest",
                          # Chrome version (turn off as Firefox will be used)
                          chromever = NULL,
                          # set which version of Gecko to use
                          geckover = "latest",
                          # status messages
                          verbose = TRUE,
                          # populate with the Firefox profile
                          extraCapabilities = fprof)
remDr <- rD[["client"]]
remDr$open(silent=T)

# Base URL
base_url <- "https://cgmix.uscg.mil/psix/psixsearch.aspx"

# Go to page
remDr$navigate(base_url)

# Loop through PVNs
i <- 1
# pvn_do <- "292133"
for(i in 1:length(pvns)){
  
  # PVN
  pvn_do <- pvns[i] %>% as.character()
  
  # Find Primary Vessel Number entry
  pvn_prompt <- remDr$findElement(using = "id", "TextBoxVesselNumber")
  
  # Enter taxa info into prompt
  pvn_prompt$sendKeysToElement(list(pvn_do)) # has to be a list for some reason
  
  # Find Search button
  search_button <- remDr$findElement(using="id", "ButtonSearch")
  search_button$clickElement()
  
  # Extract the table using rvest
  # Note: Assuming the table has id "GridViewPSIX"
  table_html <- remDr$getPageSource()[[1]]
  table <- read_html(table_html) %>%
    html_node("#GridViewPSIX") %>%
    html_table(fill = TRUE)
  
  # Extract the first table (you may need to adjust the index if there are multiple tables)
  table_df <- as.data.frame(table)
  
  # Find the name of vessel you want by eliminating ones with "DUPLICATE" in the name
  # The VIN must also match the PVN
  vessel_name_to_click <- table_df %>% 
    janitor::clean_names() %>% 
    filter(!grepl("DUPLICATE", vessel_name) & vin==pvn_do) %>% 
    pull(vessel_name)
  
  # Determine the correct ID dynamically based on the vessel name
  link_xpath <- sprintf("//a[text()='%s']", vessel_name_to_click)
  link_element <- remDr$findElement(using = "xpath", value = link_xpath)
  
  # Click on the hyperlink directly
  link_element$clickElement()
  
  # # 2)Determine the correct ID dynamically based on the vessel name
  # link_xpath <- sprintf("//a[text()='%s']", vessel_name_to_click)
  # link_element <- remDr$findElement(using = "xpath", value = link_xpath)
  # link_id <- link_element$getElementAttribute("id")$value
  # 
  # # 2) Execute JavaScript to click on the hyperlink
  # js_code <- sprintf("__doPostBack('%s','')", link_id)
  # remDr$executeScript(js_code)
  
  # 1) Execute JavaScript to click on the "NIGHT STALKER" hyperlink
  # vessel_name_to_click <- "NIGHT STALKER"
  # js_code <- sprintf("__doPostBack('GridViewPSIX$ctl03$ButtonDetails','')")
  # remDr$executeScript(js_code)
  
  # Table 1 text
  table1 <- remDr$findElement(using = "id", value = "LabelVesselInfo")
  table1_text <- table1$getElementText()[[1]]
  
  # Format text
  table1_lines <- unlist(strsplit(table1_text, "\n"))
  table1_df <- purrr::map_df(table1_lines, function(x){
    parts <- strsplit(x, ": ") %>% unlist()
    df <- tibble(attrbute=parts[1],
                 value=parts[2])
  })
  
  # Table 2 text
  table2 <- remDr$findElement(using = "id", value = "LabelVesselPart")
  table2_text <- table2$getElementText()[[1]]
  
  # Format Table 2 text
  table2_lines <- unlist(strsplit(table2_text, "\n"))
  table2_df <- purrr::map_df(table2_lines, function(x){
    parts <- strsplit(x, ": ") %>% unlist()
    df <- tibble(attrbute=parts[1],
                 value=parts[2])
  })
  
  # Merge text
  df_out <- bind_rows(table1_df, table2_df)
  
  # Export
  filename <- paste0(pvn_do, ".csv")
  write.csv(df_out, file=file.path(outdir, filename), row.names = F)
  
  # Go back to main
  remDr$navigate(base_url)
  
}


# Close the Selenium browser
remDr$close()

# Stop the Selenium server
rD$server$stop()






