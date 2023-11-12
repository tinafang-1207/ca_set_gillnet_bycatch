

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(RSelenium)

# Directories
outdir <- "data/vessel_ids/raw"


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


# PVNs 
pvns <- c("546360", "592017", "570760", "611940", "638547", "622026", "647513", "512678", "550580", "610884", "690689", "537827",
          "563793", "608155", "559214", "611816", "590891", "956837", "1044140", "628101", "591067", "627282", "1078115", "292133",
          "597524", "276600", "978988", "906370", "638547", "956837", "553607", "1078115", "512678", "906370", "292133", "611816",
          "647513", "553607", "592017", "276600", "927890", "690689", "969797", "570760", "584748", "597524", "608155", "618104",
          "626847", "628101", "1044140", "611940", "978988", "580562", "590891", "656254", "697944", "610884", "253247", "620078", 
          "563793", "632207", "622587", "1217070", "629818", "566847", "509557", "622026", "635102", "514371", "550580", "668163",
          "299076", "571399", "537827", "648910", "559214", "1217070", "295397", "550062", "620078", "550062", "253247", "644228", 
          "509557", "618104", "635102", "545886", "584748", "295397", "656254", "969797", "571063", "552120", "1107052", "571399",
          "614077", "248798", "629818")

# Loop through PVNs
i <- 1
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
  
  # Find and press link
  vessel_link <- remDr$findElement(using = "id", "GridViewPSIX_ctl02_ButtonDetails")
  vessel_link$clickElement()
  
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









