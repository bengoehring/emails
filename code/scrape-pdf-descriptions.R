library(rvest)
library(httr)
library(tidyverse)


# Create a destination folder
dest_dir <- "data/texas-personnel/job-descriptions/raw-pdfs/"

# Read the HTML page
page <- read_html("https://hr.sao.texas.gov/compensationsystem/jobdescriptions")

# Extract all links in the table that point to PDFs
pdf_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  na.omit() %>%
  str_subset("\\.pdf$") %>% 
  unique() %>% 
  str_subset("R\\d+")

# Some links may be relative, convert to full URLs
pdf_urls <- url_absolute(pdf_links, 
                         "https://hr.sao.texas.gov/compensationsystem/jobdescriptions")

# Download each file
for (u in pdf_urls) {
  fname <- basename(u)
  dest <- file.path(dest_dir, 
                    fname)
  Sys.sleep(.25)
  if (!file.exists(dest)) {
    cat("Downloading", fname, "...\n")
    resp <- GET(u)
    
    if (status_code(resp) == 200) {
      writeBin(content(resp, "raw"), dest)
      
    } else {
      warning("Failed to download: ", u)
    }
    
  } else {
    cat("Already exists:", 
        fname, 
        "\n")
  }
}
