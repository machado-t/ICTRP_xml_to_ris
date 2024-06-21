# install.packages("xml2")
# install.packages("dplyr")

library(xml2)
library(dplyr)

# Function to convert XML to RIS format
xml_to_ris <- function(xml_file, ris_file) {
  # Read and parse the XML file
  xml_data <- read_xml(xml_file)
  
  # # Extract relevant data
  trial_ids <- xml_find_all(xml_data, ".//TrialID") %>% xml_text()
  scientific_titles <- xml_find_all(xml_data, ".//Scientific_title") %>% xml_text()
  date_reg3 <- xml_find_all(xml_data, ".//Date_registration3") %>% xml_text()
  urls <- xml_find_all(xml_data, ".//web_address") %>% xml_text()
  source_registers <- xml_find_all(xml_data, ".//Source_Register") %>% xml_text()
  primary_sponsors <- xml_find_all(xml_data, ".//Primary_sponsor") %>% xml_text()
  
  
  # Open a connection to the RIS file
  ris_conn <- file(ris_file, "w")
  
  
  # Write data in RIS format
  for (i in seq_along(scientific_titles)) {
    cat("TY  - ELEC\n", file = ris_conn)
    cat("TI  - ", scientific_titles[i], "\n", file = ris_conn)
    
    # Add the primary sponsor as the author
    if (i <= length(primary_sponsors) && primary_sponsors[i] != "") {
      cat("AU  - ", primary_sponsors[i], "\n", file = ris_conn)
    }

    # Use Date_registration3 for the year and full date
    if (i <= length(date_reg3) && date_reg3[i] != "") {
      year <- substr(date_reg3[i], 1, 4)
      cat("PY  - ", year, "\n", file = ris_conn)

      full_date <- paste(substr(date_reg3[i], 1, 4), substr(date_reg3[i], 5, 6), substr(date_reg3[i], 7, 8), sep = "-")
      cat("DA  - ", full_date, "\n", file = ris_conn)
    }

    if (i <= length(urls)) {
      cat("UR  - ", urls[i], "\n", file = ris_conn)
    }
    
    if (i <= length(source_registers)) {
      cat("PB  - ", source_registers[i], "\n", file = ris_conn)
    }
    
    # Add trial ID as website title
    if (i <= length(trial_ids)) {
      cat("T2  - ", trial_ids[i], "\n", file = ris_conn)
    }
    
    cat("ER  - \n\n", file = ris_conn)
  }
  
  # Close the connection to the RIS file
  close(ris_conn)
}

# Example usage
xml_file <- "ICTRP-Results.xml"
ris_file <- "output_file.ris"
xml_to_ris(xml_file, ris_file)

