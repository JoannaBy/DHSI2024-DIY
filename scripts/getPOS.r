library(xml2)

# Function to extract attribute values from a TEI file
extract_attribute_values <- function(file_path, attribute) {
  doc <- read_xml(file_path)
  
  # Extract all nodes with the specified attribute
  nodes <- xml_find_all(doc, paste0("//*[@", attribute, "]"))
  
  # Extract attribute values
  values <- xml_attr(nodes, attribute)
  
  return(values)
}

# Function to save attribute values to a file
save_attribute_values <- function(values, output_file_path) {
  writeLines(values, con = output_file_path)
}

# Function to process TEI files in a directory and save attribute values to new directories
process_tei_files <- function(input_directory, output_directory_pos, output_directory_lemma) {
  if (!dir.exists(output_directory_pos)) {
    dir.create(output_directory_pos)
  }
  if (!dir.exists(output_directory_lemma)) {
    dir.create(output_directory_lemma)
  }
  
  files <- list.files(input_directory, pattern = "\\.xml$", full.names = TRUE)
  
  for (file_path in files) {
    pos_values <- extract_attribute_values(file_path, "pos")
    lemma_values <- extract_attribute_values(file_path, "lemma")
    
    output_file_path_pos <- file.path(output_directory_pos, paste0(basename(sub("\\.xml$", "_pos.txt", file_path))))
    output_file_path_lemma <- file.path(output_directory_lemma, paste0(basename(sub("\\.xml$", "_lemma.txt", file_path))))
    
    save_attribute_values(pos_values, output_file_path_pos)
    save_attribute_values(lemma_values, output_file_path_lemma)
    
    cat("Processed", basename(file_path), 
        "- pos values saved to", basename(output_file_path_pos),
        "- lemma values saved to", basename(output_file_path_lemma), "\n")
  }
}

# Specify the directories
input_directory <- "corpus"
output_directory_pos <- "pos-corpus"
output_directory_lemma <- "lemma-corpus"

# Process the TEI files
process_tei_files(input_directory, output_directory_pos, output_directory_lemma)