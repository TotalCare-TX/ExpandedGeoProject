#find_address_coordinates.R
library(httr)
library(jsonlite)
library(stringr)

# Function to get coordinates from an address or text
get_coordinates <- function(input_text) {
  input_text <- trimws(input_text)
  
  tryCatch({
    res <- GET(
      url = "https://nominatim.openstreetmap.org/search",
      query = list(q = input_text, format = "json", limit = 1),
      user_agent("CoordinateFetcherR/1.0")
    )
    
    if (status_code(res) != 200) {
      return(list(status = "error", message = paste("API error:", status_code(res))))
    }
    
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    
    if (length(data) == 0) {
      return(list(status = "error", message = "No coordinates found for the given input."))
    }
    
    display_name <- tolower(data$display_name[1])
    input_words <- str_split(tolower(input_text), "\\W+")[[1]]
    input_words <- input_words[nchar(input_words) >= 4]
    
    match_count <- sum(sapply(input_words, function(word) grepl(word, display_name, fixed = TRUE)))
    
    if (match_count < 1) {
      return(list(status = "error", message = "Weak match. The result likely does not reflect the input."))
    }
    
    return(list(
      status = "success",
      latitude = data$lat,
      longitude = data$lon,
      display_name = data$display_name
    ))
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Unexpected error:", e$message)))
  })
}
