

# Function to get coordinates from an address or text
get_coordinates <- function(input_text) {
  input_text <- trimws(input_text)
  
  tryCatch({
    # Make API request
    res <- GET(
      url = "https://nominatim.openstreetmap.org/search",
      query = list(q = input_text, format = "json", limit = 1),
      user_agent("CoordinateFetcherR/1.0")
    )
    
    # Check if request failed
    if (status_code(res) != 200) {
      return(list(status = "error", message = paste("API error:", status_code(res))))
    }
    
    # Parse JSON response
    content_text <- content(res, as = "text", encoding = "UTF-8")
    data <- fromJSON(content_text)
    
    if (length(data) == 0) {
      return(list(status = "error", message = "No coordinates found for the given input."))
    }
    
    # Matching logic
    display_name <- tolower(data$display_name[1])
    input_words <- str_split(tolower(input_text), "\\W+")[[1]]  
    input_words <- input_words[nchar(input_words) >= 4]        
    
    # Count matches
    match_count <- sum(sapply(input_words, function(word) grepl(word, display_name, fixed = TRUE)))
    
    # Require at least 1 matching meaningful word
    if (match_count < 1) {
      return(list(status = "error", message = "Weak match. The result likely does not reflect the input."))
    }
    
    # Return clean output
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

# MAIN SCRIPT 

# Enter your location between quotes
#input <- "Enter Location Here"

# Call function
#result <- get_coordinates(input)

# Display result
#if (!is.null(result$status) && result$status == "success") {
  #cat("Location found:\n")
  #cat("Display Name:", result$display_name, "\n")
  #cat("Latitude:", result$latitude, "\n")
  #cat("Longitude:", result$longitude, "\n")
#} else {
  #cat("Error:", result$message, "\n")
#}
