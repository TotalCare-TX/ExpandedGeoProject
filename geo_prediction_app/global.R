#global.R
library(shinymanager)

# Define credentials
credentials <- data.frame(
  user = c("admin", "user"),                # usernames
  password = c("adminpass", "userpass"),    # passwords
  stringsAsFactors = FALSE
)