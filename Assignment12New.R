
library(rvest)
library(knitr)

# RE-READ the three airline websites (connections expired)
klm <- read_html("https://www.airlinequality.com/airline-reviews/klm-royal-dutch-airlines/")
transavia <- read_html("https://www.airlinequality.com/airline-reviews/transavia/")
air_france <- read_html("https://www.airlinequality.com/airline-reviews/air-france/")

scrape.av <- function(page, airline_name) {
  
  # Scrape the star ratings
  stars <- page %>%
    html_nodes(".stars .fill") %>%
    html_text()
  
  # Convert to numeric
  stars_num <- as.numeric(stars)
  
  # Find where "1,2,3,4,5" sequence first appears
  first_12345_pos <- NA
  for(i in 1:(length(stars_num)-4)) {
    if(all(stars_num[i:(i+4)] == 1:5)) {
      first_12345_pos <- i
      break
    }
  }
  
  # Extract everything before the "12345" sequence
  before_12345 <- stars_num[1:(first_12345_pos-1)]
  
  # Find positions where the next value is 1 (or end of vector)
  # These are the "peaks" - our category ratings
  is_peak <- c(before_12345[-1] == 1, TRUE)  # Check if next element is 1
  avg_ratings <- before_12345[is_peak][1:5]  # Take first 5 peaks
  
  # Create data frame
  result <- data.frame(
    Airline = airline_name,
    Food_Beverages = avg_ratings[1],
    Inflight_Entertainment = avg_ratings[2],
    Seat_Comfort = avg_ratings[3],
    Staff_Service = avg_ratings[4],
    Value_for_Money = avg_ratings[5]
  )
  
  return(result)
}

# Apply to airlines using sapply (more R-like!)
airlines <- list(
  "KLM Royal Dutch Airlines" = klm,
  "Transavia" = transavia,
  "Air France" = air_france
)

# Use lapply to apply function to each airline
avg_list <- lapply(names(airlines), function(name) {
  scrape.av(airlines[[name]], name)
})

# Combine using do.call
average_ratings <- do.call(rbind, avg_list)

print(average_ratings)
kable(average_ratings, row.names = FALSE)










library(ggplot2)
library(tidyr)

# Reshape the data from wide to long format for ggplot
average_ratings_long <- pivot_longer(
  average_ratings,
  cols = c(Food_Beverages, Inflight_Entertainment, Seat_Comfort, 
           Staff_Service, Value_for_Money),
  names_to = "Category",
  values_to = "Rating"
)

# Clean up category names for better display
average_ratings_long$Category <- gsub("_", " ", average_ratings_long$Category)

# Create the grouped barplot
ggplot(average_ratings_long, aes(x = Airline, y = Rating, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Ratings by Category for Three Airlines",
    x = "Airline",
    y = "Average Rating (out of 5 stars)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")