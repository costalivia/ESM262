fish_analysis <- function(fish_vector) {
  # Convert the vector to a data frame
  fish_data <- data.frame(fish = fish_vector)

  # Calculate the most common fish
  most_common_fish <- names(sort(table(fish_data$fish), decreasing = TRUE))[1]

  # Calculate the rarest fish
  rarest_fish <- names(sort(table(fish_data$fish), decreasing = FALSE))[1]

  # Calculate the total number of fish
  total_fish <- length(fish_vector)

  # Return the results
  result <- list(
    most_common_fish = most_common_fish,
    rarest_fish = rarest_fish,
    total_fish = total_fish
  )
}
