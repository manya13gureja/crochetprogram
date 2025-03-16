# Load required libraries
library(tidyverse)
library(purrr)
library(tibble)

# Define the pattern function
pattern <- function(a, b, c_value = pi/2, d_value = pi/5, e_value = 4) {
  tibble(
    x = accumulate(1:40, ~ .x - sin((.y %% e_value) * c_value - ceiling((.y - 1) / e_value) * d_value), .init = a), # nolint # nolint: line_length_linter.
    y = accumulate(1:40, ~ .x + cos((.y %% e_value) * c_value - ceiling((.y - 1) / e_value) * d_value), .init = b) # nolint # nolint: line_length_linter.
  )
}

# Arrange patterns on a circle
t <- seq(0, 2 * pi, length.out = 120)
centers <- tibble(x = sin(t), y = cos(t))

# Initialize parameters
c_value <- sample(seq(from = 0, to = 2 * pi, by = pi / 32), 1)
d_value <- sample(seq(from = 0, to = 2 * pi, by = pi / 32), 1)
e_value <- sample(1:20, 1)

# Generate and combine patterns
result <- map_df(1:seq_len(nrow(centers)), function(i) {
  pattern(a = centers$x[i],
          b = centers$y[i],
          c_value = c_value,
          d_value = d_value,
          e_value = e_value) %>%
    mutate(df = i)  # Add identifier
})

# Plot the patterns
ggplot(result) +
  geom_path(aes(x, y, group = df), alpha = 0.15) +
  coord_fixed() +
  theme_void()

# Save the output
ggsave("choose_a_name.png", height = 5, width = 5, units = "in", dpi = 800)
ggsave("choose_a_name.png", height = 5, width = 5, units = "in", dpi = 800)