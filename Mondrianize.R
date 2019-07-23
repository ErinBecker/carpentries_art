# July 23, 2019
# Erin Becker
# Create Mondian style paintings given input tabular data

# load libraries
library(ggplot2)
library(dplyr)

# parameters of dataset
# number of columns
# number of rows
# number of types of columns
# for each column:
# - order
# - type
# - mean (or mean length for character vectors)
# - sum (or sum length for character vectors)

# parameters of painting
# width
# height
# number of colors
# colors
# for each element: 
# - order
# - width
# - height
# - color
# - number of borders
# - position of borders
# - position on canvas

# how each of the painting parameters will be
# determined by the dataset
# number of elements = number of columns
# canvas is a square with side of length = number of rows
# average number of spaces covered by each element = 
#   (number rows)^2 / (number cols)
# for each element:
# - order = row order
# - width = sum(column)/nrow remainder + 1 (to keep from being zero)
# - height = mean(column)/nrow remainder + 1 (to keep from being zero)
# - color = 
#     - numeric = red
#     - character = black
#     - logical = d. blue
#     - factor = l. blue
#     - int = yellow
# - num borders = (num na)/4 remainder + 1?
# - position of borders = ?
# any space not covered by elements will be white
# using stringsAsFactors = TRUE, 
#   so characters can be treated as numeric

# Read in data
# and convert to data.frame (won't work if tibble)
data <- iris %>%
  as.data.frame()

# create color palette
colors <- c("red", "black", "darkblue", "lightblue", "yellow")
names(colors) <- c("numeric", "character", "logical", "factor", "integer")

# Create painting parameter df from data
# initialize df
n <- ncol(data)
paint_df <- data.frame(order = integer(n), width = integer(n), 
                       height = integer(n),
                       num_border = integer(n), position_border = numeric(n))
# # populate df
# for (i in 1:n) {
#     # convert column to numeric
#     current_col <- as.numeric(data[,i])
#     # extract class of column
#     col_class <- class(data[,i])
#   paint_df$order[i] <- i
#   paint_df$width[i] <- round(sum(current_col)) %% nrow(data) + 1
#   paint_df$height[i] <- round(mean(current_col)) %% nrow(data) + 1
#   paint_df$color[i] <- unname(colors[col_class])
# }

max_width <- nrow(data)/(ncol(data)*2)
max_height <- nrow(data)/(ncol(data)*2)
total_width <- 0
total_height <- max_height

# populate df
for (i in 1:n) {
  # convert column to numeric
  current_col <- as.numeric(data[,i])
  # extract class of column
  col_class <- class(data[,i])
  paint_df$order[i] <- i
  paint_df$width[i] <- round(median(current_col)) %% nrow(data) + 1
  paint_df$height[i] <- round(mean(current_col)) %% nrow(data) + 1
  paint_df$color[i] <- unname(colors[col_class])
  # add x coordinates based on width
  paint_df$xmin[i] <- total_width
  paint_df$xmax[i] <- paint_df$xmin[i] + paint_df$width[i]
  total_width <- paint_df$xmax[i]
  # add y coordinates based on height
  paint_df$ymax[i] <- total_height
  paint_df$ymin[i] <- paint_df$ymax[i] - paint_df$height[i]
  # if total element width exceeds canvas width
  # reset xcoord to 0
  # and set ycoord to min height of previous elements
  if (total_width >= max_width) {
    total_width = 0
    total_height = min(paint_df$ymin)
  }
  # total_height <- paint_df$ymin[i]
}

# Plot using geom_rect in ggplot2
# need to fix color mapping
# don't like how rects are distributed across canvas - 
# need white space between rects and to allow for white
# elements bordering edge of canvas
ggplot() + 
  geom_rect(data = paint_df,
          mapping = 
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax, 
                fill = color),
          color = "black") + 
  theme_void() + 
  theme(legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA)) + 
  expand_limits(x = 0, y = 0) + 
  expand_limits(x = c(0, max_width),
                y = c(0, max_height))
  
