install.packages("palmerpenguins")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(palmerpenguins)

# variables in penguins dataset: 
  # species - Adelie, Chinstrap, Gentoo
  # flipper_length_mm
  # body_mass_g

view(penguins)

ggplot(data = penguins)

# defining how variables will be plotted on graph
  # mapping argument always uses aes() function
  # flipper length mapped to x aesthetic; body mass mapped to y aesthetic

ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm, y=body_mass_g) 
) +
  geom_point(mapping = aes(color=species, shape=species)) +
  geom_smooth(method="lm") +
  labs(
    title="Relationship between Body Mass and Flipper Length",
    subtitle="Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x="Flipper Length (mm)", y="Body mass (g)", 
    color="Species", shape="Species"
  )
#to plot observations in dataframe, geom needs to be defined - geom_point() function adds points to plot
#color=species in geom_point to show relationship betw. flipper length & body mass by species at GLOBAl level

# 1.2.5 Exercises 

# Q1 - 344 rows, 8 columns

# Q2 - describes the bill length of the penguins in millimeters

# Q3
ggplot(
  data=penguins,
  mapping=aes(x=bill_length_mm, y=bill_depth_mm)
) +
  geom_point(mapping=aes(color=species, shape=species)) +
  geom_smooth(method="lm")+
  labs(
    title="Q3 - Relationship between Bill Length and Bill Depth",
    subtitle="Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x="Bill Length (mm)", y="Bill Depth (mm)", 
    color="Species", shape="Species"
  )
 
#Q4 - box plot is best for species vs bill depth
ggplot(
  data=penguins,
  mapping=aes(x=species, y=bill_depth_mm)
)+
  geom_point(mapping=aes(color=species, shape=species))+
  geom_smooth(method="lm")+
  labs(
    title="Q4 - Scatterplot of Species vs Bill Depth",
    x="Species", y="Bill Depth (mm)", 
    color="Species", shape="Species"
  )

ggplot(
  data=penguins,
  mapping=aes(x=species, y=bill_depth_mm)
)+
  geom_line(mapping=aes(color=species))+
  labs(
    title="Q4 - Scatterplot of Species vs Bill Depth",
    x="Species", y="Bill Depth (mm)", 
    color="Species", shape="Species"
  )

ggplot(
  data=penguins,
  mapping=aes(x=species, y=bill_depth_mm)
)+
  geom_boxplot(mapping=aes(color=species))+
  labs(
    title="Q4 - Box Plot of Species vs Bill Depth",
    x="Species", y="Bill Depth (mm)", 
    color="Species"
  )

#Q5
  # ggplot(data=penguins)+
  # geom_point() # No x & y axes defined, plus syntax

ggplot(
  data = penguins, 
  mapping = aes(x = bill_depth_mm, y = body_mass_g)
) + 
  geom_point(mapping=aes(color=species, shape=species)) + 
  labs(
    title="Q5 - Relationship between Body Mass and Bill Depth",
    x="Bill Depth (mm)", y="Body Mass (g)", 
)

#Q6
  # When na.rm is set to True, it omits missing values without producing a warning message
ggplot(
  data = penguins, 
  mapping = aes(x = bill_depth_mm, y = body_mass_g)
) + 
  geom_point(
    mapping=aes(color=species, shape=species),
    na.rm = TRUE
    ) + 
  labs(
    title="Q6 - Relationship between Body Mass and Bill Depth",
    x="Bill Depth (mm)", y="Body Mass (g)", 
  )

#Q7
ggplot(
  data = penguins, 
  mapping = aes(x = bill_depth_mm, y = body_mass_g)
) + 
  geom_point(
    mapping=aes(color=species, shape=species),
    na.rm = TRUE
  ) + 
  labs(
    title="Q7 - Relationship between Body Mass and Bill Depth",
    x="Bill Depth (mm)", y="Body Mass (g)", 
    caption = "Data come from the palmerpenguins package."
  )

#Q8:
ggplot(
  data = penguins, 
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point(
    mapping=aes(color=bill_depth_mm), # bill depth mapped to geom_point at local level 
  ) + 
  geom_smooth()+ 
  labs(
    title="Q8 - Relationship between Body Mass and Flipper Length",
    x="Flipper Length (mm)", y="Body Mass (g)",
  )

#Q9:
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)+ 
  labs(
    title="Q9")

#Q10 - plots would look the same - mappings are the same in both - plot A is mapped at global level, plot B splits them out at geom level
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()+
  labs(
    title = "Q10 - Plot A"
  )

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )+ 
  labs(
    title = "Q10 - Plot B"
  )

## 1.4 Visualising Distributions

### 1.4.1 A Categorical Variable 

ggplot(penguins, aes(x = species)) + 
  geom_bar() + 
  labs(title = "Bar Plot of Count of Species")

ggplot(penguins, aes(x = fct_infreq(species))) + #fct_infreq transforms categorical varible into a factor
  geom_bar() +
  labs(title = "Bar Plot of Count of Species (ordered)")

### 1.4.2 A Numerical Variable

ggplot(penguins, aes(x = body_mass_g)) + 
  geom_histogram(binwidth = 200) +
  labs(title = "Histogram Displaying Distribution of Body Mass Values")

ggplot(penguins, aes(x = body_mass_g)) + 
  geom_density() +
  labs(title = "Density Plot Displaying Distribution of Body Mass Values")

### 1.4.3 Exercises

# Q1
ggplot(penguins, aes(y = species))+
  geom_bar() +
  labs(title = "Q1 - Bar Plot of Species, Mapped to Y Aes") # count of species is on x axis, species is on y

# Q2
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red") +
  labs(title = "Q2 - Plot A") # produces a plot with red-outlined bars

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")+
  labs(title = "Q2 - Plot B") # produces a plot with red-filled bars

# Q3 - determines the number of bars in a histogram

# Q4 - 
ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 50) + 
  labs(title = "Q4 - Histogram Displaying Distribution of Carats (bins = 50)")

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 10) + 
  labs(title = "Q4 - Histogram Displaying Distribution of Carats (bins = 10)")  

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 1) + 
  labs(title = "Q4 - Histogram Displaying Distribution of Carats (bins = 1)")

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 0.10) + 
  labs(title = "Q4 - Histogram Displaying Distribution of Carats (bins = 0.1)")

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 0.01) + 
  labs(title = "Q4 - Histogram Displaying Distribution of Carats (bins = 0.01)")

## 1.5 Visualising Relationships 

### 1.5.1 A numerical and a categorical variable
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot() + 
  labs(title = "Box Plot Displaying Distribution of Body Mass per Species")

ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75) + 
  labs(title = "Density Plot Displaying Distribution of Body Mass per Species")

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot Displaying Distribution of Body Mass per Species (+ Fill")

### 1.5.2 Two Categorical Variables
ggplot(penguins, aes(x = island, fill = species)) + 
  geom_bar() + 
  labs(title = "Frequency of Species per Island")

ggplot(penguins, aes(x = island, fill = species)) + 
  geom_bar(position = "fill") + 
  labs(y = "proportion", title = "Proportion of Species per Island")

### 1.5.3 Two Numeric Variables - scatter plots using geom_point + geom_smooth (to add a line of best fit)

### 1.5.4 Three + Variables 
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = island)) + 
  labs(title = "Scatterplot Showing Relationship Between Body Mass, Flipper Length, and Species")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island) + 
  labs(title = "Scatterplot Showing Relationship Between Body Mass, Flipper Length, and Species by Island")





