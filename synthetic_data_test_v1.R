## test making synthetic data

# Create a one dimensional array for our independent variable

install.packages("tidyverse")
library(tidyverse)

NumEntries = 10
X=1:NumEntries

# Create an array with random values as our dependent data

RandomMean=0 # this will be the mean of our random distribution
RandomStdDev=1 # this will be the standard deviation of the distribution


#rnoorm creates random values for a normal distribution

RandomValues=rnorm(NumEntries, mean = RandomMean, sd = RandomStdDev) 
Y=RandomValues # one number between -1 and 1


plot(Y)

# Plot X against Y 
plot(X,Y) 


# Setup the parameters for the trend

B0=1 # intercept
B1=1 # slope

Y=B0+B1*X

plot(Y)
abline(B0,B1)


# Combine the trend and range data

Y=B0+B1*X + RandomValues

Patient <- 1:10

condition <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
prevalence <- c(10, 10, 20, 20, 1, 0.1, 5, 5, 30, 15)
impact <- c(1, 10, 2, 1, 7, 4, 5, 3, 6, 3)


conditions <- tibble(condition,  prevalence, impact, prob = prevalence / 100)

set.seed(42) #maked random numbers reproducable

data <- tibble(pt = 1:1000)

# For each condition: simulate has_, store prob_, calculate impact_
for (i in seq_len(nrow(conditions))) {
  
  cond <- conditions$condition[i]
  p    <- conditions$prob[i]
  imp  <- conditions$impact[i]
  
data <- data %>%
    mutate(
      "prob_{cond}"   := p,
      "has_{cond}"    := rbinom(n(), size = 1, prob = p),
      "impact_{cond}" := ifelse(.data[[paste0("has_", cond)]] == 1,
                                pmax(rnorm(n(), mean = imp, sd = imp * 0.5), 1),
                                0)
    )
}


data <- data %>%
  mutate(
    n_conditions = rowSums(across(starts_with("has_"))),
    total_impact = rowSums(across(starts_with("impact_")))
  )

# library(ggplot2)



ggplot(data, aes(x = n_conditions, y = total_impact)) +
  geom_jitter(width = 0.2, height = 0) +
  labs(
    x = "Total number of conditions",
    y = "Total impact",
    title = "Condition count vs. total impact per person"
  ) +
  scale_x_continuous(
    breaks = 0:max(data$n_conditions),
    # limits = c(0, max(data$n_conditions))
  ) +
  scale_y_continuous(
    breaks = c(5, 15, 25),
    labels = c("Low", "Medium", "High"),
    # limits = c(0, max(data$total_impact))
  ) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  theme_classic()



