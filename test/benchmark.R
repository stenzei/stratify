library(dplyr)
library(foreach)
library(doParallel)
library(ggplot2)
source("../app/stratify-data.R")

dta <- NULL
for (len in c(1e+3, 1e+4, 1e+5, 1e+6, 1e+07)) {
  # Create dependent data.
  dist1 <- runif(len, min = 1, max = 5) 
  dist2 <- rnorm(len, mean = 0, sd = 3)
  dist3 <- rnorm(len, mean = 4, sd = 1) 
  dist4 <- rlnorm(len, meanlog = 0, sdlog = 0.5)
  l <- list("var1" = dist1 * dist3,
            "var2" = dist2,
            "var3" = dist3 * dist2,
            "var4" = dist4 * dist3 + dist2)
  original.data <- as_data_frame(l)
  size <- object.size(original.data)
  
  # Run in parallel
  registerDoParallel(2)
  res <- foreach(i = c(1:50), .combine = c) %dopar% {
    # Stratify the data:
    system.time(stratify(original.data))[[3]]
  }
  stopImplicitCluster()
  res <- c(len, size, res)
  dta <- rbind(dta, res)
}
write.csv(dta, file = "../data/benchmark.csv")

dta <- as_data_frame(read.csv("../data/benchmark.csv"))
dta <- dta %>% 
  select(-X) %>% 
  gather(key = "no.use", value = "time", 3:52) %>% 
  select(-no.use) %>% 
  transmute(no.data.points = V1, data.size = V2, time)

ggplot(dta, aes(factor(no.data.points), time)) +
  geom_violin(trim = TRUE, 
              scale = "width", 
              fill = "lightgreen", 
              colour = "darkgreen",
              adjust = 1.5) + 
  coord_flip() +
  scale_y_log10() +
  labs(x = "number of data points",
       y = "log of elapsed time in seconds")

