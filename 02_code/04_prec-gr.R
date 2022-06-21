# get precision and annual growth rate

library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
library(paletteer) # Comprehensive Collection of Color Palettes

# mean precision----
load("03_output/united_df.RData")

precision <- united_df %>%
  mutate(prec = 1 / sd^2) %>%
  filter(year %in% 2005:2012)

prec_sum <- precision %>%
  group_by(dataset, resolution) %>%
  mutate(prec = mean(prec)) %>%
  ungroup() %>%
  select(prec, resolution, dataset) %>%
  distinct()

# annual growth rate----
# Function for annual growth rate
annual_growth_rate <- function(first, last, nyrs) {
  (((last / first)^(1 / nyrs)) - 1) * 100
}
load("03_output/post_samples.RData")

# we want the annual growth rate per model, resolution, species combo
growth <- function(samples) {
  ls <- list()
  ls1 <- list()
  ls2 <- list()
  for (i in 1:7) { # model level
    if (i == 1) {
      ds <- "iBats"
    } else if (i == 2) {
      ds <- "Field"
    } else if (i == 3) {
      ds <- "Mammal Society"
    } else if (i == 4) {
      ds <- paste("iBats +", "Field", sep = "\n")
    } else if (i == 5) {
      ds <- paste("iBats +", "Mammal Society", sep = "\n")
    } else if (i == 6) {
      ds <- paste("Field +", "Mammal Society", sep = "\n")
    } else if (i == 7) {
      ds <- paste("iBats +", "Field +", "Mammal Society", sep = "\n")
    }
    ls3 <- list()
    ls4 <- list()
    for (j in 1:3) { # resolution level

      if (j == 1) {
        res <- "1 km"
      } else if (j == 2) {
        res <- "2 km"
      } else if (j == 3) {
        res <- "5 km"
      }
      ls5 <- list()
      ls6 <- list()
      for (k in 1:4) { # species level

        post <- samples[[i]][[j]][[k]]

        if (length(post) == 10) {
          first_yr <- 2005
          last_yr <- 2012
        } else if (length(post) == 23) {
          first_yr <- 1998
          last_yr <- 2018
        } else if (length(post) == 14) {
          first_yr <- 2005
          last_yr <- 2016
        }

        colnames(post) <- first_yr:last_yr
        post <- post[1:(length(post) - 1)]
        names(post)[length(names(post))] <- "spp"

        first <- post[, 1]
        last <- post[, ncol(post) - 1]
        rates <- annual_growth_rate(first, last, ncol(post))

        ann_growth_rate <- round(mean(rates), 2)

        # Estimate the credible intervals from the posterior samples
        CI_lower <- quantile(rates, probs = 0.025, na.rm = TRUE)
        CI_upper <- quantile(rates, probs = 0.975, na.rm = TRUE)
        # Present the results
        results <- as.data.frame(cbind(ann_growth_rate, CI_lower, CI_upper)) %>%
          mutate(species = post$spp[1]) %>%
          mutate(resolution = res) %>%
          mutate(model = ds)

        rates <- as.data.frame(rates) %>%
          mutate(species = post$spp[1]) %>%
          mutate(resolution = res) %>%
          mutate(model = ds)

        ls5[[k]] <- results
        ls6[[k]] <- rates
      }
      ls4[[j]] <- bind_rows(ls5)
      ls3[[j]] <- bind_rows(ls6)
    }
    ls2[[i]] <- bind_rows(ls4)
    ls1[[i]] <- bind_rows(ls3)
  }
  results <- bind_rows(ls2)
  rates <- bind_rows(ls1)
  ls <- list(results, rates)
  return(ls)
}

both <- growth(samples)
