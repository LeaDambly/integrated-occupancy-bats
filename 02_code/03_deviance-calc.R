# calculating likelihood-based deviance: non-integrated first, then integrated
# as this was also done on JASMIN, code needs to be changed as needed for different models
# load libraries
library(sparta) # Trend Analysis for Unstructured Data
library(rjags) # Bayesian Graphical Models using MCMC
library(tidyverse) # Easily Install and Load the 'Tidyverse'

# for non-integrated models----
# change folder as needed to direct to sparta output
folder1 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field1km/"
files1 <- list.files(folder1, pattern = "*.rdata", full.names = TRUE)

modellist1 <- list()
for (i in 1:length(files1)) {
  load(files1[i])
  modellist1[[i]] <- out
}
# name list item by species
for (i in 1:length(modellist1)) names(modellist1)[i] <- modellist1[[i]]$SPP_NAME
getpy2 <- function(out) {
  
  # recompile model
  out$model$recompile()
  
  # sample Py 100 times in 3 chains
  out_recomp <- coda.samples(out$model, variable.names = c("Py"), n.iter = 100)
  
  # extract each chain, make columns into rows, add column with chain number
  Py1 <- as.data.frame(out_recomp[[1]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 1)
  
  Py2 <- as.data.frame(out_recomp[[2]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 2)
  
  Py3 <- as.data.frame(out_recomp[[3]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 3)
  
  allPy <- bind_rows(Py1, Py2, Py3)
  
  # get meanPy per visit
  meanPy <- allPy %>%
    group_by(visit) %>%
    mutate(meanPy = mean(Py)) %>%
    ungroup() %>%
    select(visit, meanPy) %>%
    distinct()
  
  # now to get Ytruth
  # extract data used in model
  df <- as.data.frame(out$model$data())
  
  # get ytruth, aka the observed data
  ytruth <- df %>%
    mutate(visit = 1:n()) %>%
    select(visit, y)
  
  ally2 <- inner_join(meanPy, ytruth, by = "visit") %>%
    mutate(lhood1 = (meanPy^y) * ((1 - meanPy)^(1 - y)))
  
  DEV1 <- -(2 * (sum(log(ally2$lhood1))))
  
  results <- list(ally2, DEV1)
  return(results)
}

devres <- list()
devres <- lapply(modellist1, getpy2)

save(devres, file = "03_output/deviances/deviance_field1km.RData")

# for all integrated models----
# change folder as needed to direct to sparta output
folder1 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+iBats1km/"
files1 <- list.files(folder1, pattern = "*.rdata", full.names = TRUE)

modellist1 <- list()
for (i in 1:length(files1)) {
  load(files1[i])
  modellist1[[i]] <- out
}
# name list item by species
for (i in 1:length(modellist1)) names(modellist1)[i] <- modellist1[[i]]$SPP_NAME

getpy <- function(out) {
  
  # recompile model
  out$model$recompile()
  
  # sample Py 100 times in 3 chains
  out_recomp <- coda.samples(out$model, variable.names = c("Py"), n.iter = 100)
  
  # extract each chain, make columns into rows, add column with chain number
  Py1 <- as.data.frame(out_recomp[[1]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 1)
  
  Py2 <- as.data.frame(out_recomp[[2]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 2)
  
  Py3 <- as.data.frame(out_recomp[[3]]) %>%
    mutate(iteration = 1:n()) %>%
    pivot_longer(., cols = starts_with("Py"), names_to = "visit", values_to = "Py") %>%
    mutate(visit = parse_number(visit)) %>%
    mutate(chain = 3)
  
  
  allPy <- bind_rows(Py1, Py2, Py3)

  # get meanPy per visit
  meanPy <- allPy %>%
    group_by(visit) %>%
    mutate(meanPy = mean(Py)) %>%
    ungroup() %>%
    select(visit, meanPy) %>%
    distinct()
  
  # now to get Ytruth
  # extract data used in model
  df <- as.data.frame(out$model$data())
  
  # get ytruth, aka the observed data
  ytruth <- df %>%
    mutate(visit = 1:n()) %>%
    mutate(DATATYPE1 = if_else(DATATYPE2 == 1, 0, if_else(DATATYPE3 == 1, 0, 1))) %>%
    select(visit, DATATYPE1, DATATYPE2, DATATYPE3, y)
  
  ally2 <- inner_join(meanPy, ytruth, by = "visit") %>%
    mutate(lhood1 = (meanPy^y) * ((1 - meanPy)^(1 - y)))
  
  DEV1 <- -(2 * (sum(log(subset(ally2, DATATYPE1 == 1)$lhood1))))
  DEV2 <- -(2 * (sum(log(subset(ally2, DATATYPE2 == 1)$lhood1))))
  DEV3 <- -(2 * (sum(log(subset(ally2, DATATYPE3 == 1)$lhood1))))
  
  DEV <- list(DEV1, DEV2, DEV3)
  
  results <- list(ally2, DEV)
  return(results)
}

devres <- list()
devres <- lapply(modellist1, getpy)

save(devres, file = "03_output/deviances/deviance_fib1km.RData")

