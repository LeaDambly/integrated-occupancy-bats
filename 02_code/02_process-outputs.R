# code to process output from jasmin
library(plyr) # Tools for Splitting, Applying and Combining Data
library(tidyverse) # Easily Install and Load the 'Tidyverse'

# because of the file structure, I'll have to loop load these - very annoying! (I'm sure there's a better way)
# load all data first, then take posterior sample for later
getSample <- function(out, nsample = 999) {
  # This selects the posterior for the occupancy estimates (psi.fs) only
  raw_occ <- data.frame(out$BUGSoutput$sims.list)
  # Get the occupancy estimates from the summary table
  raw_occ <- raw_occ[, grep("psi.fs", colnames(raw_occ))]
  post <- raw_occ[sample(1:nrow(raw_occ), nsample), ]
  # Add metadata to our data
  post$spp <- out$SPP_NAME
  post$iter <- 1:nsample
  return(post)
}
# field----
# 1km
folder1 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field1km/"
files1 <- list.files(folder1, pattern = "*.rdata", full.names = TRUE)

modellist1 <- list()
for (i in 1:length(files1)) {
  load(files1[i])
  modellist1[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist1)) names(modellist1)[i] <- modellist1[[i]]$SPP_NAME
smpl1 <- lapply(modellist1, getSample)


# 2km
folder2 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field2km/"
files2 <- list.files(folder2, pattern = "*.rdata", full.names = TRUE)

modellist2 <- list()
for (i in 1:length(files2)) {
  load(files2[i])
  modellist2[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist2)) names(modellist2)[i] <- modellist2[[i]]$SPP_NAME
smpl2 <- lapply(modellist2, getSample)

# 5km
folder3 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field5km/"
files3 <- list.files(folder3, pattern = "*.rdata", full.names = TRUE)

modellist3 <- list()
for (i in 1:length(files3)) {
  load(files3[i])
  modellist3[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist3)) names(modellist3)[i] <- modellist3[[i]]$SPP_NAME
smpl3 <- lapply(modellist3, getSample)

field <- list(modellist1, modellist2, modellist3)
field_smpl <- list(smpl1, smpl2, smpl3)

rm(modellist1, modellist2, modellist3)
rm(smpl1, smpl2, smpl3)

# iBats----
# 1km
folder4 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats1km/"
files4 <- list.files(folder4, pattern = "*.rdata", full.names = TRUE)

modellist4 <- list()
for (i in 1:length(files4)) {
  load(files4[i])
  modellist4[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist4)) names(modellist4)[i] <- modellist4[[i]]$SPP_NAME
smpl4 <- lapply(modellist4, getSample)

# 2km
folder5 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats2km/"
files5 <- list.files(folder5, pattern = "*.rdata", full.names = TRUE)

modellist5 <- list()
for (i in 1:length(files5)) {
  load(files5[i])
  modellist5[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist5)) names(modellist5)[i] <- modellist5[[i]]$SPP_NAME
smpl5 <- lapply(modellist5, getSample)

# 5km
folder6 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats5km/"
files6 <- list.files(folder6, pattern = "*.rdata", full.names = TRUE)

modellist6 <- list()
for (i in 1:length(files6)) {
  load(files6[i])
  modellist6[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist6)) names(modellist6)[i] <- modellist6[[i]]$SPP_NAME
smpl6 <- lapply(modellist6, getSample)

iBats <- list(modellist4, modellist5, modellist6)
iBats_smpl <- list(smpl4, smpl5, smpl6)

rm(modellist4, modellist5, modellist6)
rm(smpl4, smpl5, smpl6)

# mamsoc----
# 1km
folder7 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/Mamsoc1km/"
files7 <- list.files(folder7, pattern = "*.rdata", full.names = TRUE)

modellist7 <- list()
for (i in 1:length(files7)) {
  load(files7[i])
  modellist7[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist7)) names(modellist7)[i] <- modellist7[[i]]$SPP_NAME
smpl7 <- lapply(modellist7, getSample)

# 2km
folder8 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/Mamsoc2km/"
files8 <- list.files(folder8, pattern = "*.rdata", full.names = TRUE)

modellist8 <- list()
for (i in 1:length(files8)) {
  load(files8[i])
  modellist8[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist8)) names(modellist8)[i] <- modellist8[[i]]$SPP_NAME
smpl8 <- lapply(modellist8, getSample)

# 5km
folder9 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/Mamsoc5km/"
files9 <- list.files(folder9, pattern = "*.rdata", full.names = TRUE)

modellist9 <- list()
for (i in 1:length(files9)) {
  load(files9[i])
  modellist9[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist9)) names(modellist9)[i] <- modellist9[[i]]$SPP_NAME
smpl9 <- lapply(modellist9, getSample)

mamsoc <- list(modellist7, modellist8, modellist9)
mamsoc_smpl <- list(smpl7, smpl8, smpl9)

rm(modellist7, modellist8, modellist9)
rm(smpl7, smpl8, smpl9)

# Field + iBats----
# 1km
folder10 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+iBats1km/"
files10 <- list.files(folder10, pattern = "*.rdata", full.names = TRUE)

modellist10 <- list()
for (i in 1:length(files10)) {
  load(files10[i])
  modellist10[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist10)) names(modellist10)[i] <- modellist10[[i]]$SPP_NAME
smpl10 <- lapply(modellist10, getSample)

# 2km
folder11 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+iBats2km/"
files11 <- list.files(folder11, pattern = "*.rdata", full.names = TRUE)

modellist11 <- list()
for (i in 1:length(files11)) {
  load(files11[i])
  modellist11[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist11)) names(modellist11)[i] <- modellist11[[i]]$SPP_NAME
smpl11 <- lapply(modellist11, getSample)

# 5km
folder12 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+iBats5km/"
files12 <- list.files(folder12, pattern = "*.rdata", full.names = TRUE)

modellist12 <- list()
for (i in 1:length(files12)) {
  load(files12[i])
  modellist12[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist12)) names(modellist12)[i] <- modellist12[[i]]$SPP_NAME
smpl12 <- lapply(modellist12, getSample)

field.p.iBats <- list(modellist10, modellist11, modellist12)
field.p.iBats_smpl <- list(smpl10, smpl11, smpl12)

rm(modellist10, modellist11, modellist12)
rm(smpl10, smpl11, smpl12)

# Field + mamsoc----
folder13 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+mamsoc1km/"
files13 <- list.files(folder13, pattern = "*.rdata", full.names = TRUE)

modellist13 <- list()
for (i in 1:length(files13)) {
  load(files13[i])
  modellist13[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist13)) names(modellist13)[i] <- modellist13[[i]]$SPP_NAME
smpl13 <- lapply(modellist13, getSample)

# 2km
folder14 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+mamsoc2km/"
files14 <- list.files(folder14, pattern = "*.rdata", full.names = TRUE)

modellist14 <- list()
for (i in 1:length(files14)) {
  load(files14[i])
  modellist14[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist14)) names(modellist14)[i] <- modellist14[[i]]$SPP_NAME
smpl14 <- lapply(modellist14, getSample)

# 5km
folder15 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/field+mamsoc5km/"
files15 <- list.files(folder15, pattern = "*.rdata", full.names = TRUE)

modellist15 <- list()
for (i in 1:length(files15)) {
  load(files15[i])
  modellist15[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist15)) names(modellist15)[i] <- modellist15[[i]]$SPP_NAME
smpl15 <- lapply(modellist15, getSample)

field.p.mamsoc <- list(modellist13, modellist14, modellist15)
field.p.mamsoc_smpl <- list(smpl13, smpl14, smpl15)

rm(modellist13, modellist14, modellist15)
rm(smpl13, smpl14, smpl15)


# iBats + mamsoc----
folder16 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats+mamsoc1km/"
files16 <- list.files(folder16, pattern = "*.rdata", full.names = TRUE)

modellist16 <- list()
for (i in 1:length(files16)) {
  load(files16[i])
  modellist16[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist16)) names(modellist16)[i] <- modellist16[[i]]$SPP_NAME
smpl16 <- lapply(modellist16, getSample)

# 2km
folder17 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats+mamsoc2km/"
files17 <- list.files(folder17, pattern = "*.rdata", full.names = TRUE)

modellist17 <- list()
for (i in 1:length(files17)) {
  load(files17[i])
  modellist17[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist17)) names(modellist17)[i] <- modellist17[[i]]$SPP_NAME
smpl17 <- lapply(modellist17, getSample)

# 5km
folder18 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/iBats+mamsoc5km/"
files18 <- list.files(folder18, pattern = "*.rdata", full.names = TRUE)

modellist18 <- list()
for (i in 1:length(files18)) {
  load(files18[i])
  modellist18[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist18)) names(modellist18)[i] <- modellist18[[i]]$SPP_NAME
smpl18 <- lapply(modellist18, getSample)

iBats.p.mamsoc <- list(modellist16, modellist17, modellist18)
iBats.p.mamsoc_smpl <- list(smpl16, smpl17, smpl18)

rm(modellist16, modellist17, modellist18)
rm(smpl16, smpl17, smpl18)

# all together----
# 1km
folder19 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/all1km/"
files19 <- list.files(folder19, pattern = "*.rdata", full.names = TRUE)

modellist19 <- list()
for (i in 1:length(files19)) {
  load(files19[i])
  modellist19[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist19)) names(modellist19)[i] <- modellist19[[i]]$SPP_NAME
smpl19 <- lapply(modellist19, getSample)

# 2km
folder20 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/all2km/"
files20 <- list.files(folder20, pattern = "*.rdata", full.names = TRUE)

modellist20 <- list()
for (i in 1:length(files20)) {
  load(files20[i])
  modellist20[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist20)) names(modellist20)[i] <- modellist20[[i]]$SPP_NAME
smpl20 <- lapply(modellist20, getSample)

# 5km
folder21 <- "D:/Lea/PhD/RProjects/Occ-bats/03_output/all5km/"
files21 <- list.files(folder21, pattern = "*.rdata", full.names = TRUE)

modellist21 <- list()
for (i in 1:length(files21)) {
  load(files21[i])
  modellist21[[i]] <- out
}

# name list item by species
for (i in 1:length(modellist21)) names(modellist21)[i] <- modellist21[[i]]$SPP_NAME
smpl21 <- lapply(modellist21, getSample)

all <- list(modellist19, modellist20, modellist21)
all_smpl <- list(smpl19, smpl20, smpl21)

rm(modellist19, modellist20, modellist21)
rm(smpl19, smpl20, smpl21)

rm(out)
rm(list = grep("files", ls(), value = TRUE))
rm(list = grep("folder", ls(), value = TRUE))

# extract data----
gc()
extract_data <- function(dlist, dataset) {
  df_list <- list()
  
  for (i in 1:length(dlist[[1]])) {
    taxa <- names(dlist[[1]][i])
    dlist_sub <- sapply(dlist, "[", taxa)
    
    df <- list()
    # get psi and Rhat
    for (j in 1:length(dlist_sub)) {
      # credit to the sparta plot function for this bit of code
      spp_data <- as.data.frame(dlist_sub[[j]]$BUGSoutput$summary)
      spp_data$X <- row.names(spp_data)
      new_data <- spp_data[grepl(paste0("^psi.fs", "\\["), spp_data$X), ]
      new_data$year <- (Year <- (dlist_sub[[j]]$min_year - 1) + as.numeric(gsub(paste0("psi.fs"), "", gsub("\\[|\\]", "", row.names(new_data)))))
      
      # rename columns, otherwise ggplot doesn't work properly
      names(new_data) <- gsub("2.5%", "quant_025", names(new_data))
      names(new_data) <- gsub("97.5%", "quant_975", names(new_data))
      
      # Add rhat T/F column
      new_data$rhat_threshold[new_data$Rhat < 1.1] <- "Good (<1.1)"
      new_data$rhat_threshold[new_data$Rhat > 1.1] <- "Bad (>1.1)"
      
      new_data$model <- j
      
      df[[j]] <- new_data
    }
    
    df <- map_df(df, ~ as.data.frame(.x))
    rownames(df) <- NULL
    
    df <- df %>%
      mutate(resolution = ifelse(model == 3, 5, model)) %>%
      mutate(resolution = paste0(resolution, "km")) %>%
      select(mean, sd, quant_025, quant_975, Rhat, X, year, rhat_threshold, resolution) %>%
      mutate(taxa = taxa) %>%
      mutate(dataset = dataset)
    
    df_list[[i]] <- df
  }
  df_list <- map_df(df_list, ~ as.data.frame(.x))
  return(df_list)
}

iBats_df <- extract_data(iBats, dataset = "iBats") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 2005:2012) %>%
  ungroup()

field_df <- extract_data(field, dataset = "Field Survey") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 1998:2018) %>%
  ungroup()

mamsoc_df <- extract_data(mamsoc, dataset = "Mammal Society") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 2005:2016) %>%
  ungroup()

field.p.iBats_df <- extract_data(field.p.iBats, dataset = "Field Survey + iBats") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 1998:2018) %>%
  ungroup()

field.p.mamsoc_df <- extract_data(field.p.mamsoc, dataset = "Field Survey + Mammal Society") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 1998:2018) %>%
  ungroup()

iBats.p.mamsoc_df <- extract_data(iBats.p.mamsoc, dataset = "iBats + Mammal Society") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 2005:2016) %>%
  ungroup()

all_df <- extract_data(all, dataset = "Field Survey + iBats + Mammal Society") %>%
  group_by(resolution, taxa) %>%
  mutate(year = 1998:2018) %>%
  ungroup()


united_df <- bind_rows(iBats_df, field_df, mamsoc_df, field.p.iBats_df, iBats.p.mamsoc_df, field.p.mamsoc_df, all_df)

save(united_df, file = "03_output/united_df.RData")

united_ls <- list(iBats, field, mamsoc, field.p.iBats, iBats.p.mamsoc, field.p.mamsoc, all)
save(united_ls, file = "03_output/united_ls.RData")

samples <- list(
  iBats_smpl, field_smpl, mamsoc_smpl,
  field.p.iBats_smpl, iBats.p.mamsoc_smpl,
  field.p.mamsoc_smpl, all_smpl
)

save(samples, file = "03_output/post_samples.RData")

