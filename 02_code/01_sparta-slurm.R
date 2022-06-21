# code to create slurm job for sparta models on JASMIN (or probably any other slurm cluster)
# first the three non-integrated models
# then the four integrated models
# km have to be changed manually as needed (1, 2, or 5)

# code to set up slurm file for Jasmin (copy + paste to jasmin)
# code if Field----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

# Load data
load(file = 'final_field.RData')

# format data
formattedOccData_field <- formatOccData(taxa = as.factor(final_field$taxa),
                                        site = as.factor(final_field$site_1km), # change resolution here as needed
                                        survey = paste(final_field$site_1km,final_field$date),
                                        replicate = final_field$ID,
                                        closure_period = final_field$clp)


slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = formattedOccData_field$occDetdata,
                    spp_vis = formattedOccData_field$spp_vis,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'nolistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(formattedOccData_field[['spp_vis']])[c(2:5)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'field_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('final_field', 'formattedOccData_field'),
                    slurm_options = list(partition = 'short-serial',
                                         time = '24:00:00', 
                                         mem = 50000,
                                         error = '%a.err'))
# code if iBats----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

# Load data
load(file = 'final_iBats.RData')

# format data
formattedOccData_iBats <- formatOccData(taxa = as.factor(final_iBats$taxa),
                                        site = as.factor(final_iBats$site_1km), # change resolution here as needed
                                        survey = paste(final_iBats$site_1km,final_iBats$date),
                                        replicate = final_iBats$ID,
                                        closure_period = final_iBats$clp)


slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = formattedOccData_iBats$occDetdata,
                    spp_vis = formattedOccData_iBats$spp_vis,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'nolistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(formattedOccData_iBats[['spp_vis']])[c(4,11,13:14)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'iBats_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('final_iBats', 'formattedOccData_iBats'),
                    slurm_options = list(partition = 'short-serial',
                                         time = '24:00:00', 
                                         mem = 60000,
                                         error = '%a.err'))
# code if Mamsoc----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

# Load data
load(file = 'final_mamsoc.RData')

# format data
formattedOccData_mamsoc <- formatOccData(taxa = as.factor(final_mamsoc$taxa),
                                         site = as.factor(final_mamsoc$site_1km), # change resolution here as needed
                                         survey = paste(final_mamsoc$site_1km,final_mamsoc$date),
                                         replicate = final_mamsoc$ID,
                                         closure_period = final_mamsoc$clp)

slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = formattedOccData_mamsoc$occDetdata,
                    spp_vis = formattedOccData_mamsoc$spp_vis,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'nolistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(formattedOccData_mamsoc[['spp_vis']])[c(3,10,12,13)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'mamsoc_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('final_mamsoc', 'formattedOccData_mamsoc'),
                    slurm_options = list(partition = 'short-serial',
                                         time = '24:00:00', 
                                         mem = 50000,
                                         error = '%a.err'))
# code if Field + iBats----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

load(file = 'final_field.RData')
load(file = 'final_iBats.RData')

# format data
formattedOccData_field <- formatOccData(taxa = as.factor(final_field$taxa),
                                        site = as.factor(final_field$site_1km), # change resolution here as needed
                                        survey = paste(final_field$site_1km,final_field$date),
                                        replicate = final_field$ID,
                                        closure_period = final_field$clp)

# change LL
formattedOccData_field$occDetdata$L[formattedOccData_field$occDetdata$L>0] <- 1

# format data
formattedOccData_iBats <- formatOccData(taxa = as.factor(final_iBats$taxa),
                                        site = as.factor(final_iBats$site_1km), # change resolution here as needed
                                        survey = paste(final_iBats$site_1km,final_iBats$date),
                                        replicate = final_iBats$ID,
                                        closure_period = final_iBats$clp)

# change LL
formattedOccData_iBats$occDetdata$L[formattedOccData_iBats$occDetdata$L>0] <- 2

# merge data
occDetdata_merged <- dplyr::bind_rows(formattedOccData_field$occDetdata,
                                      formattedOccData_iBats$occDetdata)

spp_vis_merged <- dplyr::bind_rows(formattedOccData_field$spp_vis,
                                   formattedOccData_iBats$spp_vis)

spp_vis_merged[is.na(spp_vis_merged)] <- FALSE

slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = occDetdata_merged,
                    spp_vis = spp_vis_merged,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'catlistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(spp_vis_merged)[c(4,11,13,14)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'fib_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('occDetdata_merged', 'spp_vis_merged'),
                    slurm_options = list(partition = 'long-serial',
                                         time = '167:59:00', 
                                         mem = 8 * 1024,
                                         error = '%a.err'))
# code if Field + Mamsoc----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

load(file = 'final_field.RData')
load(file = 'final_mamsoc.RData')

# format data
formattedOccData_field <- formatOccData(taxa = as.factor(final_field$taxa),
                                        site = as.factor(final_field$site_1km), # change resolution here as needed
                                        survey = paste(final_field$site_1km,final_field$date),
                                        replicate = final_field$ID,
                                        closure_period = final_field$clp)

# change LL
formattedOccData_field$occDetdata$L[formattedOccData_field$occDetdata$L>0] <- 1

# format data
formattedOccData_mamsoc <- formatOccData(taxa = as.factor(final_mamsoc$taxa),
                                         site = as.factor(final_mamsoc$site_1km), # change resolution here as needed
                                         survey = paste(final_mamsoc$site_1km,final_mamsoc$date),
                                         closure_period = final_mamsoc$clp)

# change LL
formattedOccData_mamsoc$occDetdata$L[formattedOccData_mamsoc$occDetdata$L>0] <- 2

occDetdata_merged <- dplyr::bind_rows(formattedOccData_field$occDetdata,
                                      formattedOccData_mamsoc$occDetdata)

spp_vis_merged <- dplyr::bind_rows(formattedOccData_field$spp_vis,
                                   formattedOccData_mamsoc$spp_vis)

spp_vis_merged[is.na(spp_vis_merged)] <- FALSE

slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = occDetdata_merged,
                    spp_vis = spp_vis_merged,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'catlistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(spp_vis_merged)[c(3,10,12,13)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'fm_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('occDetdata_merged', 'spp_vis_merged'),
                    slurm_options = list(partition = 'long-serial',
                                         time = '167:59:00', 
                                         mem = 8 * 1024,
                                         error = '%a.err'))
# code if iBats + Mamsoc----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

load(file = 'final_iBats.RData')
load(file = 'final_mamsoc.RData')

# format data
formattedOccData_iBats <- formatOccData(taxa = as.factor(final_iBats$taxa),
                                        site = as.factor(final_iBats$site_1km), # change resolution here as needed
                                        survey = paste(final_iBats$site_1km,final_iBats$date),
                                        replicate = final_iBats$ID,
                                        closure_period = final_iBats$clp)

# change LL
formattedOccData_iBats$occDetdata$L[formattedOccData_iBats$occDetdata$L>0] <- 2

# format data
formattedOccData_mamsoc <- formatOccData(taxa = as.factor(final_mamsoc$taxa),
                                         site = as.factor(final_mamsoc$site_1km), # change resolution here as needed
                                         survey = paste(final_mamsoc$site_1km,final_mamsoc$date),
                                         closure_period = final_mamsoc$clp)

# change LL
formattedOccData_mamsoc$occDetdata$L[formattedOccData_mamsoc$occDetdata$L>0] <- 1

occDetdata_merged <- dplyr::bind_rows(formattedOccData_iBats$occDetdata,
                                      formattedOccData_mamsoc$occDetdata)

spp_vis_merged <- dplyr::bind_rows(formattedOccData_iBats$spp_vis,
                                   formattedOccData_mamsoc$spp_vis)

spp_vis_merged[is.na(spp_vis_merged)] <- FALSE

slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = occDetdata_merged,
                    spp_vis = spp_vis_merged,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'catlistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(spp_vis_merged)[c(4,11,13,14)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'ibm_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('occDetdata_merged', 'spp_vis_merged'),
                    slurm_options = list(partition = 'long-serial',
                                         time = '167:59:00', 
                                         mem = 8 * 1024,
                                         error = '%a.err'))

# code if Field + iBats + Mamsoc----
rm(list = ls())
setwd("/home/users/leadam/bats_occ/")

# load libraries
require('rslurm')
require('sparta')
require('reshape2')

load(file = 'final_field.RData')
load(file = 'final_iBats.RData')
load(file = 'final_mamsoc.RData')

# format data
formattedOccData_field <- formatOccData(taxa = as.factor(final_field$taxa),
                                        site = as.factor(final_field$site_1km), # change resolution here as needed
                                        survey = paste(final_field$site_1km,final_field$date),
                                        replicate = final_field$ID,
                                        closure_period = final_field$clp)

# change LL
formattedOccData_field$occDetdata$L[formattedOccData_field$occDetdata$L>0] <- 1

# format data
formattedOccData_iBats <- formatOccData(taxa = as.factor(final_iBats$taxa),
                                        site = as.factor(final_iBats$site_1km), # change resolution here as needed
                                        survey = paste(final_iBats$site_1km,final_iBats$date),
                                        replicate = final_iBats$ID,
                                        closure_period = final_iBats$clp)

# change LL
formattedOccData_iBats$occDetdata$L[formattedOccData_iBats$occDetdata$L>0] <- 2

# format data
formattedOccData_mamsoc <- formatOccData(taxa = as.factor(final_mamsoc$taxa),
                                         site = as.factor(final_mamsoc$site_1km), # change resolution here as needed
                                         survey = paste(final_mamsoc$site_1km,final_mamsoc$date),
                                         closure_period = final_mamsoc$clp)

# change LL
formattedOccData_mamsoc$occDetdata$L[formattedOccData_mamsoc$occDetdata$L>0] <- 4

#merge
occDetdata_merged <- dplyr::bind_rows(formattedOccData_field$occDetdata,
                                      formattedOccData_iBats$occDetdata,
                                      formattedOccData_mamsoc$occDetdata)

spp_vis_merged <- dplyr::bind_rows(formattedOccData_field$spp_vis,
                                   formattedOccData_iBats$spp_vis,
                                   formattedOccData_mamsoc$spp_vis)
spp_vis_merged[is.na(spp_vis_merged)] <- FALSE

slurm_occDetFunc <- function(taxa_name){
  
  out <- occDetFunc(taxa_name = as.character(taxa_name),
                    occDetdata = occDetdata_merged,
                    spp_vis = spp_vis_merged,
                    write_results = TRUE,
                    n_chains = 3,
                    n_iterations = 50000,
                    burnin = 25000,
                    thinning = 3,
                    nyr = 2,
                    modeltype = c('ranwalk', 'halfcauchy', 'catlistlength'),
                    return_data = FALSE,
                    seed = 123)
  return(NULL)
}

# Create roster
pars <- data.frame(taxa_name = as.character(names(spp_vis_merged)[c(4,11,13,14)]))

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
sjob <- slurm_apply(f = slurm_occDetFunc,
                    params = pars, 
                    jobname = 'all_1km',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = F,
                    global_objects = c('occDetdata_merged', 'spp_vis_merged'),
                    slurm_options = list(partition = 'long-serial',
                                         time = '167:59:00', 
                                         mem = 8 * 1024,
                                         error = '%a.err'))