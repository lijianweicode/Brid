
library(raster)
library(rasterVis)
library(gridExtra)
library(reshape2)

## read data ----
## read data ----
## species occurences data


## read data ----
## read data ----
## species occurences data
data <- read.csv('E:/birddata/cr/cr.csv', stringsAsFactors = FALSE)
head(data)
table(data$species)
spp_to_model <- unique(data$species)

spp_to_model <- unique(data$species)

## curent climatic variables
stk_current <- 
  raster::stack(
    c(
      bio_01 = 'E:/envdata/bio_1.tif',
      bio_03 = 'E:/envdata/bio_3.tif',
      bio_07 = 'E:/envdata/bio_7.tif',
      bio_13 = 'E:/envdata/bio_13.tif',
      bio_15 = 'E:/envdata/bio_15.tif',
      bio_19 = 'E:/envdata/bio_19.tif',
      elev = 'E:/envdata/elev.tif'
    ),
    RAT = FALSE
  )

# setwd("E:/birddata/nt")
# bird_data <- read.csv('nt_newonly.csv', stringsAsFactors = FALSE)
# head(bird_data)
# pts.clim<-extract(stk_current, bird_data, method="bilinear")
# pts.clim
# write.csv(pts.clim,"nt_climonly.csv")


# current_df <- as.data.frame(stk_current)
# current_df <- na.omit(current_df)
# vif(current_df)

## 2050 climatic variables
stk_CCSM4rcp45 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/CCSM4rcp452070/bio_1.tif',
      bio_03 = 'E:/clim/CCSM4rcp452070/bio_3.tif',
      bio_07 = 'E:/clim/CCSM4rcp452070/bio_7.tif',
      bio_13 = 'E:/clim/CCSM4rcp452070/bio_13.tif',
      bio_15 = 'E:/clim/CCSM4rcp452070/bio_15.tif',
      bio_19 = 'E:/clim/CCSM4rcp452070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )

## 2070 climatic variables
stk_CCSM4rcp85 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/CCSM4rcp852070/bio_1.tif',
      bio_03 = 'E:/clim/CCSM4rcp852070/bio_3.tif',
      bio_07 = 'E:/clim/CCSM4rcp852070/bio_7.tif',
      bio_13 = 'E:/clim/CCSM4rcp852070/bio_13.tif',
      bio_15 = 'E:/clim/CCSM4rcp852070/bio_15.tif',
      bio_19 = 'E:/clim/CCSM4rcp852070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )

#BCCrcp45
stk_BCCrcp45 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/BCCrcp452070/bio_1.tif',
      bio_03 = 'E:/clim/BCCrcp452070/bio_3.tif',
      bio_07 = 'E:/clim/BCCrcp452070/bio_7.tif',
      bio_13 = 'E:/clim/BCCrcp452070/bio_13.tif',
      bio_15 = 'E:/clim/BCCrcp452070/bio_15.tif',
      bio_19 = 'E:/clim/BCCrcp452070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )

#BCCrcp45
stk_BCCrcp85 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/BCCrcp852070/bio_1.tif',
      bio_03 = 'E:/clim/BCCrcp852070/bio_3.tif',
      bio_07 = 'E:/clim/BCCrcp852070/bio_7.tif',
      bio_13 = 'E:/clim/BCCrcp852070/bio_13.tif',
      bio_15 = 'E:/clim/BCCrcp852070/bio_15.tif',
      bio_19 = 'E:/clim/BCCrcp852070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )



#MIROC-ESMrcp452070
stk_ESMrcp45 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/ESMrcp452070/bio_1.tif',
      bio_03 = 'E:/clim/ESMrcp452070/bio_3.tif',
      bio_07 = 'E:/clim/ESMrcp452070/bio_7.tif',
      bio_13 = 'E:/clim/ESMrcp452070/bio_13.tif',
      bio_15 = 'E:/clim/ESMrcp452070/bio_15.tif',
      bio_19 = 'E:/clim/ESMrcp452070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )


#MIROC-ESMrcp852070
stk_ESMrcp85 <- 
  raster::stack(
    c(
      bio_01 = 'E:/clim/ESMrcp852070/bio_1.tif',
      bio_03 = 'E:/clim/ESMrcp852070/bio_3.tif',
      bio_07 = 'E:/clim/ESMrcp852070/bio_7.tif',
      bio_13 = 'E:/clim/ESMrcp852070/bio_13.tif',
      bio_15 = 'E:/clim/ESMrcp852070/bio_15.tif',
      bio_19 = 'E:/clim/ESMrcp852070/bio_19.tif',
      elev = 'E:/envdata/elev.tif'

    ),
    RAT = FALSE
  )


setwd("F:/reviewbird")
## build species modelling wrapper ----
biomod2_wrapper <- function(sp){
  cat("\n> species : ", sp)
  
  ## get occurrences points
  sp_dat <- data[data$species == sp, ]
  
  ## formating the data
  sp_format <- 
    BIOMOD_FormatingData(
      resp.var = rep(1, nrow(sp_dat)), 
      expl.var = stk_current,
      resp.xy = sp_dat[, c("long", "lat")],
      resp.name = sp,
      PA.strategy = "random", 
      PA.nb.rep = 2, 
      PA.nb.absences = 100
    )
  
  ## print formatting summary
  sp_format
  
  ## define models options
  sp_opt <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(
    path_to_maxent.jar = list(path_to_maxent.jar = "D:/maxent.jar")))
  
  ## model species
  #generalized linear model, generalized additive mode, random forest, and maximum entropy
  sp_model <- BIOMOD_Modeling( 
    sp_format, 
    models = c("GLM", "CTA", "ANN", "RF","MAXENT.Phillips"), 
    models.options = sp_opt, 
    NbRunEval = 5, 
    DataSplit = 80, 
    Yweights = NULL, 
    VarImport = 3, 
    models.eval.meth = c('TSS', 'ROC'),
    eval.metric.quality.threshold = c(0.6,0.8),
    SaveObj = TRUE,
    rescal.all.models = FALSE,
    do.full.models = FALSE,
    modeling.id = "demo2"
  )
  
  
  ## save some graphical outputs
  #### models scores
  sp_models_var_import <- get_variables_importance(sp_model)
  im <- apply(sp_models_var_import, c(1,2), mean)
  file_path <- paste0("F:/reviewbird/", sp, "_cr_import.csv")
  write.csv(im, file = file_path)
  
  ## build ensemble models
  sp_ens_model <- 
    BIOMOD_EnsembleModeling(
      modeling.output = sp_model,
      em.by = 'all',
      eval.metric = 'TSS',
      eval.metric.quality.threshold = 0.4,
      models.eval.meth = c('TSS','ROC'),
      prob.mean = FALSE,
      prob.mean.weight = TRUE,
      VarImport = 0
    )
  
  ## do projections
  proj_scen <- c("current", "CCSM4rcp45", "CCSM4rcp85", "BCCrcp45", "BCCrcp85", "ESMrcp45", "ESMrcp85")
  
  for(scen in proj_scen){
    cat("\n> projections of ", scen)
    
    ## single model projections
    sp_proj <- 
      BIOMOD_Projection(
        modeling.output = sp_model,
        new.env = get(paste0("stk_", scen)),
        proj.name = scen,
        selected.models = 'all',
        binary.meth = "TSS",
        filtered.meth = NULL,
        compress = TRUE,
        build.clamping.mask = FALSE,
        do.stack = FALSE,
        output.format = ".img"
      )
    
    ## ensemble model projections
    sp_ens_proj <- 
      BIOMOD_EnsembleForecasting(
        EM.output = sp_ens_model,
        projection.output = sp_proj,
        binary.meth = "TSS",
        compress = TRUE,
        do.stack = FALSE,
        output.format = ".img"
      )
  }
  
  return(paste0(sp," modelling completed !"))
}


## launch the spiecies modelling wrapper over species list ----
if(require(snowfall)){ ## parallel computation
  ## start the cluster
  sfInit(parallel = TRUE, cpus = 6) ## here we only require 4 cpus
  sfExportAll()
  sfLibrary(biomod2)
  ## launch our wrapper in parallel
  sf_out <- sfLapply(spp_to_model, biomod2_wrapper)
  ## stop the cluster
  sfStop()
} else { ## sequencial computation
  for (sp in spp_to_model){
    biomod2_wrapper(sp)
  }
  ## or with a lapply function in sequential model
  ## all_species_bm <- lapply(spp_to_model, biomod2_wrapper)
}

