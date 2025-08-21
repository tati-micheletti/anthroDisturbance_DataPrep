## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "anthroDisturbance_DataPrep",
  description = paste0("This is a data preparation module to harmonize different",
                       " athropogenic disturbance datasets. It's primarily intended",
                       " for the Northwest Territories region (default), but the ",
                       "structure is universal. All needed to do is provide the ",
                       "metadata information required by the `disturbanceDT` ",
                       "object and it generated the list (general category) of ",
                       "lists (specific class) needed for generating disturbance ",
                       "layers"),
  keywords = "",
  authors = structure(list(list(given = "Tati", 
                                family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", 
                                comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(anthroDisturbance_DataPrep = "1.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "anthroDisturbance_DataPrep.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2", "googledrive",
                  "PredictiveEcology/reproducible", "tictoc",
                  "raster", "terra", "crayon", "stringi", "R.utils"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("studyAreaName", "character", "NT1", NA, NA,
                    "Name to be used to save interin disturbance layers, related to location"),
    defineParameter("checkDisturbanceProportions", "logical", FALSE, NA, NA,
                    paste0("Should the disturbances from the original data be pre-buffered to calculate area?",
                           " Mainly designed for debugging and information on original data.")),
    defineParameter("whatNotToCombine", "character", "potential", NA, NA,
                    paste0("Here the user should specify which dataClass from ",
                           "the object disturbances should NOT be combined.",
                           "This is especially important for potential resource ",
                           "layers that need idiosyncratic processes to be ",
                           "generated, which might happen on a separate module (",
                           "i.e., potentialResourcesNT_DataPrep). This parameter ",
                           "is used as a pattern string to identify which dataClass ",
                           "contains the pattern and excludes these from the ",
                           "harmonization of the datasets.")
    ),
    defineParameter("useSavedList", "logical", TRUE, NA, NA,
                    paste0("If the disturbances object was saved and the parameter",
                           "is TRUE, it returns the list. Saves time but attention ",
                           "is needed to make sure the objects are correct!"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "disturbanceDT", objectClass = "data.table", 
                 desc = paste0("This data.table needs to contain the following",
                               " columns: ",
                               
                               "dataName --> this column groups the type of data ",
                               "by sector (i.e., Energy, Settlements, OilGas, ",
                               "Mining, Forestry, Roads)",
                               
                               "URL --> URL link for the specific dataset",
                               
                               "classToSearch --> exact polygon type/class to ",
                               "search for when picking from a dataset with ",
                               "multiple types. If this is not used (i.e., your",
                               " shapefile is alreday all the data needed), you ",
                               "should still specify this so each entry has a ",
                               "different name",
                               
                               "fieldToSearch --> where should classToSearch be ",
                               "found? If this is specified, then the function ",
                               "will subset the spatial object (most likely a ",
                               "shapefile) to classToSearch. Only provide this ",
                               "if this is necessary!",
                               
                               "dataClass --> this column details the type of data ",
                               "further (i.e., Settlements, potentialSettlements ",
                               "otherPolygons, otherLines, windTurbines, potentialWindTurbines, ",
                               "hydroStations, oilFacilities, pipelines, etc). ",
                               "Common class to rename the dataset to, so we ",
                               "can harmonize different ones. Potential data classes ",
                               "can be of three general types (that will be ",
                               "specified in the disturbanceGenerator module as ",
                               "a parameter -- ALWAYS with 'potential' starting): ",
                               "1. Enlarging (i.e., potentialSettlements",
                               " and potentialSeismicLines): where the potential one is ",
                               "exactly the same as the current layer, and we",
                               " only buffer it with time",
                               "2. Generating (i.e., potentialWind, potentialOilGas",
                               "potentialMineral, potentialForestry): where the ",
                               "potential layers are only the potential where ",
                               "structures can appear based on a specific rate",
                               "3. Connecting (i.e., potentialPipelines, ",
                               "potentialTransmission, potentialRoads incl. ",
                               "forestry ones): where the potential layer needs ",
                               "to have the current/latest transmission, pipeline,",
                               " and road network. This process will depend on ",
                               "what is generated in point 2.",
                               
                               "fileName --> If the original file is a .zip and ",
                               "the features are stored in one of more shapefiles",
                               " inside the .zip, please provide which shapefile ",
                               " to be used",
                               
                               "dataType --> please provide the data type of the ",
                               "layer to be used. These are the current accepted ",
                               " formats: 'shapefile' (.shp or .gdb), 'raster' ",
                               "(.tif, which will be converted into shapefile), ",
                               "and 'mif' (which will be read as a shapefile).",
                               
                               "It defaults to an example in the Northwest ",
                               "Territories and needs to be provided if the ",
                               "study area is not in this region (i.e., union ",
                               "of BCR6 and NT1)"), 
                 sourceURL = "https://raw.githubusercontent.com/tati-micheletti/anthroDisturbance_DataPrep/refs/heads/main/data/disturbanceDT.csv"),
    expectsInput(objectName = "studyArea", 
                 objectClass = "SpatialPolygonDataFrame|SpatVector", 
                 desc = paste0("Study area to which the module should be ",
                               "constrained to. Defaults to NT1+BCR6. Object ",
                               "can be of class 'vect' from terra package"), 
                 sourceURL = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/view?usp=sharing"),
    expectsInput(objectName = "rasterToMatch", 
                 objectClass = "RasterLayer|SpatRaster", 
                 desc = paste0("All spatial outputs will be reprojected and ",
                               "resampled to it. Defaults to NT1+BCR6. Object ",
                               "can be of class 'rast' from terra package"), 
                 sourceURL = "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM/view?usp=sharing")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "disturbances", objectClass = "list", 
                  desc = paste0("List (general category) of lists (specific ",
                                "class) needed for generating ",
                                "disturbances. This last list contains: ",
                                "Outter list names: dataName from disturbanceDT",
                                "Inner list names: dataClass from disturbanceDT, ",
                                "which might not be an unique class")),
    createsOutput(objectName = "disturbanceList", objectClass = "list",
                  desc = paste0("List (general category) of lists (specific ",
                                "class) needed for generating ",
                                "disturbances. This last list contains: ",
                                "Outter list names: dataName from disturbanceDT",
                                "Inner list names: dataClass from disturbanceDT, ",
                                "which is a unique class after harmozining, except ",
                                "for any potential resources that need idiosyncratic",
                                " processing. This  means that each combination ",
                                "of dataName and dataClass (except for 'potential')",
                                " will only have only one element. Another module",
                                "can deal with the potential layers. For the ",
                                "current defaults, this is the potentialResourcesNT_DataPrep",
                                "If none of the potential layers needs to be modified ",
                                "or combines, you might skip this idiosyncratic module",
                                " and directly use the anthroDisturbance_Generator ",
                                "module."))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.anthroDisturbance_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      ### Make sure that both studyArea and rasterToMatch are in terra format
      if (class(sim$rasterToMatch) != "SpatRaster"){
        message(crayon::yellow("rasterToMatch is not a SpatRaster. Converting."))
        sim$rasterToMatch <- terra::rast(sim$rasterToMatch)        
      }
      
      if (class(sim$studyArea) != "SpatVector"){
        message(crayon::yellow("studyArea is not a SpatVector. Converting."))
        sim$studyArea <- terra::vect(sim$studyArea)
      }
      
      ### Make sure studyArea had the same projection as rasterToMatch
      if (crs(sim$studyArea) != crs(sim$rasterToMatch)){
        sim$studyArea <- terra::project(sim$studyArea, sim$rasterToMatch)
      }
      
      ### Make sure that disturbanceDT is data.table      
      if (any(class(sim$disturbanceDT) != "data.table")){
        tryCatch({
          sim$disturbanceDT <- as.data.table(sim$disturbanceDT)
        }, error = function(e){
          warning(paste0("disturbanceDT was provided with class ",
                         class(sim$disturbanceDT), " and conversion to ",
                         "data.table failed. Please provide this object as a ",
                         "data.table"), immediate. = TRUE)
          stop(e)
        })
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim = sim, 
                           eventTime = start(sim), 
                           moduleName = "anthroDisturbance_DataPrep", 
                           eventType = "loadAndHarmonizeDisturbanceDT", eventPriority = 2)
    },
    loadAndHarmonizeDisturbanceDT = {
        sim$disturbances <- createDisturbanceList(DT = sim[["disturbanceDT"]],
                                                  destinationPath = getOption("reproducible.destinationPath"), #Paths[["outputPath"]],
                                                  studyArea = sim$studyArea,
                                                  studyAreaName = P(sim)$studyAreaName,
                                                  rasterToMatch = sim$rasterToMatch,
                                                  checkDisturbanceProportions = P(sim)$checkDisturbanceProportions)
      
      sim$disturbanceList <- harmonizeList(disturbances = sim$disturbances, 
                                            whatNotToCombine = P(sim)$whatNotToCombine)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  # dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  dPath <- dataPath(sim)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere(object = "studyArea", sim = sim)) {
    sim$studyArea <- prepInputs(url = extractURL("studyArea"),
                                targetFile = "NT1_BCR6.shp",
                                alsoExtract = "similar",
                                destinationPath = dPath)
    
    warning(paste0("studyArea was not supplied. Defaulting to BCR6+NT1 in the",
                   " Northwest Territories"), immediate. = TRUE)
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- prepInputs(url = extractURL("rasterToMatch"),
                                    targetFile = "RTM.tif",
                                    destinationPath = dPath)
    
    warning(paste0("rasterToMatch was not supplied. Defaulting to BCR6+NT1 in the",
                   " Northwest Territories"), immediate. = TRUE)
  }
  
  if (!suppliedElsewhere(object = "disturbanceDT", sim = sim)) {

    sim$disturbanceDT <- prepInputs(url = extractURL("disturbanceDT"),
                                    targetFile = "disturbanceDT.csv",
                                    destinationPath = dPath,
                                    fun = "data.table::fread",
                                    header = TRUE, 
                                    userTags = "disturbanceDT")

    warning(paste0("disturbanceDT was not supplied. Defaulting to an example from ",
                   " Northwest Territories"), immediate. = TRUE)
  }
  
  return(invisible(sim))
}
