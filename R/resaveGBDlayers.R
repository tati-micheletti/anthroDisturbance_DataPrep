if (FALSE){
  library("Require")
  Require("reproducible")
  Require("raster")
  Require("terra")
  
  NT_FORCOV <- preProcess(url = "https://drive.google.com/file/d/1A8xyQ5Ug4v-rKKGH8oGm76mSlObu7BKk/view?usp=sharing", 
                          alsoExtract = "similar",
                          destinationPath = tempdir(), 
                          overwrite = FALSE)
  NT_FORCOV <- rgdal::readOGR(dsn = file.path(tempdir(), "NT_Forcov_and_Harvesting.gdb.zip"),
                              layer = "NT_FORCOV")
  NT_FORCOV <- projectInputs(NT_FORCOV, crs(disturbanceList$studyArea))
  NT_FORCOV <- cropInputs(NT_FORCOV, sf::st_as_sf(disturbanceList$studyArea))
  rgdal::writeOGR(obj = NT_FORCOV, dsn = tempdir(), layer = "NT_FORCOV.shp",
                  driver = "ESRI Shapefile")
  
  CutblocksHistory_UpdatedJan2022 <- rgdal::readOGR(dsn = file.path(tempdir(), "NT_Forcov_and_Harvesting.gdb.zip"),
                                                    layer = "CutblocksHistory_UpdatedJan2022")
  CutblocksHistory_UpdatedJan2022 <- projectInputs(CutblocksHistory_UpdatedJan2022, crs(disturbanceList$studyArea))
  CutblocksHistory_UpdatedJan2022 <- cropInputs(CutblocksHistory_UpdatedJan2022, disturbanceList$studyArea)
  rgdal::writeOGR(obj = CutblocksHistory_UpdatedJan2022, dsn = tempdir(), layer = "CutblocksHistory_UpdatedJan2022.shp", 
                  drive = "ESRI Shapefile")
  
  
  Digaa_LandUsePermitArea <- rgdal::readOGR(dsn = file.path(tempdir(), "NT_Forcov_and_Harvesting.gdb.zip"),
                                            layer = "Digaa_LandUsePermitArea")
  Digaa_LandUsePermitArea <- projectInputs(Digaa_LandUsePermitArea, crs(disturbanceList$studyArea))
  Digaa_LandUsePermitArea <- cropInputs(Digaa_LandUsePermitArea, disturbanceList$studyArea)
  rgdal::writeOGR(obj = Digaa_LandUsePermitArea, dsn = tempdir(), layer = "Digaa_LandUsePermitArea.shp", 
                  drive = "ESRI Shapefile")
  
  Timberworks_LandUsePermitArea <- rgdal::readOGR(dsn = file.path(tempdir(), "NT_Forcov_and_Harvesting.gdb.zip"),
                                                  layer = "Timberworks_LandUsePermitArea")
  Timberworks_LandUsePermitArea <- projectInputs(Timberworks_LandUsePermitArea, crs(disturbanceList$studyArea))
  Timberworks_LandUsePermitArea <- cropInputs(Timberworks_LandUsePermitArea, disturbanceList$studyArea)
  rgdal::writeOGR(obj = Timberworks_LandUsePermitArea, dsn = tempdir(), layer = "Timberworks_LandUsePermitArea.shp", 
                  drive = "ESRI Shapefile")
}
