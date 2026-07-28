# # tests of camtrapR workflow with simulated data sets
# 
# # Simulate basic data (10 stations, 1 camera each, 1 season)
# library(unmarked)
# 
# seed <- 100
# nStation <- 20
# 
# # single season, 1 camera per station
# simcam1 <- simulateCamtrapData(nStation = nStation,
#                                seed = seed)
# 
# # single season, 2 cameras per station
# simcam2 <- simulateCamtrapData(nStation = nStation,
#                                camerasPerStation = 2,
#                                seed = seed)
# 
# # two seasons, 1 camera per station
# simcam3 <- simulateCamtrapData(nStation = nStation,
#                                nSeasons = 2,
#                                seed = seed)
# 
# # two seasons, 2 cameras per station
# simcam4 <- simulateCamtrapData(nStation = nStation,
#                                camerasPerStation = 2,
#                                nSeasons = 2,
#                                seed = seed)
# 
# 
# # create camera operation matrices for all simulated data sets
# camop1 <- cameraOperation(simcam1$camtraps,
#                                 setupCol = "Setup_date",
#                                 retrievalCol = "Retrieval_date")
# 
# camop2 <- cameraOperation(simcam2$camtraps,
#                           setupCol = "Setup_date",
#                           retrievalCol = "Retrieval_date",
#                           cameraCol = "Camera",
#                           byCamera = F,
#                           allCamsOn = F,
#                           camerasIndependent = T)
# 
# camop3 <- cameraOperation(simcam3$camtraps,
#                           setupCol = "Setup_date",
#                           retrievalCol = "Retrieval_date",
#                           sessionCol = "Season")
# 
# camop4 <- cameraOperation(simcam4$camtraps,
#                           setupCol = "Setup_date",
#                           retrievalCol = "Retrieval_date",
#                           sessionCol = "Season",
#                           cameraCol = "Camera",
#                           byCamera = F,
#                           allCamsOn = F,
#                           camerasIndependent = T)
# 
# 
# # plot detection histories
# camtrapR:::camopPlot(camop1)
# camtrapR:::camopPlot(camop2)
# camtrapR:::camopPlot(camop3)
# camtrapR:::camopPlot(camop4)
# 
# 
# 
# 
# 
# 
# simcam1$recordTable_filt <- filterRecordTable(simcam1$recordTable, 
#                   deltaTimeComparedTo = "lastIndependentRecord",
#                   minDeltaTime = 60)
# 
# nrow(simcam1$recordTable_filt)
# nrow(simcam1$recordTable)
# 
# simcam2$recordTable_filt <- filterRecordTable(simcam2$recordTable, 
#                                               deltaTimeComparedTo = "lastIndependentRecord",
#                                               cameraCol = "Camera",
#                                               camerasIndependent = FALSE,
#                                               minDeltaTime = 60)
# 
# 
# simcam3$recordTable_filt <- filterRecordTable(simcam3$recordTable, 
#                                               deltaTimeComparedTo = "lastIndependentRecord",
#                                               minDeltaTime = 60)
# 
# 
# simcam4$recordTable_filt <- filterRecordTable(simcam4$recordTable, 
#                                               deltaTimeComparedTo = "lastIndependentRecord",
#                                               cameraCol = "Camera",
#                                               camerasIndependent = FALSE,
#                                               minDeltaTime = 60)
# 
# 
# # create detection histories (single species) 
# dethist1 <- detectionHistory(recordTable = simcam1$recordTable, 
#                              species = "Sp_08",
#                              camOp = camop1,
#                              occasionLength = 5)
# 
# # create unmarked frame
# umf1 <- unmarkedFrameOccu(y = dethist1$detection_history,
#                              siteCovs = simcam1$camtraps,
#                              obsCovs = list(effort = dethist1_05$effort))
# 
# summary(umf1)
# plot(umf1)
# 
# # fit single-species occupancy model
# occu1 <- occu(~effort ~ scale(elev), umf1)
# summary(occu1)
# 
# 
# # create detection histories (multi-species)
# dethist_comm <- detectionHistory(recordTable = simcam1$recordTable, 
#                              species = paste0("Sp_0", 1:8),
#                              camOp = camop1,
#                              occasionLength = 5)
# 
# # fit multi-species occupancy model
# # TODO: check if this works
# umf_comm <- unmarkedFrameOccuMulti(y = dethist_comm$detection_history,
#                                   siteCovs = simcam1$camtraps,
#                                   obsCovs = list(effort = dethist_comm$effort))
# 
# # test if surveyDashboard can be loaded with simulated data
# surveyDashboard(CTtable = simcam1$camtraps,
#                 recordTable = simcam1$recordTable,
#                 stationCol = "Station",
#                 speciesCol = "Species",
#                 xcol = "longitude",
#                 ycol = "latitude",
#                 setupCol = "Setup_date",
#                 retrievalCol = "Retrieval_date")
# 
