# 3 Dashboard text----
## 3.1 Data sources ----
folder <- "3-1_DataTable"
sheetNum <- 1
I_DataTable <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
# Tidy up data table
names(I_DataTable) <- gsub(".", " ", names(I_DataTable), fixed = TRUE)
write.csv(I_DataTable, file = "Data\\AppData\\I_DataTable.csv", row.names = FALSE)

## 3.2 Data notes and caveats ----
folder <- "3-2_dataText"
sheetNum <- 1
I_DataText <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Tidy up data text table
names(I_DataText) <- gsub(".", " ", names(I_DataText), fixed = TRUE)
write.csv(I_DataText, file = "Data\\AppData\\I_DataText.csv", row.names = FALSE)

## 3.4 Load FE sources and tools tables ----
folder <- "3-4_FeSources"
sheetNum <- "Tools"
I_ToolsTable <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
sheetNum <- "Sources"
I_SourcesTable <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)
sheetNum <- "Reports"
I_ReportsTable <- openxlsx::read.xlsx(xlsxFile = paste0("./Data/", folder, "/", list.files(path = paste0("./Data/", folder))), sheet = sheetNum, skipEmptyRows = T)

# Tidy up sources table
names(I_SourcesTable) <- gsub(".", " ", names(I_SourcesTable), fixed = TRUE)
write.csv(I_SourcesTable, file = "Data\\AppData\\I_SourcesTable.csv", row.names = FALSE)
names(I_ToolsTable) <- gsub(".", " ", names(I_ToolsTable), fixed = TRUE)
write.csv(I_ToolsTable, file = "Data\\AppData\\I_ToolsTable.csv", row.names = FALSE)
names(I_ReportsTable) <- gsub(".", " ", names(I_ReportsTable), fixed = TRUE)
write.csv(I_ReportsTable, file = "Data\\AppData\\I_ReportsTable.csv", row.names = FALSE)

