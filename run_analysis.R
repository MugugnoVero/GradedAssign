#-----------------------------------------#
#         Peer graded assignment.         #
#-----------------------------------------#

#### Load libraries and prepare session. ####

library(RMugugno)

clean.files = function(x){
  loginfo("Opening file %s ...", x)
  conn = file(x)
  lines = str_trim(readLines(conn))
  close(conn)
  # Convert string lines into .csv readable raw dataset.
  cleaned_lines = gsub(pattern = "( {1})|( {2})", replacement = ",", lines)
  rm(lines)
  # Cache file as .csv.
  destfile = gsub(".txt", ".csv", x)
  if(file.exists(destfile)){
    unlink(destfile)
  }
  loginfo("Writing the dataset in .csv format...")
  conn = file(destfile)
  writeLines(cleaned_lines, conn)
  close(conn)
  loginfo("Returning file location...")
  return(destfile)
}

#### The assignment. ####

# 1. Get the data.
orig = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
tempfile = tempfile(fileext = ".zip")
download.file(orig, destfile = tempfile)
tempdir = tempdir()
unzip(tempfile, exdir = tempdir)

# What does the data set look like?
list.files(tempdir);shell.exec(tempdir)

# 2. Get the files location.
workdir = file.path(tempdir, "UCI HAR Dataset")
files = file.info(list.files(workdir, full.names = T))
filepaths = row.names(files)
dirs = filepaths[files$isdir]
dimNamesFile = filepaths[filepaths %like% "features.txt"]
rawData = sapply(dirs, list.files, full.names = T, pattern = "(subject|X|x|Y|y)_(test|train).txt")

# 3. Process data.
files = sapply(rawData, clean.files, USE.NAMES = F)
names(files) = c("Subject_test", "X_test", "Y_test","Subject_train", "X_train", "Y_train")
reslist = sapply(files, function(x, header = header){
  res = read.csv(x, header = header)
  # Renaming label and activity file to avoid duplicated names.
  if(ncol(res) == 1L){
    if(grepl('subject', x)){
      loginfo("Reading subjects dataset...")
      cname = "subject"
    } else {
      loginfo("Reading labelss dataset...")
      cname = "label"
    }
    colnames(res) = cname
  }
  dims = dim(res)
  loginfo("Dataset %s check: dimensions %d %d...", x, dims[1], dims[2])
  return(res)
}, header = F, USE.NAMES = T)

# 1. Binding labels, subjects and observations.
# 2. Changing by reference to DT.
# 3. Adding original column names.
# 4. Row binding the datas ets.
colNamesRaw = c("subject", "label", read.delim(dimNamesFile, sep = " ", header = F)[,2])
dtlist = list(
  test_merged = setDT(cbind.data.frame(reslist$Subject_test, reslist$Y_test, reslist$X_test)),
  train_merged = setDT(cbind.data.frame(reslist$Subject_train, reslist$Y_train, reslist$X_train))
  )
lapply(dtlist, function(x, cnames){
  setnames(x, old = colnames(x), new = cnames)
  return(invisible(NULL))
}, cnames = colNamesRaw)
res = rbindlist(dtlist, use.names = T, fill = F)

# Check that dimensions match.
orig = dim(res)
target = apply(sapply(dtlist, dim), MARGIN = 1, sum)
orig[1] == target[1]
orig[2] == target[2]/2

#### Rename the data set columns. ####
colNamesRaw = colnames(res)
colSubsRaw = c("subject", "label", colNamesRaw[grepl("(mean()|std())", colNamesRaw) & !grepl("meanFreq", colNamesRaw)])
#getOption("editor")#edit(colNamesRaw, editor = "C:\\WINDOWS\\system32\\notepad.exe")

# Subset the data table and rename the columns.
res = res[, colSubsRaw, with = F]

# 1. Remove illegal symbols.
# 2. Turn cols from camelCase to snake_case.
newCols = gsub("[()]", "", gsub("-", "_", colSubsRaw))
for (i in seq(length(letters))) {
  newCols = gsub(pattern = paste0("^", LETTERS[i]), replacement = letters[i], newCols)
  newCols = gsub(pattern = paste0("_", LETTERS[i]), replacement = paste0("_", letters[i]), newCols)
  newCols = gsub(pattern = LETTERS[i], replacement = paste0("_", letters[i]), newCols)
}
setnames(res, old = colSubsRaw, new = newCols)

# Save data in a temporary storage.
saveRDS(res, file = "C:/Users/Andrea/Desktop/TempResAssignment.RData", compress = T)

# Continue where left off.
if(!exists("res", envir = globalenv())){
  require(RMugugno)
  res = readRDSFix(file = "C:/Users/Andrea/Desktop/TempResAssignment.RData")
}

#### Generate the new aggregated data set and save work. ####
# Applying the mean recursively using data.table .SD notation.
res1 = res[, lapply(.SD, mean), by = .(subject, label), .SDcols = !c("subject", "label")]
setkeyv(res1, c("subject", "label")) # Sort the data set.

# Replace label with activity name.
label_mapping = data.table(
  label_id = 1:6,
  activity = c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
)
res[label_mapping, activity := i.activity, on = .(label = label_id)][, label := NULL]
res1[label_mapping, activity := i.activity, on = .(label = label_id)][, label := NULL]
setkeyv(res, c("subject", "activity"))
setkeyv(res1, c("subject", "activity"))
setcolorder(res, neworder = key(res))
setcolorder(res1, neworder = key(res1))

# Final check on the data consistency.
dims = dim(table(res$subject, res$label))
nrow(res1) == dims[1] * dims[2]

# Saving data in serialized .RData format.
destdir = "C:/Users/Andrea/Desktop/GradedAssignment"
dir.create(destdir)
saveRDS(res, file = paste0(destdir, "/CleanData.RData"))
saveRDS(res1, file = paste0(destdir, "/CleanDataAggregated.RData"))
unlink("C:/Users/Andrea/Desktop/TempResAssignment.RData")

#rm(list = ls());.rs.restartR()
