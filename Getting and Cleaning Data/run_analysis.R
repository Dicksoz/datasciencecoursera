#Name:      DownloadData
#Usage:     FilesToExtract <- DownloadData()
#Desription:Uses a fixed url to download and extract the required files for the
#     analysis to temporary storage. Additionally returns a dataframe
#     containing the filepath to each of the temporary files
#Returns:   A data frame with a column called "Name", and row names
#     corresponding to the data contained within the associated file.
#           Act_Labels - Contains the Labels corresponding to activity numbers
#                 in y test and train files.
#           Features - Contains the names of every measurement calculated in
#                 the raw data.
#           Sub_(Test/Train) - Contains the subject ID's for those subjects in
#                 the corresponding dataset
#           X_(Test/Train) - Contains the raw data for each dataset
#           Y_(Test/Train) - Contains the activity for which the corresponding
#                 measurments were made
DownloadData <- function() {
      #Download URL
      url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      tmpZip <- tempfile()
      #Download to temporary storage
      download.file(url,tmpZip)
      tmpDir <- tempdir()
      #Get names of specific files from the zip file (only those needed)
      Name = unzip(tmpZip,list = T)[c(1,2,16:18,30:32),"Name"]
      Name <- paste(tmpDir,Name,sep = "/")
      #Assign Labels to each file name for easy retrieval later
      FileLabels = c("Act_Labels","Features","Sub_Test",
            "X_Test", "Y_Test","Sub_Train","X_Train", "Y_Train")
      exFiles = data.frame(Name,row.names = FileLabels,
            stringsAsFactors = FALSE)
      #Extract desired files to temporary storage
      unzip(tmpZip,exFiles$Names,exdir = tmpDir)
      exFiles
}


#Name:      GetMergedDataSet
#Usage:     Data <- GetMergedDataSet(exFiles)
#Description:Given a dataframe of files required the various files are read
#     and arranged into a tidy dataframe. Only measurements which are a mean or
#     standard deviation are read. Activites are given text names rather than 
#     numbers. Measurement names are left as found in the features file, as the
#     names were sufficiently descriptive.
#Returns:   A tidy dataframe with the first two columns being the subject ID
#     and activity, and the remaing columns the various measurements
GetMergedDataSet <- function(exFiles) {
      ActivityLabels <- read.table(exFiles["Act_Labels",],
            stringsAsFactors = FALSE)[,2]
      #Read the Feature list into a vector
      Features <- read.table(exFiles["Features",],stringsAsFactors = FALSE)[,2]
      #Find the indecies of all mean or standard deviation mesurements
      include <- grep("mean|std\\b",Features,ignore.case = TRUE)
      #columns are numeric, but "NULL" columns will be skipped in read.table
      classes <- rep("NULL",length(Features))
      classes[include] <- "numeric"
      #Read Test and Train Subject Files and read into vector
      subjectFiles <- c(exFiles["Sub_Train",],exFiles["Sub_Test",])
      SubjectID <- do.call("rbind",lapply(subjectFiles,read.table,
            stringsAsFactors = FALSE, nrows = 7353))[,1]
      #Concactenate Test and Train Activity(Y) Files and read into vector
      actFiles <- c(exFiles["Y_Train",],exFiles["Y_Test",])
      Activities <- do.call("rbind",lapply(actFiles,read.table,
            stringsAsFactors = FALSE, nrows = 7353))[,1]
      Activity <- ActivityLabels[Activities]
      #Concacentate Test and Train Data(X) Files and read into data Frame
      datFiles <- c(exFiles["X_Train",],exFiles["X_Test",])
      Data <- do.call("rbind",lapply(datFiles,read.table, nrows = 7353,
            stringsAsFactors = FALSE, colClasses = classes,
            check.names = FALSE, comment.char = "",
            col.names = Features))
      #Add SubjectID and Activity columns, sort by these factors
      data.frame(SubjectID,Activity,Data,check.names = FALSE,
                  stringsAsFactors = FALSE)[order(SubjectID,Activities),]
}

#Name:      AnalyzeData
#Usage:     MeanData <- AnalyzeData(Data)
#Description:Given A Dataset returned by GetMergedDataSet, calculates the mean
#     measurement over all runs for a given subject and activity. The data are
#     stored and returned as a dataframe
#Returns:   A Tidy Dataframe where the first two columns are the subject ID and
#     activity being meausred, and the remaining columns are the mean
#     measurement for that subject and activity
AnalyzeData <- function(x) {
      #colMeans Requires numeric arguments
      #Activity is coerced by matching to the unique set of activities
      ActivityLabels <- unique(x[,2])
      Activities <- match(x[,2],ActivityLabels)
      x[,2] <- Activities
      #iterate over each interaction of subjectID and activity and calculate
      #mean of each column in the iteration
      x <- data.frame(do.call(rbind,lapply(split(x,list(x[,1],x[,2])),colMeans)))
      #Convert Activity back to character vectors
      x[,2] <- ActivityLabels[x[,2]]
      row.names(x) = NULL
      #return data frame sorted by subject ID and activity
      x[order(x[,1],x[,2]),]
}


#Name:      run_analysis
#Usage:     MeanData <- run_analysis()
#Description:This function Downloads a dataset from a hardcoded url, extracts
#     the raw data and arranges it into a tidy dataframe. This dataframe is
#     then processed so that each interaction of subject and activity has a
#     single value, the mean of all measurements for that interaction. The 
#     resulting dataset is written to a tab delimeted text file with each
#     record on a new line, and returned to the calling environment
#Returns:   A Tidy Dataframe where the first two columns are the subject ID and
#     activity being meausred, and the remaining columns are the mean
#     measurement for that subject and activity
run_analysis <- function() {
      print("Downloading Data...")
      exFiles <- DownloadData()
      print("Getting Merged Data...")
      Data <- GetMergedDataSet(exFiles)
      print("Analyzing Data...")
      FinalData <- AnalyzeData(Data)
      print("Writing to File...")
      write.table(FinalData, file = "MeanData.txt",row.names = FALSE, sep = "\t",quote = FALSE)
      print("Data Saved to MeanData.txt")
      FinalData
}