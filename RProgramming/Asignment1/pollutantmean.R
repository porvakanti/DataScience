pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    
    #set the working directory
    if(grep("specdata", directory) == 1) {
        path <- ("./specdata/")
    }
    
    #get the file List in that directory
    fileList = list.files(path)
    
    #extract the file names and store as numeric for comparison
    file.names = as.numeric(sub("\\.csv$","",fileList))
    
    #select files to be imported based on the user input or default
    selected.files = fileList[match(id,file.names)]
    
    #import data
    Data = lapply(file.path(path,selected.files),read.csv)
    
    #convert into data frame
    Data = do.call(rbind.data.frame,Data)
    
    #calculate mean
    result <- mean(Data[,pollutant],na.rm=TRUE)
    
    #round the result to 3 digits
    round(result, 3)
    
}


#tests
pollutantmean("specdata", "sulfate", 1:10) 
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
