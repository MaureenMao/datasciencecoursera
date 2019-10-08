# datasciencecoursera

# No.1
pollutantmean <- function(directory, pollutant, ID = 1:332){
        # Get file names from directory argument
        files_list <- list.files(directory, full.names = TRUE)
        
        # Create a single data frame from all of the files
        dat <- lapply(files_list,
                      read.csv)
        dat <- do.call("rbind", dat)
        
        # Subset the data appropriately
        dat_subset <- dat[dat$ID %in% ID, ]
        
        # Get the mean of the two columns, return in a list
        lapply(dat_subset[c("nitrate", "sulfate")],
               mean,
               na.rm = TRUE)
}


# No 2


complete <- function(directory, id = 1:332) {

        cvsfiles <- list.files(directory, full.names=TRUE)
        
        nobs <- c()
        for (i in id) {
                dat <- read.csv(files_list[i])
                nobs <- c(nobs, sum(complete.cases(dat)))
        }
        data.frame(id,nobs)
        
}

complete("specdata", 30:25)

# No 3
source("complete.R")
corr <- function(directory, threshold = 0) {
        
        result <- numeric()
        
        # Get file names from directory argument
        files_list <- list.files(directory, full.names = TRUE)
        
        complete_df <- complete(directory)
        
        # subset rows that have number of observations greater than `threshold`
        usable_df <- complete_df[complete_df$nobs > threshold,]
        
        for(monitor_id in usable_df$id) {
                # read data frame for monitors past the threshold
                path <- sprintf("%s/%03d.csv", directory, monitor_id)
                df <- read.csv(path, header = TRUE, comment.char = "")
                
                correlation <- cor(x = df$sulfate, y = df$nitrate, use = "complete.obs")
                result <- c(result, correlation)
        }
        result
}

cr <- corr("specdata", 400)
head(cr)
        


