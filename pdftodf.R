#------------------------------------------------------------------------#
# TITLE:        pdftodf.R
#
# COURSE:       converts many pdfs to text files and then into a data frame
#               
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         July 23, 2015
#
# STAGE:        pdf to df
#
#
#--Setting the working directory and clearing the workspace-----------#


#-----Rcode starts-----

rm(list=ls())

# Use text-mining package to extract text from PDF files    
library(tm)
library('stringi')
setwd("/git/data/USDA/pdfs/")
path <- setwd("/git/data/USDA/pdfs/")

myfiles <- list.files(path = path, pattern = "table.pdf",  full.names = F, recursive=FALSE)

myfiles_length <- length(myfiles)
file_length_half <- myfiles_length*2
newmatrix <- matrix(, nrow = myfiles_length, ncol = 1)

for (i in myfiles){
  newmatrix[i] <- i


#newframe <- data.frame(newmatrix)

# Function to read a PDF file and turn it into a data frame
#PDFtoDF = function(file) {
  ## Extract PDF text. Each line of PDF becomes one element of the string vector dat.
  dat = readPDF(control=list(text="-layout"))(elem=list(uri=i), 
                                              language="en", id="id1") 
  dat = c(as.character(dat))
  
  ## Keep only those strings that contain the data we want. 
  ## These are the ones that begin with a number.
  dat = dat[grep("^ {0,2}[0-9]{1,3}", dat)]
  
  ## Create separators so we can turn strings into a data frame. We'll use the 
  ## pipe "|" as a separator.
  
  # Add pipe after first number (the row number in the PDF file)
  dat = gsub("^ ?([0-9]{1,3}) ?", "\\1|", dat)
  
  # Replace each instance of 2 or more spaces in a row with a pipe separator. This 
  # works because the company names have a single space between words, while data
  # fields generally have more than one space between them. 
  # (We just need to first add an extra space in a few cases where there's only one
  # space between two data fields.)
  #dat = gsub("(, HVOL )","\\1 ", dat)
  dat = gsub(" {2,100}", "|", dat)
  dat = gsub(" {1}", "-", dat)
  ## Check for data format problems
  # Identify rows without the right number of fields (there should 
  # be six pipe characters per row) and save them to a file for 
  # later inspection and processing (in this case row 11 of the PDF file is excluded))
  #excludeRows = lapply(gregexpr("\\|", dat), function(x) length(x)) != 6
  #write(dat[excludeRows], "rowsToCheck.txt", append=TRUE)
  write(dat, "rowsToCheck.txt", append=TRUE)
}
newmatrix1 <- data.frame(newmatrix)
newmatrix2 <- data.frame(newmatrix1[myfiles_length:file_length_half,])
newmatrix3 <- na.omit(newmatrix1)
#newmatrix4 <- vector(newmatrix3)

newmatrix5 <- data.frame(newmatrix3$newmatrix <- stri_sub(newmatrix3$newmatrix, 1, -10))

#--need to add date column


xx <- read.table("/git/data/USDA/pdfs/rowsToCheck.txt")
xxx <- data.frame(xx)

newrowstocheck <- data.frame(do.call('rbind', strsplit(as.character(xxx$V1),'|',fixed=TRUE)))

newrowstocheck$X1 <- paste(newrowstocheck$X1,newrowstocheck$X2, sep="")
final <- newrowstocheck[,-2]
