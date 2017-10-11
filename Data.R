library(R6)
library(anytime)
library(zipcode)

Data <- R6Class(
  "Data",
  public = list(
    #ctor
    initialize = function(dataFile, maxReportableAge = 89L) {
      #check File Exists
      if (!file.exists(dataFile)) {
        stop('file "', dataFile, '" does not exist', call. = FALSE)
      }
      
      private$maxReportableAge = maxReportableAge
      
      
      #populate dataframe
      #private$documentDf <- read.csv(dataFile,quote="",row.names=NULL)
      #private$documentDf <- read.csv(dataFile,header=TRUE,quote="",sep=",")
      private$documentDf <- read.csv(dataFile)
      
      
      #add RowNumber
      private$addRowNumber()
      
      
      
      
      
      #print(head(private$documentDf))
      #normalize ColNames
      private$normalizeColumnNames()
      
      #replace symbols that cause formatting errors for report
      private$replaceProblematicSymbols()
      
      #normalize postal codes columns
      private$normalizePostalCodeColumns()
      
      #populate cleanZipCodes Column
      private$cleanZipCodes()
      
      #normalize PatientAge Column
      private$normalizePatientAge
      
      
      #check for unambiguous patient draw dates
      private$checkForUnambiguousPatientDrawDates()
      
      
    },
    
    searchBlobColumn = function(pattern) {
      if (!("resultmessage" %in% colnames(private$documentDf)))
        stop("No resultmessage column in document")
      
      ndx <-
        grep(pattern, private$documentDf$resultmessage)
      selected_rows <- private$documentDf[ndx,]
      private$returnRowSet(selected_rows)
    },
    
    
    totalRows = function() {
      return(nrow(private$documentDf))
    },
    
    overMaxAge = function()
    {
      if (!("patientage" %in% colnames(private$documentDf)))
        stop("No patientage column in document")
      
      selected_rows <-
        subset(private$documentDf,  patientage > private$maxReportableAge)
      
      selected_rows$patientage <-
        as.character(selected_rows$patientage)
      private$returnRowSet(selected_rows)
      
    },
    validPatientDrawDates = function()
    {
      selected_rows <-
        private$documentDf[!is.na(private$documentDf$patientdrawdate),]
      
      if (nrow(selected_rows) > 0)
        selected_rows$patientdrawdate <-
          as.character(as.Date(selected_rows$patientdrawdate , format = '%m/%d/%Y'))
      
      private$returnRowSet(selected_rows)
    },
    
    validZipCodes = function() {
      selected_rows <-
        private$documentDf[!is.na(private$documentDf$cleanzipcodes),]
      private$returnRowSet(selected_rows)
      
    }
    
  ),
  
  
  ##private members
  private = list(
    documentDf = NULL,
    maxReportableAge = NULL,
    
    addRowNumber = function() {
      private$documentDf$RowNumber <-
        seq.int(nrow(private$documentDf))
      
      
      private$documentDf$RowNumber<-private$documentDf$RowNumber+1
      private$documentDf$RowNumber<-as.character(private$documentDf$RowNumber)
      
    },
    
    replaceProblematicSymbols = function() {
      private$documentDf$resultmessage <-
        gsub('-', '_', private$documentDf$resultmessage)
     
    },
    
    normalizePatientAge = function() {
      if ("patientage" %in% colnames(private$documentDf))
        private$documentDf$patientage <-
          as.integer(as.character(private$documentDf$patientage))
      
    },
    
    normalizeColumnNames = function() {
      ## set column names to lowercase
      names(private$documentDf) <-
        tolower(names(private$documentDf))
      
      ## remove period from column names
      names(private$documentDf) <-
        gsub("\\.", "", names(private$documentDf))
      
      ##remove spaces from column names
      names(private$documentDf) <-
        gsub(" ", "", names(private$documentDf))
      
      
    },
    normalizePostalCodeColumns = function() {
      if ("patientaddresspostalcodezipcode" %in% colnames(private$documentDf))
        names(private$documentDf)[names(private$documentDf) == 'patientaddresspostalcodezipcode'] <-
          'patientaddresspostalcode'
      
      if ("postalcodezipcode" %in% colnames(private$documentDf))
        names(private$documentDf)[names(private$documentDf) == 'postalcodezipcode'] <-
          'patientaddresspostalcode'
      
      if ("patientzipcode" %in% colnames(private$documentDf))
        names(private$documentDf)[names(private$documentDf) == 'patientzipcode'] <-
          'patientaddresspostalcode'
      
      if ("patientpostalcode" %in% colnames(private$documentDf))
        names(private$documentDf)[names(private$documentDf) == 'patientpostalcode'] <-
          'patientaddresspostalcode'
      
    },
    
    cleanZipCodes = function() {
      private$documentDf["cleanzipcodes"] <- NA
      
      if ("patientaddresspostalcode" %in% colnames(private$documentDf))
      {
        private$documentDf$patientaddresspostalcode <-
          as.character(private$documentDf$patientaddresspostalcode)
        private$documentDf$ziplength = nchar(private$documentDf$patientaddresspostalcode)
        
        #nullify zipcodes with length<=3
        private$documentDf$patientaddresspostalcode[private$documentDf$ziplength <= 3] <-
          NA
        #use zipcodes lib clean.zipcodes function to find valid zipcodes and populate cleanZipCodes column
        private$documentDf$cleanzipcodes = clean.zipcodes(private$documentDf$patientaddresspostalcode)
      }
      
    },
    
    checkForUnambiguousPatientDrawDates = function() {
      if ("patientdrawdate" %in% colnames(private$documentDf))
      {
        private$documentDf$patientdrawdate <-
          anytime(private$documentDf$patientdrawdate, tz = "America/Los_Angeles")
        
      } else{
        private$documentDf$patientdrawdate <- NA
      }
    },
    
    returnRowSet = function(selected_rows) {
      if (nrow(selected_rows) > 0)
        return (selected_rows)
      else
        return(private$documentDf[FALSE,])
      
    }
    
    
  )
  
)
