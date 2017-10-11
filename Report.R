



source("Data.r")
source("ObfuscateR.r")



ReportParameters <- R6Class(
  "ReportParameters",
  public = list(
    showOriginal = FALSE,
    extractedTextLength = 0,
    instancesToShow = 0,
    dataFile = "",
    printInstances = FALSE ,
    wrapTextWidth = 0,
    #ctor
    initialize = function(showOriginal = FALSE,
                          extractedTextLength = 200L,
                          instancesToShow = 100L,
                          dataFile,
                          wrapTextWidth = 0) {
      self$showOriginal = showOriginal
      self$extractedTextLength = extractedTextLength
      self$instancesToShow = instancesToShow
      self$dataFile = dataFile
      self$wrapTextWidth = wrapTextWidth
      
    }
    
  )
)



Report <- R6Class(
  "Report",
  public = list(
    #ctor
    initialize = function(reportParams) {
      #check File Exists
      if (!file.exists(reportParams$dataFile)) {
        stop('file "', reportParams$dataFile, '" does not exist', call. = FALSE)
      }
      
      
      
      private$reportParams = reportParams
      private$rdata = Data$new(dataFile = private$reportParams$dataFile)
      private$obfuscateR = ObfuscateR$new()
      
      
      #populate collections
      private$patientAgeOverMax <- private$rdata$overMaxAge()
      private$patientIdentifiers <-
        private$rdata$searchBlobColumn(private$PATIENT_ID_PATTERN)
      
      
      private$surgicalNumbers <-
        private$rdata$searchBlobColumn(private$SURGICAL_NUMBER_PATTERN)
      private$patientNameChanges <-
        private$rdata$searchBlobColumn(private$NAME_CHANGED_PATTERN)
      private$patientDateOfBirth <-
        private$rdata$searchBlobColumn(private$DATE_OF_BIRTH_PATTERN)
      private$validatedZipCodes <- private$rdata$validZipCodes()
      private$validPatDrawDates <-
        private$rdata$validPatientDrawDates()
      
      
      #prepare collections for reporting
      
      private$surgicalNumbers <-
        private$obfuscateCollection(private$surgicalNumbers, private$SURGICAL_NUMBER_PATTERN)
      
      private$patientIdentifiers <-
        private$obfuscateCollection(private$patientIdentifiers, private$PATIENT_ID_PATTERN)
      
      private$patientNameChanges <-
        private$obfuscateCollection(private$patientNameChanges, private$NAME_CHANGED_PATTERN)
      
      private$patientDateOfBirth <-
        private$obfuscateCollection(private$patientDateOfBirth, private$DATE_OF_BIRTH_PATTERN)
      
      
      
      
    }
    
    ,
    #print text titles for simple report
    printTitle = function() {
      writeLines(paste0("Data Source File:      ", "\t\t", basename(reportParams$dataFile)))
      writeLines(paste0("Rows in Dataset:       ", "\t\t", private$rdata$totalRows()))
      private$NL()
    }
    #
    ,
    #print text totals for simple report
    printTotals = function() {
      writeLines("Found PHI Totals:")
      
      private$printLength("Patient Identifiers:  ", private$patientIdentifiers)
      private$printLength("Surgical Numbers:     ", private$surgicalNumbers)
      private$printLength("Name Changes:         ", private$patientNameChanges)
      private$printLength("Patient Age Over Max: ", private$patientAgeOverMax)
      private$printLength("Date Of Birth:        ", private$patientDateOfBirth)
      private$printLength("Validated Zip Codes:  ", private$validatedZipCodes)
      private$printLength("Patient Draw Dates:   ", private$validPatDrawDates)
      
      private$NL()
    }
    ,
    
    #print instances for the text based report
    printExamples = function() {
      p <- private
      
      p$printStringSearchInstances("Patient Id Instances:", p$patientIdentifiers)
      
      p$printStringSearchInstances("Name Change Instances:", p$patientNameChanges)
      
      p$printStringSearchInstances("Surgical Number Instances:", p$surgicalNumbers)
      
      p$printStringSearchInstances("Date Of Birth Instances", p$patientDateOfBirth)
      
      
      p$printFieldInstances("Sample Patient Age Over Max Instances:",
                            p$patientAgeOverMax,
                            "patientage" ,
                            "Age:")
      
      p$printFieldInstances("Sample Zip Code Instances:",
                            p$validatedZipCodes,
                            "cleanzipcodes",
                            "Zip:")
      
      p$printFieldInstances(
        "Sample Patient Draw Date Instances:",
        p$validPatDrawDates,
        "patientdrawdate",
        "Patient Draw Date:"
      )
      
    }
    ,
    
    #these functions expose  data for knittr
    ShowTotalsdt = function() {
      return(data.table(
        "type" = c(
          "Total Records:",
          "Patient Identifiers:",
          "Surgical Numbers:",
          "Name Changes:",
          "Patient Age Over Max:"
          ,
          "Date Of Birth:",
          "Validated Zip Codes:",
          "Patient Draw Dates:"
        ),
        "total" = c(
          private$rdata$totalRows(),
          nrow(private$patientIdentifiers),
          nrow(private$surgicalNumbers),
          nrow(private$patientNameChanges),
          nrow(private$patientAgeOverMax),
          nrow(private$patientDateOfBirth),
          nrow(private$validatedZipCodes),
          nrow(private$validPatDrawDates)
        )
      ))
    }
    ,
    
    
    
    
    getPatDates = function() {
      colnames(private$validPatDrawDates)[colnames(private$validPatDrawDates) ==
                                            "patientdrawdate"] <- "patient draw date"
      return(private$validPatDrawDates)
    },
    
    getValidZipCodes = function() {
      colnames(private$validatedZipCodes)[colnames(private$validatedZipCodes) ==
                                            "cleanzipcodes"] <- "patient address postal code"
      
      return(private$validatedZipCodes)
    },
    
    getPatAgeOverMax = function() {
      colnames(private$patientAgeOverMax)[colnames(private$patientAgeOverMax) ==
                                            "patientage"] <- "patient age over max"
      
      return(private$patientAgeOverMax)
    },
    
    getSurgicalNumbers = function() {
      return(private$surgicalNumbers)
    },
    
    getPatientIdentifiers = function() {
      return(private$patientIdentifiers)
    },
    
    
    getNameChanges = function() {
      return(private$patientNameChanges)
    },
    
    getpatientDateOfBirth = function() {
      return(private$patientDateOfBirth)
    }
    
    
    
    
  )
  
  ,
  private = list(
    rdata = NULL,
    obfuscateR = NULL,
    reportParams = NULL,
    patientAgeOverMax = NULL,
    patientIdentifiers = NULL,
    surgicalNumbers = NULL,
    patientNameChanges = NULL,
    patientDateOfBirth = NULL,
    validatedZipCodes = NULL,
    validPatDrawDates = NULL,
    PATIENT_ID_PATTERN = 'PATIENT ID',
    NAME_CHANGED_PATTERN = 'name changed',
    SURGICAL_NUMBER_PATTERN = 'SURGICAL #',
    DATE_OF_BIRTH_PATTERN = 'date of birth',

    
    
    
    printLength = function(instancename, instance)
    {
      writeLines(paste0(instancename, "\t\t", nrow(instance)))
    },
    
    #prepare collection  for report by removing names and digits from resultmessage field
    obfuscateCollection = function(collection, pattern) {
      if (nrow(collection) > 0) {
        collection["obfuscatedtext"] <- NA
        collection["extractedtext"] <- NA
        for (i in 1:nrow(collection)) {
          collection[i, "extractedtext"] <-
            private$obfuscateR$extractStringUsingPattern(
              pattern, 
              collection[i, "resultmessage"], 
              private$reportParams$extractedTextLength)
          
          collection[i, "obfuscatedtext"] <-
            private$obfuscateR$obfuscateString(collection[i, "extractedtext"])
          
          
        }
      }
      return(collection)
    },
    printStringSearchRow = function(row) {
      if (reportParams$showOriginal == TRUE)
      {
        writeLines(paste0("Row:", as.integer(row$rownumber), " Original"))
        if (private$reportParams$wrapTextWidth > 0)
        {
          writeLines(strwrap(row$extractedtext, width = private$reportParams$wrapTextWidth))
        }
        else{
          writeLines(row$extractedtext)
        }
      }
      
      writeLines(paste0("Row:", as.integer(row$rownumber) , " Obfuscated"))
      
      if (private$reportParams$wrapTextWidth > 0) {
        writeLines(strwrap(row$obfuscatedtext, width = private$reportParams$wrapTextWidth))
      }
      else{
        writeLines(row$obfuscatedtext)
      }
      
    },
    
    printStringSearchInstances = function(title, instanceCollection) {
      writeLines(paste0(basename(private$reportParams$dataFile),
                        "\t",
                        title))
      
      if (nrow(instanceCollection) > 0)
        for (i in 1:nrow(instanceCollection)) {
          private$printStringSearchRow(instanceCollection[i,])
        }
      private$NL()
    },
    
    #For simple Report. Print lines to represent non string search field counts
    printFieldInstances = function(title,
                                   instanceCollection,
                                   columnName,
                                   rowDecorator) {
      private$NL()
      if (nrow(instanceCollection) > 0) {
        writeLines(paste0(basename(private$reportParams$dataFile),
                          "\t",
                          title))
        private$NL()
        for (i in 1:nrow(head(instanceCollection, n = private$reportParams$instancesToShow))) {
          row <- instanceCollection[i,]
          writeLines(paste0(
            "Row:",
            "\t",
            format(paste0(as.character(
              as.integer(row$rownumber) + 1
            ), " "), justify = "right"),
            "\t\t",
            rowDecorator,
            "\t",
            row[columnName]
          ))
        }
        
        
      }
      
      private$NL()
      
    }
    ,
    NL = function() {
      writeLines("")
    }
    
    
    
    
    
    
  )
)
