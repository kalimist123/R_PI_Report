library(tm)




ObfuscateR <- R6Class(
  "ObfuscateR",
  public = list(
    #ctor
    initialize = function() {
      
      
      
      ##load dictionary data
      private$first.male <-
        read.table("dist.male.first.txt", stringsAsFactors = F)
      private$first.female <-
        read.table("dist.female.first.txt", stringsAsFactors = F)
      private$last.names <-
        read.table("dist.all.last.txt", stringsAsFactors = F)
      
      
      ##name table columns
      names(private$first.male) <-
        c("name", "perc", "cum.perc", "rank")
      names(private$first.female) <-
        c("name", "perc", "cum.perc", "rank")
      names(private$last.names) <-
        c("name", "perc", "cum.perc", "rank")
      
      #set dictionary exceptions
      private$exceptions <-
        read.table("exceptions.txt", stringsAsFactors = F)$V1
      
      
      ##remove exceptions from dictionaries
      private$first.male <-
        subset(private$first.male, !tolower(name) %in% private$exceptions)
      private$first.female <-
        subset(private$first.female,
               !tolower(name) %in% private$exceptions)
      private$last.names <-
        subset(private$last.names, !tolower(name) %in% private$exceptions)
      private$first.names = c(private$first.male$name, private$first.female$name)
      
      
      
    },
    #extract a substring from text based on first found position of pattern
    extractStringUsingPattern = function(pattern, text, extractedTextLength) {
      pos = regexpr(pattern, text)
      rowchar <- as.character(text)
      
      for (i in 1:extractedTextLength)
        rowchar <- paste0(rowchar, ' ')
      
      #extract text from start of identifier
      rowchar <-
        substr(
          rowchar,
          as.integer(pos[1]),
          as.integer(pos[1]) + extractedTextLength
        )
      
      return (tolower(rowchar))
    }
    ,
    
    #replace digits and person names with a #
    obfuscateString = function(text) {
      possibleEntities = lapply(text, self$get.any.names)
      
      for (RO in unlist(possibleEntities)[!(unlist(possibleEntities) %in% stopwords("en"))])
      {
        replaceMentString <- ""
        
        numchars <- nchar(RO)
        
        for (i in 1:numchars) {
          replaceMentString <- paste0(replaceMentString, "#")
        }
        
        text <-
          gsub(paste0('\\<', RO, '\\>'), replaceMentString, text)
        
      }
      #obfuscate all number
      text <- gsub('[[:digit:]]', '#', as.character(text))
      return (text)
      
    }
    ,
    ##check text for person names
    get.any.names = function(text) {
      text <- gsub("'s", "", text)
      text.split <- strsplit(text, " ")[[1]]
      text <- paste(gsub("[^a-zA-Z]", "", text.split), collapse = " ")
      
      unigrams.list <- private$unigrams(text)
      unigrams.list <-
        lapply(unigrams.list, function(x)
          gsub("[^a-zA-Z]", "", x))
      
      
      is.name <- function(unigram) {
        has.last.name <-
          tolower(unigram[1]) %in% tolower(private$last.names$name)
        
        has.first.name <-
          tolower(unigram[1]) %in% tolower(private$first.names)
        
        isname <- has.first.name | has.last.name
        
        return(isname)
      }
      if (length(unigrams.list) > 0) {
        name.indexes <- sapply(unigrams.list, is.name)
        unigrams.list <- unigrams.list[name.indexes]
        name.vec <-
          sapply(unigrams.list, function(x)
            paste(x, collapse = " "))
      } else{
        name.vec <- ""
      }
      
      return(name.vec)
      
    }
    
  ),
  
  
  private = list(
    first.names = NA,
    first.male = NA,
    first.female = NA,
    last.names = NA,
    exceptions = NA,
    word.vec = NA,
    unigrams = function(text) {
      word.vec <- strsplit(text, "\\s+")[[1]]
    }
    
    
    
  )
  
)
