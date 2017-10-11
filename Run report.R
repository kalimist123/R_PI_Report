

#install required packages
list.of.packages <- c("tm", "R6","anytime", "zipcode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


source("Report.r")

#create a report file in the current directory
sink(paste0(sub('\\..*', '', 'PIReport'), format(Sys.time(),'_%Y%m%d_%H%M%S'), '.txt'))


   for (csv in dir(pattern = ".csv$"))
   {
     reportParams =ReportParameters$new(dataFile=csv, showOriginal = TRUE,extractedTextLength=100L, instancesToShow=100L )
     report<-Report$new(reportParams)
     report$printTitle()
     report$printTotals()
     report$printExamples()
   }

# Uncomment these to test 1 report
# reportParams =
#   ReportParameters$new(dataFile="Copy of DeepSee_Diaceutics_20170208_20170228_v1.csv",
#                        showOriginal = TRUE,
#                        extractedTextLength=100L,
#                        instancesToShow=100L
#                        )
# report<-Report$new(reportParams)
# report$printTitle()
# report$printTotals()
# report$printExamples()
# 
# sink()





