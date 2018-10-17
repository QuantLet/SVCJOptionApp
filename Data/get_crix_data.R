# Adjust the following line to your desired directory on your computer
setwd("~/Ivan/MSc Statistics/Thesis/Code/Final Code")

if (!require("jsonlite")) install.packages("jsonlite")

# Get updated Crix data from: http://crix.hu-berlin.de/
crix = fromJSON("http://crix.hu-berlin.de/data/crix.json", flatten = TRUE)
crix = fromJSON("http://thecrix.de/data/crix.json", flatten = TRUE)

# Save date variable as date
crix[, "date"] = as.Date(crix[["date"]])

write.csv(crix, file = "crix_price.csv", row.names=FALSE)
