library(arules)

# Import the document
complaints <- read.csv("/home/nolan/Downloads/Consumer_Complaints.csv",  header = TRUE)

# Factorize the data, obtain the first set of rules on all the data.
complaints[,18] <- factor(complaints[,18])

# Place the complaint IDs in the first column
tmp <- complaints[,18]
tmp <- cbind(tmp, complaints[,1:17])

# Pare down the data to remove uninteresting columns.
tmp[["Company.public.response"]] <- NULL
tmp[["Consumer.complaint.narrative"]] <- NULL
tmp[["Sub.product"]] <- NULL
tmp[["Sub.issue"]] <- NULL
tmp[["Tags"]] <- NULL
tmp[["Timely.response."]] <- NULL
tmp[["Consumer.disputed."]] <- NULL
tmp[["Submitted.via"]] <- NULL
tmp[["Company.response.to.consumer"]] <- NULL
tmp[["Consumer.consent.provided."]] <- NULL
tmp[["Date.sent.to.company"]] <- NULL
tmp[["Product"]] <- NULL
tmp[["Company"]] <- NULL
#tmp[["State"]] <- NULL
tmp[["ZIP.code"]] <- NULL

# Group the dates into quarters
tmp$Date.received <- as.Date(tmp$Date.received, format="%Y")
tmp$Date.received <- strptime(tmp$Date.received, format="%Y")
# tmp$Date.received <- quarters(tmp$Date.received)
tmp[,1] <- factor(tmp[,1])
tmp[,2] <- factor(tmp[,2])

# Coerce the tmp data.frame into a transactions itemMatrix
im <- as(tmp, "transactions")

# Inspect the itemMatrix with histograms
itemFrequency(im, support=0.1, cex.names=0.8)

# Run arules on the transaction itemMatrix
paredRules <- apriori(im,
                      parameter = list(support=0.005,
                                       confidence=0.2))
# Summarize the rules
summary(paredRules)
inspect(head(paredRules, n = 5, by = "confidence"))

# Analyze the rules
allComplaintsRules
summary(allComplaintsRules)

names(complaints)
allComplaintsRules[1][1]
