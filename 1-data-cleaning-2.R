# Arranging the testing data - 1
data.name <- names(data.tr)
data.ca <- read.csv("AngleClosure_ValidationCases.csv")
data.co <- read.csv("AngleClosure_ValidationControls.csv")
# Choose right eyes data first. If no, take left eyes data
logic.r.ca <- c(19,21,22,23,24,25,26,27,31,32,36)
logic.r.co <- c(18,20,22,23,24,25,26,29,30,31,35)
logic.l.ca <- c(7,9,11,12,13,14,15,30,31,32,36)
logic.l.co <- c(6,8,10,11,12,13,14,29,30,31,35)

# Combining the data of cases
logic.r <- which(complete.cases(data.ca[,logic.r.ca]))
logic.l <- which(complete.cases(data.ca[,logic.l.ca]))
logic.l <- logic.l[which(!logic.l %in% logic.r)]
data.temp.1 <- data.ca[logic.r,logic.r.ca]
data.temp.2 <- data.ca[logic.l,logic.l.ca]
names(data.temp.1) <- names(data.temp.2) <- data.name[-1]
data.ca <- rbind(data.temp.1, data.temp.2)
y <- rep("YES", nrow(data.ca))
data.ca <- cbind(y, data.ca)

# Combining the data of controls
logic.r <- which(complete.cases(data.co[,logic.r.co]))
logic.l <- which(complete.cases(data.co[,logic.l.co]))
logic.l <- logic.l[which(!logic.l %in% logic.r)]
data.temp.1 <- data.co[logic.r,logic.r.co]
data.temp.2 <- data.co[logic.l,logic.l.co]
names(data.temp.1) <- names(data.temp.2) <- data.name[-1]
data.co <- rbind(data.temp.1, data.temp.2)
y <- rep("NO", nrow(data.co))
data.co <- cbind(y, data.co)

# Comnining the whole data and storing it
data.te <- rbind(data.ca, data.co)
names(data.te) <- data.name
row.names(data.te) <- NULL
dput(data.te, "0-testing-data.r")