# read in csv

data <- read.csv("~/PycharmProjects/moving-bubbles-tutorial/data/elec_results.csv")

# scale based on 1000
data_thou <- data 
data_thou[ ,2:7] <- round(data_thou[ ,2:7]*1000) 

# ensure sum to 1000

for (i in 1:nrow(data_thou)) {
    data_thou[i, 7] <- data_thou[i, 7] + (1000 - sum(data_thou[i, 2:7]))
}

# create a vector of 1000 voters for each election
for (j in data_thou$Year) {
  assign(paste0("voters_", j),
         c(rep(0, data_thou[data_thou$Year == j, c("CON")]),
           rep(1, data_thou[data_thou$Year == j, c("LAB")]),
           rep(2, data_thou[data_thou$Year == j, c("LD")]),
           rep(3, data_thou[data_thou$Year == j, c("Other")]),
           rep(4, data_thou[data_thou$Year == j, c("PC.SNP")]),
           rep(5, data_thou[data_thou$Year == j, c("Did.not.vote")])
           ))
}

colnames <- paste0("voters_", data_thou$Year)

# bind all vectors to form a dataframe of 1000 voters for each election

voter_frame <- data.frame(get(colnames[1]))

for (k in 2:length(colnames)) {
  voter_frame <- cbind(voter_frame, get(colnames[k]))
}

colnames(voter_frame) <- colnames

# add in equal timings of based on a split of 1440 minutes over 27 elections

new_data <- data.frame()

for (l in 1:nrow(voter_frame)) {
  for (j in 1:27) {
    new_data[l, 2*j - 1] <- voter_frame[l, j]
    new_data[l, 2*j] <- round(1440/27)
  }
}

new_data[ , 54] <- 1440 - 26*round(1440/27)

# collapse each person into a comma separated string and save as a tsv
tsv_data <- c()

for (i in 1:nrow(new_data)) {
  tsv_data[i] <- paste(new_data[i, ], collapse = ",")
}

write.table(tsv_data, file='~/PycharmProjects/moving-bubbles-tutorial/data/elec_results.tsv', quote=TRUE, sep=',', col.names = FALSE, row.names = FALSE)

