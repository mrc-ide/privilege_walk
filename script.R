### Install the required packages
# install.packages('googlesheets4')

### Load the required libraries
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)

### Read google sheets data into R
data <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/18sx4mZehmDCrlRzuzCMv3c_iMJTDO6vE0yOQ5AWqEBc/edit?resourcekey#gid=1171520726'))

### calculate score
privilege_questions <- grep("privilege", names(data))
etnic_question <- grep("ethnic", names(data))
gender_question <- grep("gender", names(data))
job_question <- grep("job", names(data))
n_q <- length(privilege_questions)
data$total_score <- apply(data[, privilege_questions], 1, function(e) sum(e %in% "Yes")) - apply(data[, privilege_questions], 1, function(e) sum(e %in% "No"))
data$n_q <- apply(data[, privilege_questions], 1, function(e) sum(e %in% c("Yes", "No")))

### TODO: mimic a spreadsheet with 1000 entries to design the graph accordingly

### rescale score in case people didn't respond to all questions
data$rescaled_score <- data$total_score * data$n_q / n_q

### sort by score
data_sorted <- data[order(data$rescaled_score),]

### plot design

# use outer colour for gender
data_sorted$gender <- data_sorted[,gender_question]
data_sorted$col <- data_sorted$gender
data_sorted$col[data_sorted$gender %in% "Male"] <- "dodgerblue"
data_sorted$col[data_sorted$gender %in% "Female"] <- "palevioletred"
data_sorted$col[!data_sorted$gender %in% c("Male", "Female")] <- "grey"

# use inner colour for etnicity
data_sorted$ethnicity <- data_sorted[,etnic_question]
data_sorted$bg <- data_sorted$ethnicity
data_sorted$bg[data_sorted$ethnicity %in% "White"] <- "white"
#data_sorted$bg[data_sorted$ethnicity %in% "Non white"] <- "black"
data_sorted$bg[data_sorted$ethnicity %in% "Non white"] <- data_sorted$col[data_sorted$ethnicity %in% "Non white"]
data_sorted$bg[!data_sorted$ethnicity %in% c("White", "Non white")] <- "grey"

# use shape for carreer stage
data_sorted$job <- data_sorted[,job_question]
data_sorted$pch <- data_sorted$job
data_sorted$pch[data_sorted$job %in% c("Tenured track academic position", "Academic position")] <- 23
data_sorted$pch[data_sorted$job %in% "Postdoc"] <- 22
data_sorted$pch[!data_sorted$job %in% c("Tenured track academic position", "Academic position", "Postdoc")] <- 21
data_sorted$pch <- as.numeric(data_sorted$pch)

### plot
plot(seq_len(nrow(data_sorted)), data_sorted$rescaled_score, axes = FALSE,
     xlab = "Respondents", ylab = "Rescaled score",
     ylim = c(-n_q, n_q),
     pch = data_sorted$pch,
     col = data_sorted$col,
     bg = data_sorted$bg,
     lwd = 2, cex = 2)
abline(h = c(0, n_q, -n_q), col = "grey", lty = 2)
axis(side = 2, at = seq(-n_q, n_q), labels = seq(-n_q, n_q))
