### Install the required packages
# install.packages('googlesheets4')

### Load the required libraries
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)

### Read google sheets data into R
data <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/18sx4mZehmDCrlRzuzCMv3c_iMJTDO6vE0yOQ5AWqEBc/edit?resourcekey#gid=1171520726'))
n_data <- 1000
sim_data <- as.data.frame(matrix(NA, nrow = n_data, ncol = ncol(data)))
names(sim_data) <- names(data)

### simulate more data in same format:
privilege_questions <- grep("privilege", names(data))
etnic_question <- grep("ethnic", names(data))
gender_question <- grep("gender", names(data))
job_question <- grep("job", names(data))

# privilege_questions
p_yes <- 0.55
p_no <- 0.35
p_other <- 0.05
p_na <- 1 - p_yes - p_no - p_other
for(i in privilege_questions)
{
  sim_data[,i] <- sample(c("Yes", "No", "Other", NA), n_data, replace = TRUE,
                         prob = c(p_yes, p_no, p_other, p_na))
}
n_q <- length(privilege_questions)
sim_data$total_score <- apply(sim_data[, privilege_questions], 1, function(e) sum(e %in% "Yes")) - apply(sim_data[, privilege_questions], 1, function(e) sum(e %in% "No"))
sim_data$n_q <- apply(sim_data[, privilege_questions], 1, function(e) sum(e %in% c("Yes", "No")))
sim_data$rescaled_score <- sim_data$total_score * sim_data$n_q / n_q
sim_data$privilege_score_0_1 <- (sim_data$rescaled_score - min(sim_data$rescaled_score)) / (max(sim_data$rescaled_score) - min(sim_data$rescaled_score))

# ethnic questions
p_white <- sim_data$privilege_score_0_1
sim_data[, etnic_question] <- sapply(seq_len(n_data), function(e)
  sample(c("White", "Non white"), 1, replace = TRUE,
         prob = c(p_white[e], 1-p_white[e])))
# small amount of other
sim_data[, etnic_question][which(runif(n_data) < 0.05)] <- "Other"
# small amount of NA
sim_data[, etnic_question][which(runif(n_data) < 0.05)] <- NA


# gender questions
p_male <- sim_data$privilege_score_0_1
sim_data[, gender_question] <- sapply(seq_len(n_data), function(e)
  sample(c("Male", "Female"), 1, replace = TRUE,
         prob = c(p_male[e], 1-p_male[e])))
# small amount of other
sim_data[, gender_question][which(runif(n_data) < 0.05)] <- "Other"
# small amount of NA
sim_data[, gender_question][which(runif(n_data) < 0.05)] <- NA


# job questions
p_prof <- sim_data$privilege_score_0_1
sim_data[, job_question] <- sapply(seq_len(n_data), function(e)
  sample(c("Tenured track academic position", "Postdoc"), 1, replace = TRUE,
         prob = c(p_prof[e], 1-p_prof[e])))
# add students
sim_data[, job_question][which(runif(n_data) < 0.25)] <- "Student"
# small amount of other
sim_data[, job_question][which(runif(n_data) < 0.05)] <- "Other"
# small amount of NA
sim_data[, job_question][which(runif(n_data) < 0.05)] <- NA

saveRDS(sim_data, "simulated_data.rds")

