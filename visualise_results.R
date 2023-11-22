### Install the required packages
# install.packages('googlesheets4')

### Load the required libraries
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)
library(scales)

#col_m <- "salmon"
#col_f <- "lightseagreen"
#col_m_dark <- "tomato4"
#col_f_dark <- "darkslategrey"

col_m <- "goldenrod1"
col_f <- "mediumorchid1"
col_m_dark <- "darkorange2"
col_f_dark <- "darkorchid4"

### Read google sheets data into R
if(use_simulated_data){
  data <- readRDS("simulated_data.rds")
}else{
  #googlesheets4::gs4_deauth()
  #googlesheets4::gs4_auth()
  data <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/18sx4mZehmDCrlRzuzCMv3c_iMJTDO6vE0yOQ5AWqEBc/edit?resourcekey#gid=1171520726'))
  data <- data[, -grep("Score", names(data))]
}

### Process the data

## first revert the questions on disability and hesitation to speak so that "Yes" means privileged
data[, grep("disability", names(data))][data[, grep("disability", names(data))] %in% "Yes"] <- "Yes_old"
data[, grep("disability", names(data))][data[, grep("disability", names(data))] %in% "No"] <- "Yes"
data[, grep("disability", names(data))][data[, grep("disability", names(data))] %in% "Yes_old"] <- "No"

data[, grep("hesitant", names(data))][data[, grep("hesitant", names(data))] %in% "Yes"] <- "Yes_old"
data[, grep("hesitant", names(data))][data[, grep("hesitant", names(data))] %in% "No"] <- "Yes"
data[, grep("hesitant", names(data))][data[, grep("hesitant", names(data))] %in% "Yes_old"] <- "No"

for(i in seq_len(ncol(data))) {
  data[data[,i] %in% "Other", i] <- "ZOther/NA"
  data[is.na(data[,i]), i] <- "ZOther/NA"
}

### calculate score
privilege_questions <- 2:11
ethnic_question <- grep("ethnic", names(data))
gender_question <- grep("gender", names(data))
job_question <- grep("job level", names(data))
n_q <- length(privilege_questions)
## version with scores which are +/-1, removed because Google forms will only calculate scores for the "correct" answer
#data$total_score <- apply(data[, privilege_questions], 1, function(e) sum(e %in% "Yes")) - apply(data[, privilege_questions], 1, function(e) sum(e %in% "No"))
## version with scores which are +1 or 0, in line with the scores that google form gives.
data$total_score <- apply(data[, privilege_questions], 1, function(e) sum(e %in% "Yes"))
data$n_q <- apply(data[, privilege_questions], 1, function(e) sum(e %in% c("Yes", "No")))

### rescale score in case people didn't respond to all questions
data$rescaled_score <- data$total_score * data$n_q / n_q

# use shape for career stage
data$job <- data[,job_question]
data$pch <- data$job
data$pch[data$job %in% c("Tenured track academic position", "Academic position")] <- 17
data$pch[data$job %in% "Postdoc"] <- 15
data$pch[!data$job %in% c("Tenured track academic position", "Academic position", "Postdoc")] <- 16

data$pch_order <- data$pch
data$pch_order[data$pch == 15] <- 2
data$pch_order[data$pch %in% 16] <- 1
data$pch_order[data$pch %in% 17] <- 3

data$pch <- as.numeric(data$pch)

### sort by score
data_sorted <- data[order(data[,gender_question],
                          data[, ethnic_question],
                          data[, "pch_order"]),]
                          #data[, "rescaled_score"]),]

### plot design

# use outer colour for gender
data_sorted$gender <- data_sorted[,gender_question]
data_sorted$ethnicity <- data_sorted[,ethnic_question]
data_sorted$col <- data_sorted$gender
data_sorted$col[data_sorted$gender %in% "Male" & data_sorted$ethnicity %in% "White"] <- col_m
data_sorted$col[data_sorted$gender %in% "Male" & data_sorted$ethnicity %in% "Ethnic minority"] <- col_m_dark
data_sorted$col[data_sorted$gender %in% "Female" & data_sorted$ethnicity %in% "White"] <- col_f
data_sorted$col[data_sorted$gender %in% "Female" & data_sorted$ethnicity %in% "Ethnic minority"] <- col_f_dark
data_sorted$col[!data_sorted$gender %in% c("Male", "Female") & data_sorted$ethnicity %in% "White"] <- "grey"
data_sorted$col[!data_sorted$gender %in% c("Male", "Female") & data_sorted$ethnicity %in% "Ethnic minority"] <- "black"
data_sorted$col[!data_sorted$ethnicity %in% c("White", "Ethnic minority")] <- "grey"

# use shape for career stage
data_sorted$job <- data_sorted[,job_question]
data_sorted$pch <- data_sorted$job
data_sorted$pch[data_sorted$job %in% c("Tenured track academic position", "Academic position")] <- 17
data_sorted$pch[data_sorted$job %in% "Postdoc"] <- 15
data_sorted$pch[!data_sorted$job %in% c("Tenured track academic position", "Academic position", "Postdoc")] <- 16
data_sorted$pch <- as.numeric(data_sorted$pch)

# create gender_ethnicity unique marker
data_sorted$gender_ethnicity <- paste(data_sorted$gender, data_sorted$ethnicity, sep = "\n")
gender_ethnicity <- c("Female\nEthnic minority", "Female\nWhite", "Female\nZOther/NA",
                      "Male\nEthnic minority", "Male\nWhite", "Male\nZOther/NA",
                      "ZOther/NA\nEthnic minority", "ZOther/NA\nWhite", "ZOther/NA\nZOther/NA")
if(length(c(grep("NA", gender_ethnicity), grep("Other", gender_ethnicity))) >0)
  gender_ethnicity <- gender_ethnicity[-c(grep("NA", gender_ethnicity), grep("Other", gender_ethnicity))]
data_sorted$gender_ethnicity_no_na <- data_sorted$gender_ethnicity
data_sorted$gender_ethnicity_no_na[!(data_sorted$gender_ethnicity_no_na %in% gender_ethnicity)] <- NA

# create job marker without NAs or others
data_sorted$job_no_na <- data_sorted$job
data_sorted$job_no_na[data_sorted$job_no_na %in% "ZOther/NA"] <- NA
data_sorted$job_no_na <- factor(data_sorted$job_no_na,
                                levels = c("Student", "Postdoc",
                                           "Non tenured track academic position",
                                           "Tenured track academic position"))
levels(data_sorted$job_no_na) <- c("Student", "Postdoc",
                                   "Academic\nnot tenured",
                                   "Academic\ntenured")

data_sorted_white_non_white_known_gender <-
  data_sorted[data_sorted$gender %in% c("Male", "Female", "ZOther/NA") &
                data_sorted$ethnicity %in% c("White", "Ethnic minority"),]

# job levels
job_levs <- c(NA, "Student", "Postdoc",
              "Non tenured track academic position",
              "Tenured track academic position")
job_levs_names = job_levs
job_levs_names[1] <- "All"
job_levs_names[4] <- "Academic \n not tenured"
job_levs_names[5] <- "Academic \n tenured"

### factoring things
data_sorted$gender_ethnicity_no_na <- factor(data_sorted$gender_ethnicity_no_na, levels = c("Female\nWhite", "Male\nWhite", "Female\nEthnic minority", "Male\nEthnic minority"))

### useful functions
get_prop_f <- function(data_sorted, by_job = NA)
{
  if(!is.na(by_job)){
    dat <- data_sorted[data_sorted$job %in% by_job,]
  } else{
    dat <- data_sorted
  }
  x <- dat$gender[dat$gender %in% c("Male", "Female")]
  x <- factor(x, levels = c("Female", "Male"))
  table_f <- table(x)
  prop_f <- binom::binom.confint(table_f["Female"], sum(table_f), method = "exact")
  c(prop_f$mean, prop_f$lower, prop_f$upper)
}

get_prop_nw <- function(data_sorted, by_job = NA)
{
  if(!is.na(by_job)){
    dat <- data_sorted[data_sorted$job %in% by_job,]
  } else{
    dat <- data_sorted
  }
  x <- dat$ethnicity[dat$ethnicity %in% c("White", "Ethnic minority")]
  x <- factor(x, levels = c("Ethnic minority", "White"))
  table_nw <- table(x)
  prop_nw <- binom::binom.confint(table_nw["Ethnic minority"], sum(table_nw), method = "exact")
  c(prop_nw$mean, prop_nw$lower, prop_nw$upper)
}



