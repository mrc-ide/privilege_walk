### Install the required packages
# install.packages('googlesheets4')

### Load the required libraries
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)
library(scales)

col_m <- "salmon"
col_f <- "cyan4"

col_m_dark <- "indianred3"
col_f_dark <- "darkslategrey"

### Read google sheets data into R
if(use_simulated_data){
  data <- readRDS("simulated_data.rds")
}else{
  data <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/18sx4mZehmDCrlRzuzCMv3c_iMJTDO6vE0yOQ5AWqEBc/edit?resourcekey#gid=1171520726'))
}

### Process the data
for(i in seq_len(ncol(data))) {
  data[data[,i] %in% "Other", i] <- "ZOther/NA"
  data[is.na(data[,i]), i] <- "ZOther/NA"
}

### calculate score
privilege_questions <- grep("privilege", names(data))
ethnic_question <- grep("ethnic", names(data))
gender_question <- grep("gender", names(data))
job_question <- grep("job", names(data))
n_q <- length(privilege_questions)
data$total_score <- apply(data[, privilege_questions], 1, function(e) sum(e %in% "Yes")) - apply(data[, privilege_questions], 1, function(e) sum(e %in% "No"))
data$n_q <- apply(data[, privilege_questions], 1, function(e) sum(e %in% c("Yes", "No")))

### rescale score in case people didn't respond to all questions
data$rescaled_score <- data$total_score * data$n_q / n_q

### sort by score
data_sorted <- data[order(data[,gender_question],
                          data[, ethnic_question],
                          data[,job_question]),]

### plot design

# use outer colour for gender
data_sorted$gender <- data_sorted[,gender_question]
data_sorted$col <- data_sorted$gender
data_sorted$col[data_sorted$gender %in% "Male"] <- col_m
data_sorted$col[data_sorted$gender %in% "Female"] <- col_f
data_sorted$col[!data_sorted$gender %in% c("Male", "Female")] <- "grey"

# use inner colour for ethnicity
data_sorted$ethnicity <- data_sorted[,ethnic_question]
data_sorted$bg <- data_sorted$ethnicity
data_sorted$bg[data_sorted$ethnicity %in% "White"] <- "white"
data_sorted$bg[data_sorted$ethnicity %in% "Non white"] <- data_sorted$col[data_sorted$ethnicity %in% "Non white"]
data_sorted$bg[!data_sorted$ethnicity %in% c("White", "Non white")] <- "grey"

# use shape for career stage
data_sorted$job <- data_sorted[,job_question]
data_sorted$pch <- data_sorted$job
data_sorted$pch[data_sorted$job %in% c("Tenured track academic position", "Academic position")] <- 23
data_sorted$pch[data_sorted$job %in% "Postdoc"] <- 22
data_sorted$pch[!data_sorted$job %in% c("Tenured track academic position", "Academic position", "Postdoc")] <- 21
data_sorted$pch <- as.numeric(data_sorted$pch)

# create gender_ethnicity unique marker
data_sorted$gender_ethnicity <- paste(data_sorted$gender, data_sorted$ethnicity)
gender_ethnicity <- unique(data_sorted$gender_ethnicity)
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

# job levels
job_levs <- c(NA, "Student", "Postdoc",
              "Non tenured track academic position",
              "Tenured track academic position")
job_levs_names = job_levs
job_levs_names[1] <- "All"
job_levs_names[4] <- "Academic \n not tenured"
job_levs_names[5] <- "Academic \n tenured"

### useful functions
get_prop_f <- function(data_sorted, by_job = NA)
{
  if(!is.na(by_job)){
    dat <- data_sorted[data_sorted$job %in% by_job,]
  } else{
    dat <- data_sorted
  }
  table_f <- table(dat$gender[dat$gender %in% c("Male", "Female")])
  prop_f <- prop.test(table_f)
  c(prop_f$estimate, prop_f$conf.int)
}

get_prop_nw <- function(data_sorted, by_job = NA)
{
  if(!is.na(by_job)){
    dat <- data_sorted[data_sorted$job %in% by_job,]
  } else{
    dat <- data_sorted
  }
  table_nw <- table(dat$ethnicity[dat$ethnicity %in% c("White", "Non white")])
  prop_nw <- prop.test(table_nw)
  c(prop_nw$estimate, prop_nw$conf.int)
}



