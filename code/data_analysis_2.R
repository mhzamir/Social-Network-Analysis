#Compare the predictive power of the features
library(ggplot2)
library("stringr")


labels_train <- read.csv((paste(HOME,"users/coded_ids_labels_train.csv", sep="")), header=T, as.is=T)
coded_ids <- read.csv((paste(HOME,"users/coded_ids.csv", sep="")), header=T, as.is=T)
features <- read.csv((paste(HOME,"users_features/features.csv", sep="")))

merged <- merge(labels_train , coded_ids, by.x = "coded_id" )
merged_features_labels <- merge(merged, features, by.x = "user_id")

##REMOVE
#remove IDs
merged_features_labels$user_id  <- NULL
merged_features_labels$coded_id <- NULL

#verifying features types
str(merged_features_labels)

#Quantity Factor Variables: 10
length(names(Filter(is.factor, merged_features_labels)))
factor_variables_names <-names(Filter(is.factor, merged_features_labels))

#Quantity Numeric Variables: 134
length(names(Filter(is.numeric, merged_features_labels)))
numeric_variables_names <- names(Filter(is.numeric, merged_features_labels))

all_variable_names <- names(merged_features_labels)

combined_variables = c(factor_variables_names , numeric_variables_names)
'%ni%' <- Negate('%in%')
all_variable_names[which(names(merged_features_labels) %ni% combined_variables)] #"spam_in_screen_name"

#NA Attribute: Not number and not factor
merged_features_labels$spam_in_screen_name

### REMOVE
merged_features_labels$spam_in_screen_name <- NULL

#### Dealing with Factor Variables
#Quantity: 10
#[1] "avg_intertweet_times"->Ok  "date_newest_tweet"->Ok    "date_oldest_tweet"->Ok      "default_profile"->Ok     
#[5] "default_profile_image"->Ok  "lang" ->Ok            "max_intertweet_times"->Ok  "min_intertweet_times" ->Ok
#[9] "std_intertweet_times"->Ok   "time_zone" ->Ok 
factor_variables_names

typeof(merged_features_labels$avg_intertweet_times)
merged_features_labels$avg_intertweet_times



#Dealing with 0 days 05:46:33.547739000 FORMAT
for(i in seq(dim(merged_features_labels)[1])){
  output <- str_trim(unlist(strsplit(toString(merged_features_labels$avg_intertweet_times[i]), "days")))
  
  #new variables
  merged_features_labels$avg_intertweet_times_day[i] <- as.numeric(output[1])
  
  z = lubridate::hms(as.character(output[2]))
  
  merged_features_labels$avg_intertweet_times_H[i] <-z$hour
  merged_features_labels$avg_intertweet_times_M[i] <-z$minute
  merged_features_labels$avg_intertweet_times_S[i] <-z$second
}
##REMOVE 
merged_features_labels$avg_intertweet_times <- NULL

merged_features_labels$date_newest_tweet <- as.POSIXct(merged_features_labels$date_newest_tweet, format = "%d/%m/%Y %H:%M:%S")
merged_features_labels$date_oldest_tweet<- as.POSIXct(merged_features_labels$date_oldest_tweet, format = "%d/%m/%Y %H:%M:%S")


### DEALING WITH BOOLEAN CONVERTING True = 1 False = 0 To Int
merged_features_labels$default_profile <- as.integer(as.logical(merged_features_labels$default_profile))
merged_features_labels$default_profile_image <- as.integer(as.logical(merged_features_labels$default_profile_image))

#as.logical(merged_features_labels$default_profile)

###Categorical Factors
merged_features_labels$lang  #No Need to change


#max_intertweet_times
#Dealing with 0 days 05:46:33.547739000 FORMAT
for(i in seq(dim(merged_features_labels)[1])){
  output <- str_trim(unlist(strsplit(toString(merged_features_labels$max_intertweet_times[i]), "days")))
  
  #new variables
  merged_features_labels$max_intertweet_times_day[i] <- as.numeric(output[1])
  
  z = lubridate::hms(as.character(output[2]))
  
  merged_features_labels$max_intertweet_times_H[i] <-z$hour
  merged_features_labels$max_intertweet_times_M[i] <-z$minute
  merged_features_labels$max_intertweet_times_S[i] <-z$second
}
##REMOVE 
merged_features_labels$max_intertweet_times <- NULL

#merged_features_labels$min_intertweet_times
#Dealing with 0 days 05:46:33.547739000 FORMAT
for(i in seq(dim(merged_features_labels)[1])){
  output <- str_trim(unlist(strsplit(toString(merged_features_labels$min_intertweet_times[i]), "days")))
  
  #new variables
  merged_features_labels$min_intertweet_times_day[i] <- as.numeric(output[1])
  
  z = lubridate::hms(as.character(output[2]))
  
  merged_features_labels$min_intertweet_times_H[i] <-z$hour
  merged_features_labels$min_intertweet_times_M[i] <-z$minute
  merged_features_labels$min_intertweet_times_S[i] <-z$second
}
##REMOVE 
merged_features_labels$min_intertweet_times <- NULL

#merged_features_labels$std_intertweet_times
#Dealing with 0 days 05:46:33.547739000 FORMAT
for(i in seq(dim(merged_features_labels)[1])){
  output <- str_trim(unlist(strsplit(toString(merged_features_labels$std_intertweet_times[i]), "days")))
  
  #new variables
  merged_features_labels$std_intertweet_times_day[i] <- as.numeric(output[1])
  
  z = lubridate::hms(as.character(output[2]))
  
  merged_features_labels$std_intertweet_times_H[i] <-z$hour
  merged_features_labels$std_intertweet_times_M[i] <-z$minute
  merged_features_labels$std_intertweet_times_S[i] <-z$second
}
##REMOVE 
merged_features_labels$std_intertweet_times <- NULL

#REMOVE
#NO NEED FORMATTING BUT NOT USED FOR ANALYSIS
merged_features_labels$time_zone <- NULL


#COUNT NAN VALUES
#168 utc_offset
sapply(merged_features_labels,function(x)sum(is.na(x)))

##REMOVE 
#NO NEED
merged_features_labels$utc_offset <- NULL

dim(merged_features_labels)


factor_variables_names_v2 <- names(Filter(is.factor, merged_features_labels))
#1: lang

numeric_variables_names_v2 <- names(Filter(is.numeric, merged_features_labels))
length(numeric_variables_names_v2) #152


out_spearman <- c()
for(i in seq(length(numeric_variables_names_v2))){
  value = as.double(cor.test(x=merged_features_labels[,numeric_variables_names_v2[i]], y=merged_features_labels$label, method = 'spearman')$estimate)
  out_spearman<- c(out_spearman, value)
}
spearman_df <- data.frame(x=numeric_variables_names_v2[-c(2, length(numeric_variables_names_v2))+1] , spearman=out_spearman[-c(2, length(numeric_variables_names_v2))+1], row.names = NULL)

spearman_bar_plot<- ggplot(data=spearman_df, aes(x=x, y=spearman))  +
  ggtitle("Spearman") +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
  geom_text(aes(label = paste(round(spearman, digits = 2))), hjust=-0.2,vjust=0.8, color="black", size=2.0) +
  coord_flip() +
  labs(x = '', y = '') +
  theme_minimal()

spearman_bar_plot
ggsave("plot 1.0.2.png", plot = spearman_bar_plot,  width = 9, height = 8, dpi = 600)


#Some papers point Kendall as more suitable than Spearman
out_kendall <- c()
for(i in seq(length(numeric_variables_names_v2))){
  value = as.double(cor.test(x=merged_features_labels[,numeric_variables_names_v2[i]], y=merged_features_labels$label, method = 'kendall')$estimate)
  out_kendall<- c(out_kendall, value)
}

kendall_df <- data.frame(x=numeric_variables_names_v2[-c(2, length(numeric_variables_names_v2))+1] , kendall=out_kendall[-c(2, length(numeric_variables_names_v2))+1], row.names = NULL)


kendall_bar_plot<- ggplot(data=kendall_df, aes(x=x, y=kendall))  +
  ggtitle("Kendall’s Tau") +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
  geom_text(aes(label = paste(round(kendall, digits = 2))), hjust=-0.2,vjust=0.8, color="black", size=2.0) +
  coord_flip() +
  labs(x = '', y = '') +
  theme_minimal()

kendall_bar_plot

ggsave("plot 1.0.1.png", plot = kendall_bar_plot,  width = 9, height = 8, dpi = 600)

kendall_df = kendall_df[order(kendall_df$kendall),]

#TOP 10 Negative cor Kendau
#17                           followers_count -0.454424494
#18                followers_count_minus_2002 -0.454424494
#27               max_nb_favourites_per_tweet -0.442766758
#109              std_nb_favourites_per_tweet -0.439916749
#112                std_nb_mentions_per_tweet -0.437506648
#39              mean_nb_favourites_per_tweet -0.436120066
#78                      nb_followers_per_day -0.430594423
#12               diversity_index_of_mentions -0.415260093
#15                          favourites_count -0.408551778
#30                 max_nb_mentions_per_tweet -0.393066760


#TOP 10 positive cor Kendau
#                  adjusted_nb_of_uses_of_url  0.313364149
#59                  median_nb_urls_per_tweet  0.320368710
#128             time_since_newest_tweet_days  0.325939770
#129           time_since_newest_tweet_months  0.325939770
#9                            default_profile  0.337871109
#60      median_nb_urls_per_word_in_the_tweet  0.364945384
#74         min_nb_urls_per_word_in_the_tweet  0.442028931
#73                     min_nb_urls_per_tweet  0.454697860
#76                       nb_collected_tweets  0.465585609
#16                followees_per_followers_sq  0.550731365

 


### OUTLIERS
all_variable_names <- names(merged_features_labels)

ggplot(merged_features_labels) +
  aes(x = merged_features_labels[,all_variable_names[2]]) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

ggplot(merged_features_labels) +
  aes(x = "", y =  merged_features_labels[,all_variable_names[2]]) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(x = '', y = all_variable_names[2]) +
  theme_minimal()

for(i in seq(length(all_variable_names))){
    box_plot <- ggplot(merged_features_labels) +
    aes(x = "", y =  merged_features_labels[,all_variable_names[i]]) +
    geom_boxplot(fill = "#0c4c8a") +
    labs(x = '', y = all_variable_names[i]) +
    theme_minimal()
  
   file_name= paste(paste("box_plot_", i, sep=""), ".png", sep="")
   ggsave(file_name, plot = box_plot,  width = 4, height = 4, dpi = 500)
}


#Rorith 
#unwanted = c("default_profile","default_profile_image","avg_intertweet_times","date_newest_tweet","lang","min_intertweet_times","std_nb_symbols_per_tweet","std_nb_symbols_per_word_in_the_tweet","date_oldest_tweet","max_intertweet_times","max_nb_symbols_per_tweet","max_nb_symbols_per_word_in_the_tweet","std_intertweet_times","user_id","coded_id")
