library(dplyr)
library(stargazer)
library(tidyr)


#Loading the data in RStudio
abd = read.csv("~/Downloads/Abandoned.csv", header = TRUE, na.strings = "")
#View(ab)
rsv = read.csv("~/Downloads/Reservation.csv", header = TRUE, na.strings = "")
#View(rs)

cat("Retargeting customers is beneficial
Retargeting customers makes business sense as it helps increase conversion 
rates, is cost-effective, provides valuable data insights, and maintains a 
competitive advantage.")

#2Analyze the test/control division. Does it seem well-executed?
# Checking if the sizes of test and control groups are balanced


  #Number of Control and Test group in abandoned.csv
  nrow(abd[abd$Test_Control=="control",])
  nrow(abd[abd$Test_Control=="test",])
  
  #Number of Control and Test group in reservation.csv
  nrow(rsv[rsv$Test_Control=="control",])
  nrow(rsv[rsv$Test_Control=="test",])

#Number of Control and Test group in both in table form, easier to compare
table(abd$Test_Control)
table(rsv$Test_Control)

# Checking if the sizes of test and control groups are balanced
test_group_size <- sum(abd$Test_Control == "test")
control_group_size <- sum(abd$Test_Control == "control")

if (test_group_size == control_group_size) {
  cat("The sizes of the test and control groups are balanced.\n")
} else {
  cat("The sizes of the test and control groups are not balanced.\n")
}


# Checking for randomization

set.seed(3150)  # Setting a seed for reproducibility

shuffled_data <- sample(abd$Test_Control)

if (all(shuffled_data == abd$Test_Control)) {
  
  cat("The assignment to test and control groups seems randomized.\n")
  
} else {
  
  cat("The assignment to test and control groups may not be randomized.\n")
  
}


#3.
#Calculating summary statistics for the test variable, segmenting by available State data
table(abd$Address, abd$Test_Control)
table(rsv$Address, rsv$Test_Control)

#4. Potential Keys: Email, Incoming_Phone and Contact_Phone
#Detail your procedure to identify customers in Purchase and Groups category 
# Creating flags for the matches
mail_match = abd$Email[complete.cases(abd$Email)] %in% rsv$Email[complete.cases(rsv$Email)]
incm_match = abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rsv$Incoming_Phone[complete.cases(rsv$Incoming_Phone)]
cntct_match = abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rsv$Contact_Phone[complete.cases(rsv$Contact_Phone)]
incm_cntct_match = abd$Incoming_Phone[complete.cases(abd$Incoming_Phone)] %in% rsv$Contact_Phone[complete.cases(rsv$Contact_Phone)]
cntct_incm_match = abd$Contact_Phone[complete.cases(abd$Contact_Phone)] %in% rsv$Incoming_Phone[complete.cases(rsv$Incoming_Phone)]

abd$mail_match = 0
abd$mail_match[complete.cases(abd$Email)] = 1*mail_match

abd$incm_match = 0
abd$incm_match[complete.cases(abd$Incoming_Phone)] = 1*incm_match

abd$cntct_match = 0
abd$cntct_match[complete.cases(abd$Contact_Phone)] = 1*cntct_match

abd$incm_cntct_match = 0
abd$incm_cntct_match[complete.cases(abd$Incoming_Phone)] = 1*incm_cntct_match

abd$cntct_incm_match = 0
abd$cntct_incm_match[complete.cases(abd$Contact_Phone)] = 1*cntct_incm_match


abd$pur = 1*(abd$mail_match | abd$incm_match | abd$cntct_match | abd$incm_cntct_match | abd$cntct_incm_match)
abd$pur

#Treatment group who purchased
abd[abd$Test_Control=='test' & abd$pur==0,]
#Treatment group who didn't purchase
abd[abd$Test_Control=='test' & abd$pur==1,]
#Control group who purchased
abd[abd$Test_Control=='control' & abd$pur==0,]
#Control group who didn't purchase
abd[abd$Test_Control=='control' & abd$pur==1,]


table(abd$pur,abd$Test_Control)

#6.
abd = abd %>%
  mutate(unm = ifelse(
    !(Email %in% rsv$Email |
        Incoming_Phone %in% rsv$Incoming_Phone |
        Contact_Phone %in% rsv$Contact_Phone |
        Incoming_Phone %in% rsv$Contact_Phone |
        Contact_Phone %in% rsv$Incoming_Phone), 1, 0))

#No. of unmatched columns
nrow(abd[abd$unm==1,])

#Removing unmatched columns
abd = abd[abd$unm==0,]


###Checking duplicates
#For Emails
is_duplicated <- duplicated(abd$Email) | duplicated(abd$Email)
#is_duplicated
#Getting duplicates excluding NA values
remove = abd$Email[is_duplicated & !is.na(abd$Email)]
remove

#For Incoming_Phone
is_duplicated <- duplicated(abd$Incoming_Phone) | duplicated(abd$Incoming_Phone)

#Getting duplicates excluding NA values
remove = abd$Incoming_Phone[is_duplicated & !is.na(abd$Incoming_Phone)]
remove

#For Contact_Phone
is_duplicated <- duplicated(abd$Contact_Phone) | duplicated(abd$Contact_Phone)

#Getting duplicates excluding NA values
remove = abd$Contact_Phone[is_duplicated & !is.na(abd$Contact_Phone)]
remove

###Removing duplicate Emails
abd = abd[!duplicated(abd$Email, fromLast = TRUE) | is.na(abd$Email), ]

#Removing duplicate Incoming_Phones
abd = abd[!duplicated(abd$Incoming_Phone, fromLast = TRUE) | is.na(abd$Incoming_Phone), ]

#Removing duplicate Contact_Phones
abd = abd[!duplicated(abd$Contact_Phone, fromLast = TRUE) | is.na(abd$Contact_Phone), ]

# also, try this to remove dupl.


sum(duplicated(clean_abd$Email[!is.na(clean_abd$Email)]))
duplicate_records <- duplicated(abd[c("Email", "Incoming_Phone", "Contact_Phone")]) 
num_duplicates <- sum(duplicate_records)
num_duplicates 


duplicate_Emails <- duplicated(abd$Email, fromLast = TRUE) | duplicated(abd$Email)





cleaned_Emails <- abd[!duplicate_Emails | is.na(abd$Email), ]





removed_duplicate_emails <- abd[duplicate_Emails & !is.na(abd$Email), ]

print(removed_duplicate_emails[c("Email")])





duplicate_ContactPhones <- duplicated(cleaned_Emails$Contact_Phone, fromLast = TRUE) | duplicated(cleaned_Emails$Contact_Phone)





cleaned_Email_Contact <- cleaned_Emails[!duplicate_ContactPhones| is.na(cleaned_Emails$Contact_Phone), ]





removed_duplicate_contact_phones <- cleaned_Emails[duplicate_ContactPhones  & !is.na(cleaned_Emails$Contact_Phone), ]

print(removed_duplicate_contact_phones[c("Contact_Phone")])





duplicates_incoming_phone <- duplicated(cleaned_Email_Contact$Incoming_Phone, fromLast = TRUE)|
  
  duplicated(cleaned_Email_Contact$Incoming_Phone)





final_cleaned_dataset <- cleaned_Email_Contact[!duplicates_incoming_phone |
                                                 
                                                 is.na(cleaned_Email_Contact$Incoming_Phone), ]





removed_duplicate_incoming_phones <- cleaned_Email_Contact[duplicates_incoming_phone &
                                                             
                                                             !is.na(cleaned_Email_Contact$Incoming_Phone), ]



print(removed_duplicate_incoming_phones[c("Incoming_Phone")])

#7.
table(abd$pur,abd$Test_Control)

#Customersv who made reservations after the targeting campaign can be found as
abd_conv_cust = abd[abd$pur==1,]

#Whereas the customersv that didn't make the reservations can be found as
abd_unconv_cust = abd[abd$pur==0,]

#8.# Create a cross-tabulation for the overall dataset 

states = unique(abd$Address[!is.na(abd$Address)])
set.seed(9996)  # Setting a seed value for reproducibility
random_states = sample(states, 5)

abd$HasEmail = 1*complete.cases(abd$Email)
abd$HasState = 1*complete.cases(abd$Address)
abd$Treatment = ifelse(abd$Test_Control == "test", 1, 0)

state_cross_tabs = list()

for (state in random_states) {
  i_data = abd[abd$Address == state, ]
  state_cross_tab = table(i_data$pur, i_data$Treatment)
  dimnames(state_cross_tab) = list(c("Not Purchased","Purchased"), c("Control Group", "Treatment Group"))
  state_cross_tabs[[state]] = state_cross_tab
}
print(state_cross_tabs)


#9. Data Refinement 
# Create a cleaned dataset with the required columns

clean_abd = data.frame (
  "Caller_ID" = abd$Caller_ID,
  "Test_Group" = abd$Treatment,
  "Outcome" = abd$pur,
  "State_Available" = abd$HasState,
  "Email_Available" = abd$HasEmail
)

clean_abd
# Save the cleaned dataset to a CSV file
write.csv(clean_abd, file = "Documents/clean_abd.csv", row.names = FALSE)

# Assuming you have the cleaned dataset with the columns: Outcome, Test Group, State Available, and Email Available

# 10. Execute a linear regression for the formula: Outcome = α + β * Test Group + error.
predic1 = lm(Outcome ~ Test_Group, data = clean_abd)
summary(predic1)

predic2 = lm(Outcome ~ Test_Group+State_Available, data = clean_abd)
summary(predic2)

predic3 = lm(Outcome ~ Test_Group+State_Available+Email_Available, data = clean_abd)
summary(predic3)

stargazer(predic1, predic2, predic3, type="text")
# 11. Justify that this regression is statistically comparable to an ANOVA/t-test.
# We can compare the results of the regression to a t-test by using the 't.test' function.

t_test <- t.test(clean_abd$Outcome ~ clean_abd$Test_Group)

t_test


data = clean_abd[2:5]
aov_data = gather(data, key = "Treatment", value = "score", 1:4)

anova_rslt <- aov(score~Treatment,data = aov_data)
summary(anova_rslt)

tukey_rslt <- TukeyHSD(anova_rslt, conf.level = 0.95)
print(tukey_rslt)
summary(tukey_rslt)
plot(tukey_rslt)


model_interaction_1 <- lm(Outcome ~ Test_Group*State_Available, data = clean_abd)
model_interaction_2 <- lm(Outcome ~ Test_Group*Email_Available, data = clean_abd)
model_interaction_3 <- lm(Outcome ~ Test_Group*(State_Available + Email_Available), data = clean_abd)

summary(model_interaction_1)
summary(model_interaction_2)
summary(model_interaction_3)

stargazer(model_interaction_1, model_interaction_2, model_interaction_3, type="text")

