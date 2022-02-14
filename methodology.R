library(readxl)
Response_data <- read_excel("finalll.xlsx")
str(Response_data)


knitr::opts_chunk$set(echo = TRUE)
Response_data$Age<-as.factor(Response_data$Age)
Response_data$Gender<-as.factor(Response_data$Gender)
Response_data$Agree_Society_Rule<-as.factor(Response_data$Agree_Society_Rule)
Response_data$Think_About_You<-as.factor(Response_data$Think_About_You)

Response_data$Have_Smart_Phone <-as.factor(Response_data$Have_Smart_Phone)

Response_data$ Like_Spend_Time_With_friend <-as.factor(Response_data$ Like_Spend_Time_With_friend)

Response_data$Happy_Current_position  <-as.factor(Response_data$Happy_Current_position )

Response_data$Tensed_About_Carrer  <-as.factor(Response_data$Tensed_About_Carrer )

Response_data$Discuss_problem_anyone <-as.factor(Response_data$Discuss_problem_anyone)

Response_data$Feel_Comfortable_being_alone  <-as.factor(Response_data$Feel_Comfortable_being_alone)

Response_data$Try_Committed_Suicide  <-as.factor(Response_data$Try_Committed_Suicide )

Response_data$suicide_is_the_solution_all_problems <-as.factor(Response_data$suicide_is_the_solution_all_problems)


Response_data$happy_with_family_members <-as.factor(Response_data$happy_with_family_members)

Response_data$consider_yourself_family_burden <-as.factor(Response_data$consider_yourself_family_burden)

Response_data$Where_feel_more_comfortable <-as.factor(Response_data$Where_feel_more_comfortable)

Response_data$Family_Members <-as.factor(Response_data$Family_Members)

Response_data$participated_extra_curricular_activities  <-as.factor(Response_data$participated_extra_curricular_activities)

Response_data$difficulties_in_your_educational_system <-as.factor(Response_data$difficulties_in_your_educational_system)

Response_data$Sleep_in_a_day <-as.factor(Response_data$Sleep_in_a_day)

Response_data$SSC_Result  <-as.factor(Response_data$SSC_Result)

Response_data$HSC_Result <-as.factor(Response_data$HSC_Result)

Response_data$University_CGPA <-as.factor(Response_data$University_CGPA)

Response_data$Family_Members <-as.factor(Response_data$Family_Members)

Response_data$Agree_Society_Rule<-as.factor(Response_data$Agree_Society_Rule)


#A-Algorithm

library(arules)
library(arulesViz)
Rule<-apriori(Response_data,parameter = list(support=0.45,confidence=0.95))
inspect(head(Rule,8))

oneRule <- sample(Rule, 8)
inspect(oneRule)

# Finding redundancy
redundant <- is.redundant(Rule, measure="confidence")
which(redundant)
rules.pruned <- Rule[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

# Rules with specified parameter valus


rules <- apriori(Response_data,parameter = list(maxlen=5,supp=.45, conf=.95))
summary(rules)
# Finding redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)


