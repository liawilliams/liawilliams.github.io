library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

diabetes<-read_csv("diabetes.csv")
nrow(diabetes)

# % of people with diabetes
prop.table(table(diabetes$Outcome)) * 100

# Age distribution of sample
ggplot(diabetes, aes(x=Age))+
  geom_histogram(binwidth=5,fill="lightpink",color="blue")
  labs(title="Histogram Showing Age Distribution of Sample",
       x="Age",
       y="Number of Individuals")+
  theme_minimal()

# pregnant vs never pregnant
preg_status<-ifelse(diabetes$Pregnancies==0,"Never been pregnant","Have been pregnant")
table(preg_status)

# Descriptive statistics for Age, BMI, Glucose Levels, BP, No. Pregnancies
# # Handle missing values for BMI, Glucose, BP
diabetes$BMI[diabetes$BMI==0]<-NA
diabetes$Glucose[diabetes$Glucose==0]<-NA
diabetes$BloodPressure[diabetes$BloodPressure==0]<-NA
# # # Calculate descriptive statistics
sapply(diabetes[c("Age","BMI","Glucose","BloodPressure","Pregnancies")],function(x)
  c(
    Mean=mean(x,na.rm=TRUE),
    Median=median(x,na.rm=TRUE),
    SD=sd(x,na.rm=TRUE),
    Min=min(x,na.rm=TRUE),
    Max=max(x,na.rm=TRUE),
    IQR=IQR(x,na.rm=TRUE))
  )
# # # # Mode of selected variables
mode_age<-as.numeric(names(sort(table(diabetes$Age),decreasing=TRUE)[1]))
show(mode_age)
mode_BMI<-as.numeric(names(sort(table(diabetes$BMI),decreasing=TRUE)[1]))
show(mode_BMI)
mode_glucose<-as.numeric(names(sort(table(diabetes$Glucose),decreasing=TRUE)[1]))
show(mode_glucose)
mode_bp<-as.numeric(names(sort(table(diabetes$BloodPressure),decreasing=TRUE)[1]))
show(mode_bp)
mode_preg<-as.numeric(names(sort(table(diabetes$Pregnancies),decreasing=TRUE)[1]))
show(mode_preg)

# Create plots for aforementioned variables
# # BMI
ggplot(diabetes,aes(x=BMI))+
  geom_boxplot(fill="lightgreen")+
  labs(title="Box Plot Displaying BMI",y="Body Mass Index (BMI)")+
  theme_minimal()
# # Glucose Levels
ggplot(diabetes,aes(x=Glucose))+
  geom_histogram(binwidth=10,fill="lightblue",color="darkblue")+
  labs(title="Histogram Displaying the Distribution of Glucose Levels",x="Glucose Levels",y="Count")+
  theme_minimal()
# # Blood Pressure
ggplot(diabetes,aes(x=BloodPressure))+
  geom_boxplot(fill="orange")+
  labs(title="Box Plot Displaying Blood Pressure of Sample",y="Blood Pressure (BP)")+
  theme_minimal()
# # Number of Pregnancies
ggplot(diabetes,aes(x=factor(Pregnancies)))+
  geom_bar(fill="purple",color="black")+
  labs(title="Bar Chart Displaying the Number of Pregnancies",x="Pregnancies",y="Count")+
  theme_minimal()

# Calculate outliers in variables
detect_outliers<-function(x){
  if(is.numeric(x)){
    Q1<-quantile(x,0.25,na.rm=TRUE)
    Q3<-quantile(x,0.75,na.rm=TRUE)
    IQR_value<-Q3-Q1
    lower<-Q1-1.5*IQR_value
    upper<-Q3+1.5*IQR_value
    sum(x<lower|x>upper,na.rm=TRUE)
  }else{
    NA
  }
}
diabetes_outliers<-sapply(diabetes,detect_outliers)
diabetes_outliers

show_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  outliers <- x[x < lower | x > upper]
  return(outliers)
}
outliers_age<-show_outliers(diabetes$Age)
outliers_bmi<-show_outliers(diabetes$BMI)
outliers_bp<-show_outliers(diabetes$BloodPressure)
outliers_glucose<-show_outliers(diabetes$Glucose)
outliers_preg<-show_outliers(diabetes$Pregnancies)

summary_outliers<-list(
  Age=outliers_age,
  BMI=outliers_bmi,
  Glucose=outliers_glucose,
  BP=outliers_bp,
  Pregnancies=outliers_preg
)%>%
  tibble::enframe(name="Variable",value="OutlierValues")
summary_outliers
summary_outliers_expand<-summary_outliers%>%
  tidyr::unnest(cols=c(OutlierValues))
print(summary_outliers_expand,n=86)

# Remove outlier values
diabetes_clean<-diabetes%>%
  filter(BloodPressure>=40)
summary(diabetes_clean)

# Create age group categories
diabetes_agegroups<-diabetes_clean%>%
  mutate(
    AgeGroup=case_when(
      Age<30~"<30",
      Age>=30&Age<40~"30-39",
      Age>=40&Age<50~"40-49",
      Age>=50~"50+"
    )
  )
# Calculate diabetes rate by age group
diabetes_rate_age <- diabetes_agegroups %>%
  group_by(AgeGroup) %>%
  summarise(
    Total = n(),
    Diabetic = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = (Diabetic / Total) * 100
  ) %>%
  arrange(AgeGroup)
show(diabetes_rate_age)

# Create BMI categories
diabetes_bmi<-diabetes_clean%>%
  mutate(
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese"
    )
  )
# Calculate rate by BMI
diabetes_rate_bmi <- diabetes_bmi %>%
  group_by(BMI_Category) %>%
  summarise(
    Total = n(),
    Diabetic = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = (Diabetic / Total) * 100
  ) %>%
  arrange(BMI_Category)
show(diabetes_rate_bmi)

# Create categories for no. pregnancies
diabetes_preg <- diabetes_clean %>%
  mutate(
    PregGroup = case_when(
      Pregnancies == 0 ~ "0",
      Pregnancies >= 1 & Pregnancies <= 2 ~ "1-2",
      Pregnancies >= 3 & Pregnancies <= 4 ~ "3-4",
      Pregnancies >= 5 ~ "5+"
    )
  )
# Calculate rate by no. pregnancies
diabetes_rate_preg <- diabetes_preg %>%
  group_by(PregGroup) %>%
  summarise(
    Total = n(),
    Diabetic = sum(Outcome == 1, na.rm = TRUE),
    DiabetesRate = (Diabetic / Total) * 100
  ) %>%
  arrange(PregGroup)
show(diabetes_rate_preg)

# Determine whether significant difference exists betw. glucose levels & diabetes diagnosus
# # Determine normality for continuous variable (glucose)
glucose_no_diabetes<-diabetes_clean$Glucose[diabetes_clean$Outcome==0]
glucose_diabetes<-diabetes_clean$Glucose[diabetes_clean$Outcome==1]
shapiro.test(glucose_no_diabetes)
shapiro.test(glucose_diabetes)

# Histogram to show distribution
ggplot(diabetes_clean, aes(x = Glucose, fill = as.factor(Outcome))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Glucose by Diabetes Status",
       x = "Glucose Level", fill = "Diabetes (0=No, 1=Yes)") +
  theme_minimal()

# Conduct t-test
t_test_glucose<-t.test(Glucose~Outcome,data=diabetes_clean)
show(t_test_glucose)

# Determine whether significant difference exists in no. pregnancies betw. diabetics & non-diabetics
# # Determine normality for continuous variable (no. pregnancies)
preg_no_diabetes<-diabetes_clean$Pregnancies[diabetes_clean$Outcome==0]
preg_diabetes<-diabetes_clean$Pregnancies[diabetes_clean$Outcome==1]
shapiro.test(preg_no_diabetes)
shapiro.test(preg_diabetes)

# Histogram to show distribution
ggplot(diabetes_clean, aes(x = Pregnancies, fill = factor(Outcome))) +
  geom_histogram(binwidth = 1, position = "dodge", color = "white") +
  labs(
    title = "Distribution of Number of Pregnancies by Diabetes Status",
    x = "Number of Pregnancies",
    y = "Count",
    fill = "Diabetes (0 = No, 1 = Yes)"
  ) +
  theme_minimal()

# Conduct Mann-Whitney U test
wilcox.test(Pregnancies~Outcome,data=diabetes_clean)

# Determine correlations between continuous variables
# # Replace zeros with NA for SkinThickness, Insulin variables
diabetes_clean<-diabetes_clean %>%
  mutate(
    SkinThickness=na_if(SkinThickness,0),
    Insulin=na_if(Insulin, 0),
  )

continuous_vars <- diabetes_clean[, c("Pregnancies", "Glucose", "BloodPressure",
                                      "SkinThickness", "Insulin", "BMI",
                                      "DiabetesPedigreeFunction", "Age")]
r_matrix <- cor(continuous_vars, use = "pairwise.complete.obs", method = "pearson")
n_matrix <- outer(
  colnames(continuous_vars),
  colnames(continuous_vars),
  Vectorize(function(x, y) sum(complete.cases(continuous_vars[, c(x, y)])))
)
dimnames(n_matrix) <- list(colnames(continuous_vars), colnames(continuous_vars))
t_matrix <- r_matrix * sqrt((n_matrix - 2) / (1 - r_matrix^2))
p_matrix <- 2 * pt(-abs(t_matrix), df = n_matrix - 2)
p_matrix <- round(p_matrix, 4)

r_p_table <- matrix(
  paste0(round(r_matrix, 2), " (p=", p_matrix, ")"),
  nrow = nrow(r_matrix)
)
rownames(r_p_table) <- rownames(r_matrix)
colnames(r_p_table) <- colnames(r_matrix)

r_p_table

# Test association between diabetes & Age Groups and BMI categories
diabetes_clean$AgeGroup<-cut(
  diabetes_clean$Age,
  breaks = c(-Inf, 29, 39, 49, Inf),
  labels = c("<30","30-39","40-49","50+")
)
diabetes_clean$BMICategory <- cut(
  diabetes_clean$BMI,
  breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
  labels = c("Underweight", "Normal", "Overweight", "Obese")
)
table_age <- table(diabetes_clean$Outcome, diabetes_clean$AgeGroup)
show(table_age)
table_bmi <- table(diabetes_clean$Outcome, diabetes_clean$BMICategory)
show(table_bmi)

chisq_age <- chisq.test(table_age)
show(chisq_age)
chisq_bmi <- chisq.test(table_bmi)
show(chisq_bmi)

# Comparison of mean glucose scores across groups 
# # View mean glucose levels per group
aggregate(Glucose ~ AgeGroup, data = diabetes_clean, mean, na.rm = TRUE)
# # Test normality of glucose levels 
by(diabetes_clean$Glucose, diabetes_clean$AgeGroup, shapiro.test)
# # Test homogeneity of variance
bartlett.test(Glucose ~ AgeGroup, data = diabetes_clean)
# # Run Kruskal-Willis test
kruskal.test(Glucose ~ AgeGroup, data = diabetes_clean)

# Create multiple linear regression of glucose & predictor variables
model_glucose <- lm(Glucose ~ Age + BMI + Pregnancies + BloodPressure +
                      SkinThickness + Insulin + DiabetesPedigreeFunction,
                    data = diabetes_clean)
summary(model_glucose)

# Create logistic regression of diabetes & predictor variables
# # Create a clean dataset with only complete cases
model_diabetes_clean <- na.omit(diabetes_clean[, c("Outcome", "BMI", "Age", "Glucose")])
model_log<- glm(Outcome~BMI+Age+Glucose,
                data=model_diabetes_clean,
                family=binomial)
summary(model_log)

library(ResourceSelection)
hoslem.test(model_diabetes_clean$Outcome,fitted(model_log),g=10)

# # Classification Performance
model_diabetes_clean$pred_prob <- predict(model_log, type = "response")
model_diabetes_clean$pred_class <- ifelse(model_diabetes_clean$pred_prob >= 0.5, 1, 0)

install.packages("caret")
library(caret)

conf_matrix <- confusionMatrix(
  factor(model_diabetes_clean$pred_class),
  factor(model_diabetes_clean$Outcome),
  positive = "1"
)

conf_matrix

# Investigate if there are any significant interactions betw. BMI & age when predicting dibaetes risk
# # Log regression model w/o interaction - main effects only
model_maineff <- glm(Outcome ~ BMI + Age + Glucose + Pregnancies,
                  data = diabetes_clean,
                  family = binomial)
model_interaction <- glm(Outcome ~ BMI * Age + Glucose + Pregnancies,
                         data = diabetes_clean,
                         family = binomial)
# # Likelihood ratio test & odds ratio
anova(model_maineff, model_interaction, test = "LRT")
summary(model_interaction)
exp(cbind(OR = coef(model_interaction), confint(model_interaction)))