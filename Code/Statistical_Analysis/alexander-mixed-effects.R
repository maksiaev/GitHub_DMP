


# Script Setup ------------------------------------------------------------

#Set working directory:
#The user will need to replace the directory path with their own in the command below:
setwd("/Users/maksi/Documents/UVA/Research/DMP/GitHub_DMP/Results")

#Required packages/libraries:
#added b/c the ginv function is needed below
library(MASS)
library(car)
library(stats)
library(psych)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(plyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
#Needed for seWithin function:
# If you don't yet have the "remotes" package installed,
# uncomment and run the following line of code.
#install.packages("remotes")
# To install the hausekeep package from github, uncomment and run the following line.
#remotes::install_github("hauselin/hausekeep")
# library(hausekeep)

#Set seed so the same user will get the same results:
set.seed(22903)

#Read in the data:
PrimaryModel_data <- read.csv("years_by_field_chatgpt_autism.csv", header = T, stringsAsFactors = T)


# year by subfield analysis -------------
# First, we compared a null model to a model with just year:

# Check the frequency distribution of the dataframe
table(PrimaryModel_data$Field)

#deleted GS and HS (took them out of .csv file) because too few datapoints for those two subfields

MNull <- lmer(Score~(1|Author), PrimaryModel_data)

Model_Year <- lmer(Score~Year+(1|Author), PrimaryModel_data)

performance::r2(MNull)
performance::r2(Model_Year)

anova(MNull, Model_Year)
summary(Model_Year)

#test whether adding year to regression improves fit

Model_Year_Field <- lmer(Score~Field+Year+(1|Author), PrimaryModel_data)

#Model comparison statistics:
performance::r2(Model_Year)
performance::r2(Model_Year_Field)

anova(Model_Year, Model_Year_Field)

#Model Estimates for the model with field and year
summary(Model_Year_Field)
confint.merMod(Model_Year_Field)

#ANCOVA for model with field and year:
anova(Model_Year_Field)

(Emms1a <- emmeans(Model_Year_Field, list(pairwise~Field), adjust="tukey",lmerTest.limit = 16445, pbkrtest.limit = 16445 ))
confint(Emms1a, level = 0.95)


# Test whether adding interaction improves

Model_Interaction <- lmer(Score~Field*Year+(1|Author), PrimaryModel_data)
#Model comparison statistics:
performance::r2(Model_Year_Field)
performance::r2(Model_Interaction)

anova(Model_Year_Field, Model_Interaction)

# not an improvement over the additive model

#to obtain CI for interaction term
#confint(MFull_1, "letter.symbolS:forward.backwardb", level = 0.95)

#Pairwise comparisons:
#(Emms1a <- emmeans(MFull_1, list(pairwise~letter.symbol*forward.backward), adjust="tukey",lmerTest.limit = 16445, pbkrtest.limit = 16445 ))
#confint(Emms1a, level = 0.95)


#PrimaryModel_data$Predicted <- predict(Model_Year_Field)

#to force regression lines to be linear

plot_sentiment <- ggplot(PrimaryModel_data, aes(x = Year, y = Score, color = Field)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Linear regression lines
  labs(y= "Sentiment Score", x= "Field")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "lightgrey"), axis.line = element_line(colour = "black"),
        text = element_text(size=12, family ="Helvetica"), legend.position = c(0.25,0.85), legend.title = element_blank()) +
  theme(
    axis.line = element_line(size = .5),          # Adjust axis line thickness
    axis.text = element_text(size = 10, color = "black"),         # Adjust axis label size
    axis.title = element_text(size = 12),        # Adjust axis title size
    axis.ticks = element_line(size = 1.5),       # Adjust axis tick thickness
    legend.key = element_rect()) +
  scale_color_hue(labels = c("Abnormal", "Developmental", "Introductory", "Neuroscience", "Social", "Special Education")) +
  
  guides(color=guide_legend(override.aes=list(fill="white"))) +
  theme(plot.margin = margin(5, 0, 5, 5)) + theme(legend.text=element_text(size=12))

plot_sentiment
ggsave("year_field_interactions_autism_chatgpt.png", width=10, height=8)

plot_sentiment2 <- ggplot(PrimaryModel_data, aes(x = Year, y = Score, color = Field)) +
  geom_point() +
  geom_smooth(method = "lm") +  # Linear regression lines
  labs(y= "Sentiment Score", x= "Field")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "lightgrey"), axis.line = element_line(colour = "black"),
        text = element_text(size=12, family ="Helvetica"), legend.position = c(.5,0.85), legend.title = element_blank()) +
  theme(
    axis.line = element_line(size = .5),          # Adjust axis line thickness
    axis.text = element_text(size = 10, color = "black"),         # Adjust axis label size
    axis.title = element_text(size = 12),        # Adjust axis title size
    axis.ticks = element_line(size = 1.5),       # Adjust axis tick thickness
  
  legend.key = element_rect()) +
  scale_color_hue(labels = c("Abnormal", "Developmental", "Introductory", "Neuroscience", "Social", "Special Education")) +
  guides(color=guide_legend(override.aes=list(fill="white"), labels = c("Abnormal", "Developmental", "Introductory", "Neuroscience", "Social", "Special Education"))) +
  theme(plot.margin = margin(5, 0, 5, 5)) + theme(legend.text=element_text(size=12)) +
  facet_wrap(~Field)

plot_sentiment2
ggsave("year_field_interactions_autism_chatgpt2.png", width=10, height=8)


