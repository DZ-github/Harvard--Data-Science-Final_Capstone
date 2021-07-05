
# load library
library(tidyverse)  # basic
library(tidyr)      # basic
library(caret)      # models
library(corrplot)  # correlation
library(data.table) 
library(dplyr)      
library(kableExtra)
library(ggplot2) # Plot
library(gridExtra)  # Plot
library(forcats)
library(matrixStats)  # Matrix
library(rpart)        
library(stringr)
library(ggcorrplot)  
options(digits = 3, warn = -1)

# set working dir
setwd("C:\\Users\\user\\Desktop\\Harvard_R\\CapStone\\Diamonds")

# Read data and check data types
dat <- read.csv("diamonds.csv", header=TRUE, sep=",")
str(dat)
#53940 rows
dim(dat)

# --- 2. Data description ---
# We first look at the distinct cut, color, clarity for types and numbers. 

dat %>% select (cut, color, clarity) %>% summarize (number_distinct_cuts=n_distinct(dat$cut),
number_distinct_color= n_distinct(dat$color), 
number_distinct_clarity = n_distinct(dat$clarity))

# Price summary, 326 - 18823
summary(dat$price)
# Carat summary, 0.2 - 5.01
summary(dat$carat)

# Diamond sold at highest price
dat %>% filter(price==18823) %>% nrow()
# Diamond sold at lowest price
dat %>% filter(carat==0.2) %>% nrow() 


# Next we like to see for the diamond sold at highest price, how the 4C is (carat, cut, color, clarity)

# check Max sold at Price high, but Avg carat, avg color, good in cutting & clarity
dat %>% filter(price==18823) %>% select (carat, price, cut, clarity, color )

# Next we like to see counts in each category of Diamond's cut, color, clarity and distribution.  

t1<-dat %>% group_by(cut)%>% 
  summarise(count_cut=n(),percent=n()/length(dat$cut) * 100) %>%
  arrange(desc(percent))
print.data.frame(t1)


# 4 colors G, E, F, H consist about 70% of total percentage, Best color D is 12%, color distribution is more variant than cut.  

t2<-dat %>% group_by(color)%>% summarise(count_color=n(), percent=n()/length(dat$color) *100) %>% arrange(desc(percent))
print.data.frame(t2)


# For clarity
t3<-dat %>% group_by(cut)%>% summarise(count_cut=n(), percent=n()/length(dat$cut) * 100) %>% arrange(desc(percent))
print.data.frame(t3)


# ---3.  Data Cleaning ---
# Before we start data analysis and prediction, we need to get rid of Invalid data (e.g: NULLS, zeros, etc).

# remove zero rows in x, y, z
dat %>% filter(x==0 | y==0 | z==0) %>% nrow()
dat <- dat %>% filter(dat$x !=0 & dat$y!=0 & dat$z!=0)
dim(dat)

# remove outliers 
dat <- dat %>% filter(x>=3.73 & x<=10.74) %>%
  filter(y>=3.7 & y<=58.9) %>%
  filter(z>=1.1 & z<=31.8) %>%
  filter(depth>=43.0 & depth<=79.0) %>%
  filter(table>=43.0 & table<=95.0) 
dim(dat)


# 3rd, we need to convert 3 categorical data to numeric for regression analysis, here is number setup:
# cut(worst to best): Fair 1, Good 2, Ideal 3, Premium 4, Very Good 5
# color(J worst to D best): J 1, I  2, H 3, G 4, F 5, E 6,  D 7
# clarity(I1 worst, SI2, SI1, VS2, VS1, VVS2, VVS1, IF best):

tmp <- dat %>% 
  mutate(
    cut_num = case_when (
      cut=="Fair" ~ 1,
      cut=="Good" ~ 2,
      cut=="Ideal" ~3,
      cut=="Premium" ~ 4,
      cut=="Very Good" ~ 5
    ), color_num = case_when (
      color=="J" ~ 1,
      color=="I" ~ 2,
      color=="H" ~3,
      color=="G" ~ 4,
      color=="F" ~ 5,
      color=="E" ~ 6,
      color=="D" ~ 7
    ), clarity_num = case_when (
      clarity=="I1" ~ 1,
      clarity=="SI2" ~ 2,
      clarity=="SI1" ~3,
      clarity=="VS2" ~ 4,
      clarity=="VS1" ~ 5,
      clarity=="VVS2" ~ 6,
      clarity=="VVS1" ~ 7,
      clarity=="IF" ~ 8 
    )
  ) 
my_dat<- tmp %>% select(carat,cut_num,color_num,clarity_num,depth,table,price,x,y,z)


# now we have a dataset all variables numeric
glimpse(my_dat)


# ---4. Data Visualization ---
# 3.1 Plot Diamond Cut by Numbers to see cut distribution
# Immediately we see 3 major cuttings: Ideal, Premium, Very Good

theme_set(theme_minimal())
p1<- dat %>% group_by(cut) %>% summarise(Number=n()) %>% arrange(desc(Number))%>%
  ggplot(aes(x=reorder(cut, -Number), y=Number)) +
  geom_col(show.legend = FALSE)+
  labs(title = "Diamond Cut Numbers") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1), legend.position = "none") +
  theme(panel.grid.major.x = element_blank())

p1


# Question: is the Best Clarity sold at highest price? 
p2 <-dat %>% 
  ggplot(aes(clarity, price)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) + 
  scale_y_continuous(trans = "log2") 
p2


# Diamond price distribution,
hist(dat$price,freq = TRUE, col="light blue", border="black", main="Diamond Price Histogram", xlab="price", ylab="count")


# Let's look into Diamonds price for those less than $950 vs higher than $5323

g1<- dat %>% filter(price<=950) %>%
   ggplot(aes(x=price)) + 
   geom_density(fill="#69b3a2", color="pink", alpha=0.8) +
   labs(title="Price Group $950 or less")
g1
g2<- dat %>% filter(price>=5323) %>%
   ggplot(aes(x=price)) + 
   geom_density(fill="pink", color="red", alpha=0.8) +
   labs(title="Price Group $5323 or more") 
g2
# install.packages("gridExtra")
grid.arrange(g1, g2, nrow=1)


# Let's check Very-Expensive diamonds that sold more than $18K

t <-dat%>% filter(price>=18000) %>% select(cut,color,carat,price)

total<-length(!is.na(t$cut))
t1<- t%>%group_by(cut)%>%summarise(percent=n()/total * 100)%>% arrange(desc(percent))
print.data.frame(t1)


# Plot 6 very expensive color
total<-length(!is.na(t$color))

t2<- t%>%group_by(color)%>%summarise(percent=n()/total * 100)%>% arrange(desc(percent))
print.data.frame(t2)

# 45 Diamonds in one percent group, very expensive by carat
total<-length(!is.na(t$carat))

t%>%group_by(carat)%>%summarise(percent=round(n()/total * 100))%>% filter(percent>=1) %>% nrow()

# Plot the distribution of carat
# min - max from 1.04 to 4.5, mid range around 2

p<-t%>%group_by(carat)%>%summarise(
  percent=round(n()/total * 100)) %>% 
  ggplot(aes(x=percent, y=carat)) +
  geom_smooth() + labs(title = "very expensive Diamond carat")

suppressWarnings(print(p))

# I add a new column VolGrp to get full size,  VolGrp
tmp<-dat %>% select(price,x,y,z,carat,cut,color,clarity) %>%
  mutate(
  Vol = round(x*y*z, digits = 2),
  VolGrp = case_when(
    Vol <65.19 ~ "small",
    Vol >170.84 ~ "large",
    TRUE ~ "medium"
  )
)

# The over-lap of price in Large, Medium, Small group
tmp %>% group_by(VolGrp) %>% 
  summarise(min_price=min(price), max_price=max(price))


# Some small diamonds are sold more than $2075
# there are 6 of small-Diamond sold at high price
tmp_tbl <-tmp %>% filter(VolGrp=='small' & (price >=2000 & price <=2366)) %>%select(price,carat,cut,color,clarity)
print(tmp_tbl)


# Plot VolGrp
tmp2<-tmp %>% select(VolGrp,cut)%>%group_by(VolGrp,cut)%>%
  summarise(number=n())

tmp2%>%
ggplot(aes(fill=cut, x=VolGrp, y=number, )) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Diamond Volume vs cut distribution")

# Plot VolGrp clarity
tmp3<-tmp %>% select(VolGrp,clarity)%>%group_by(VolGrp,clarity)%>%
  summarise(number=n())

tmp3%>%
  ggplot(aes(fill=clarity, x=VolGrp, y=number)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Diamond Volume vs clarity distribution")


# Plot 9 Expensive
tmp4<-tmp %>% select(VolGrp,price) %>% mutate(
  Expensive=case_when(
    price >=5350 ~ 1,
    price <5350 ~ 0
  ))

tbl <- tmp4 %>% group_by(VolGrp,Expensive) %>% summarise(number=n()) %>% mutate(percent=round((number/sum(number)*100),2))

tbl %>%
  ggplot(aes(x=VolGrp,fill=Expensive, y=number)) +
  geom_bar(position="dodge", stat="identity", fill="gray") +
  geom_text(data = tbl, 
            aes(label = number), 
            position = position_dodge(width=0.8), 
            vjust=-0.2, 
            fontface = "bold", size=3) +
  theme(legend.position = 'none') +
  ggtitle("Expensive Rate by VolumeGroup") +
  scale_x_discrete(name= "Volume") +
  scale_y_continuous(name = "price") +
  geom_label(data = tbl, 
             aes(x = VolGrp, y = number, label = paste0(percent, "%")),
             position = position_stack(vjust = 0.2), size=3)


# --- Data Analysis ---
# 1. Correlation
cormat <- signif(cor(my_dat), 2)
ggcorrplot(cormat)

# cor 4 var
my_dat2 <- my_dat %>% select(carat,x,y,z,price)

cor(my_dat2$price, my_dat2$carat)
cor(my_dat2$price, my_dat2$x)
cor(my_dat2$price, my_dat2$y)
cor(my_dat2$price, my_dat2$z)

# "x", "y", "z" show a higher correlation to price column
# "depth", "cut" and "table" show low correlation


# --- 2. Test the Models ---
# 1. Split into train 0.75 and test set 0.25 of dataset

set.seed(123) 
test_index <- createDataPartition(my_dat2$price, times = 1, p = 0.25, list = FALSE)
test <- my_dat2[test_index,]
train <- my_dat2 [-test_index,]

# The RMSE function that will be used 
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  round(sqrt(mean((true_ratings - predicted_ratings)^2)), digits = 4)
}

mu<- mean(train$price) #3935.8

# Base-line RMSE, 1.127
rmse_output <- RMSE(log(test$price), log(mu))
RMSE_table <- tibble(Method = "Base-line RMSE", 
                      RMSE = rmse_output)
RMSE_table

# 2. Test models
set.seed(123) 
lm_model <- train(price ~ x + y + z + carat, data=train, method="lm")
glm_model <- train(price ~ x + y + z + carat, data=train, method="glm")
glm_carat_model <- train(price ~ carat, data=train, method="glm")
loess_model<- train(price ~ x + y + z + carat, data=train, method="gamLoess")


# glm gives the best in Rsquare, RMSE, MAE
results <- resamples(list(LM=lm_model, GLM=glm_model, GLM_carat=glm_carat_model, LOESS=loess_model))
summary(results)

bwplot(results)


# 3. Pick GLM model
set.seed(123) 
# glm with carat only
fit_glm_carat <- glm(price ~ carat, data=train)
pred<-predict(fit_glm_carat, test)
pred<-abs(pred)
rmse_output <- RMSE(log(test$price), log(pred))

RMSE_table <- rbind(RMSE_table,
                    tibble(Method = "Regression model by carat", 
                            RMSE = rmse_output))

# glm with 4 predictors x, y, z, carat
set.seed(123) 
fit_glm_4 <- glm(price ~ x + y + z + carat, data=train)
pred<-predict(fit_glm_4, test)
pred <- abs(pred)
rmse_output <- RMSE(log(test$price), log(pred))

RMSE_table <- rbind(RMSE_table,
                    tibble(Method = "Regression model by 4 predictors", 
                            RMSE = rmse_output))

# The glm regression model coefficients with carat
# Y_hat = -2253 + 7749*carat
fit_glm_carat$coefficients

# The glm regression model coefficients with x,y,z,carat
# Y_hat = 2423 -1391*x +571*y -624*z + 10544*carat
fit_glm_4$coefficients
RMSE_table

# Tune for data partitioning percentage

ps <- seq(from=.10, to=.90, by=.05)

rmse_list <- sapply(ps, function(p){
  train_index <- createDataPartition(my_dat2$price,
                                     times=1,
                                     p=ps,
                                     list=FALSE)
  train <- my_dat2[train_index,]
  test <- my_dat2[-train_index,]
  fit <- glm(price ~ x+y+z+carat, data = train)
  test <- test %>% 
    mutate(pred_price = abs(predict.glm(fit, newdata=test)))
  RMSE(log(test$price), log(test$pred_price))
})

# Pick suggested percentage
min(rmse_list) #0.332 min RMSE
ps[which.min(rmse_list)] #0.6
plot(ps, rmse_list) # no clear winner


#  new data split
set.seed(123) 
train_index <- createDataPartition(my_dat2$price, times = 1, p = 0.6, list = FALSE)
train <- my_dat2 [train_index,]
test <- my_dat2[-train_index,]

fit_glm_carat_final <- glm(price ~ carat, data=train)
pred_3<-predict(fit_glm_carat_final, test)
pred_3<-abs(pred_3)

fit_glm_4_final <- glm(price ~ x + y + z + carat, data=train)
pred_4<-predict(fit_glm_4_final, test)
pred_4<-abs(pred_4)

rmse_output_3 <- RMSE(log(test$price), log(pred_3))
rmse_output_4 <- RMSE(log(test$price), log(pred_4))


RMSE_table <- rbind(RMSE_table,
                    tibble(Method = "Regression model using carat + better partition", 
                           RMSE = rmse_output_3),
                    tibble(Method = "Regression model using 4 predictors + better partition", 
                           RMSE = rmse_output_4))

RMSE_table


# final model formula
# Y_hat <- 2685 - 1346*x + 349*y - 452*z + 10731*carat
fit_glm_4_final$coefficients




