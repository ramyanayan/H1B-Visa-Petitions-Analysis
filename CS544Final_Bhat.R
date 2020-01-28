library(sampling)
library(plotly)
library(stringr)

#-----------------------------------------------------------------Data pre-processing--------------------------------------------------------------------------------------------------------------------------------------------------------------------# 

H1B_Dataset <- read.csv("/Users/ramyabhat/Desktop/ram-544/project/1. Master H1B Dataset.csv")
H1B_Dataset <- H1B_Dataset[H1B_Dataset$VISA_CLASS=='H1B' & H1B_Dataset$DECISION_YEAR=='2017' & H1B_Dataset$PREVAILING_WAGE !='0' & H1B_Dataset$EMPLOYER_COUNTRY=="UNITED STATES OF AMERICA",]


Dataset_summary <- summary(H1B_Dataset$PREVAILING_WAGE)
Dataset_summary

q1 <-(Dataset_summary[2]-(1.5 * (Dataset_summary[5]-Dataset_summary[2]))) 
q3 <- (Dataset_summary[5]+(1.5 * (Dataset_summary[5]-Dataset_summary[2])))

index_outlrs <- which(H1B_Dataset$PREVAILING_WAGE<q1 | H1B_Dataset$PREVAILING_WAGE>=q3)

H1B_Dataset <- H1B_Dataset[-index_outlrs,]


#-------Piechart-------#
plot_ly(H1B_Dataset, labels = ~names(table(H1B_Dataset$CASE_STATUS)), 
        values = ~table(H1B_Dataset$CASE_STATUS), type = 'pie',
        textposition = 'inside',textinfo = 'label+percent') %>% layout(title = "Status of Applications",
                                                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE,
                                                                                                                                                              zeroline = FALSE, showticklabels = FALSE))

#-------Box-Plot-------#
First_plot <- H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "CERTIFIED"]
Sal_plot<- plot_ly(x = ~H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "CERTIFIED"], type = "box" , name = "CERTIFIED") %>% 
  add_trace(x = ~H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "DENIED"], name = "DENIED") %>%
  add_trace(x = ~H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "CERTIFIEDWITHDRAWN"], name = "CERTIFIEDWITHDRAWN") %>%
  add_trace(x = ~H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "WITHDRAWN"], name = "WITHDRAWN") %>% layout(title = "Salary distribution of Groups",
                                                                                                                 xaxis = list(title = "Prevailing wage", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE,
                                                                                                                                                                                                                                   zeroline = FALSE, showticklabels = FALSE))
Sal_plot


#-------Histogram-------#

histogram_1 <- plot_ly(x=H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "CERTIFIED"], type='histogram' ,nbinsx=100, color = "", colors = c("Green"), name="Certified")
histogram_2 <- plot_ly(x=H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "DENIED"], type='histogram' ,nbinsx=100, color = "", colors = c("Red"), name="Denied")
histogram_3 <- plot_ly(x=H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "CERTIFIEDWITHDRAWN"], type='histogram' ,nbinsx=100, color = "", colors = c("Orange"), name="Certified-Withdrawn")
histogram_4 <- plot_ly(x=H1B_Dataset$PREVAILING_WAGE[H1B_Dataset$CASE_STATUS == "WITHDRAWN"], type='histogram' ,nbinsx=100, color = "", colors = c("Blue"), name="Withdrawn")

Dist_Sal <- subplot(nrows=2,histogram_1,histogram_2,histogram_3,histogram_4) %>% layout(title = "Distribution of Salaries ")
Dist_Sal


#-------Histogram For Salary-------#

hist(H1B_Dataset$PREVAILING_WAGE,breaks=100, ylim = c(0,40000))

plot_ly(x=H1B_Dataset$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("cyan"), name="10")


#-------Central Limit Theorem-------#

Sum <- as.numeric(H1B_Dataset$PREVAILING_WAGE)

samples <- 10000

mean_size_10 <- numeric(samples)
mean_size_20 <- numeric(samples)
mean_size_30 <- numeric(samples)
mean_size_40 <- numeric(samples)

set.seed(123)

for(i in 1:samples){
  mean_size_10[i]<- mean(sample(Sum, size=10, replace = T))
  mean_size_20[i]<- mean(sample(Sum, size=20, replace = T))
  mean_size_30[i]<- mean(sample(Sum, size=30, replace = T))
  mean_size_40[i]<- mean(sample(Sum, size=40, replace = T))
}


plot_1 <- plot_ly(x=mean_size_10, type='histogram' ,nbinsx=100, color = "", colors = c("yellow"), name="10")

plot_2 <- plot_ly(x=mean_size_20, type='histogram' ,nbinsx=100, color = "", colors = c("brown"), name="20") 

plot_3 <- plot_ly(x=mean_size_30, type='histogram' ,nbinsx=100, color = "", colors = c("navy"),name="30")

plot_4 <- plot_ly(x=mean_size_40, type='histogram' ,nbinsx=100, color = "", colors = c("Aquamarine"), name="40")

cat("Population , Mean =",round(mean(Sum),3), ", SD = ", round(sd(Sum),3))
cat("Sampe Size 10, Mean =", round(mean(mean_size_10),3), ', SD =',round(sd(Sum)/(sqrt(10)),3),"\nSampe Size 20, Mean =", round(mean(mean_size_20),3),", SD =",round(sd(Sum)/(sqrt(20)),3),"\nSampe Size 30, Mean =", round(mean(mean_size_30),3),", SD =",round(sd(Sum)/(sqrt(30)),3),"\nSampe Size 40, Mean =", round(mean(mean_size_40),3),", SD =",round(sd(Sum)/(sqrt(40)),3))
subplot(nrows=2,plot_1,plot_2,plot_3,plot_4) %>% layout(title = "Sampling Distribution ")


#-------Sampling-------#
#---Simple random sampling with replacement---#
set.seed(123)

samp_wrep <- srswr(100, nrow(H1B_Dataset))

select_data <- (1:nrow(H1B_Dataset))[samp_wrep!=0]

select_data <- rep(select_data,samp_wrep[samp_wrep!=0])
sample_srswr <- H1B_Dataset[select_data,] 

orig_data <- plot_ly(x=H1B_Dataset$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Greys"), name="Salary")
histo_srswr <- plot_ly(x=sample_srswr$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Blues"), name="SRSWR")
histo_srswr

#---Simple random sampling without replacement---#
set.seed(123)

samp_worep <- srswor(100, nrow(H1B_Dataset))
sample_srswor <- H1B_Dataset[samp_worep!=0,]

histo_srswor <- plot_ly(x=sample_srswor$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("Reds"), name="SRSWOR")
histo_srswor

#---Systematic Sampling---#
set.seed(123)

N <- nrow(H1B_Dataset)
n <- 100
k <- ceiling(N / n)
r <- sample(k, 1)
s <- seq(r, by = k, length = n)

sample_syst <- H1B_Dataset[s, ]

histo_sys <- plot_ly(x=sample_syst$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("BLUE"), name="Systematic")
histo_sys

#---unequal prob systematic sampling---#
set.seed(123)

unequal_prob <- inclusionprobabilities(H1B_Dataset$PREVAILING_WAGE, 100)

s <- UPsystematic(unequal_prob)
  
sample.unpsyst <- H1B_Dataset[s != 0, ]


histo_unpsyst <- plot_ly(x=sample.unpsyst$PREVAILING_WAGE, type='histogram' ,nbinsx=100, color = "", colors = c("BLUE"), name="Unequal Prob Systematic")
histo_unpsyst


#-------Confidence-------#
dataset.mean <- mean(H1B_Dataset$PREVAILING_WAGE)
dataset.sd <- sd(H1B_Dataset$PREVAILING_WAGE)
confid <- c(80, 90)
c_alpha <- 1 - confid/100

cat("Prevailing wage : mean =",dataset.mean," and sd =",dataset.sd)


for (i in c_alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (c_alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       dataset.mean - qnorm(1-i/2) * dataset.sd,
                       dataset.mean + qnorm(1-i/2) * dataset.sd)
  cat(str_srswr,"\n")
}

#srswr#

sd.sample.means_srswr <- dataset.sd/sqrt(nrow(sample_srswr))
xbar_srswr <- mean(sample_srswr$PREVAILING_WAGE)
cat("SRSWR : mean =",xbar_srswr," and sd =",sd.sample.means_srswr)

for (i in c_alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (c_alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       xbar_srswr - qnorm(1-i/2) * sd.sample.means_srswr,
                       xbar_srswr + qnorm(1-i/2) * sd.sample.means_srswr)
  cat(str_srswr,"\n")
}

#srswor#
sd.sample.means_srswor <- dataset.sd/sqrt(nrow(sample_srswor))
xbar_srswor <- mean(sample_srswor$PREVAILING_WAGE)
cat("SRSWOR : mean =",xbar_srswor," and sd =",sd.sample.means_srswor)

for (i in c_alpha) {
  str_srswor <- sprintf("%2d%% Conf Level (c_alpha = %.2f), CI = %.2f - %.2f",
                        100*(1-i), i, 
                        xbar_srswor - qnorm(1-i/2) * sd.sample.means_srswor,
                        xbar_srswor + qnorm(1-i/2) * sd.sample.means_srswor)
  cat(str_srswor,"\n")
}


#systematic#

sd.sample.means_syst <- dataset.sd/sqrt(nrow(sample_syst))
xbar_syst <- mean(sample_syst$PREVAILING_WAGE[!is.na(sample_syst$PREVAILING_WAGE)])
cat("SRSWOR : mean =",xbar_syst," and sd =",sd.sample.means_syst)

for (i in c_alpha) {
  str_syst <- sprintf("%2d%% Conf Level (c_alpha = %.2f), CI = %.2f - %.2f",
                            100*(1-i), i, 
                            xbar_syst - qnorm(1-i/2) * sd.sample.means_syst,
                            xbar_syst + qnorm(1-i/2) * sd.sample.means_syst)
  cat(str_syst,"\n")
}

#Upsystematic#
sd.sample.means_upsyst <- dataset.sd/sqrt(nrow(sample.unpsyst))
xbar_upsyst <- mean(sample.unpsyst$PREVAILING_WAGE[!is.na(sample.unpsyst$PREVAILING_WAGE)])
cat("UPSystematic : mean =",xbar_upsyst," and sd =",sd.sample.means_upsyst)

for (i in c_alpha) {
  str_upsyst <- sprintf("%2d%% Conf Level (c_alpha = %.2f), CI = %.2f - %.2f",
                              100*(1-i), i, 
                              xbar_upsyst - qnorm(1-i/2) * sd.sample.means_upsyst,
                              xbar_upsyst + qnorm(1-i/2) * sd.sample.means_upsyst)
  cat(str_upsyst,"\n")
}


#On-Demand

dem_2 <- numeric(1)
dem <- names(tail(sort(table(H1B_Dataset$SOC_NAME))))

for (i in 1:length(dem)) {
  
  dem_2[i] <- mean((H1B_Dataset$PREVAILING_WAGE[as.character(H1B_Dataset$SOC_NAME) == dem[i]]))
}


job_freq_plot <- plot_ly(x = ~dem, y = ~dem_2, color = ~dem, type = "bar" ) %>% layout(title = "Popular Jobs with average Income per annum", xaxis = list(title = ""), yaxis = list(title = "Salary"))
job_freq_plot

#state-wise

l <- list(color = toRGB("white"), width = 2)

g <- list(scope = 'usa', projection = list(type = 'albers usa'), showlakes = TRUE, lakecolor = toRGB('white'))

m <- list( l = 50, r = 50, b = 100,  t = 100,  pad = 4)

employer_by_state <- plot_geo(H1B_Dataset, locationmode = 'USA-states') %>%
  add_trace(z = as.vector(sort(table(H1B_Dataset$EMPLOYER_STATE))), 
            locations = names(sort(table(H1B_Dataset$EMPLOYER_STATE))), 
            color = as.vector(sort(table(H1B_Dataset$EMPLOYER_STATE))), 
            locations = names(table(H1B_Dataset$STATE)), 
            colors = 'Reds') %>% 
  colorbar(title = "Number of H1B") %>%  
  layout(title = 'H1B Visa Applications By State', geo = g,autosize = F)

employer_by_state
