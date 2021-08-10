### 1. DEMOGRAPHIC DESCRITPIONS

```{r}

# Establish the working directory 
setwd("D:/Georgina/Documents/Creating_M1")
list.files()

library(data.table)

# Let's load M5, the matrix that contains all the individuals with available genetic information, covariates, smoking and maternal education information as well. 
M5 <- fread("M5_results.txt")
class(M5)

```

#### 1.1. Covariates

```{r}
# Print the type of data of each variable 
str(M5)

# Convert categorical variables into factor: 
M5$sex <- as.factor(M5$sex)
M5$msmok_a <- as.factor(M5$msmok_a)
M5$msmok_s <- as.factor(M5$msmok_s)
M5$msmok_d <- as.factor(M5$msmok_d)
M5$h_edumc <- as.factor(M5$h_edumc)

# Age
age_mean <- mean(M5$age_sample_months, na.rm = T)
# Take only 2 first digits
age_mean <- format(age_mean,digits=2)
age_sd <- sd(M5$age_sample_months, na.rm = T)
# Take only 2 first digits
age_sd <- format(age_sd,digits=2)

# Sex
females <- length(which(M5$sex == "female")) 
total_sex <- length(M5$sex)
females_perc <- (females/total_sex)*100
# Take only 2 first digits
females_perc <- format(females_perc,digits=2)


# Maternal education (REF GROUP = Low education level)
N_total_edu <- length(M5$h_edumc)

N_low_edu <- length(which(M5$h_edumc == 0))
N_middle_edu <- length(which(M5$h_edumc == 1))
N_high_edu <- length(which(M5$h_edumc == 2))

low_edu_perc <- (N_low_edu/N_total_edu)*100
# Take only 2 first digits
low_edu_perc <- format(low_edu_perc,digits=2)

middle_edu_perc <- (N_middle_edu/N_total_edu)*100
# Take only 2 first digits
middle_edu_perc <- format(middle_edu_perc,digits=2)

high_edu_perc <- (N_high_edu/N_total_edu)*100
# Take only 2 first digits
high_edu_perc <- format(high_edu_perc,digits=2)


# Smoking
    # Any
    N_total_smokers_a <- length(M5$msmok_a)
    N_smokers_a <- length(which(M5$msmok_a == 0))
    perc_non_smok_a <- (N_smokers_a/N_total_smokers_a)*100
    # Take only 2 first digits
    perc_non_smok_a <- format(perc_non_smok_a,digits=2)
    
```


#### 1.2. HELIX phenotypes 

```{r}

# Birth Weight
N_BW <- (nrow(M5) - sum(is.na(M5$BW))) # 1422
mean_BW <- mean(M5$BW, na.rm = T)
# Take only 2 first digits
mean_BW <- format(mean_BW,digits=2)
sd_BW <- sd(M5$BW, na.rm = )
# Take only 2 first digits
sd_BW <- format(sd_BW,digits=2)

# Gestational Age
N_GA <- (nrow(M5) - sum(is.na(M5$GA))) # 1422 
mean_GA <- mean(M5$GA, na.rm = T)
# Take only 2 first digits
mean_GA <- format(mean_GA,digits=2)
sd_GA <- sd(M5$GA, na.rm = T)
# Take only 2 first digits
sd_GA <- format(sd_GA,digits=2)

# Body mass index
N_BMI <- (nrow(M5) - sum(is.na(M5$BMI))) # 1422
mean_BMI <- mean(M5$BMI, na.rm = T)
# Take only 2 first digits
mean_BMI <- format(mean_BMI,digits=2)
sd_BMI <- sd(M5$BMI, na.rm = T)
# Take only 2 first digits
sd_BMI <- format(sd_BMI,digits=2)

# Height
N_HGT <- (nrow(M5) - sum(is.na(M5$HGT))) # 1422
mean_HGT <- mean(M5$HGT, na.rm = T)
# Take only 2 first digits
mean_HGT <- format(mean_HGT,digits=2)
sd_HGT <- sd(M5$HGT, na.rm = T)
# Take only 2 first digits
sd_HGT <- format(sd_HGT,digits=2)

# Fat mass
N_FM <- (nrow(M5) - sum(is.na(M5$FM))) # 1402 
mean_FM <- mean(M5$FM, na.rm = T)
# Take only 2 first digits
mean_FM <- format(mean_FM,digits=2)
sd_FM <- sd(M5$FM, na.rm = T)
# Take only 2 first digits
sd_FM <- format(sd_FM,digits=2)

# Waist Circumference
N_WC <- (nrow(M5) - sum(is.na(M5$WC))) # 1416
mean_WC <- mean(M5$WC, na.rm = T)
# Take only 2 first digits
mean_WC <- format(mean_WC,digits=2)
sd_WC <- sd(M5$WC, na.rm = T)
# Take only 2 first digits
sd_WC <- format(sd_WC,digits=2)

# Systolic blood pressure
N_SBP <- (nrow(M5) - sum(is.na(M5$SBP))) # 1420
mean_SBP <- mean(M5$SBP, na.rm = T)
# Take only 2 first digits
mean_SBP <- format(mean_SBP,digits=2)
sd_SBP <- sd(M5$SBP, na.rm = T)
# Take only 2 first digits
sd_SBP <- format(sd_SBP,digits=2)

# Diastolic blood pressure
N_DBP <- (nrow(M5) - sum(is.na(M5$DBP))) # 1420 
mean_DBP <- mean(M5$DBP, na.rm = T)
# Take only 2 first digits
mean_DBP <- format(mean_DBP,digits=2)
sd_DBP <- sd(M5$DBP, na.rm = T)
# Take only 2 first digits
sd_DBP <- format(sd_DBP,digits=2)

# Forced expiratory volume
N_FEV <- (nrow(M5) - sum(is.na(M5$FEV))) # 1174
mean_FEV <- mean(M5$FEV, na.rm = T)
# Take only 2 first digits
mean_FEV <- format(mean_FEV,digits=2)
sd_FEV <- sd(M5$FEV, na.rm = T)
# Take only 2 first digits
sd_FEV <- format(sd_FEV,digits=2)

# Forced vital capacity
N_FVC <- (nrow(M5) - sum(is.na(M5$FVC))) # 1314
mean_FVC <- mean(M5$FVC, na.rm = T)
# Take only 2 first digits
mean_FVC <- format(mean_FVC,digits=2)
sd_FVC <- sd(M5$FVC, na.rm = T)
# Take only 2 first digits
sd_FVC <- format(sd_FVC,digits=2)


```


#### 1.3. Creating the demographic table 

```{r}

# Create the values that are going to go inside the matrix (table)

# Age
age <- paste0(age_mean, "(", age_sd, ")") 

# Females
female <- paste0(females, ",", females_perc, "%")

# Mat Ed
mat_educ <- paste0(N_low_edu, ",", low_edu_perc, "%")

# Smok
smok <- paste0(N_smokers_a, ",", perc_non_smok_a, "%")

# BW
t_bw <- paste0(N_BW,",", mean_BW,"(",sd_BW,")")

# GA
t_ga <- paste0(N_GA,",", mean_GA,"(",sd_GA,")")

# BMI
t_bmi <- paste0(N_BMI,",", mean_BMI,"(",sd_BMI,")")

# FM
t_fm <- paste0(N_FM,",", mean_FM,"(",sd_FM,")")

# WC
t_wc <- paste0(N_WC,",", mean_WC,"(",sd_WC,")")

# HGT
t_hgt <- paste0(N_HGT,",", mean_HGT,"(",sd_HGT,")")

# SBP
t_sbp <- paste0(N_SBP,",", mean_SBP,"(",sd_SBP,")")

# DBP
t_dbp <- paste0(N_DBP,",", mean_DBP,"(",sd_DBP,")")

# FEV
t_fev <- paste0(N_FEV,",", mean_FEV,"(",sd_FEV,")")

# FVC
t_fvc <- paste0(N_FVC,",", mean_FVC,"(",sd_FVC,")")


# Creat the matrix 
tab <- matrix(c(age,female,mat_educ,smok,t_bw,t_ga,t_bmi,t_fm,t_wc,t_hgt,t_sbp, 
              t_dbp,t_fev,t_fvc))

# Label the rows 
rownames(tab) <- c('Age (months) (N, mean, sd)','Females (N, %)',
                   'Low maternal education level (N, %)', 
                   'Non-smoker mothers (N, %)', 'BW (N, mean, sd)', 
                   'GA (N, mean, sd)', 'BMI (N, mean, sd)',
                   'FM (N, mean, sd)', 'WC (N, mean, sd)', 
                   'HGT (N, mean, sd)', 'SBP (N, mean, sd)', 
                   'DBP (N, mean, sd)', 'FEV (N, mean, sd)', 
                   'FVC (N, mean, sd)')
names(tab) <- NULL
tab <- as.data.frame(tab)

# Exporting without headers 
write.table(tab, "Demographic_results.csv", sep=",", col.names=FALSE)

# Importing it 
dat <- fread("Demographic_results.csv", header=F)
# change the column names 
colnames(dat) <- c("", "")

# Generating a nice table
library(knitr)
library(kableExtra)
library(magrittr)

x <- dat %>%
    kbl(caption="Table 1. Demographic descriptions") %>%
    kable_classic(full_width = F, html_font = "Cambria")

save_kable(x, 'Table 1.pdf')
save_kable(x, 'Table 1.png')


```

### 2. CORRELATION DESCRIPTIONS

```{r}
library(corrplot)
M5.PRS <- M5[,3:12]
M5.cor <- cor(M5.PRS)
corrplot(M5.cor)

pdf("Corrplot_M5_PRSs.pdf") 
corrplot(M5.cor)
dev.off()


png("Corrplot_M5_PRSs.png") 
corrplot(M5.cor)
dev.off()

```
