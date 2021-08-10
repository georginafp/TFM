In the following script, several regression models will be performed for each of the phenotypes of interest, for which one wants to determine its interaction with maternal smoking. 

The regression models are the followings: 

  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs 
  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
  (3) M3 = tobacco + PRS (of phenotype) + maternal education  + age (or gestational age) + sex +        10 GWAS PCs
  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs

*** NOTE: if the phenotype is z-adjusted, nor age nor sex are included in the model!


## IMPORTING DATA 

```{r}
library(data.table)
M5 <- fread("M5_results.txt")

```


### PHENOTYPE 1: Birth Weight (BW)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M1.1 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M1.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M1.2 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.01_BW, data=M5)
summary(M1.2)
coef(summary(M1.2))

  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M1.3 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.01_BW + as.factor(h_edumc), data=M5)
summary(M1.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M1.4 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.01_BW, data=M5)
summary(M1.4)

```

### PHENOTYPE 2: Gestational Age (GA)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M2.1 <- glm(GA ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              as.factor(h_edumc), data=M5)
summary(M2.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M2.2 <- glm(GA ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Pt_0.05_GA, data=M5)
summary(M2.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M2.3 <- glm(GA ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              Pt_0.05_GA + as.factor(h_edumc), data=M5)
summary(M2.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M2.4 <- glm(GA ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              as.factor(msmok_a)*Pt_0.05_GA, data=M5)
summary(M2.4)

```

### PHENOTYPE 3: Body Mass Index (BMI)

```{r}
#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M3.1 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M3.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M3.2 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.05_BMI, data=M5)
summary(M3.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M3.3 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.05_BMI + as.factor(h_edumc), data=M5)
summary(M3.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M3.4 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_BMI, data=M5)
summary(M3.4)

```

### PHENOTYPE 4: Systolic Blood Pressure (SBP)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M4.1 <- glm(SBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M4.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M4.2 <- glm(SBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_1_SBP, data=M5)
summary(M4.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M4.3 <- glm(SBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_1_SBP + as.factor(h_edumc), data=M5)
summary(M4.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M4.4 <- glm(SBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_1_SBP, data=M5)
summary(M4.4)

```

### PHENOTYPE 5: Diastolic Blood Pressure (DBP)

```{r}
#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M5.1 <- glm(DBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M5.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M5.2 <- glm(DBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.01_DBP, data=M5)
summary(M5.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M5.3 <- glm(DBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.01_DBP + as.factor(h_edumc), data=M5)
summary(M5.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M5.4 <- glm(DBP ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.01_DBP, data=M5)
summary(M5.4)

```

### PHENOTYPE 6: Forced Expiratory Volume (FEV)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M6.1 <- glm(FEV ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              as.factor(h_edumc), data=M5)
summary(M6.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M6.2 <- glm(FEV ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Pt_0.01_FEV, data=M5)
summary(M6.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M6.3 <- glm(FEV ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              Pt_0.01_FEV + as.factor(h_edumc), data=M5)
summary(M6.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M6.4 <- glm(FEV ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              as.factor(msmok_a)*Pt_0.01_FEV, data=M5)
summary(M6.4)

```

### PHENOTYPE 7: Height (HGT)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M7.1 <- glm(HGT ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              as.factor(h_edumc), data=M5)
summary(M7.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M7.2 <- glm(HGT ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Pt_0.001_HGT, data=M5)
summary(M7.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M7.3 <- glm(HGT ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              Pt_0.001_HGT + as.factor(h_edumc), data=M5)
summary(M7.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M7.4 <- glm(HGT ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              as.factor(msmok_a)*Pt_0.001_HGT, data=M5)
summary(M7.4)

```

### PHENOTYPE 8: Forced Vital Capacity (FVC)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M8.1 <- glm(FVC ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              as.factor(h_edumc), data=M5)
summary(M8.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M8.2 <- glm(FVC ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + Pt_0.001_FVC, data=M5)
summary(M8.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M8.3 <- glm(FVC ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(msmok_a) + 
              Pt_0.001_FVC + as.factor(h_edumc), data=M5)
summary(M8.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M8.4 <- glm(FVC ~ as.factor(sex) + age_sample_months + PC1 + PC2 + PC3 + PC4 + 
              PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              as.factor(msmok_a)*Pt_0.001_FVC, data=M5)
summary(M8.4)

```

### PHENOTYPE 9: Fat Mass (FM)

```{r}

#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M9.1 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M4.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M9.2 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.05_FM, data=M5)
summary(M4.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M9.3 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.05_FM + as.factor(h_edumc), data=M5)
summary(M9.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M9.4 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_FM, data=M5)
summary(M9.4)

```

### PHENOTYPE 10: ´Waist Circumference (WC)

```{r}
#  (1) M1 = tobacco + maternal education + age (or gestational age) + sex + 10 GWAS PCs
M10.1 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc), data=M5)
summary(M10.1)

#  (2) M2 = PRS (of phenotype) + age (or gestational age) + sex + 10 GWAS PCs
M10.2 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               Pt_0.001_WC, data=M5)
summary(M10.2)
  
#  (3) M3 = tobacco + PRS (of phenotype) + maternal education + age (or gestational age) + sex +         10 GWAS PCs
M10.3 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.001_WC + as.factor(h_edumc), data=M5)
summary(M10.3)
  
#  (4) M4 = tobacco*PRS (of phenotype) + maternal education + age (or gestational age) + 10 GWAS         PCs
M10.4 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.001_WC, data=M5)
summary(M10.4)

```

