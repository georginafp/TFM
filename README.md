# Computational approach for calculating polygenic risk scores and assess their interaction with maternal smoking 

The following bunch of commands contains the required pieces of code for properly calculate polygenic risk scores (an estimate of an individual’s genetic liability to a trait or disease, calculated according to their genotype profile and relevant genome-wide association study (GWAS) data), enabling person’s individual risk of developing a particular disease. 
- In this study we calculated PRSs for 42 common traits in the Human Early Life Exposome (HELIX) project. The mentioned project contains data from 1.304 children aged between six and eleven years old, coming from six on-going European cohorts. Then, we tested the effect modification of each PRS on the association between maternal smoking during pregnancy and each common trait so that we could define first, how maternal smoking during pregnancy modifies the mentioned phenotypes, and second, the combined effect. 

## Preamble

A first file is included, containing the commands needed for preparing the base data. A second script is uploaded for preparing the phenotypic data. Then, two more files are included whose hold the code for computing the polygenic risk scores with, and without vallidation arguments. Then, a script is uploaded for presenting the scaling and the plotting procedure applied, and a script that summarises results and creates matrices is incorporated as well. The code for computing the descriptive analyses is added too and finally, the script for the statistical analyses is included as well. 
- The wiki section contains each command with specific comments to better understand each function used for the analyses. 