Rscript /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/scripts/Templates_PRSice_v2.0/PRSice.R --dir . \
--prsice /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/scripts/Templates_PRSice_v2.0/PRSice_linux \
--base /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/base_data/010_IQ/base_data.txt \
--target /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/gwas/Final_data_HRCimp_QC2/HELIX.impQC.rs.EUR \
--thread 16 \
--beta \
--stat BETA \
--clump-r2 0.1 \
--bar-levels 0.00000005,0.000005,0.0001,0.001,0.01,0.05,0.1,0.2,0.5,1 \
--fastscore T \
--no-regress \
--perm 10000 \
--all-score \
--out /home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/010_IQ/IQ_EUR/HELIX_IQ \


## NOTES: 
# if there are spaces after " \ "next command will not be recogized
# --fastscore T \  : true if you want to get different thresholds
# --all-score : to get PRS for each of the thesholds indicated
# --out : indicate name of the output files
# by default: makes average, if --score sum indicated, sum of effect sizes
# We have to specify the names of the variables in the GWAS reference as:
    # --snp SNP \
    # --chr CHR \
    # --bp BP \
    # --A1 A1 \ #effect allele
    # --A2 A2 \ #non effect allele
    # --stat OR \
    # --pvalue P

# A temporal folder that PRSice needs to run is automatically and by default created
# Therefore, one can remove them by the following line of command
rm -r lib
