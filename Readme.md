#pop names

Pop_1	QBED1-1
Pop_2	QBED1-2
Pop_3	QBED2-1
Pop_4	QBED2-2
Pop_5	QTUR-1
Pop_6	QTUR-2
Pop_7	QLL1-1
Pop_8	QLL1-2
Pop_9	QLL3-1
Pop_10	QLL3-2
Pop_11	QRL1-1
Pop_12	QRL1-2
Pop_13	QRL2-1
Pop_14	QRL2-2
# scripts:
## MEM_floral_data_AWNestedAnalysis.R
MPB updated this script. This script has the mixed models used to estimate the variance attributed to outcrops, plots and plants within plots. 

## PCA Euclidean Dist.R
MPB updated this script. This script pulls in the floral data and pairwise Fst estimates from Arlequin to do isolation by distance analysis using Mantel's test (Spearman's rank correlation)
I also do a Mantel test for Floral_distance~Fst


# data:

## floral08.csv
This dataset is also housed:/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/flower data/Nested ANOVA
There is no difference between these two datasets. I moved a copy into my R project folder to more easily keep track of data that will be submit along with the manucscipt.


WHAT IS LEFT TO DO:
1. check genotype data (find the right file, possibly check how he did Fst etimates), decide which version gets uploaded
2. possibly redo InStruct and make assignment plots like structure plots?


Checking the genotype data:
1. there are 5040 rows of data in the genoB dataset, 10 rows are misinterpreted by Excel for microsat ID Jul-50
HierStat data has 560 rows, but the 
GenoB has 5040
2008Geno has 275 rows.
../../Current R actives/allgenotype2008.csv (called geno08 in R scripts) has 2520 rows. corresponding to 20 plants sampled at 14 sites for 9 microsat loci
AW removed microsat 100.2.8 from this set because it has too many missing data. 


There are 275 individuals for which genotype data exists for 9 loci
That means the HierStat data format and the InStruct data format should have 550 rows but they each have 560 (560 would be correct if there were complete genotype information for all 280 individuals). Where did the extra samples come from?
1. AW replaced those 5 plants for which genotypes were missing
2. there is a mistake


I submit Wed jul 13:
    InStruct_gcc -d InStruct2008popquad.txt -o 10chains.txt -K 14 -v 1 -x 0 w- 1 j- 2000 -f 0 -af 0 -lb 1 -L 13 -N 280 -m -9 -a 1 -ik 1 -kv 1 15 -w 1 -p 2 -mm 3.0e9 -r 2000 -g 1 -c 10 -pi 1 -pf 0 -b 100000 -u 200000 -t 10 -sl 0.95 -df 1 -cf updated_10chains.txt &

