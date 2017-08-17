#expected heterozygosity:

file= my_dat=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/2008Geno_mpb_temp.csv",'r')
count=1
pop={}
genos_list={}

locus_name=["loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9"]
pop_names=["QBED1", "QBED2","QLL1","QLL3","QRL1","QRL2", "QTUR"]
for pop in pop_names:
    genos_list[pop]={}
    for i in locus_name:
        genos_list[pop][i]={}
    
for line in file :
    line=line.rstrip()
    sline=line.split(",")
    if count==1:
        loc_names=[]
        for i  in sline[4:]:
            loc_names.append(i)
            count+=1
        continue
    pop=sline[1]
    genos=sline[4:]
    
    count_locus=0
    for i  in genos:
        locus=locus_name[count_locus]
        count_locus+=1
        
        genotype=i.split(":")
        if i=="NA":
            genotype="000000"   
            continue
        if genotype[0] not in genos_list[pop][locus].keys():
            genos_list[pop][locus][str(genotype[0])]=1
        else:
            genos_list[pop][locus][str(genotype[0])]+=1
            
        if genotype[1] not in genos_list[pop][locus].keys():
            genos_list[pop][locus][str(genotype[1])]=1
        else:
            genos_list[pop][locus][str(genotype[1])]+=1
            
        
    pop_name=sline[1]
    

total_allele_count=0
total_count={}
for pop in pop_names:
    total_count[pop]={}
    for i in locus_name:
        total_count[i]={}

for pop in  pop_names:   
    for locus in genos_list[pop].keys():
        total_allele_count=0
        #print genos_list[pop][locus].keys()
        for alleles in genos_list[pop][locus].keys():
            total_allele_count+=genos_list[pop][locus][alleles]
    
        total_count[pop][locus]=total_allele_count
sum_p=0
sum_p2=0
for pop in pop_names:
    for locus in locus_name:
        p2=0
        sum_p2=0
        sum_p=0   
        for alleles in genos_list[pop][locus].keys():
            #print locus, alleles, float(genos_list[locus][alleles]),float(total_count[locus])
 
            p=(float(genos_list[pop][locus][alleles])/float(total_count[pop][locus]))
            p2=p*p
            
            sum_p2+=p2
            sum_p+=p
           # print locus, p,p2
        #print sum_p, sum_p2
        print pop,"He", locus, 1-sum_p2
        sum_p=0
    