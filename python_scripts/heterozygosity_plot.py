#expected heterozygosity:

file= my_dat=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/2008Geno_mpb_temp.csv",'r')
count=1
pop={}
genos_list={}

locus_name=["loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9"]
pop_names=["QBED1", "QBED2","QLL1","QLL3","QRL1","QRL2", "QTUR"]
plot=["1","2"]

for pop in pop_names:
    genos_list[pop]={}
    genos_list[pop]["1"]={}
    genos_list[pop]["2"]={}
    
    for i in locus_name:
        genos_list[pop]["1"][i]={}
        genos_list[pop]["2"][i]={}
    
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
    plot=sline[2]
    count_locus=0
    for i  in genos:
        locus=locus_name[count_locus]
        count_locus+=1
        
        genotype=i.split(":")
        if i=="NA":
            genotype="000000"   
            continue
        if genotype[0] not in genos_list[pop][plot][locus].keys():
            genos_list[pop][plot][locus][str(genotype[0])]=1
        else:
            genos_list[pop][plot][locus][str(genotype[0])]+=1
            
        if genotype[1] not in genos_list[pop][plot][locus].keys():
            genos_list[pop][plot][locus][str(genotype[1])]=1
        else:
            genos_list[pop][plot][locus][str(genotype[1])]+=1
            
        
    pop_name=sline[1]
    

total_allele_count=0
total_count={}
for pop in pop_names:
    total_count[pop]={}
    total_count[pop]["1"]={}
    total_count[pop]["2"]={}
    
    for i in locus_name:
        total_count[i]={}

for pop in  pop_names:   
    for plot in ["1","2"]:
        for locus in genos_list[pop][plot].keys():

                
            total_allele_count=0
            #print genos_list[pop][locus].keys()
            for alleles in genos_list[pop][plot][locus].keys():
                total_allele_count+=genos_list[pop][plot][locus][alleles]
        
            total_count[pop][plot][locus]=total_allele_count
sum_p=0
sum_p2=0
summary={}
plot=["1","2"]

for pop in pop_names:
    summary[pop]={}
    
    for i in plot:
        summary[pop][i]={}
        for locus in locus_name:
            summary[pop][i]=[]

print "Pop","Plot","Locus", "He","Num_alleles"
           
for pop in pop_names:
    for plot in ["1","2"]:
        p2=0
        sum_p2=0
        sum_p=0   

        for locus in locus_name:
            p2=0
            sum_p2=0
            sum_p=0               

            for alleles in genos_list[pop][plot][locus].keys():
                #print locus, alleles, float(genos_list[locus][alleles]),float(total_count[locus])
     
                p=(float(genos_list[pop][plot][locus][alleles])/float(total_count[pop][plot][locus]))
                p2=p*p
                
                sum_p2+=p2
                sum_p+=p
               # print locus, p,p2
            #print sum_p, sum_p2
            print pop,plot,locus, 1-sum_p2,len(genos_list[pop][plot][locus].keys())
            
            summary[pop][plot].append( 1-sum_p2)
            
            sum_p=0
sum_het=0.0
count=0.0

print "Pop", "Plot", "Avg_He", "number_alleles", "avg_num_alleles"
for pop in pop_names:
    for plot in ["1","2"]:
       # print pop, plot, sum(summary[pop][plot])/9
        #get avg number of alleles per locus per plot
        number_alleles=0
        for locus in locus_name:
            number_alleles+=len(genos_list[pop][plot][locus].keys())
        
        print pop, plot, sum(summary[pop][plot])/9,number_alleles ,number_alleles/9.0
     
          
count2=0.0
number_alleles=0
for pop in pop_names:
    for plot in ["1","2"]:
        sum_het+=sum(summary[pop][plot])
        count+=len(summary[pop][plot])
        
        for locus in locus_name:
            
            number_alleles+=len(genos_list[pop][plot][locus].keys())
            #print pop, plot,genos_list[pop][plot][locus].keys(), len(genos_list[pop][plot][locus].keys())
            count2+=1
            
        print pop, plot, number_alleles
            
print "Grand_mean", sum_het, count, sum_het/count, number_alleles, count2, float(number_alleles)/count2