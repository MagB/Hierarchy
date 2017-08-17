
file= my_dat=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/data_for_publication/2008Geno_mpb_temp.csv",'r')

print "\n"
count=1
pop={}
old_pop="no"
for line in file:
    line=line.rstrip()
    sline=line.split(",")
    
    if count==1:
        loc_names=[]
        for i  in sline[4:]:
            loc_names.append(i)
            count+=1
        print "\t",", ".join(loc_names)    
        continue
        
    new_pop=sline[1]
    newline=[]
    if new_pop!=old_pop:
        print "Pop"
    for i  in sline[4:]:
        genotype=i.split(":")
        if i=="NA":
            genotype="000000"
        newline.append( "".join(genotype))
    print sline[1]+",", " ".join(newline)
    old_pop=new_pop
