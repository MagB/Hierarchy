HierFstat2008=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/HierFstat/HierFstat2008.csv", 'r')
HierFstat2008_out=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/comparing_genotype_data/HierFstat2008_out.txt", 'w')

Geno2008=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/data/Gstudio/2008Geno.csv", 'r')
Geno2008_out=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/comparing_genotype_data/Geno2008_out.txt", 'w')


#HierFstat2008 data has 560 lines, which corresponds to 260 individuals
#2008Geno.csv only has 275 lines one for each sample. 
#the goal is to find which sample is missing


#part 1 is to convert HierFstat2008 into the same format as 2008Geno.csv
line_count=1
g={}
for i in range(1,10):
    g[i]=[]

def rest_dicts():
    for i in range(1,10):
        g[i]=[]
        line_count=1
    return(line_count, g)


for line in HierFstat2008:
    line=line.strip("\n")
    sline=line.split(",")
   # print sline,sline[0]

    if sline[0]=='"pop"':
        continue
    
    
    
    if line_count>2:
        newline=",".join([pop[1:-1], quad, ":".join(g[1]), ":".join(g[2]),":".join(g[3]),":".join(g[4]),":".join(g[5]),":".join(g[6]),":".join(g[7]),":".join(g[8]),":".join(g[9]),"\n"])
        line_count,g =rest_dicts()

        HierFstat2008_out.write(newline)
        
    pop=sline[0]
    quad=sline[1]
    line_count+=1
    
    for i in range(2,11):
        g[i-1].append(sline[i])
newline=",".join([pop[1:-1], quad, ":".join(g[1]), ":".join(g[2]),":".join(g[3]),":".join(g[4]),":".join(g[5]),":".join(g[6]),":".join(g[7]),":".join(g[8]),":".join(g[9]),"\n"])
HierFstat2008_out.write(newline)

HierFstat2008_out.close()
line_count=1

line_num=0
for line in Geno2008:
    line=line.rstrip('\r\n')
    
    sline=line.split(",")
    if line_count==1:
        line_count+=1
        continue
    #print sline[12]
    
    for i in range(4,13):
        if sline[i]=="NA":
            sline[i]="NA:NA"
        
    newline=",".join([sline[1],sline[2],sline[4],sline[5],sline[6],sline[7],sline[8],sline[9],sline[10],sline[11],sline[12], "\n"])
    Geno2008_out.write(newline)
    line_num+=1

Geno2008_out.close()



allgenos=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/comparing_genotype_data/allgenotype2008.csv", 'r')
allgenos_out=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/comparing_genotype_data/allgenotype2008_out.csv", 'w')
for line in allgenos:
    line=line.rstrip('\r\n')
    sline=line.split(",")
    
    new_line=",".join([ line, "-".join([ sline[1], sline[3], sline[2]]),'\n'])
    allgenos_out.write(new_line)
allgenos_out.close()