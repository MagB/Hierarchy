import sys

#my_intstruct_files=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/instruct_files/10chains.txt")
my_intstruct_files= open(sys.argv[1], 'r')
#my_K=str(1)
my_K= sys.argv[2]
my_N=sys.argv[3]

#my_K= "1"
#my_N="280"
I_chose_my_k=False



for line in my_intstruct_files:
    if not line[0]:
        continue
        
    sline=line.split()
    if len(sline)<3:
        continue
    
 
    if "current" not in sline and I_chose_my_k==False:
        continue
    elif "current" in sline:
       # print sline[-1]
        if sline[-1]==my_K:
            #print "yes"
            I_chose_my_k=True  

      
    if "Gelman-Rubin" in sline and I_chose_my_k==True:
       # print "yes"
        break  
    
    
    if sline[1][0:3]=="pop" and sline[2][0]=="(" and I_chose_my_k==True:
       # print line
        
        next_line="Yes"
        #print line
       # print len(sline)
        sline[3]=str(int(sline[3])+1)
        
        new_2_cols="\t".join([sline[0],sline[0],sline[2], sline[3]])
        new_2_cols=new_2_cols+ " "+ sline[4]+" " 
        #print new_2_cols
        
        rest="\t".join(sline[5:(len(sline)+1)])
       # print rest
        new_line="\t".join([new_2_cols, rest])

        print new_line
        if sline[0]==my_N:
            print
            #print "\n" prints out two spaces not one. to get only one space just use print.
            #print "\n"
        
     