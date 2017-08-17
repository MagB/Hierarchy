import sys

#my_intstruct_files=open("/Users/Maggie/Documents/Adriana/data/Allpops1Sep4_out_Aug252016.txt")
#my_intstruct_files=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/instruct_files/10chains.txt")

my_intstruct_files= open(sys.argv[1], 'r')
my_K= sys.argv[2]
my_N=sys.argv[3]

#my_K= "4"
#my_N="1391"
I_chose_my_k=False

chain_num=0

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
    
    #if your pop codes change just change the letter in the following line. or add another or statement in the first set of brackets
    if (sline[1][0]=="B" or sline[1][0]=="C" or sline[1][0]=="O") and "(" in sline[2] and I_chose_my_k==True:

        next_line="Yes"
        #print line
       # print len(sline)
        sline[3]=str(int(sline[3])+1)
        
        new_2_cols="\t".join([sline[0],sline[0],sline[2], sline[3]])
        new_2_cols=new_2_cols+ " "+ sline[4]+" " 
        #print new_2_cols
        
        rest="\t".join(sline[5:(len(sline)+1)])
        if "#" in rest:
           # print "YES"
           # print rest
            new_sum=0
            rest2=rest.split("\t")
            for item in rest2:
                
                if "#" not in item:
                    new_sum+=float(item)
                else:
                    item_index= rest2.index(item)
            rest2[item_index]=round((1-new_sum),3)
            rest="\t".join(map(str, rest2))
            print_error=str("Error found in Instruct file. Found # at") +str(my_K)+ str(chain_num) +"\n"
            sys.stderr.write(print_error)
                
                    
        all="\t".join([new_2_cols, rest])

        print all
        if sline[0]==my_N:
            chain_num+=1
            print
            #print "\n" prints out two spaces not one. to get only one space just use print.
            #print "\n"
        
        
     