import sys

file=open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/Arlequin_mpb/Fst_pvalues.csv", 'r')

N=14
my_line=[]
count_sig_p=0
count_dunn_sidak=0


total_comparisons=(N*(N-1)/2)
print total_comparisons

dunn_sidak_p=(1-(0.5)**(1/float(total_comparisons)))
print dunn_sidak_p
pop_list=["QBED1_1", "QBED1_2","QBED2_1","QBED2_2","QLL1_1","QLL1_2","QLL3_1","QLL3_2","QRL1_1","QRL1_2","QRL2_1","QRL2_2","QTUR_1","QTUR_2"]
popcount=0

pop_dict={}
for i in pop_list:
    pop_dict[i]={}
    for k in pop_list:
        pop_dict[i][k]=200
        
count_pop=0
for line in file:
    line=line.strip("\r\n")
    sline=line.split(",")
    
    my_line=[]
    my_corr_p=[]
    
    count_column=0
    for i in sline[1:]:
        count_column+=1
        try:
            my_p=float(i.split("+")[0])

            
        except:
            continue
        my_line.append(my_p)
        
        if my_p <0.05:
            count_sig_p+=1
        if my_p < dunn_sidak_p:
            count_dunn_sidak+=1
            my_corr_p.append("*")
        else:
            my_corr_p.append("NA")
            
    #print sline[0], " ".join(map(str,my_line))
    
    print pop_list[popcount], " ". join(map(str, my_corr_p))

    count_pop=0
    for i in my_corr_p:
        pop_dict[pop_list[popcount]][pop_list[count_pop]]=i
        count_pop+=1
    popcount+=1

print "number of sig pvals",count_sig_p
print "number of sig pvals after Dunn-sidak",count_dunn_sidak
print "proportion of sig pvals", count_dunn_sidak/float(total_comparisons)


pop_sig_counts={}
pop_sig_counts2={}

for i in pop_list:
    pop_sig_counts[i]=0
    pop_sig_counts2[i]=0
    
for i in pop_list:
    pop_dict[i] 

    for j in pop_list:
        pop_sig_counts2[i]+=1
        
        if pop_dict[i][j]=="*":
            pop_sig_counts[i]+=1
            
       # print i, j, pop_dict[i][j]


#how many comparisons for each pop are significant?
print""
print "plot", "num_comparisons", "num_sig_comparisons"
for i in pop_list:
    print i, pop_sig_counts2[i], pop_sig_counts[i]
    