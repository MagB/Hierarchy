import sys

my_intstruct_files=open("/Users/Maggie/Downloads/AllpopsJUNE20out.txt")
#my_intstruct_files= open(sys.argv[1], 'r')
#my_K= sys.argv[2]
I_chose_my_k=False
my_K="4"

new_cols="hi"
next_line_pops=False
counter=10
j=0
first=1

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

    if sline[0]=="1" and next_line_pops==True:
        try:
            pop_num=int(sline[-1])
        except:
            pop_num=sline[0]
            
    if sline[0]=="Given":
        next_line_pops=True
        j+=1
        counter=0
        first+=1

    if sline[0][-1]==":" and sline[1][0]=="0":

        next_line_pops=False

        sline[0]=str(int(sline[0][0:-1])+1)
        sline[0]=sline[0]+":"
        new_cols="\t".join(sline)

        #print counter, first
        if counter==0 and j>1 and next_line_pops==False:
            #using the "\n" actually adds two lines. To just have one line only use printg
            print 
        next_line_pops=True
        print new_cols
        counter+=1