import sys

#thisc script will either run on a single file or multiple files. The first column of the output of this script indicates which instruct file was processed. 
#The user must proovide an input name and output name if desired.

#e.g.
#python exctact_info_from_instruct_output.py 10chains.txt > instruct_summary_stats.csv
#note if the instruct file is not housed in the same folder as the python script then (assuming th user is working in the instruct folder) simply type the full path name for the python program and let 'er fly.

first=1
if len(sys.argv)==2:
    #print "yes"
    #my_file=open("/Users/Maggie/Documents/Adriana/MS38outDec18steves.txt", 'r')
    my_file=open(sys.argv[1], 'r')
    
    
    #my_file=open(sys.argv[1], 'r')
    print "Run_ID", ",", "K", ",", "Chain", ",", "Posterior_mean",",", "Posterior_var",",", "DIC", ", ", "Gelman"
    for line in my_file:
        sline=line.split()
        if "Data File" in line:
            run_ID=sline[2]        
            
        if "The current K" in line:
            new_k=sline[4]
            if first==1:
                current_k=sline[4]
                first=6
    
            
            #IF we have reached a new k then print out parameters for that k
            if new_k!=current_k:
                for i in range(0,len(chain)):
                    print run_ID, ",", current_k, ",", chain[i], ",", post_mean_list[i],",", post_var_list[i],",", DIC_list[i], ", ", gelman
            
            chain=[]
            post_mean_list=[]
            post_var_list=[]
            DIC_list=[]
            
            current_k=sline[4]
            
            
            
            
            
        if "Chain#" in line:
    
            current_chain=line[6:].strip("\x00:\r\r\n")
            
            chain.append(current_chain)
    
        if "Posterior Mean" in line:
            post_mean=sline[3]
            post_mean_list.append(post_mean)
        if "Posterior Variance" in line:
            post_var=sline[3]
            post_var_list.append(post_var)
        if "Deviance information criterion of this model" in line:
            DIC=sline[8].strip(".")
            DIC_list.append(DIC)
            
            
        if "Gelman-Rubin" in line:
            gelman=sline[9].strip(".")
            DIC_list.append(DIC)


        
#IF WE HAVE MULTIPLE FILES THEN RUN ALL OF THEM            
else:
    file_list=sys.argv
    
    for i in file_list[1:]:   
        my_file=open(i, 'r')
        name_output=str(i.strip(".txt") + "_output.csv")
        #print name_output
        
        my_output_file=open(name_output, 'w')
        chain=[]
        post_mean_list=[]
        post_var_list=[]
        DIC_list=[]
        first=1
        
        #reset my counter before running the main script on a new file
        first==1
        
        for line in my_file:
            sline=line.split()
            if "Data File" in line:
                run_ID=sline[2]        
                
            if "The current K" in line:
                new_k=sline[4]
                if first==1:
                    current_k=sline[4]
                    first=6
        
                
                #IF we have reached a new k then print out parameters for that k
                if new_k!=current_k:
                    for i in range(0,len(chain)):
                        print run_ID, ",", current_k, ",", chain[i], ",", post_mean_list[i],",", post_var_list[i],",", DIC_list[i], ", ", gelman
                
                chain=[]
                post_mean_list=[]
                post_var_list=[]
                DIC_list=[]
                
                current_k=sline[4]
                
                
                
                
                
            if "Chain#" in line:
        
                current_chain=line[6:].strip("\x00:\r\r\n")
                
                chain.append(current_chain)
        
            if "Posterior Mean" in line:
                post_mean=sline[3]
                post_mean_list.append(post_mean)
            if "Posterior Variance" in line:
                post_var=sline[3]
                post_var_list.append(post_var)
            if "Deviance information criterion of this model" in line:
                DIC=sline[8].strip(".")
                DIC_list.append(DIC)
                
                
            if "Gelman-Rubin" in line:
                gelman=sline[9].strip(".")
                
  
        

for i in range(0,len(chain)):
    print run_ID, ",", current_k, ",", chain[i], ",", post_mean_list[i],",", post_var_list[i],",", DIC_list[i], ", ", gelman