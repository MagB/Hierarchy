file= open("/Users/Maggie/Dropbox/AndyWong/1. Hierarchical Structure/Maggie_Hierarchy/data/Hierstruct_microsat_by_plot.csv", 'r')


print str("#Project file created by Arlequin")
print "[Profile]"
print
print str('Title="9 nuclear microsatellites in 14 populations across Europe"')

print str('NbSamples=14')
print('DataType=MICROSAT  # - {DNA, RFLP, MICROSAT, STANDARD, FREQUENCY}')
print str('GenotypicData=1  # - {0, 1}')
print str('GameticPhase=0  # - {0, 1}')
print str('LocusSeparator=WHITESPACE  # - {TAB, WHITESPACE, NONE}')
print str("MissingData='?'")
print
print "[Data]"
print
print "[[Samples]]"





line1=1
new_pop="QBED1"
for line in file:
    if line1==1:
        line1=2
        
        continue
    line1+=1
    line=line.rstrip("\n")
    sline=line.split(",")
    ind=sline[0]
    ind=ind[1:-1]
    
    current_pop=sline[0][0:8]

    if new_pop!=current_pop:
        if line1>3:
            print "}"
            print
        #print
        if current_pop[-1]=="-":
            current_pop=current_pop[0:-1]
    
        
        sample_name=str("SampleName=") + current_pop +"\""
        
        print(str(sample_name))
        print(str("SampleSize= 20"))
        print(str("SampleData= {"))


    if "*" in ind:
        last="st"
        ind=ind[0:-2]
        ind="".join([ind, last])
    
    genos=sline[4:13]
    if "NA" in genos:
        genos2_index= [i for i, x in enumerate(genos) if "NA" in x]
        
        for y in genos2_index:
            genos[y]="??:??"
            
    allel1=[x.split(":")[0][1:] for x in genos ]
    allel2=[x.split(":")[1][:-1] for x in genos ]
    
    print ind, 1, " ".join(allel1)
    newline=" ".join([ind, "1", " ".join(allel1)])

    
    print "  ", " ".join(allel2)

    
    
    new_pop=sline[0][0:8]


print "}"
print
print
print
