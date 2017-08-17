
USETEXTLINKS = 1
STARTALLOPEN = 0
WRAPTEXT = 1
PRESERVESTATE = 0
HIGHLIGHT = 1
ICONPATH = 'file:///C:/Documents%20and%20Settings/Administrator.ECKERTPC.000/Desktop/winarl35/WinArl35/'    //change if the gif's folder is a subfolder, for example: 'images/'

foldersTree = gFld("<i>ARLEQUIN RESULTS (arlequin2008final.arp)</i>", "")
insDoc(foldersTree, gLnk("R", "Arlequin log file", "Arlequin_log.txt"))
	aux1 = insFld(foldersTree, gFld("Run of 31/08/11 at 14:49:33", "arlequin2008final.xml#31_08_11at14_49_33"))
	insDoc(aux1, gLnk("R", "Settings", "arlequin2008final.xml#31_08_11at14_49_33_run_information"))
		aux2 = insFld(aux1, gFld("Samples", ""))
		insDoc(aux2, gLnk("R", "QBED1-1", "arlequin2008final.xml#31_08_11at14_49_33_group0"))
		insDoc(aux2, gLnk("R", "QBED1-2", "arlequin2008final.xml#31_08_11at14_49_33_group1"))
		insDoc(aux2, gLnk("R", "QBED2-1", "arlequin2008final.xml#31_08_11at14_49_33_group2"))
		insDoc(aux2, gLnk("R", "QBED2-2", "arlequin2008final.xml#31_08_11at14_49_33_group3"))
		insDoc(aux2, gLnk("R", "QLL1-1", "arlequin2008final.xml#31_08_11at14_49_33_group4"))
		insDoc(aux2, gLnk("R", "QLL1-2", "arlequin2008final.xml#31_08_11at14_49_33_group5"))
		insDoc(aux2, gLnk("R", "QLL3-1", "arlequin2008final.xml#31_08_11at14_49_33_group6"))
		insDoc(aux2, gLnk("R", "QLL3-2", "arlequin2008final.xml#31_08_11at14_49_33_group7"))
		insDoc(aux2, gLnk("R", "QRL1-1", "arlequin2008final.xml#31_08_11at14_49_33_group8"))
		insDoc(aux2, gLnk("R", "QRL1-2", "arlequin2008final.xml#31_08_11at14_49_33_group9"))
		insDoc(aux2, gLnk("R", "QRL2-1", "arlequin2008final.xml#31_08_11at14_49_33_group10"))
		insDoc(aux2, gLnk("R", "QRL2-2", "arlequin2008final.xml#31_08_11at14_49_33_group11"))
		insDoc(aux2, gLnk("R", "QTUR-1", "arlequin2008final.xml#31_08_11at14_49_33_group12"))
		insDoc(aux2, gLnk("R", "QTUR-2", "arlequin2008final.xml#31_08_11at14_49_33_group13"))
		aux2 = insFld(aux1, gFld("Within-samples summary", ""))
		insDoc(aux2, gLnk("R", "Basic indices", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_Basic"))
		insDoc(aux2, gLnk("R", "Heterozygosity", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_het"))
		insDoc(aux2, gLnk("R", "Theta(H)", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_thetaH"))
		insDoc(aux2, gLnk("R", "No. of alleles", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_numAll"))
		insDoc(aux2, gLnk("R", "Allelic range", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_allRange"))
		insDoc(aux2, gLnk("R", "Garza-Williamson index", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_GW"))
		insDoc(aux2, gLnk("R", "Garza-Williamson modified index", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_GWN"))
		aux2 = insFld(aux1, gFld("Genetic structure (samp=pop)", "arlequin2008final.xml#31_08_11at14_49_33_pop_gen_struct"))
		insDoc(aux2, gLnk("R", "AMOVA", "arlequin2008final.xml#31_08_11at14_49_33_pop_amova"))
		insDoc(aux2, gLnk("R", "FIS per pop", "arlequin2008final.xml#31_08_11at14_49_33_amova_POP_AMOVA_FIS"))
		insDoc(aux2, gLnk("R", "Pairwise distances", "arlequin2008final.xml#31_08_11at14_49_33_pop_pairw_diff"))
		insDoc(aux2, gLnk("R", "Locus by locus AMOVA", "arlequin2008final.xml#31_08_11at14_49_33pop_Loc_by_Loc_AMOVA"))
		insDoc(aux2, gLnk("R", "F-stat bootstraps", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_bootstrap"))
		insDoc(aux2, gLnk("R", "FIS per pop per locus", "arlequin2008final.xml#31_08_11at14_49_33_comp_sum_LBL_POP_AMOVA_FIS"))
		insDoc(aux2, gLnk("R", "Exact tests", "arlequin2008final.xml#31_08_11at14_49_33_pop_exct_tests"))
