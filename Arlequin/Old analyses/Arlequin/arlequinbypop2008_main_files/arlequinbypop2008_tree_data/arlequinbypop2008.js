
USETEXTLINKS = 1
STARTALLOPEN = 0
WRAPTEXT = 1
PRESERVESTATE = 0
HIGHLIGHT = 1
ICONPATH = 'file:///C:/Arlequin311/'    //change if the gif's folder is a subfolder, for example: 'images/'

foldersTree = gFld("<i>ARLEQUIN RESULTS (arlequinbypop2008.arp)</i>", "")
insDoc(foldersTree, gLnk("R", "Arlequin log file", "Arlequin_log.txt"))
	aux1 = insFld(foldersTree, gFld("Run of 15/09/11 at 21:52:51", "arlequinbypop2008.htm#15_09_11at21_52_51"))
	insDoc(aux1, gLnk("R", "Settings", "arlequinbypop2008.htm#15_09_11at21_52_51_run_information"))
		aux2 = insFld(aux1, gFld("Genetic structure", "arlequinbypop2008.htm#15_09_11at21_52_51_gen_struct"))
		insDoc(aux2, gLnk("R", "Pairwise distances", "arlequinbypop2008.htm#15_09_11at21_52_51_pairw_diff"))
		aux2 = insFld(aux1, gFld("Samples", ""))
		insDoc(aux2, gLnk("R", "QBED1", "arlequinbypop2008.htm#15_09_11at21_52_51_samp0"))
		insDoc(aux2, gLnk("R", "QBED2", "arlequinbypop2008.htm#15_09_11at21_52_51_samp1"))
		insDoc(aux2, gLnk("R", "QLL1", "arlequinbypop2008.htm#15_09_11at21_52_51_samp2"))
		insDoc(aux2, gLnk("R", "QLL3", "arlequinbypop2008.htm#15_09_11at21_52_51_samp3"))
		insDoc(aux2, gLnk("R", "QRL1", "arlequinbypop2008.htm#15_09_11at21_52_51_samp4"))
		insDoc(aux2, gLnk("R", "QRL2", "arlequinbypop2008.htm#15_09_11at21_52_51_samp5"))
		insDoc(aux2, gLnk("R", "QTUR", "arlequinbypop2008.htm#15_09_11at21_52_51_samp6"))
		aux2 = insFld(aux1, gFld("Within-samples summary", ""))
		insDoc(aux2, gLnk("R", "Basic indices", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_Basic"))
		insDoc(aux2, gLnk("R", "Heterozygosity", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_het"))
		insDoc(aux2, gLnk("R", "Theta(H)", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_thetaH"))
		insDoc(aux2, gLnk("R", "No. of alleles", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_numAll"))
		insDoc(aux2, gLnk("R", "Allelic range", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_allRange"))
		insDoc(aux2, gLnk("R", "Garza-Williamson index", "arlequinbypop2008.htm#15_09_11at21_52_51_comp_sum_GW"))
