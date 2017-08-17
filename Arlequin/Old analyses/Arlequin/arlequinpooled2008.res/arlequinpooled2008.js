
USETEXTLINKS = 1
STARTALLOPEN = 0
WRAPTEXT = 1
PRESERVESTATE = 0
HIGHLIGHT = 1
ICONPATH = 'file:///C:/Arlequin311/'    //change if the gif's folder is a subfolder, for example: 'images/'

foldersTree = gFld("<i>ARLEQUIN RESULTS (arlequinpooled2008.arp)</i>", "")
insDoc(foldersTree, gLnk("R", "Arlequin log file", "Arlequin_log.txt"))
	aux1 = insFld(foldersTree, gFld("Run of 15/09/11 at 19:19:24", "arlequinpooled2008.htm#15_09_11at19_19_24"))
	insDoc(aux1, gLnk("R", "Settings", "arlequinpooled2008.htm#15_09_11at19_19_24_run_information"))
		aux2 = insFld(aux1, gFld("Genetic structure", "arlequinpooled2008.htm#15_09_11at19_19_24_gen_struct"))
		insDoc(aux2, gLnk("R", "Pairwise distances", "arlequinpooled2008.htm#15_09_11at19_19_24_pairw_diff"))
		aux2 = insFld(aux1, gFld("Samples", ""))
		insDoc(aux2, gLnk("R", "Everything", "arlequinpooled2008.htm#15_09_11at19_19_24_samp0"))
		aux2 = insFld(aux1, gFld("Within-samples summary", ""))
		insDoc(aux2, gLnk("R", "Basic indices", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_Basic"))
		insDoc(aux2, gLnk("R", "Heterozygosity", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_het"))
		insDoc(aux2, gLnk("R", "Theta(H)", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_thetaH"))
		insDoc(aux2, gLnk("R", "No. of alleles", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_numAll"))
		insDoc(aux2, gLnk("R", "Allelic range", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_allRange"))
		insDoc(aux2, gLnk("R", "Garza-Williamson index", "arlequinpooled2008.htm#15_09_11at19_19_24_comp_sum_GW"))
