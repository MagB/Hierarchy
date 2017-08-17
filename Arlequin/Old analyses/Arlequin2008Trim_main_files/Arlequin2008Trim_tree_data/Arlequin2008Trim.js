
USETEXTLINKS = 1
STARTALLOPEN = 0
WRAPTEXT = 1
PRESERVESTATE = 0
HIGHLIGHT = 1
ICONPATH = 'file:///C:/Arlequin311/'    //change if the gif's folder is a subfolder, for example: 'images/'

foldersTree = gFld("<i>ARLEQUIN RESULTS (Arlequin2008Trim.arp)</i>", "")
insDoc(foldersTree, gLnk("R", "Arlequin log file", "Arlequin_log.txt"))
	aux1 = insFld(foldersTree, gFld("Run of 18/09/11 at 16:08:26", "Arlequin2008Trim.htm#18_09_11at16_08_26"))
	insDoc(aux1, gLnk("R", "Settings", "Arlequin2008Trim.htm#18_09_11at16_08_26_run_information"))
		aux2 = insFld(aux1, gFld("Genetic structure", "Arlequin2008Trim.htm#18_09_11at16_08_26_gen_struct"))
		insDoc(aux2, gLnk("R", "AMOVA", "Arlequin2008Trim.htm#18_09_11at16_08_26_amova"))
		insDoc(aux2, gLnk("R", "FIS per pop", "Arlequin2008Trim.htm#18_09_11at16_08_26_amova_AMOVA_FIS"))
		insDoc(aux2, gLnk("R", "Pairwise distances", "Arlequin2008Trim.htm#18_09_11at16_08_26_pairw_diff"))
		insDoc(aux2, gLnk("R", "Locus by locus AMOVA", "Arlequin2008Trim.htm#18_09_11at16_08_26Loc_by_Loc_AMOVA"))
		insDoc(aux2, gLnk("R", "FIS per pop per locus", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_LBL_AMOVA_FIS"))
		insDoc(aux2, gLnk("R", "Exact tests", "Arlequin2008Trim.htm#18_09_11at16_08_26_exct_tests"))
	insDoc(aux1, gLnk("R", "Genotype assignment", "Arlequin2008Trim.htm#18_09_11at16_08_26_genot_assignment"))
		aux2 = insFld(aux1, gFld("Samples", ""))
		insDoc(aux2, gLnk("R", "QBED1-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp13"))
		insDoc(aux2, gLnk("R", "QBED1-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp12"))
		insDoc(aux2, gLnk("R", "QBED2-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp11"))
		insDoc(aux2, gLnk("R", "QBED2-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp10"))
		insDoc(aux2, gLnk("R", "QLL1-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp9"))
		insDoc(aux2, gLnk("R", "QLL1-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp8"))
		insDoc(aux2, gLnk("R", "QLL3-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp7"))
		insDoc(aux2, gLnk("R", "QLL3-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp6"))
		insDoc(aux2, gLnk("R", "QRL1-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp5"))
		insDoc(aux2, gLnk("R", "QRL1-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp4"))
		insDoc(aux2, gLnk("R", "QRL2-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp2"))
		insDoc(aux2, gLnk("R", "QRL2-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp3"))
		insDoc(aux2, gLnk("R", "QTUR-1", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp1"))
		insDoc(aux2, gLnk("R", "QTUR-2", "Arlequin2008Trim.htm#18_09_11at16_08_26_samp0"))
		aux2 = insFld(aux1, gFld("Within-samples summary", ""))
		insDoc(aux2, gLnk("R", "Basic indices", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_Basic"))
		insDoc(aux2, gLnk("R", "Heterozygosity", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_het"))
		insDoc(aux2, gLnk("R", "Theta(H)", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_thetaH"))
		insDoc(aux2, gLnk("R", "No. of alleles", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_numAll"))
		insDoc(aux2, gLnk("R", "Allelic range", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_allRange"))
		insDoc(aux2, gLnk("R", "Garza-Williamson index", "Arlequin2008Trim.htm#18_09_11at16_08_26_comp_sum_GW"))
