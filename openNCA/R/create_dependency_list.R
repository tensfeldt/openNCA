### 2019-09-01/TGT/
### valid_models
###  M1   - applies to M1 both SD and SS models
###  M1SD - applies to only M1 SD model and dosingtype
###  M1SS - applies to only M1 SS model and dosingtype
### predecessors
###  names of parameters that the parameter requires in order to be computed properly
### 2019-09-18/TGT/ Added uclass entries
###
create_dependency_list <- function() {
    m1   <- "M1"
    m2   <- "M2"
    m3   <- "M3"
    m4   <- "M4"
    m1sd <- "M1SD"
    m1ss <- "M1SS"
    m2sd <- "M2SD"
    m2ss <- "M2SS"
    m3sd <- "M3SD"
    m3ss <- "M3SS"
    m4sd <- "M4SD"
    m4ss <- "M4SS"

    uclass_time      <- "TIMEU"     # Unit Class  1: Time
    uclass_amount    <- "AMOUNTU"   # Unit Class  2: Amount
    uclass_dose      <- "DOSEU"     # Unit Class  3: Dose Amount
    uclass_volume    <- "VOLUMEU"   # Unit Class  4: Volume
    uclass_amtvol    <- "CONCU"     # Unit Class  5: Amount/Volume (Concentration)
    uclass_slope     <- "KELU"      # Unit Class  6: 1/Time (KEL/slope)
    uclass_cl        <- "CLU"       # Unit Class  7: Volume/Time (Clearance)
    uclass_auc       <- "AUCU"      # Unit Class  8: Amount.Time/Volume (AUC)
    uclass_aumc      <- "AUMCU"     # Unit Class  9: Amount.Time^2/Volume (Amount.Time.Time/Volume)
    uclass_aucnorm   <- "AUCNORMU"  # Unit Class 10: (Amount.Time/Volume)/Amount (Normalized AUC)
    uclass_aurc      <- "AURCU"     # Unit Class 11: (Volume.Amount)/Volume (AURC)
    uclass_concnorm  <- "CONCNORMU" # Unit Class 12: (Amount/Volume)/Amount (Normalized Concentration)
    uclass_rate      <- "RATEU"     # Unit Class 13: Amount/Time
    uclass_volwnorm  <- "VOLUMEWU"  # Unit Class 14: Volume/Body weight (Volume normalized by body size)
    uclass_clwnorm   <- "CLWU"      # Unit Class 15: Volume/Time/Body Weight (Clearance normalized by body size)
    uclass_ratio     <- "RATIOU"    # Unit Class 16: Ratio (No Units)
    uclass_percent   <- "PERCENTU"  # Unit Class 17: Percentage
    uclass_none      <- "NONEU"     # Unit Class 18: No Units
    
    dependency_list <- list()
    dependency_list[["AE"]] <- list(callfun=c("ae"), regex="^AE(.[0-9]+?.[0-9]+?)*?$", unit_class=c(uclass_amount), valid_models=c(m4), display_list_models=c(m4sd, m4ss), predecessors=c())
    dependency_list[["AEPCT"]] <- list(callfun=c("aepct"), regex="^AEPCT$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(m4sd, m4ss), predecessors=c("AE"))
    dependency_list[["AET"]] <- list(callfun=c("aet"), regex="^(AET|AMT(.[0-9]+?.[0-9]+?)*?)$", unit_class=c(uclass_amount), valid_models=c(m4), display_list_models=c(m4sd, m4ss), predecessors=c())
    dependency_list[["AETAUi"]] <- list(callfun=c("aet"), regex="^AETAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_amount), valid_models=c(m4), display_list_models=c(m4ss), predecessors=c())
    dependency_list[["AETAUPTi"]] <- list(callfun=c("aetpct"), regex="^AETAUPT(i{1}?|[0-9]+?)$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(m4ss), predecessors=c("AETAUi", "TAUi"))
    dependency_list[["AETPCT"]] <- list(callfun=c("aetpct"), regex="^AETPCT$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(m4sd, m4ss), predecessors=c("AET"))
    dependency_list[["AUCALL"]] <- list(callfun=c("auc_all"), regex="^AUCALL$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("TMAX"))
    dependency_list[["AUCALLDN"]] <- list(callfun=c("auc_dn"), regex="^AUCALLDN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCALL"))
    dependency_list[["AUCDN"]] <- list(callfun=c("auc_dn"), regex="^AUCDN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCALL"))
    dependency_list[["AUCINFO"]] <- list(callfun=c("auc_inf_o"), regex="^AUCINFO$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("CLAST", "AUCLAST", "KEL", "SPANRATIO"))
    dependency_list[["AUCINFOC"]] <- list(callfun=c("auc_inf_oc"), regex="^AUCINFOC$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFO", "C0", "KEL"))
    dependency_list[["AUCINFODN"]] <- list(callfun=c("auc_dn"), regex="^AUCINFODN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFO"))
    dependency_list[["AUCINFOi"]] <- list(callfun=c("auc_inf_o"), regex="^AUCINFO(i{1}?|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CLASTi", "AUCLASTi", "KEL", "SPANRATIO", "TAUi", "TOLDi"))
    dependency_list[["AUCINFP"]] <- list(callfun=c("auc_inf_p"), regex="^AUCINFP$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m2sd, m3sd), predecessors=c("TLAST", "CEST", "KEL", "KELC0", "AUCLAST", "SPANRATIO"))
    dependency_list[["AUCINFPC"]] <- list(callfun=c("auc_inf_pc"), regex="^AUCINFPC$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFP", "C0", "KEL"))
    dependency_list[["AUCINFPDN"]] <- list(callfun=c("auc_dn"), regex="^AUCINFPDN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFP"))
    dependency_list[["AUCINFPi"]] <- list(callfun=c("auc_inf_p"), regex="^AUCINFP(i{1}?|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TLAST", "CEST", "KEL", "KELC0", "AUCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUCLAST"]] <- list(callfun=c("auc_last"), regex="^AUCLAST$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m2sd, m3sd), predecessors=c("TLAST", "TMAX"))
    dependency_list[["AUCLASTi"]] <- list(callfun=c("auc_last"), regex="^AUCLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TLASTi", "TMAXi", "TAUi", "TOLDi"))
    dependency_list[["AUCLASTC"]] <- list(callfun=c("auc_lastc"), regex="^AUCLASTC$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCLAST", "C0", "KEL", "TLAST"))
    dependency_list[["AUCLASTCi"]] <- list(callfun=c("auc_lastc"), regex="^AUCLASTC(i{1}?|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCLASTi", "C0", "TLAST", "KEL", "TAUi", "TOLDi"))
    dependency_list[["AUCLASTDN"]] <- list(callfun=c("auc_dn"), regex="^AUCLASTDN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCLAST"))
    dependency_list[["AUCLASTDNi"]] <- list(callfun=c("auclasti_dn"), regex="^AUCLASTDN(i{1}?|[0-9]+?)$", unit_class=c(uclass_aucnorm), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUCT"]] <- list(callfun=c("auc_t1_t2"), regex="^AUC(T|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("TMAX", "TMAXi"))
    dependency_list[["AUCT1_T2"]] <- list(callfun=c("auc_t1_t2"), regex="^AUC(T1|[0-9]+?)_(T2|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("TMAX", "TMAXi", "AUCT"))
    dependency_list[["AUCTAUDNi"]] <- list(callfun=c("auc_dn"), regex="^AUCTAUDN(i{1}?|[0-9]+?)$", unit_class=c(uclass_aucnorm), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCTAUi", "TAUi", "TOLDi"))
    dependency_list[["AUCTAU"]] <- list(callfun=c("auc_all"), regex="^AUCTAU$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TMAX", "TAUi", "TOLDi"))
    dependency_list[["AUCTAUi"]] <- list(callfun=c("auc_all"), regex="^AUCTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_auc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TMAX", "TAUi", "TOLDi"))
    dependency_list[["AUCTDN"]] <- list(callfun=c("auc_dn"), regex="^AUCTDN$", unit_class=c(uclass_aucnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCT"))
    dependency_list[["AUCXPCTO"]] <- list(callfun=c(), regex="^AUCXPCTO$", unit_class=c(uclass_percent), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFO", "AUCLAST"))
    dependency_list[["AUCXPCTOi"]] <- list(callfun=c(), regex="^AUCXPCTO(i{1}?|[0-9]+?)$", unit_class=c(uclass_percent), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCINFOi", "AUCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUCXPCTP"]] <- list(callfun=c(), regex="^AUCXPCTP$", unit_class=c(uclass_percent), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m2sd, m3sd), predecessors=c("AUCINFP", "AUCLAST"))
    dependency_list[["AUCXPCTPi"]] <- list(callfun=c(), regex="^AUCXPCTP(i{1}?|[0-9]+?)$", unit_class=c(uclass_percent), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("AUCINFPi", "AUCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUCXBPCTO"]] <- list(callfun=c(), regex="^AUCXBPCTO$", unit_class=c(uclass_percent), valid_models=c(m2sd, m2ss), display_list_models=c(m2sd, m2ss), predecessors=c("AUCINFO", "AUCT"))
    dependency_list[["AUCXBPCTP"]] <- list(callfun=c(), regex="^AUCXBPCTP$", unit_class=c(uclass_percent), valid_models=c(m2sd, m2ss), display_list_models=c(m2sd, m2ss), predecessors=c("AUCINFP", "AUCT"))
    dependency_list[["AUMCINFO"]] <- list(callfun=c(), regex="^AUMCINFO$", unit_class=c(uclass_aumc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("KEL"))
    dependency_list[["AUMCINFOi"]] <- list(callfun=c(), regex="^AUMCINFO(i{1}?|[0-9]+?)$", unit_class=c(uclass_aumc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CLASTi", "AUCLASTi", "KEL", "TAUi", "TOLDi"))
    dependency_list[["AUMCINFP"]] <- list(callfun=c(), regex="^AUMCINFP$", unit_class=c(uclass_aumc), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("KEL"))
    dependency_list[["AUMCINFPi"]] <- list(callfun=c(), regex="^AUMCINFP(i{1}?|[0-9]+?)$", unit_class=c(uclass_aumc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CEST", "AUCLASTi", "KEL", "TLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUMCLAST"]] <- list(callfun=c(), regex="^AUMCLAST$", unit_class=c(uclass_aumc), valid_models=c(m1sd, m2sd, m3sd), display_list_models=c(), predecessors=c("TMAX"))
    dependency_list[["AUMCLASTi"]] <- list(callfun=c("aumclasti"), regex="^AUMCLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_aumc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TMAXi", "TAUi", "TOLDi"))
    dependency_list[["AUMCTAUi"]] <- list(callfun=c(), regex="^AUMCTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_aumc), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TMAXi", "TAUi", "TOLDi"))
    dependency_list[["AUMCXPTO"]] <- list(callfun=c(), regex="^AUMCXPTO$", unit_class=c(uclass_percent), valid_models=c(m1sd, m2sd, m3sd), display_list_models=c(), predecessors=c("AUMCINFO", "AUMCLAST"))
    dependency_list[["AUMCXPTOi"]] <- list(callfun=c(), regex="^AUMCXPTO(i{1}?|[0-9]+?)$", unit_class=c(uclass_percent), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUMCINFOi", "AUMCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AUMCXPTP"]] <- list(callfun=c(), regex="^AUMCXPTP$", unit_class=c(uclass_percent), valid_models=c(m1sd, m2sd, m3sd), display_list_models=c(), predecessors=c("AUMCINFP", "AUMCLAST"))
    dependency_list[["AUMCXPTPi"]] <- list(callfun=c(), regex="^AUMCXPTP(i{1}?|[0-9]+?)$", unit_class=c(uclass_percent), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUMCINFPi", "AUMCLASTi", "TAUi", "TOLDi"))
    dependency_list[["AURCALL"]] <- list(callfun=c("auc_all"), regex="^AURCALL$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["AURCINFO"]] <- list(callfun=c("auc_inf_o"), regex="^AURCINFO$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["AURCINFP"]] <- list(callfun=c("auc_inf_p"), regex="^AURCINFP$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["AURCLAST"]] <- list(callfun=c("auc_last"), regex="^AURCLAST$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["AURCT"]] <- list(callfun=c("auc_t1_t2"), regex="^AURC(T|[0-9]+?)$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c("TMAXRATE"))
    dependency_list[["AURCT1_T2"]] <- list(callfun=c("auc_t1_t2"), regex="^AURC(T1|[0-9]+?)_(T2|[0-9]+?)$", unit_class=c(uclass_aurc), valid_models=c(m4), display_list_models=c(), predecessors=c("TMAXRATE"))
    dependency_list[["AURCXPCTO"]] <- list(callfun=c(), regex="^AURCXPCTO$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["AURCXPCTP"]] <- list(callfun=c(), regex="^AURCXPCTP$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["C0"]] <- list(callfun=c(), regex="^C0$", unit_class=c(uclass_amtvol), valid_models=c(m2), display_list_models=c(m2sd), predecessors=c())
    dependency_list[["CAVi"]] <- list(callfun=c(), regex="^CAV(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("AUCTAUi", "TAUi", "TOLDi"))
### 2019-09-01/TGT/ CENDINF not currently defined in computation engine codebase
    dependency_list[["CENDINF"]] <- list(callfun=c(), regex="^CENDINF$", unit_class=c(uclass_amtvol), valid_models=c(m3), display_list_models=c(m3sd), predecessors=c())
### 2019-09-01/TGT/ CENDINFi not currently defined in computation engine codebase
    dependency_list[["CENDINFi"]] <- list(callfun=c(), regex="^CENDINF(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m3), display_list_models=c(m3ss), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ CENDINFDN not currently defined in computation engine codebase
    dependency_list[["CENDINFDN"]] <- list(callfun=c(), regex="^CENDINFDN$", unit_class=c(uclass_concnorm), valid_models=c(m3), display_list_models=c(), predecessors=c("CENDINF", "CENDINFi"))
    dependency_list[["CEST"]] <- list(callfun=c(), regex="^CEST$", unit_class=c(uclass_amtvol), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("TLAST", "KEL", "KELC0"))
    dependency_list[["CLAST"]] <- list(callfun=c(), regex="^CLAST$", unit_class=c(uclass_amtvol), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c())
    dependency_list[["CLASTi"]] <- list(callfun=c(), regex="^CLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["CLFO"]] <- list(callfun=c(), regex="^CLFO$", unit_class=c(uclass_cl), valid_models=c(m1sd), display_list_models=c(), predecessors=c("AUCINFO", "DOSEC"))
    dependency_list[["CLFOW"]] <- list(callfun=c(), regex="^CLFOW$", unit_class=c(uclass_clwnorm), valid_models=c(m1sd), display_list_models=c(), predecessors=c("CLFO"))
    dependency_list[["CLFP"]] <- list(callfun=c(), regex="^CLFP$", unit_class=c(uclass_cl), valid_models=c(m1sd), display_list_models=c(m1sd), predecessors=c("AUCINFP", "DOSEC"))
    dependency_list[["CLFPW"]] <- list(callfun=c(), regex="^CLFPW$", unit_class=c(uclass_clwnorm), valid_models=c(m1sd), display_list_models=c(), predecessors=c("CLFP"))
    dependency_list[["CLFTAUi"]] <- list(callfun=c(), regex="^CLFTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_cl), valid_models=c(m1ss), display_list_models=c(m1ss), predecessors=c("AUCTAUi", "DOSEC", "TAUi", "TOLDi"))
    dependency_list[["CLFTAUWi"]] <- list(callfun=c(), regex="^CCLTAUW(i{1}?|[0-9]+?)$", unit_class=c(uclass_clwnorm), valid_models=c(m1ss), display_list_models=c(), predecessors=c("CLFTAUi", "DOSEC", "TAUi", "TOLDi"))
    dependency_list[["CLO"]] <- list(callfun=c(), regex="^CLO$", unit_class=c(uclass_cl), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("AUCINFO", "DOSEC"))
    dependency_list[["CLOW"]] <- list(callfun=c(), regex="^CLOW$", unit_class=c(uclass_clwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("CLO"))
    dependency_list[["CLP"]] <- list(callfun=c(), regex="^CLP$", unit_class=c(uclass_cl), valid_models=c(m2sd, m3sd), display_list_models=c(m2sd, m3sd), predecessors=c("AUCINFP", "DOSEC"))
    dependency_list[["CLPW"]] <- list(callfun=c(), regex="^CLPW$", unit_class=c(uclass_clwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("CLP"))
    dependency_list[["CLR"]] <- list(callfun=c(), regex="^CLR$", unit_class=c(uclass_cl), valid_models=c(m4sd), display_list_models=c(), predecessors=c("AE", "AUCINFP"))
    dependency_list[["CLRT"]] <- list(callfun=c(), regex="^CLRT$", unit_class=c(uclass_cl), valid_models=c(m4), display_list_models=c(), predecessors=c("AET", "AUCT"))
    dependency_list[["CLRTAU"]] <- list(callfun=c(), regex="^CLRTAU$", unit_class=c(uclass_cl), valid_models=c(m4), display_list_models=c(), predecessors=c("AETAUi", "AUCTAUi"))
    dependency_list[["CLTAUi"]] <- list(callfun=c(), regex="^CLTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_cl), valid_models=c(m2sd, m3sd), display_list_models=c(m3ss), predecessors=c("AUCTAUi"))
    dependency_list[["CLTAUWi"]] <- list(callfun=c(), regex="^CLTAUW(i{1}?|[0-9]+?)$", unit_class=c(uclass_clwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("CLTAUi"))
    dependency_list[["CMAX"]] <- list(callfun=c(), regex="^CMAX$", unit_class=c(uclass_amtvol), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m3sd, m3ss), predecessors=c())
    dependency_list[["CMAXi"]] <- list(callfun=c(), regex="^CMAX(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["CMAXC"]] <- list(callfun=c(), regex="^CMAXC$", unit_class=c(uclass_amtvol), valid_models=c(m2sd), display_list_models=c(), predecessors=c("CMAX"))
    dependency_list[["CMAXCi"]] <- list(callfun=c(), regex="^CMAXC(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m2ss), display_list_models=c(), predecessors=c("CMAXi", "TAUi", "TOLDi"))
    dependency_list[["CMAXDN"]] <- list(callfun=c(), regex="^CMAXDN$", unit_class=c(uclass_concnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("CMAX"))
    dependency_list[["CMAXDNi"]] <- list(callfun=c(), regex="^CMAXDN(i{1}?|[0-9]+?)$", unit_class=c(uclass_concnorm), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CMAXi", "TAUi", "TOLDi"))
    dependency_list[["CMIN"]] <- list(callfun=c(), regex="^CMIN$", unit_class=c(uclass_amtvol), valid_models=c(m1, m2, m3), display_list_models=c(m1ss), predecessors=c())
    dependency_list[["CMINi"]] <- list(callfun=c(), regex="^CMIN(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["CMINDN"]] <- list(callfun=c(), regex="^CMINDN$", unit_class=c(uclass_concnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("CMIN"))
    dependency_list[["CMINDNi"]] <- list(callfun=c(), regex="^CMINDN(i{1}?|[0-9]+?)$", unit_class=c(uclass_concnorm), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("CMINi"))
### 2019-09-01/TGT/ CTROUGHi not currently defined in computation engine codebase
    dependency_list[["CTROUGHi"]] <- list(callfun=c(), regex="^CTROUGH(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ CTROUGHENDi not currently defined in computation engine codebase
    dependency_list[["CTROUGHENDi"]] <- list(callfun=c(), regex="^CTROUGHEND(i{1}?|[0-9]+?)$", unit_class=c(uclass_amtvol), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["DIi"]] <- list(callfun=c(), regex="^DI(i{1}?|[0-9]+?)$", unit_class=c(uclass_none), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["DOFi"]] <- list(callfun=c(), regex="^DOF(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m3), display_list_models=c(), predecessors=c())
    dependency_list[["DOSE"]] <- list(callfun=c(), regex="^DOSE$", unit_class=c(uclass_dose), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c())
    dependency_list[["DOSEi"]] <- list(callfun=c(), regex="^DOSE(i{1}?|[0-9]+?)$", unit_class=c(uclass_dose), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ DOSEC not currently defined in computation engine codebase
    dependency_list[["DOSEC"]] <- list(callfun=c(), regex="^DOSEC$", unit_class=c(), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c())
    dependency_list[["DOSECi"]] <- list(callfun=c(), regex="^DOSEC(i{1}?|[0-9]+?)$", unit_class=c(), valid_models=c(m1ss, m2ss, m3ss, m4ss), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ F not currently defined in computation engine codebase
###    dependency_list[["F"]] <- list(callfun=c(), regex="^F$", unit_class=c(uclass_ratio), valid_models=c(m1sd, m2sd, m3sd), display_list_models=c(), predecessors=c("AUCINFP"))
### 2019-09-01/TGT/ FREL not currently defined in computation engine codebase
###    dependency_list[["FREL"]] <- list(callfun=c(), regex="^FREL$", unit_class=c(uclass_ratio), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCINFP"))
### 2019-09-01/TGT/ FRELLAST not currently defined in computation engine codebase
###    dependency_list[["FRELLAST"]] <- list(callfun=c(), regex="^FRELLAST$", unit_class=c(uclass_ratio), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("AUCLAST"))
### 2019-09-01/TGT/ FRELLASTi not currently defined in computation engine codebase
###    dependency_list[["FRELLASTi"]] <- list(callfun=c(), regex="^FRELLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCLASTi", "TAUi", "TOLDi"))
### 2019-09-01/TGT/ FA not currently defined in computation engine codebase
###    dependency_list[["FA"]] <- list(callfun=c(), regex="^FA$", unit_class=c(uclass_percent), valid_models=c(m4), display_list_models=c(), predecessors=c("AE", "AEPCT"))
### 2019-09-01/TGT/ FTAUi not currently defined in computation engine codebase
###    dependency_list[["FTAUi"]] <- list(callfun=c(), regex="^FTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCTAUi", "AUCINFPi", "AUCINFP", "TAUi", "TOLDi"))
    dependency_list[["KEL"]] <- list(callfun=c(), regex="^KEL$", unit_class=c(uclass_slope), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c())
    dependency_list[["KELC0"]] <- list(callfun=c(), regex="^KELC0$", unit_class=c(uclass_amtvol), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c())
    dependency_list[["KELNOPT"]] <- list(callfun=c(), regex="^KELNOPT$", unit_class=c(uclass_none), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c())
    dependency_list[["KELR"]] <- list(callfun=c(), regex="^KELR$", unit_class=c(uclass_none), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("KEL"))
    dependency_list[["KELRSQ"]] <- list(callfun=c(), regex="^KELRSQ$", unit_class=c(uclass_none), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c("KEL"))
    dependency_list[["KELRSQA"]] <- list(callfun=c(), regex="^KELRSQA$", unit_class=c(uclass_none), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("KEL"))
    dependency_list[["KELTMHI"]] <- list(callfun=c(), regex="^KELTMLO$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c())
    dependency_list[["KELTMLO"]] <- list(callfun=c(), regex="^KELTMHI$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c())
    dependency_list[["LASTTIME"]] <- list(callfun=c(), regex="^LASTTIME$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c())
    dependency_list[["LASTTIMEi"]] <- list(callfun=c(), regex="^LASTTIME(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["MAXRATE"]] <- list(callfun=c(), regex="^MAXRATE$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c("RATE"))
    dependency_list[["MAXRATEi"]] <- list(callfun=c(), regex="^MAXRATE(i{1}?|[0-9]+?)$", unit_class=c(uclass_rate), valid_models=c(m4ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi", "RATEi"))
    dependency_list[["MIDPT"]] <- list(callfun=c(), regex="^MIDPT$", unit_class=c(uclass_time), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ MIDPTA not currently defined in computation engine codebase
    dependency_list[["MIDPTA"]] <- list(callfun=c(), regex="^(MIDPT)([0-9]*?|A|N)$", unit_class=c(uclass_time), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ MIDPTN not currently defined in computation engine codebase
    dependency_list[["MIDPTN"]] <- list(callfun=c(), regex="^(MIDPT)([0-9]*?|A|N)$", unit_class=c(uclass_time), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ MIDPTLAST not currently defined in computation engine codebase
    dependency_list[["MIDPTLAST"]] <- list(callfun=c(), regex="^MIDPTLAST$", unit_class=c(uclass_time), valid_models=c(m4), display_list_models=c(), predecessors=c("TLAST"))
    dependency_list[["MIDPTLASTi"]] <- list(callfun=c(), regex="^MIDPTLASTi$", unit_class=c(uclass_time), valid_models=c(m4ss), display_list_models=c(), predecessors=c("TLASTi", "TAUi", "TOLDi"))
    dependency_list[["MRTEVIFO"]] <- list(callfun=c(), regex="^MRTEVIFO$", unit_class=c(uclass_time), valid_models=c(m1sd), display_list_models=c(), predecessors=c("AUCINFO", "AUMCINFO"))
    dependency_list[["MRTEVIFOi"]] <- list(callfun=c(), regex="^MRTEVIFO(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss), display_list_models=c(), predecessors=c("AUCINFOi", "AUCTAUi", "AUMCTAUi", "TAUi", "TOLDi"))
    dependency_list[["MRTEVIFP"]] <- list(callfun=c(), regex="^MRTEVIFP$", unit_class=c(uclass_time), valid_models=c(m1sd), display_list_models=c(), predecessors=c("AUCINFP", "AUMCINFP"))
    dependency_list[["MRTEVIFPi"]] <- list(callfun=c(), regex="^MRTEVIFP(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss), display_list_models=c(), predecessors=c("AUCINFPi", "AUCTAUi", "AUMCTAUi", "TAUi", "TOLDi"))
    dependency_list[["MRTIVIFO"]] <- list(callfun=c(), regex="^MRTIVIFO$", unit_class=c(uclass_time), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("AUCINFO", "AUMCINFO"))
    dependency_list[["MRTIVIFOi"]] <- list(callfun=c(), regex="^MRTIVIFO(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["MRTIVIFP"]] <- list(callfun=c(), regex="^MRTIVIFP$", unit_class=c(uclass_time), valid_models=c(m2sd, m3sd), display_list_models=c(m3sd, m3sd), predecessors=c("AUCINFP", "AUMCINFP"))
    dependency_list[["MRTIVIFPi"]] <- list(callfun=c(), regex="^MRTIVIFP(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m2ss, m3ss), display_list_models=c(m3ss, m3ss), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["MRTLAST"]] <- list(callfun=c(), regex="^MRTLAST$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m3sd, m3ss), predecessors=c("AUCLAST", "AUMCLAST"))
    dependency_list[["MRTLASTi"]] <- list(callfun=c(), regex="^MRTLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("AUCLASTi", "AUMCLASTi", "TAUi", "TOLDi"))
    dependency_list[["PTF"]] <- list(callfun=c(), regex="^PTF$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss), predecessors=c("CMAX", "CMIN", "CAV", "TAUi", "TOLDi"))
    dependency_list[["PTFi"]] <- list(callfun=c(), regex="^PTF(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss), predecessors=c("CMAXi", "CMINi", "CAVi", "TAUi", "TOLDi"))
    dependency_list[["PTR"]] <- list(callfun=c(), regex="^PTR$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss), predecessors=c("CMAX", "CMIN", "TAUi", "TOLDi"))
    dependency_list[["PTRi"]] <- list(callfun=c(), regex="^PTR(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss), predecessors=c("CMAXi", "CMINi", "TAUi", "TOLDi"))
### 2019-09-01/TGT/ PTROUGHRi not currently defined in computation engine codebase
    dependency_list[["PTROUGHRi"]] <- list(callfun=c(), regex="^PTROUGHR(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CMAXi", "CTROUGHi"))
### 2019-09-01/TGT/ PTROUGHRENDi not currently defined in computation engine codebase
    dependency_list[["PTROUGHRENDi"]] <- list(callfun=c(), regex="^PTROUGHREND(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("CMAXi", "CTROUGHENDi"))
### 2019-09-01/TGT/ RACi not currently defined in computation engine codebase
###    dependency_list[["RACi"]] <- list(callfun=c(), regex="^RAC(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ RACCMAXi not currently defined in computation engine codebase
###    dependency_list[["RACCMAXi"]] <- list(callfun=c(), regex="^RACCMAX(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ RACCMINi not currently defined in computation engine codebase
###    dependency_list[["RACCMINi"]] <- list(callfun=c(), regex="^RACCMIN(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["RATELAST"]] <- list(callfun=c(), regex="^RATELAST$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["RATELASTi"]] <- list(callfun=c(), regex="^RATELAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["RATE"]] <- list(callfun=c(), regex="^RATE$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["RATEi"]] <- list(callfun=c(), regex="^RATE(i{1}?|[0-9]+?)$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ RATEAi not currently defined in computation engine codebase
    dependency_list[["RATEA"]] <- list(callfun=c(), regex="^RATEA$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["RATEAi"]] <- list(callfun=c(), regex="^RATEA(i{1}?|[0-9]+?)$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ RATENi not currently defined in computation engine codebase
    dependency_list[["RATEN"]] <- list(callfun=c(), regex="^RATEN$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["RATENi"]] <- list(callfun=c(), regex="^RATEN(i{1}?|[0-9]+?)$", unit_class=c(uclass_rate), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ RSSi not currently defined in computation engine codebase
###    dependency_list[["RSSi"]] <- list(callfun=c(), regex="^RSS(i{1}?|[0-9]+?)$", unit_class=c(uclass_ratio), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["TAU"]] <- list(callfun=c(), regex="^TAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss, m4ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["TAUi"]] <- list(callfun=c(), regex="^TAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss, m4ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ TENDINF not currently defined in computation engine codebase
    dependency_list[["TENDINF"]] <- list(callfun=c(), regex="^TENDINF$", unit_class=c(uclass_time), valid_models=c(m3), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ TENDINFi not currently defined in computation engine codebase
    dependency_list[["TENDINFi"]] <- list(callfun=c(), regex="^TENDINF(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["THALF"]] <- list(callfun=c(), regex="^THALF$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c("KEL"))
    dependency_list[["THALFF"]] <- list(callfun=c(), regex="^THALFF$", unit_class=c(uclass_ratio), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c("THALF", "KELTMHI", "KELTMLO"))
    dependency_list[["TLAG"]] <- list(callfun=c(), regex="^TLAG$", unit_class=c(uclass_time), valid_models=c(m1, m4), display_list_models=c(), predecessors=c())
    dependency_list[["TLAST"]] <- list(callfun=c(), regex="^TLAST$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m1ss, m2sd, m3sd, m3ss), predecessors=c())
    dependency_list[["TLASTi"]] <- list(callfun=c(), regex="^TLAST(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["TMAX"]] <- list(callfun=c(), regex="^TMAX$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(m1sd, m3sd), predecessors=c())
    dependency_list[["TMAXi"]] <- list(callfun=c(), regex="^TMAX(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(m1ss, m3ss), predecessors=c("TAUi", "TOLDi"))
### 2019-09-01/TGT/ TMAXRATE not currently defined in computation engine codebase
    dependency_list[["TMAXRATE"]] <- list(callfun=c(), regex="^TMAXRATE$", unit_class=c(uclass_time), valid_models=c(m4), display_list_models=c(), predecessors=c())
    dependency_list[["TMAXRATEi"]] <- list(callfun=c(), regex="^TMAXRATEi", unit_class=c(uclass_time), valid_models=c(m4ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["TMIN"]] <- list(callfun=c(), regex="^TMIN$", unit_class=c(uclass_time), valid_models=c(m1, m2, m3), display_list_models=c(), predecessors=c())
    dependency_list[["TMINi"]] <- list(callfun=c(), regex="^TMIN(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["TOLD"]] <- list(callfun=c(), regex="^TOLD(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss, m4ss), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ TOLDi not currently defined in computation engine codebase
    dependency_list[["TOLDi"]] <- list(callfun=c(), regex="^TOLD(i{1}?|[0-9]+?)$", unit_class=c(uclass_time), valid_models=c(m1ss, m2ss, m3ss, m4ss), display_list_models=c(), predecessors=c("TAUi", "TOLDi"))
    dependency_list[["V0"]] <- list(callfun=c(), regex="^V0$", unit_class=c(uclass_volume), valid_models=c(m2), display_list_models=c(m2sd), predecessors=c("C0"))
    dependency_list[["VOLSUM"]] <- list(callfun=c(), regex="^VOLSUM$", unit_class=c(uclass_volume), valid_models=c(m4), display_list_models=c(), predecessors=c())
### 2019-09-01/TGT/ VSSP not currently defined in computation engine codebase
    dependency_list[["VSSP"]] <- list(callfun=c("vss"), regex="^VSSP$", unit_class=c(uclass_volume), valid_models=c(m2sd, m3sd), display_list_models=c(m3sd), predecessors=c("CLP", "MRTIVIFP"))
### 2019-09-01/TGT/ VSSPi not currently defined in computation engine codebase
    dependency_list[["VSSPi"]] <- list(callfun=c("vss"), regex="^VSSP(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m2ss, m3ss), display_list_models=c(m3ss), predecessors=c("CLP", "CLTAUi", "MRTIVIFPi", "TAUi", "TOLDi"))
    dependency_list[["VSSPW"]] <- list(callfun=c(), regex="^VSSPW$", unit_class=c(uclass_volume), valid_models=c(m2sd, m3sd), display_list_models=c(m3sd), predecessors=c("VSSP"))
    dependency_list[["VSSPWi"]] <- list(callfun=c(), regex="^VSSPW(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m2ss, m3ss), display_list_models=c(m3ss), predecessors=c("VSSPi"))
### 2019-09-01/TGT/ VSSO not currently defined in computation engine codebase
    dependency_list[["VSSO"]] <- list(callfun=c("vss"), regex="^VSSO$", unit_class=c(uclass_volume), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("CLO", "MRTIVIFO"))
    dependency_list[["VSSOW"]] <- list(callfun=c(), regex="^VSSOW$", unit_class=c(uclass_volwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("VSSO"))
### 2019-09-01/TGT/ VSSOi not currently defined in computation engine codebase
    dependency_list[["VSSOi"]] <- list(callfun=c("vss"), regex="^VSSO(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m2ss, m3ss), display_list_models=c(), predecessors=c("CLO", "CLTAUi", "MRTIVIFOi", "TAUi", "TOLDi"))
    dependency_list[["VSSOWi"]] <- list(callfun=c(), regex="^VSSOW(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m2ss, m3ss), display_list_models=c(), predecessors=c("VSSOi"))
    dependency_list[["VZFO"]] <- list(callfun=c(), regex="^VZFO$", unit_class=c(uclass_volume), valid_models=c(m1sd), display_list_models=c(), predecessors=c("KEL", "AUCINFO", "AUCINFOi", "DOSEC"))
    dependency_list[["VZFOW"]] <- list(callfun=c(), regex="^VZFOW$", unit_class=c(uclass_volwnorm), valid_models=c(m1sd), display_list_models=c(), predecessors=c("VZFO"))
    dependency_list[["VZFP"]] <- list(callfun=c(), regex="^VZFP$", unit_class=c(uclass_volume), valid_models=c(m1sd), display_list_models=c(m1sd, m1ss), predecessors=c("KEL", "AUCINFP", "AUCINFPi", "DOSEC"))
    dependency_list[["VZFPW"]] <- list(callfun=c(), regex="^VZFPW$", unit_class=c(uclass_volwnorm), valid_models=c(m1sd), display_list_models=c(), predecessors=c("VZFP"))
    dependency_list[["VZFTAUi"]] <- list(callfun=c(), regex="^VZFTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m1ss), display_list_models=c(), predecessors=c("KEL", "AUCTAUi", "DOSEC", "TAUi", "TOLDi"))
    dependency_list[["VZFTAUWi"]] <- list(callfun=c(), regex="^VZFTAUW(i{1}?|[0-9]+?)$", unit_class=c(uclass_volwnorm), valid_models=c(m1ss), display_list_models=c(), predecessors=c("VZFTAUi", "TAUi", "TOLDi"))
    dependency_list[["VZO"]] <- list(callfun=c(), regex="^VZO$", unit_class=c(uclass_volume), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("KEL", "AUCINFO", "DOSEC"))
    dependency_list[["VZOW"]] <- list(callfun=c(), regex="^VZOW$", unit_class=c(uclass_volwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("VZO"))
    dependency_list[["VZP"]] <- list(callfun=c(), regex="^VZP$", unit_class=c(uclass_volume), valid_models=c(m2sd, m3sd), display_list_models=c(m2sd, m3sd), predecessors=c("KEL", "AUCINFP", "DOSEC"))
    dependency_list[["VZPW"]] <- list(callfun=c(), regex="^VZPW$", unit_class=c(uclass_volwnorm), valid_models=c(m2sd, m3sd), display_list_models=c(), predecessors=c("VZP"))
### 2019-09-01/TGT/ VZTAUi not currently defined in computation engine codebase
    dependency_list[["VZTAUi"]] <- list(callfun=c(), regex="^VZTAU(i{1}?|[0-9]+?)$", unit_class=c(uclass_volume), valid_models=c(m2ss, m3ss), display_list_models=c(), predecessors=c("KEL", "AUCTAUi", "DOSEC", "TAUi", "TOLDi"))
### 2019-09-01/TGT/ VZTAUWi not currently defined in computation engine codebase
    dependency_list[["VZTAUWi"]] <- list(callfun=c(), regex="^VZTAUW(i{1}?|[0-9]+?)$", unit_class=c(uclass_volwnorm), valid_models=c(m2ss, m3ss), display_list_models=c(), predecessors=c("VZTAUi", "TAUi", "TOLDi"))
    dependency_list[["FLGACCEPTPREDOSE"]] <- list(callfun=c(), regex="^FLGACCEPTPREDOSE$", unit_class=c(), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c("CMAX", "CMAXi"))
    dependency_list[["FLGACCEPTTMAX"]] <- list(callfun=c(), regex="^FLGACCEPTTMAX$", unit_class=c(), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c("TMAX", "TMAXi"))
    dependency_list[["FLGACCEPTKEL"]] <- list(callfun=c(), regex="^FLGACCEPTKEL$", unit_class=c(), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c("KEL", "KELNOPT", "AUCXPCTO", "AUCXPCTP"))
    dependency_list[["FLGACCEPTTAU"]] <- list(callfun=c(), regex="^FLGACCEPTTAU$", unit_class=c(), valid_models=c(m1, m2, m3, m4), display_list_models=c(), predecessors=c("LASTTIME"))
    
    
    return(dependency_list)
}
