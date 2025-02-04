* load data
clear 
import excel using "C:\Users\vl22683\OneDrive - University of Bristol\Documents\PhD Papers\Paper 2a - Romania PWID\data\ARAS DATA IDU 2013-2022.xlsx", firstrow

distinct CODALT

bysort CODALT (Data): egen id_seq = seq()

tab DiagTB prepostHIVtestandcounseling 

gen hiv_pos = 1 if prepostHIVtestandcounseling == 2
bysort CODALT: egen hiv_pos_max = max(hiv_pos)

gen tb_pos = 1 if DiagTB == "2"
bysort CODALT: egen tb_pos_max = max(tb_pos)

tab vulnerabilityhomelessperson IncentivesforTBtreatment if hiv_pos_max == 1, missing row