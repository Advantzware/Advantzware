/* -------------------------------------------------- sa/sa-mtdsl.i 08/97 JLF */
/* S/A Month To Date Sales Report Program - S/A Module                        */
/* -------------------------------------------------------------------------- */

assign
 tt-report.key-01 = if sort-by-inv then string({1}.inv-no,"999999")
		            else tt-report.key-09

 tt-report.key-02 = string({2},"99/99/9999")

 tt-report.key-02 = substr(tt-report.key-02,7,4) +
		            substr(tt-report.key-02,1,2) +
		            substr(tt-report.key-02,4,2)

 tt-report.key-03 = string({1}.inv-no,"999999"). 
 tt-report.key-04 = string({1}.line,"9999") .

/* End ---------------------------------- Copr. 1997  Advanced Software, Inc. */
