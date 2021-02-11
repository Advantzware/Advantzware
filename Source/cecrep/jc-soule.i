/*bpv - Soule Job Card*/
DEF {1} SHARED TEMP-TABLE ttSoule
    FIELD job-no LIKE job-hdr.job-no
    FIELD job-no2 LIKE job-hdr.job-no2
    FIELD frm LIKE job-hdr.frm
    FIELD qty LIKE job-hdr.qty
    FIELD i-no LIKE job-mch.i-no
    FIELD runForm AS LOG
    FIELD blank-no LIKE job-hdr.blank-no 
    FIELD lPrintDieImage AS LOGICAL
    FIELD lPrintCadImage AS LOGICAL
    FIELD lPrintPlateImage AS LOGICAL
    FIELD lPrintFGImage AS LOGICAL .

