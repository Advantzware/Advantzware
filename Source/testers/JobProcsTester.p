
/*------------------------------------------------------------------------
    File        : JobProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for JobProcs.p		

    Author(s)   : BV
    Created     : Thu Feb 13 21:34:29 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdJobProcs AS HANDLE.
DEFINE VARIABLE dCostMat AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostLab AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostVO AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostFO AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostTot AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
FIND FIRST job-hdr NO-LOCK 
    WHERE job-hdr.company EQ '001'
    AND job-hdr.job-no EQ 'W14301'
    AND job-hdr.job-no2 EQ 0
    NO-ERROR.
RUN  GetRecalcJobCostForJobHdr IN hdJobProcs (ROWID(job-hdr), OUTPUT dCostMat, OUTPUT dCostLab, OUTPUT dCostVO, OUTPUT dCostFO).
MESSAGE "Mat: New " dCostMat " vs Old " job-hdr.std-mat-cost SKIP
        "Lab: New " dCostLab " vs Old " job-hdr.std-lab-cost SKIP 
        "VO: New " dCostVO " vs Old " job-hdr.std-var-cost SKIP
        "FO: New " dCostFO " vs Old " job-hdr.std-fix-cost 
VIEW-AS ALERT-BOX.  
