
/*------------------------------------------------------------------------
    File        : MoldJobProcs.p
    Purpose     : create job from estimate 

    Syntax      :

    Description : Job Builder Procedure.

    Author(s)   : Sewa Singh
    Created     : Mon Nov 23 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO. 
DEFINE INPUT PARAMETER ipdtDueDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcKeyItem AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opriJob AS ROWID NO-UNDO.

DEFINE VARIABLE riEb      AS ROWID   NO-UNDO.
DEFINE VARIABLE li        LIKE job.job.
DEFINE VARIABLE iJobNo2   AS INTEGER NO-UNDO.
DEFINE VARIABLE v-bld-job LIKE job.job-no NO-UNDO.
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-job FOR job.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST eb NO-LOCK
    WHERE ROWID(eb) EQ ipriRowid NO-ERROR.
FIND FIRST est NO-LOCK
    WHERE est.company EQ eb.company
    AND est.est-no EQ eb.est-no NO-ERROR .
     
RUN pGetInternalJob (
    INPUT  eb.company,
    OUTPUT li
    ).    
     
CREATE bf-job.    
 
ASSIGN
    bf-job.job        = li
    bf-job.company    = eb.company
    bf-job.loc        = est.loc
    bf-job.due-date   = ipdtDueDate 
    bf-job.stat       = "P"
    bf-job.est-no     = est.est-no 
    bf-job.csrUser_id = est.csrUser_id .
   
RUN jc/job-no.p (INPUT-OUTPUT v-bld-job, 
    INPUT-OUTPUT iJobNo2,
    INPUT "", 
    INPUT "").
                       
IF v-bld-job EQ "" THEN 
    RUN pGetJobNo(BUFFER bf-job, INPUT bf-job.company, INPUT bf-job.est-no, OUTPUT v-bld-job, OUTPUT iJobNo2).
                       
ASSIGN
    bf-job.job-no  = v-bld-job
    bf-job.job-no2 = iJobNo2.  
       
IF bf-job.job-no2 GT 0 THEN 
    bf-job.orderType = "R".                                 
ELSE 
    bf-job.orderType = "O".               

RUN jc/BuildJob.p (ROWID(bf-job), 0, OUTPUT lError, OUTPUT cMessage).                       
//RUN jc/jc-calc.p (RECID(bf-job), YES) NO-ERROR.

RUN pUpdateFGItemQty(BUFFER bf-job).

RUN pUpdateKeyItem(BUFFER bf-job, INPUT ipcKeyItem).

opriJob = ROWID(bf-job).


/* **********************  Internal Procedures  *********************** */
PROCEDURE pGetInternalJob PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJob     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    opiJob = 1.
        
    FIND LAST bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany 
        USE-INDEX job NO-ERROR.
    
    FIND LAST bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        USE-INDEX job NO-ERROR.
    
    IF bf-job-hdr.job GT bf-job.job THEN 
        opiJob = bf-job-hdr.job + 1.
    
    IF bf-job.job GE bf-job-hdr.job THEN
        opiJob = bf-job.job + 1.

END PROCEDURE.

PROCEDURE pGetJobNo PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimate  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobNo     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2     AS INTEGER   NO-UNDO.
           
    DEFINE VARIABLE cJobCreat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBldJob   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li        AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-job FOR job.
        
    RUN pGetControlValue(INPUT ipcCompany, OUTPUT cJobCreat).
    
   
    ASSIGN
        cBldJob = " " + ipbf-job.est-no
        li      = 0.  
            
    /* Use last 5 digits of estimate# */
    IF LENGTH(TRIM(cBldJob)) GT 5 THEN 
        cBldJob = " " + SUBSTRING(TRIM(cBldJob), 2).
             
    IF cJobCreat NE "" THEN
        cBldJob = SUBSTR(cJobCreat,1,1) + TRIM(cBldJob).

    ASSIGN
        cBldJob = FILL(" ",6 - LENGTH(TRIM(cBldJob))) + TRIM(cBldJob).              
                      
    FOR EACH bf-job FIELDS(job-no2) NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ cBldJob
        AND ROWID(bf-job)  NE ROWID(ipbf-job)
        USE-INDEX job-no
        BY bf-job.job-no2 DESCENDING:

        li = bf-job.job-no2 + 1. 
        LEAVE.
    END.       
    ASSIGN
        opcJobNo  = cBldJob
        opiJobNo2 = li.
       
END PROCEDURE.

PROCEDURE pGetControlValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets all required NK1 settings for posting run
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcValue    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.   
    
    RUN sys/ref/nk1look.p (ipcCompany, "JOBCREAT", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN opcValue = cReturn.   
    
       
END PROCEDURE.
                              
PROCEDURE pUpdateKeyItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  update job-hdr item 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT PARAMETER ipcKeyItem  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    FIND FIRST bf-job-hdr EXCLUSIVE-LOCK
         WHERE bf-job-hdr.company EQ ipbf-job.company
         AND bf-job-hdr.job-no EQ ipbf-job.job-no
         AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2
         AND bf-job-hdr.i-no EQ ipcKeyItem NO-ERROR.
    IF AVAIL bf-job-hdr THEN
    DO:
       ASSIGN bf-job-hdr.keyItem = YES.        
    END.
       
END PROCEDURE.  

PROCEDURE pUpdateFGItemQty PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  update job-hdr item 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
        
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    FOR EACH bf-job-hdr NO-LOCK
         WHERE bf-job-hdr.company EQ ipbf-job.company
         AND bf-job-hdr.job-no EQ ipbf-job.job-no
         AND bf-job-hdr.job-no2 EQ ipbf-job.job-no2:
         
         RUN util/upditmfg.p (
                    INPUT ROWID(bf-job-hdr),
                    INPUT 1
                    ).       
    END.   
END PROCEDURE.   
