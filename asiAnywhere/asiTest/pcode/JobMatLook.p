
/*------------------------------------------------------------------------
    File         : Job Lookup
    Purpose     :  Job lookup

    Syntax      :

    Description : Return a Dataset of job look

    Author(s)   :
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttJobMatLookup NO-UNDO 
        FIELD JOB       AS CHARACTER
        FIELD JOB2      AS INTEGER
        FIELD rm-i-no   AS CHAR
        FIELD rm-name   AS CHAR
        FIELD cust-no   AS CHAR
        FIELD frm       AS INT
        FIELD blank-no  AS INT
        FIELD i-code    AS CHAR
        FIELD jk-st  AS CHAR
        .
        
DEFINE DATASET dsJobMatLookup FOR ttJobMatLookup .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmjob2      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsJobMatLookup.

DEFINE VAR vJob AS CHAR.
DEFINE VAR vEst AS CHAR.


ASSIGN vJob = FILL(" ",6 - LENGTH(TRIM(prmJob))) + TRIM(prmJob)
       vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmjob2      = ? THEN ASSIGN prmjob2      = "0".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FIND FIRST job
        WHERE job.company  EQ prmComp
          AND job.job-no   EQ vJob
          AND (job.job-no2 EQ int(prmjob2) OR prmjob2 EQ "0")
        NO-LOCK NO-ERROR.

    FOR EACH job-mat WHERE job-mat.company = job.company 
        and job-mat.job = job.job 
        and job-mat.job-no = job.job-no 
        and job-mat.job-no2 = job.job-no2 NO-LOCK, 
        FIRST item WHERE item.company = job-mat.company 
        AND item.i-no = job-mat.rm-i-no NO-LOCK BY job-mat.rm-i-no:
        /*FIRST report WHERE report.rec_key = string(rowid(ASI.job-mat)) 
        AND report.term-id eq v-term + USERID("nosweat") NO-LOCK:*/


        create ttJobMatLookup.
        assign                                     
            ttJobMatLookup.JOB      = job-mat.job-no
            ttJobMatLookup.JOB2      = job-mat.job-no2 
            ttJobMatLookup.rm-i-no  = job-mat.rm-i-no
            ttJobMatLookup.rm-name  = item.i-name
            ttJobMatLookup.frm       = job-mat.frm
            ttJobMatLookup.blank-no = job-mat.blank-no
            ttJobMatLookup.i-code   = item.i-code  .                       


    END.	 /* FOR EACH job-hdr */
   
END.  /*ifif prmAction <> "search" */

IF prmAction = "Search" THEN do:
    IF prmField = "Item" then do:
        IF prmCondition = "EQUAL" then do:

           FIND FIRST job
        WHERE job.company  EQ prmComp
          AND job.job-no   EQ vJob
          AND (job.job-no2 EQ int(prmjob2) OR prmjob2 EQ "0")
        NO-LOCK NO-ERROR.

    FOR EACH job-mat WHERE job-mat.company = job.company 
        and job-mat.job = job.job 
        AND job-mat.rm-i-no = prmText
        and job-mat.job-no = job.job-no 
        and job-mat.job-no2 = job.job-no2 NO-LOCK, 
        FIRST item WHERE item.company = job-mat.company 
        AND item.i-no = job-mat.rm-i-no NO-LOCK BY job-mat.rm-i-no:
        /*FIRST report WHERE report.rec_key = string(rowid(ASI.job-mat)) 
        AND report.term-id eq v-term + USERID("nosweat") NO-LOCK:*/


        create ttJobMatLookup.
        assign                                     
            ttJobMatLookup.JOB      = job-mat.job-no
            ttJobMatLookup.JOB2      = job-mat.job-no2 
            ttJobMatLookup.rm-i-no  = job-mat.rm-i-no
            ttJobMatLookup.rm-name  = item.i-name
            ttJobMatLookup.frm       = job-mat.frm
            ttJobMatLookup.blank-no = job-mat.blank-no
            ttJobMatLookup.i-code   = item.i-code  .   
               
            END.   /*FOR EACH job-hdr*/
        END.   /* IF prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
            
            FIND FIRST job
        WHERE job.company  EQ prmComp
          AND job.job-no   EQ vJob
          AND (job.job-no2 EQ int(prmjob2) OR prmjob2 EQ "0")
        NO-LOCK NO-ERROR.

    FOR EACH job-mat WHERE job-mat.company = job.company 
        and job-mat.job = job.job 
        AND job-mat.rm-i-no BEGINS prmText
        and job-mat.job-no = job.job-no 
        and job-mat.job-no2 = job.job-no2 NO-LOCK, 
        FIRST item WHERE item.company = job-mat.company 
        AND item.i-no = job-mat.rm-i-no NO-LOCK BY job-mat.rm-i-no:
        /*FIRST report WHERE report.rec_key = string(rowid(ASI.job-mat)) 
        AND report.term-id eq v-term + USERID("nosweat") NO-LOCK:*/


        create ttJobMatLookup.
        assign                                     
            ttJobMatLookup.JOB      = job-mat.job-no
            ttJobMatLookup.JOB2      = job-mat.job-no2 
            ttJobMatLookup.rm-i-no  = job-mat.rm-i-no
            ttJobMatLookup.rm-name  = item.i-name
            ttJobMatLookup.frm       = job-mat.frm
            ttJobMatLookup.blank-no = job-mat.blank-no
            ttJobMatLookup.i-code   = item.i-code  .   
               

               
      END.
        END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
                                       
END. /* IF prmAction = search then do: */
