
/*------------------------------------------------------------------------
    File        : ItemJob.p
    Purpose     : ItemJob

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewJob1.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJobNum as Character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewJob1.


DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust = ? THEN ASSIGN prmCust = "".
IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmJobNum = ? THEN ASSIGN prmJobNum = "".
FOR EACH oe-ordl NO-LOCK:        
FOR EACH job where job.company eq oe-ordl.company
               and job.job-no  eq prmJobNum no-lock.
    create ttViewJob1.
    assign
        ttViewJob1.job = job.job-no         
        ttViewJob1.job2 = job.job-no2
        ttViewJob1.est= job.est-no
        ttViewJob1.stats = job.stat
        ttViewJob1.Startdate = job.start-date
        ttViewJob1.Enddate  = job.close-date
        ttViewJob1.Duedate = job.due-date
        .
         
END.  /*FOR EACH job where job.company*/
END. /*FOR EACH oe-ordl no-lock*/
      
        

 

