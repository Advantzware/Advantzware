
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
{ViewJob.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as CHAR no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewJob.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vJob AS INT NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

                                                       FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp 
     AND oe-ordl.ord-no = int(prmOrderNum)
     AND oe-ordl.LINE = int(vLine) NO-LOCK NO-ERROR .
 IF AVAIL oe-ordl THEN
     FIND FIRST job-hdr where job-hdr.company eq prmComp
                              and job-hdr.job-no  eq (prmOrderNum)
                              AND job-hdr.i-no  EQ oe-ordl.i-no NO-LOCK NO-ERROR.
 IF AVAIL job-hdr THEN
     ASSIGN
     vJob = job-hdr.job-no2 .

    FOR EACH job WHERE job.company = prmComp 
                   AND job.job-no = prmOrderNum 
                  AND (job.job-no2 EQ vJob OR vJob EQ 0 )  NO-LOCK:
        create ttViewJob.
        assign
            ttViewJob.job = job.job-no         
            ttViewJob.job2 = job.job-no2
            ttViewJob.est = job.est-no
            ttViewJob.vUser    = job.user-id
            ttViewJob.stats = job.stat
            ttViewJob.Startdate = job.start-date
            ttViewJob.Enddate  = job.close-date
            ttViewJob.Duedate = job.due-date
            . 

    END. /* FOR EACH job*/

/*

find job-hdr where job-hdr.company eq oe-ordl.company
                              and job-hdr.i-no    eq oe-ordl.i-no
                              and job-hdr.job-no  eq oe-ordl.job-no
                              use-index i-no no-lock no-error.
If avail job-hdr then
assign 
    ttViewJob.Sheet = job-hdr.frm
    ttViewJob.BlankNum = job-hdr.blank-no
    ttViewJob.Customer = job-hdr.cust-no
    ttViewJob.Item = job-hdr.i-no
    ttViewJob.Qty = job-hdr.qty
    ttViewJob.Inch = job-hdr.sq-in
    ttViewJob.Order  = job-hdr.ord-no
    ttViewJob.Mat = job-hdr.std-mat-cost
    ttViewJob.DL  = job-hdr.std-lab-cost
    ttViewJob.Var = job-hdr.std-var-cost
    ttViewJob.Fixed = job-hdr.std-fix-cost
    
    .    
Find First job where job.company eq job-hdr.company
    and job.job-no  eq job-hdr.job-no no-lock.
If avail job then
   
assign
    ttViewJob.job = job.job-no         
    ttViewJob.job2 = job.job-no2
    ttViewJob.est= job.est-no
    ttViewJob.stats = job.stat
    ttViewJob.Startdate = job.start-date
    ttViewJob.Enddate  = job.close-date
    ttViewJob.Duedate = job.due-date
    
    . 
                            
END. /*FOR EACH oe-ordl no-lock*/
      
        

 
  */
