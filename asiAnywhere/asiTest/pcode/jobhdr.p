
/*------------------------------------------------------------------------
    File        : jobhdr.p
    Purpose     : ItemJob

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{jobhdr.i}


DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as CHAR no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsjobhdr.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR vitem  AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

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
     ASSIGN vitem = oe-ordl.i-no .
  FIND FIRST job WHERE job.company = prmComp 
                   AND job.job-no = prmOrderNum  NO-LOCK NO-ERROR.
  FOR  EACH job-hdr where job-hdr.company eq job.company
                              /*and job-hdr.job-no2    eq job.job-no2*/
                              and job-hdr.job-no  eq job.job-no
                              AND job-hdr.i-no  EQ vitem 
                             NO-LOCK :
        create ttjobhdr.
        assign 
            ttjobhdr.Sheet = job-hdr.frm
            ttjobhdr.BlankNum = job-hdr.blank-no
            ttjobhdr.Customer = job-hdr.cust-no
            ttjobhdr.Item = job-hdr.i-no
            ttjobhdr.Qty = job-hdr.qty
            ttjobhdr.Inch = job-hdr.sq-in
            ttjobhdr.Order  = job-hdr.ord-no
            ttjobhdr.Mat = job-hdr.std-mat-cost
            ttjobhdr.DL  = job-hdr.std-lab-cost
            ttjobhdr.Var = job-hdr.std-var-cost
            ttjobhdr.Fixed = job-hdr.std-fix-cost
            .
END. /*FOR EACH oe-ordl no-lock*/
