
/*------------------------------------------------------------------------
    File        : MchHrInt.p
    Purpose     : Machine Hour

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{MachHrHdrInt.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine as Character no-undo.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmAction as Character no-undo.
DEFINE INPUT PARAMETER vFormNo as Character no-undo.
DEFINE INPUT PARAMETER vBlankNo as Character no-undo.
DEFINE INPUT PARAMETER prmItem as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachHrHdrInt .

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF vFormNo = ? THEN ASSIGN vFormNo = "".
IF vBlankNo = ? THEN ASSIGN vBlankNo = "".

IF prmItem = "ALL" THEN
   RETURN.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FOR EACH oe-ordl where
    oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.LINE = int(vLine) NO-LOCK,
    EACH job WHERE job.company = oe-ordl.company 
               AND job.job-no = oe-ordl.job-no 
               AND job.job-no2 = oe-ordl.job-no2  NO-LOCK:
        
    CREATE tt_mch_Hr.
    ASSIGN 
        tt_mch_Hr.job-no = job.job-no
        tt_mch_Hr.job-no2 = job.job-no2
        tt_mch_Hr.machine-code = prmItem
        tt_mch_Hr.sheet-no = int(vFormNo ) 
        tt_mch_Hr.blank-no = int(vBlankNo ).
    
END.  /*   FOR EACH oe-ordl */
