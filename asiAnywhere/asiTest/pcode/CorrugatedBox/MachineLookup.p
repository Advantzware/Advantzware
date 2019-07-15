/*------------------------------------------------------------------------
    File      : MachineLookup
    Purpose   :  Corrugated Machine Lookup
    Syntax    :

    Description : Return a Dataset of MachineLookup

    Author(s)   : 
    Created     : 21 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMachineLookup NO-UNDO 
        FIELD vCode         AS character
        FIELD vCodeDesc     AS CHARACTER
        FIELD vDept     AS CHARACTER
        FIELD bocal     AS CHAR
      .
    
DEFINE DATASET dsMachineLookup FOR ttMachineLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBoard     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmXgrain    AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMachineLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".

DEF VAR prmComp AS CHAR NO-UNDO.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xest for est.
DEF var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.

{sys/inc/var.i new shared}

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp 
    locode = "MAIN" .


if prmAction <> "search" then do:
    FOR EACH mach WHERE  mach.company = prmComp NO-LOCK BY mach.m-code:
        IF AVAIL mach THEN DO:
             create ttMachineLookup.
             assign  

                 ttMachineLookup.vCode         = mach.m-code
                 ttMachineLookup.vCodeDesc     = mach.m-dscr 
                 ttMachineLookup.vDept         = mach.dept[1]
                 .
  

         END.	 /* IF AVAIL mach */
    END. /* for each mach */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "code" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH mach WHERE  mach.company = prmComp AND mach.m-code = prmText  NO-LOCK:
        IF AVAIL mach THEN DO:
             create ttMachineLookup.
             assign  

                 ttMachineLookup.vCode         = mach.m-code
                 ttMachineLookup.vCodeDesc     = mach.m-dscr 
                 ttMachineLookup.vDept         = mach.dept[1]
                 .
             
         END.	 /* IF AVAIL mach */
    END. /* for each mach */
       
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH mach WHERE  mach.company = prmComp AND mach.m-code BEGINS prmText  NO-LOCK:
        IF AVAIL mach THEN DO:
             create ttMachineLookup.
             assign  

                 ttMachineLookup.vCode         = mach.m-code
                 ttMachineLookup.vCodeDesc     = mach.m-dscr 
                 ttMachineLookup.vDept         = mach.dept[1]
                 .
            
              
         END.	 /* IF AVAIL mach */
    END. /* for each mach */
     
END. /*IF prmCondition = "BEGIN" */
END.  /*IF prmField = code */

IF prmField = "dept" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH mach WHERE  mach.company = prmComp AND mach.dept[1] = prmText  NO-LOCK:
        IF AVAIL mach THEN DO:
             create ttMachineLookup.
             assign  

                 ttMachineLookup.vCode         = mach.m-code
                 ttMachineLookup.vCodeDesc     = mach.m-dscr 
                 ttMachineLookup.vDept         = mach.dept[1]
                 .

             
         END.	 /* IF AVAIL mach */
    END. /* for each mach */
       
    END. /*if prmCondition = EQUAL*/

IF prmCondition = "BEGIN" then do:
    FOR EACH mach WHERE  mach.company = prmComp AND mach.dept[1] BEGINS prmText  NO-LOCK:
        IF AVAIL mach THEN DO:
             create ttMachineLookup.
             assign  

                 ttMachineLookup.vCode         = mach.m-code
                 ttMachineLookup.vCodeDesc     = mach.m-dscr 
                 ttMachineLookup.vDept         = mach.dept[1]
                 .
            

              
         END.	 /* IF AVAIL mach */
    END. /* for each mach */
     
END. /*IF prmCondition = "BEGIN" */

END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


