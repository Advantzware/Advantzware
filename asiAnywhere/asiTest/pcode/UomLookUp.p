


/*------------------------------------------------------------------------
    File         : UomLookUp.p
    Purpose     :  Uom lookup

    Syntax      :

    Description : Return a Dataset of all Item Inquiry

    Author(s)   : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUom NO-UNDO 
    FIELD uom AS CHARACTER
    FIELD dscr AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dsuom FOR ttuom .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsuom.

IF prmText      = ? THEN ASSIGN prmText      = "".
def var uom-list as cha init "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
   FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 NO-LOCK :
       create ttuom.
       assign           
           ttuom.uom = uom.uom 
           ttuom.dscr = uom.dscr.
   END.
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    if prmField = "uom" then do:

    if prmCondition = "EQUAL" then do:
        FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 AND uom.uom = prmText NO-LOCK :
       create ttuom.
       assign           
           ttuom.uom = uom.uom 
           ttuom.dscr = uom.dscr.
        END.
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
       FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 AND uom.uom begins prmText NO-LOCK :
           create ttuom.
           assign                                                                  
               ttuom.uom    = uom.uom
               ttuom.dscr = uom.dscr.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "uom" then do:*/
END.   /*IF prmAction = "search" then do:*/
