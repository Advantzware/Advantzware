


/*------------------------------------------------------------------------
    File         : CorUomLook.p
    Purpose     :  Uom lookup

    Syntax      :

    Description : Return a Dataset of all Item Inquiry

    Author(s)   : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorUomLook NO-UNDO 
    FIELD vUom AS CHARACTER
    FIELD vDscr AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dsCorUomLook FOR ttCorUomLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorUomLook.
    

IF prmText      = ? THEN ASSIGN prmText      = "".
def var uom-list as cha init ["M,EA,L,CS,C"] no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
   
    FIND FIRST ITEM WHERE ITEM.company = prmComp AND
        ITEM.i-no = prmItem NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN DO:
        RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
    END.
    ELSE DO:
        RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
    END.
    
   FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 NO-LOCK :
       create ttCorUomLook.
       assign           
           ttCorUomLook.vUom = uom.uom 
           ttCorUomLook.vDscr = uom.dscr.
   END.
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    if prmField = "uom" then do:

    if prmCondition = "EQUAL" then do:
        FIND FIRST ITEM WHERE ITEM.company = prmComp AND
        ITEM.i-no = prmItem NO-LOCK NO-ERROR.
        
        IF AVAIL ITEM THEN DO:
            RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
        END.
        ELSE DO:
            RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
        END.

        FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 AND uom.uom = prmText NO-LOCK :
       create ttCorUomLook.
       assign           
           ttCorUomLook.vUom = uom.uom 
           ttCorUomLook.vDscr = uom.dscr.
        END.
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FIND FIRST ITEM WHERE ITEM.company = prmComp AND
        ITEM.i-no = prmItem NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN DO:
            RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
        END.
        ELSE DO:
            RUN sys/ref/uom-fg.p  (NO, OUTPUT uom-list).
        END.

       FOR EACH uom WHERE lookup(uom.uom, uom-list) gt 0 AND uom.uom begins prmText NO-LOCK :
           create ttCorUomLook.
           assign                                                                  
               ttCorUomLook.vUom    = uom.uom
               ttCorUomLook.vDscr = uom.dscr.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "uom" then do:*/
END.   /*IF prmAction = "search" then do:*/
