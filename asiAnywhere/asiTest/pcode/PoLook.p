
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{PoLook.i}
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPoLook.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
       
if prmAction <> "search" then do:
    v-count = 0.
    MAIN-LOOP:
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        OE-LOOP:
        FOR EACH oe-rel WHERE oe-rel.company = prmComp AND oe-rel.cust-no = usercust.cust-no NO-LOCK  :
            IF oe-rel.po-no = "" THEN NEXT OE-LOOP.
            FIND FIRST ttPoLook WHERE ttPoLook.CustPo = oe-rel.po-no NO-LOCK NO-ERROR.
            IF AVAIL ttPoLook THEN NEXT OE-LOOP.
            create ttPoLook.
            assign                                     
                ttPoLook.CustPo  = oe-rel.po-no
                ttPoLook.Number = oe-rel.cust-no . 
                v-count = v-count + 1.
                      IF v-count = 150 THEN LEAVE MAIN-LOOP.
        END.	 /* FOR EACH oe-rel */
    END.
    
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           v-count = 0.
            MAIN-LOOP:
           FOR EACH oe-rel WHERE oe-rel.company = prmComp no-lock:

               IF oe-rel.po-no = prmText or oe-rel.cust-no = prmText THEN
               DO:
                  create ttPoLook.
                  assign                                     
                    ttPoLook.CustPo    = oe-rel.po-no
                    ttPoLook.Number = oe-rel.cust-no  .
                   v-count = v-count + 1.
                      IF v-count = 150 THEN LEAVE MAIN-LOOP.
               END.
           END.   /*FOR EACH oe-rel*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-rel WHERE oe-rel.company = prmComp no-lock:

               IF oe-rel.po-no begins prmText or oe-rel.cust-no begins prmText THEN
               DO:
                  create ttPoLook.
                  assign                                                              
                    ttPoLook.CustPo    = oe-rel.po-no
                    ttPoLook.Number = oe-rel.cust-no .

               END.

           END.  /*FOR EACH oe-rel*/

    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "po-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH oe-rel where oe-rel.company = prmComp AND oe-rel.po-no = prmText no-lock:
                       create ttPoLook.
                       assign                                                                
                         ttPoLook.CustPo    = oe-rel.po-no
                         ttPoLook.Number = oe-rel.cust-no . 
           END.  /*FOR EACH oe-rel*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-rel where oe-rel.company = prmComp AND oe-rel.po-no begins prmText no-lock:
                       create ttPoLook.
                       assign                                                                  
                         ttPoLook.CustPo    = oe-rel.po-no
                         ttPoLook.Number = oe-rel.cust-no   .                

           END.  /*FOR EACH oe-rel*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "part-no" then do:*/
if prmField = "cust-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH oe-rel where oe-rel.company = prmComp AND oe-rel.cust-no = prmText no-lock:
                       create ttPoLook.
                       assign                                                                
                         ttPoLook.CustPo    = oe-rel.po-no
                         ttPoLook.Number = oe-rel.cust-no    .                

           END.
    END.
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-rel where  oe-rel.company = prmComp AND oe-rel.cust-no begins prmText no-lock:
                       create ttPoLook.
                       assign                                                                  
                         ttPoLook.CustPo    = oe-rel.po-no
                         ttPoLook.Number = oe-rel.cust-no    .                

           END.
    END.        
END.
 
END. /* IF prmAction = search then do: */

