

/*------------------------------------------------------------------------
    File         : Carrier Lookup
    Purpose     :  Carrier lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : praveen sharma
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.


DEFINE TEMP-TABLE ttCarrierLook NO-UNDO 
    FIELD vcarrier AS CHARACTER
    FIELD vdscr AS CHARACTER .
    
 
   DEFINE DATASET  dsCarrierLookup FOR ttCarrierLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCarrierLookup.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmCust AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
   FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        IF prmText = "" THEN DO:
            OE-LOOP:
            FOR EACH carrier WHERE carrier.company = prmComp
                 NO-LOCK:
                IF carrier.carrier = "" THEN NEXT OE-LOOP.
                   FIND FIRST ttCarrierLook WHERE ttCarrierLook.vcarrier = carrier.carrier NO-LOCK NO-ERROR.
                    IF AVAIL ttCarrierLook THEN NEXT OE-LOOP.
                    create ttCarrierLook.
                    assign                                     
                    
                                          
                   ttCarrierLook.vcarrier    = carrier.carrier
                   ttCarrierLook.vdscr       = carrier.dscr .
                  
                   
               END.
        END.
   
                   
        END.  /*FOR EACH carrier*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
     if prmField = "carrier"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH carrier WHERE
                carrier.company = prmComp AND (carrier.carrier = prmText OR prmText = "")
                no-lock:

                IF carrier.carrier = prmText 
                    THEN
                DO: 
                 create ttCarrierLook.
                 assign                                     
                  ttCarrierLook.vcarrier  = carrier.carrier
                   ttCarrierLook.vdscr    = carrier.dscr .
                  
            
               
         
               
                END.
          END. /*FOR EACH carrier*/
         END.
          IF prmCondition = "BEGIN" then do:
                   FOR EACH carrier WHERE
                carrier.company = prmComp AND  (carrier.carrier BEGINS  prmText OR prmText="" )
                no-lock:

                IF carrier.carrier BEGINS prmText 
                    THEN
                DO: 
                      create ttCarrierLook.
                      assign   
                   ttCarrierLook.vcarrier          = carrier.carrier
                   ttCarrierLook.vdscr           = carrier.dscr .
                  
                  end.  
            end. 
               
            /*if prmCondition = BEGIN*/    
          END.
           
     END.
    
END.  /*ifif prmAction <> "search" */

   

