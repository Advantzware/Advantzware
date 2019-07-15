

/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{CarrierLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCarrierLook.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmAction    = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN  prmUser      = "".
IF prmShip   = ? THEN ASSIGN prmShip      = "".
IF prmField     = ? THEN ASSIGN prmField      = "".
IF prmCondition = ? THEN ASSIGN prmCondition      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    MESSAGE "carrier" prmAction prmComp.
    FIND FIRST shipto WHERE shipto.ship-id = prmShip  NO-LOCK NO-ERROR.
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier NO-LOCK:
        create ttCarrierLook.
        assign                                     
            ttCarrierLook.carrier    = carrier.carrier
            ttCarrierLook.dscr       = carrier.dscr.  
          
    
   END.
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH carrier no-lock:
                IF carrier.carrier = prmText OR
                   carrier.dscr = prmText THEN
                   DO:
                      create ttCarrierLook.
                      assign                                     
                        ttCarrierLook.carrier = carrier.carrier
                        ttCarrierLook.dscr = carrier.dscr.
                   END.
            END.    /*for each itemfg*/
    END.   /* IF prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH carrier no-lock:
            IF carrier.carrier begins prmText OR
               carrier.dscr begins prmText THEN
                DO:
                create ttCarrierLook.
                assign                                                              
                    ttCarrierLook.carrier = carrier.carrier
                    ttCarrierLook.dscr = carrier.dscr.
                END.
        END.
    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "carrier" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH carrier WHERE carrier.carrier = prmText no-lock:
                create ttCarrierLook.
                assign                                                                
                    ttCarrierLook.carrier    = carrier.carrier
                    ttCarrierLook.dscr = carrier.dscr.                       
            
        END. /*for each itemfg*/
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH carrier WHERE carrier.carrier begins prmText no-lock:
                create ttCarrierLook.
                assign                                                                  
                    ttCarrierLook.carrier    = carrier.carrier
                    ttCarrierLook.dscr = carrier.dscr.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "carrier" then do:*/
if prmField = "dscr" then do:
     if prmCondition = "EQUAL" then do:
         
              FOR EACH carrier WHERE carrier.dscr = prmText no-lock:
                    create ttCarrierLook.
                    assign                                                                
                        ttCarrierLook.carrier    = carrier.carrier
                        ttCarrierLook.dscr = carrier.dscr.
          END.
    END.   /*if prmCondition = "EQUAL" */
    if prmCondition = "BEGIN" then do:
         
              FOR EACH carrier WHERE carrier.dscr begins prmText no-lock:
                  create ttCarrierLook.
                  assign                                                                  
                      ttCarrierLook.carrier    = carrier.carrier
                      ttCarrierLook.dscr = carrier.dscr.                       
              
          END.
    END. /*if prmCondition = "BEGIN" */       
END.
END. /* IF prmAction = search then do: */


