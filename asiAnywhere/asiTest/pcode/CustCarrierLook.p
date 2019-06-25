

/*------------------------------------------------------------------------
    File         : CustCarrierLook
    Purpose     :  Cust carrier lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : april 22 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustCarrierLook NO-UNDO 
    FIELD carrier AS CHARACTER
    FIELD dscr AS CHARACTER
    FIELD hfdskdk AS CHAR 
    .
                                           
    
DEFINE DATASET dsCustCarrierLook FOR ttCustCarrierLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLoc       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustCarrierLook.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmAction    = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN  prmUser      = "".
IF prmLoc       = ? THEN ASSIGN prmLoc      = "".
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
    MESSAGE "carrier" prmAction prmComp prmLoc.
     FOR EACH carrier WHERE  carrier.company = prmComp AND carrier.loc = prmLoc NO-LOCK :
        create ttCustCarrierLook.
        assign                                     
            ttCustCarrierLook.carrier    = carrier.carrier
            ttCustCarrierLook.dscr       = carrier.dscr.  
          
    
   END.
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    

if prmField = "carrier" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH carrier WHERE  carrier.company = prmComp AND carrier.loc = prmLoc AND carrier.carrier = prmText NO-LOCK :
            create ttCustCarrierLook.
                assign                                                                
                    ttCustCarrierLook.carrier    = carrier.carrier
                    ttCustCarrierLook.dscr = carrier.dscr.                       
            
        END. /*for each itemfg*/
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH carrier WHERE  carrier.company = prmComp AND carrier.loc = prmLoc AND carrier.carrier BEGINS prmText NO-LOCK :
                create ttCustCarrierLook.
                assign                                                                  
                    ttCustCarrierLook.carrier    = carrier.carrier
                    ttCustCarrierLook.dscr = carrier.dscr.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "carrier" then do:*/
if prmField = "dscr" then do:
     if prmCondition = "EQUAL" then do:
              FOR EACH carrier WHERE  carrier.company = prmComp AND carrier.loc = prmLoc AND carrier.dscr = prmText NO-LOCK :
              create ttCustCarrierLook.
                    assign                                                                
                        ttCustCarrierLook.carrier    = carrier.carrier
                        ttCustCarrierLook.dscr = carrier.dscr.
          END.
    END.   /*if prmCondition = "EQUAL" */
    if prmCondition = "BEGIN" then do:
         
             FOR EACH carrier WHERE  carrier.company = prmComp AND carrier.loc = prmLoc AND carrier.dscr BEGINS prmText NO-LOCK :
                  create ttCustCarrierLook.
                  assign                                                                  
                      ttCustCarrierLook.carrier    = carrier.carrier
                      ttCustCarrierLook.dscr = carrier.dscr.                       
              
          END.
    END. /*if prmCondition = "BEGIN" */       
END.
END. /* IF prmAction = search then do: */


