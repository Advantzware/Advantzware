
/*------------------------------------------------------------------------
    File         : OrderLookup
    Purpose     : Order lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrdLook.i}

DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrdLook.
DEFINE VAR vEst AS CHAR NO-UNDO.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".

ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
if prmAction <> "search" then do:
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp NO-LOCK:
        OE-LOOP:
        FOR EACH oe-ord where
               oe-ord.company = prmComp 
            AND oe-ord.cust-no = usercust.cust-no AND (oe-ord.cust-no = prmCust OR prmCust = "") NO-LOCK BY oe-ord.ord-no DESC:
            FIND FIRST ttOrdLook WHERE ttOrdLook.Order = oe-ord.ord-no NO-LOCK NO-ERROR.
                IF AVAIL ttOrdLook THEN NEXT OE-LOOP.
        create ttOrdLook.
        assign                                     
            ttOrdLook.Order    = oe-ord.ord-no
            ttOrdLook.Estimate = oe-ord.est-no 
            ttOrdLook.cust-no  = oe-ord.cust-no.
        END.
     END.
     
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH oe-ord where
               oe-ord.company = prmComp 
               no-lock:

               IF (oe-ord.ord-no = int(prmText) OR
                   oe-ord.est-no = vEst OR
                   oe-ord.cust-no = prmText) THEN
               DO:
                  create ttOrdLook.
                  assign                                     
                      ttOrdLook.Order    = oe-ord.ord-no
                      ttOrdLook.Estimate = oe-ord.est-no
                      ttOrdLook.cust-no = oe-ord.cust-no.
               END.
               
           END.   /*FOR EACH oe-ord*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ord where
               oe-ord.company = prmComp 
               no-lock:

               IF (oe-ord.ord-no = int(prmText) OR
                   oe-ord.est-no begins vEst OR
                   oe-ord.cust-no begins prmText) THEN
               DO:
                  create ttOrdLook.
                  assign                                                              
                      ttOrdLook.Order    = oe-ord.ord-no
                      ttOrdLook.Estimate = oe-ord.est-no 
                      ttOrdLook.cust-no = oe-ord.cust-no.
               END.
                

           END.  /*FOR EACH oe-ord*/

    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "ord-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH oe-ord where oe-ord.company = prmComp AND oe-ord.ord-no = int(prmText) no-lock:
               create ttOrdLook.
               assign                                                                
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                   ttOrdLook.cust-no = oe-ord.cust-no.
                

           END.  /*FOR EACH oe-ord*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ord WHERE oe-ord.company = prmComp AND  oe-ord.ord-no = int(prmText) no-lock:
               create ttOrdLook.
               assign                                                                  
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                   
                   ttOrdLook.cust-no = oe-ord.cust-no.
                
           END.  /*FOR EACH oe-ord*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "ord-no" then do:*/
if prmField = "est-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH oe-ord where oe-ord.company = prmComp AND oe-ord.est-no = vEst no-lock:
               create ttOrdLook.
               assign                                                                
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                   ttOrdLook.cust-no = oe-ord.cust-no.
                
           END.
    END.
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ord WHERE oe-ord.company = prmComp AND  oe-ord.est-no begins vEst no-lock:
               create ttOrdLook.
               assign                                                                  
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                    
                   ttOrdLook.cust-no = oe-ord.cust-no.
           END.
    END.        
END.

if prmField = "cust-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH oe-ord where oe-ord.company = prmComp AND oe-ord.cust-no = prmText no-lock:
               create ttOrdLook.
               assign                                                                
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                   ttOrdLook.cust-no = oe-ord.cust-no.
           END.
    END.
    if prmCondition = "BEGIN" then do:
           FOR EACH oe-ord WHERE oe-ord.company = prmComp AND  oe-ord.cust-no begins prmText no-lock:
               create ttOrdLook.
               assign                                                                  
                   ttOrdLook.Order    = oe-ord.ord-no
                   ttOrdLook.Estimate = oe-ord.est-no 
                   ttOrdLook.cust-no = oe-ord.cust-no.
           END.
    END.        
END.

END. /* IF prmAction = search then do: */

