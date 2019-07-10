

/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE  ttuserlook NO-UNDO
FIELD user_id AS CHARACTER
FIELD user_name AS CHARACTER .
DEFINE DATASET dsuserlook FOR ttuserlook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsuserlook.

IF prmText      = ? THEN ASSIGN prmText      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
   FOR EACH users 
       NO-LOCK:
       
           
            create ttuserlook.
            assign                                     
                ttuserlook.user_id    = users.user_id
                ttuserlook.user_name       = users.user_name.                       
   END.
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    
if prmField = "user_id" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH users WHERE
           users.user_id= prmText NO-LOCK:
            
                create ttuserlook.
            assign                                     
                ttuserlook.user_id    = users.user_id
                ttuserlook.user_name       = users.user_name.                       
            
        END. /*for each itemfg*/
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH users WHERE
            users.user_id BEGINS prmText NO-LOCK:
            create ttuserlook.
            assign                                     
                ttuserlook.user_id    = users.user_id
                ttuserlook.user_name       = users.user_name.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "carrier" then do:*/
if prmField = "user_name" then do:
     if prmCondition = "EQUAL" then do:
          FOR EACH users WHERE
             users.user_name = prmText NO-LOCK:
              create ttuserlook.
            assign                                     
                ttuserlook.user_id    = users.user_id
                ttuserlook.user_name       = users.user_name.
          END.
    END.   /*if prmCondition = "EQUAL" */
    if prmCondition = "BEGIN" then do:
          FOR EACH users WHERE
              users.user_name BEGINS prmText NO-LOCK:
              create ttuserlook.
            assign                                     
                ttuserlook.user_id    = users.user_id
                ttuserlook.user_name       = users.user_name.                       
              
          END.
    END. /*if prmCondition = "BEGIN" */       
END.
END. /* IF prmAction = search then do: */


