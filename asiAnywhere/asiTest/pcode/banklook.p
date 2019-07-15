/*------------------------------------------------------------------------
    File        : banklook.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBankLookup NO-UNDO 
    FIELD bank            AS CHAR
    FIELD bnkname         AS CHAR
    FIELD actnum          AS CHAR
    FIELD lstchk          AS INT
    FIELD extra           AS CHAR
    .

DEFINE DATASET dsBankLookup FOR ttBankLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBankLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction = "Select" then do:

    FOR EACH bank WHERE bank.company = prmComp NO-LOCK:

        IF AVAIL bank THEN
              create ttBankLookup.
                      assign
                           ttBankLookup.bank        = bank.bank-code
                           ttBankLookup.bnkname     = bank.bank-name
                           ttBankLookup.actnum      = bank.actnum
                           ttBankLookup.lstchk      = bank.last-chk .
       

    END.  /*FOR EACH bank*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmField = "bank"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH bank WHERE bank.company = prmComp 
                  AND bank.bank-code = prmText NO-LOCK:

                  IF AVAIL bank THEN
                      create ttBankLookup.
                  assign
                      ttBankLookup.bank        = bank.bank-code
                      ttBankLookup.bnkname     = bank.bank-name
                      ttBankLookup.actnum      = bank.actnum
                      ttBankLookup.lstchk      = bank.last-chk .
              END.  /*FOR EACH bank*/
         END. 
     END .  /* if prmField = state  */
     if prmField = "bankname"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH bank WHERE bank.company = prmComp
                  AND bank.bank-code = prmText NO-LOCK:

                  IF AVAIL bank THEN
                      create ttBankLookup.
                  assign
                      ttBankLookup.bank        = bank.bank-code
                      ttBankLookup.bnkname     = bank.bank-name
                      ttBankLookup.actnum      = bank.actnum
                      ttBankLookup.lstchk      = bank.last-chk .
              END.  /*FOR EACH bank*/
         END. 
         if prmCondition = "BEGINS" then do:
              FOR EACH bank WHERE bank.company = prmComp
                  AND bank.bank-name BEGINS prmText NO-LOCK:

                  IF AVAIL bank THEN
                      create ttBankLookup.
                  assign
                      ttBankLookup.bank        = bank.bank-code
                      ttBankLookup.bnkname     = bank.bank-name
                      ttBankLookup.actnum      = bank.actnum
                      ttBankLookup.lstchk      = bank.last-chk .
              END.  /*FOR EACH bank*/
         END. 
     END .  /* if prmField = state  */
END.  /* IF prmAction = search then do: */
