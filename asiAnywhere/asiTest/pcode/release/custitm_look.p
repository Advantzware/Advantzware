/*------------------------------------------------------------------------
    File        : custitm_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustomerItemLookup NO-UNDO 
    FIELD estno          AS CHAR
    FIELD ino            AS CHAR
    FIELD iname          AS CHAR
    FIELD qty            AS DEC
    FIELD extra          AS CHAR
    .

DEFINE DATASET dsCustomerItemLookup FOR ttCustomerItemLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmord       AS INT  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustomerItemLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmord      = ? THEN ASSIGN prmord      = 0.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction = "Select" then do:

    FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
        AND oe-ordl.ord-no = prmord  NO-LOCK:

        create ttCustomerItemLookup.
        assign
            ttCustomerItemLookup.estno     = oe-ordl.est-no
            ttCustomerItemLookup.ino       = oe-ordl.i-no
            ttCustomerItemLookup.iname     = oe-ordl.i-name
            ttCustomerItemLookup.qty       = oe-ordl.qty .


    END.  /*FOR EACH oe-ordl*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmField = "ino"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                  AND oe-ordl.i-no = prmText
                  AND oe-ordl.ord-no = prmord  NO-LOCK:
                  
                  create ttCustomerItemLookup.
                  assign
                      ttCustomerItemLookup.estno     = oe-ordl.est-no
                      ttCustomerItemLookup.ino       = oe-ordl.i-no
                      ttCustomerItemLookup.iname     = oe-ordl.i-name
                      ttCustomerItemLookup.qty       = oe-ordl.qty .
              END. /*FOR EACH oe-ordl*/
         END. 
     END .  /* if prmField = state  */
END.  /* IF prmAction = search then do: */
