/*------------------------------------------------------------------------
    File        : custpo_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustomerPOLookup NO-UNDO 
    FIELD pono          AS CHAR
    FIELD ino            AS CHAR
    FIELD qty            AS DEC
    FIELD shpid          AS CHAR
    FIELD reldate        AS CHAR
    FIELD extra           AS CHAR
    .

DEFINE DATASET dsCustomerPOLookup FOR ttCustomerPOLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmord       AS INT  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustomerPOLookup.
 
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

    FOR EACH oe-rel WHERE oe-rel.company = prmComp
        AND oe-rel.ord-no = prmord  NO-LOCK:

        create ttCustomerPOLookup.
        assign
            ttCustomerPOLookup.pono        = oe-rel.po-no
            ttCustomerPOLookup.ino         = oe-rel.i-no
            ttCustomerPOLookup.qty         = oe-rel.qty
            ttCustomerPOLookup.shpid       = oe-rel.ship-id 
            ttCustomerPOLookup.reldate     = string(oe-rel.rel-date)
            
             .


    END.  /*FOR EACH oe-rel*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmField = "pono"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH oe-rel WHERE oe-rel.company = prmComp
                  AND oe-rel.po-no = prmText
                  AND oe-rel.ord-no = prmord  NO-LOCK:
                  
                  create ttCustomerPOLookup.
                  assign
                        ttCustomerPOLookup.pono        = oe-rel.po-no
                        ttCustomerPOLookup.ino         = oe-rel.i-no
                        ttCustomerPOLookup.qty         = oe-rel.qty
                        ttCustomerPOLookup.shpid       = oe-rel.ship-id 
                        ttCustomerPOLookup.reldate     = string(oe-rel.rel-date) .
              END. /*FOR EACH oe-rel*/
         END. 
     END .  /* if prmField = state  */
END.  /* IF prmAction = search then do: */
