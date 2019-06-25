

                                 
/*------------------------------------------------------------------------
    File        : RelEmail.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Jyoti Bajaj
    Created     : mon March 10 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttRelEmail NO-UNDO 
     FIELD ord-no AS INT
    FIELD i-no AS CHAR
    FIELD rel-qty AS DEC
    FIELD rel-date AS DATE
    FIELD po-no AS CHAR
    FIELD cust-no AS cha
    .

DEFINE DATASET dsRelEmail FOR ttRelEmail.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer     AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRelEmail.

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ?   THEN ASSIGN prmAction = "".
IF prmOrderNum  = ?   THEN ASSIGN prmOrderNum = 0.
IF prmCustomer = ?   THEN ASSIGN prmCustomer = "".
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".

DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

/*************************************/

 IF prmAction = "Select" THEN DO:
   FIND FIRST oe-rel WHERE oe-rel.company = prmComp
                        AND oe-rel.loc = prmLoc
                        AND oe-rel.ord-no = prmOrderNum 
                        AND oe-rel.cust-no = prmCustomer NO-LOCK NO-ERROR.
   FIND FIRST oe-relh
            WHERE oe-relh.company  EQ oe-rel.company
              AND oe-relh.rel-date EQ oe-rel.rel-date
              AND oe-relh.cust-no  EQ oe-rel.cust-no
              AND oe-relh.ship-id  EQ oe-rel.ship-id  NO-LOCK NO-ERROR.
                  CREATE ttRelEmail.
    
        ASSIGN 
             ttRelEmail.cust-no = oe-rel.cust-no
             ttRelEmail.ord-no  = oe-rel.ord-no
             ttRelEmail.i-no    = oe-rel.i-no
             ttRelEmail.rel-qty = oe-rel.qty
             ttRelEmail.rel-date = IF AVAIL oe-relh THEN oe-relh.rel-date
                                                  ELSE oe-rel.rel-date
             ttRelEmail.po-no    = oe-rel.po-no.
                           
        
END. /*IF prmAction = "Select" */
/*****************************************PROCEDURE assign-RelEmail******************************/



