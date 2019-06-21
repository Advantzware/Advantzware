/*------------------------------------------------------------------------
    File        : Custype.p
    Purpose     : Custype

    Syntax      :

    Description : Return a Output value for custype

    Author(s)   : Kuldeep
    Created     : mar 18 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustype NO-UNDO
    FIELD vCustype LIKE custype.Custype FORMAT "X(8)"
    FIELD vDscr LIKE custype.dscr FORMAT "X(20)"
    FIELD vCommrate LIKE custype.commrate FORMAT ">>9.99"
    FIELD vDiscount LIKE custype.discount FORMAT ">>9.99"
    .
        
DEFINE DATASET dsCustype FOR ttCustype.

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCustype    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vDscr       AS CHAR    NO-UNDO.
DEFINE INPUT PARAMETER vDiscount   AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER vCommrate   AS DECIMAL    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustype.
DEFINE BUFFER buff-custype FOR custype.

    FOR EACH ttCustype:
        DELETE ttCustype.
    END.

     IF prmAction = ?  THEN ASSIGN prmAction = "Select".
     IF prmComp = ?   THEN ASSIGN prmComp = "".
     IF prmUser = ?   THEN ASSIGN prmUser = "".
     IF prmAction = ? THEN ASSIGN prmAction = "".
     IF vCommrate = ? THEN ASSIGN vCommrate = 0.
     IF vDiscount = ? THEN ASSIGN vDiscount = 0.
     IF vCustype = ?  THEN ASSIGN vCustype = "".
     IF vDscr  = ?    THEN ASSIGN vDscr  = "".

     IF prmComp EQ "" THEN DO:
         FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.
         prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
      END.



  IF prmAction = "Search" THEN DO:
    FOR EACH buff-custype WHERE buff-custype.company = prmComp
                   AND (buff-custype.custype = vCustype OR vCustype = "") 
                   AND (buff-custype.dscr = vDscr OR vDscr = "")
                   NO-LOCK:
        CREATE ttCustype.
        ASSIGN 
            ttCustype.vCustype  = buff-custype.custype 
            ttCustype.vDscr     = buff-custype.dscr
            ttCustype.vCommrate = buff-custype.commrate
            ttCustype.vDiscount = buff-custype.discount 
            .

      END. /*FOR EACH buff-custype  */
END. /*IF prmAction = "Select" THEN DO:*/
IF prmAction = "Select" THEN DO:
    FOR EACH buff-custype WHERE buff-custype.company = prmComp 
        NO-LOCK:
        CREATE ttCustype.
           ASSIGN 
               ttCustype.vCustype  = buff-custype.custype 
               ttCustype.vDscr     = buff-custype.dscr
               ttCustype.vCommrate = buff-custype.commrate
               ttCustype.vDiscount = buff-custype.discount
               .

    END. /*FOR EACH buff-custype  */
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Add" THEN DO:
    FIND buff-custype WHERE buff-custype.custype = vCustype  AND buff-custype.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff-custype  THEN DO:
        CREATE buff-custype .
        ASSIGN
            buff-custype.custype     = vCustype
            buff-custype.company     = prmComp
            buff-custype.dscr        = vDscr
            buff-custype.commrate    = vComm
            buff-custype.discount    = vDiscount
            .
        RELEASE buff-custype.
    END.
END. /*
IF prmAction = "Add" THEN DO:*/
IF prmAction = "Update" THEN DO:
    FIND FIRST buff-custype WHERE buff-custype.custype = vCustype  AND buff-custype.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-custype  THEN DO:
        ASSIGN
            buff-custype.dscr        = vDscr
            buff-custype.commrate    = vComm
            buff-custype.discount    = vDiscount
            
            .
        RELEASE buff-custype.
    END.
END. /*IF prmAction = "Update" THEN DO:*/
IF prmAction = "Delete" THEN DO:
    FIND buff-custype WHERE buff-custype.custype = vCustype  AND buff-custype.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE buff-custype  THEN DO:
        DELETE buff-custype.
    END.
END. /*IF prmAction = "Delete" THEN DO:*/
