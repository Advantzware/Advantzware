


/*------------------------------------------------------------------------
    File        : BrwsItemStatus.p
    Purpose     : Browse Item

    Syntax      :

    Description : Return a Dataset of all Item Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttbrwsitemstatus NO-UNDO
    FIELD  vItemno   AS CHARACTER FORMAT "x(15)"     
    FIELD  vName     AS CHARACTER FORMAT "x(30)"     
    FIELD  vPartdsc  AS CHARACTER FORMAT "x(25)"    
    FIELD  vCust     AS CHARACTER FORMAT "x(8)"     
    FIELD  vStyle    AS CHARACTER FORMAT "x(6)"     
    FIELD  vProcat   AS CHARACTER FORMAT "x(5)"     
    FIELD  vCode     AS CHARACTER FORMAT "x(1)"     
    FIELD  vEstno    AS CHARACTER FORMAT "x(8)"     
    FIELD  vCad      AS CHARACTER FORMAT "x(15)"     
    FIELD  vSpc      AS CHARACTER FORMAT "x(15)"     
    FIELD  vStocked  AS LOGICAL     
    FIELD  vQonh     AS DECIMAL FORMAT "->>,>>>,>>9.999"
    FIELD  cCustPart AS CHARACTER FORMAT "x(15)"
    FIELD  cDie      AS CHARACTER FORMAT "x(20)"
    FIELD  dAllocQty AS DECIMAL 
    FIELD  dAvalQty  AS DECIMAL
    FIELD  dOrderQty AS DECIMAL
    FIELD  dShipQty  AS DECIMAL
    FIELD  cReckey   AS CHARACTER      .

   DEFINE DATASET dsbrwsitemstatus FOR ttbrwsitemstatus.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmActItem   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmItemName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmCad       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmEst       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmStyle     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmProcat    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmDie       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmStartDate AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmEndDate   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbrwsitemstatus.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR custcount AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VARIABLE cocode AS CHARACTER NO-UNDO .
DEFINE VARIABLE vEst AS CHAR NO-UNDO.
DEFINE VARIABLE li-all AS INT NO-UNDO.
DEFINE VARIABLE dOrderQty AS DECIMAL NO-UNDO .
DEFINE VARIABLE dInvQty AS DECIMAL NO-UNDO .
IF prmActItem        = ? THEN ASSIGN prmActItem     = "".
IF prmUser           = ? THEN ASSIGN prmUser     = "".
IF prmItem           = ? THEN ASSIGN prmItem     = "".
IF prmItemName       = ? THEN ASSIGN prmItemName     = "".
IF prmCad            = ? THEN ASSIGN prmCad     = "".
IF prmEst            = ? THEN ASSIGN prmEst     = "".
IF prmStyle          = ? THEN ASSIGN prmStyle     = "".
IF prmProcat         = ? THEN ASSIGN prmProcat     = "".
IF prmCustPart       = ? THEN ASSIGN prmCustPart   = "" .
IF prmDie            = ? THEN ASSIGN prmDie        = "".
IF prmStartDate      = ? THEN ASSIGN prmStartDate  = "01/01/1980".
IF prmEndDate        = ? THEN ASSIGN prmEndDate    = "01/01/2099" .
IF prmStartDate      = "" THEN ASSIGN prmStartDate  = "01/01/1980".
IF prmEndDate        = "" THEN ASSIGN prmEndDate    = "01/01/2099" .
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
cocode = prmComp .
FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 
{sys/inc/oereordr.i}
/* ********************  Preprocessor Definitions  ******************** */
    

IF prmActItem = "SearchItem" THEN DO:
    ASSIGN vESt = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).
    v-count = 0.  
    MAIN-LOOP:
    
FOR EACH itemfg where itemfg.company = prmComp AND LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE ""
                                               AND (itemfg.i-no BEGINS prmItem OR itemfg.i-no = prmItem or prmItem = "")
                                               AND (itemfg.i-name BEGINS prmItemName OR itemfg.i-name = prmItemName OR prmItemName = "")
                                               AND (itemfg.cust-no BEGINS prmCad OR itemfg.cust-no = prmCad OR prmCad = "")  /* prmCad use as cust*/
                                               AND (itemfg.est-no = vEst OR vest = "")
                                               AND (itemfg.style BEGINS prmStyle OR itemfg.style = prmStyle OR prmStyle = "")
                                               AND (itemfg.procat BEGINS prmProcat OR itemfg.procat = prmProcat OR prmProcat = "") 
                                               AND (itemfg.part-no BEGINS prmCustPart  OR prmCustPart = "")
                                               AND (itemfg.die-no BEGINS prmDie  OR prmDie = "")
                                               no-lock BY itemfg.i-no:
    create ttbrwsitemstatus.
    assign 
            ttbrwsitemstatus.vItemno          = itemfg.i-no
            ttbrwsitemstatus.vName            = itemfg.i-name
            ttbrwsitemstatus.vPartdsc         = itemfg.part-dscr1
            ttbrwsitemstatus.vCust            = itemfg.cust-no
            ttbrwsitemstatus.vStyle           = itemfg.style
            ttbrwsitemstatus.vProcat          = itemfg.procat
            ttbrwsitemstatus.vCode            = itemfg.i-code
            ttbrwsitemstatus.vEstno           = itemfg.est-no
            ttbrwsitemstatus.vCad             = itemfg.cad-no
            ttbrwsitemstatus.vSpc             = itemfg.spc-no
            ttbrwsitemstatus.vStocked         = itemfg.stocked
            ttbrwsitemstatus.vQonh            = itemfg.q-onh
            ttbrwsitemstatus.cCustPart        = itemfg.part-no
            ttbrwsitemstatus.cDie             = itemfg.die-no
            ttbrwsitemstatus.cReckey          = itemfg.rec_key
            .
            
            ASSIGN 
                ttbrwsitemstatus.dAllocQty        = itemfg.q-alloc
                ttbrwsitemstatus.dAvalQty         = itemfg.q-avail
                ttbrwsitemstatus.dOrderQty        = itemfg.q-ono
                ttbrwsitemstatus.dShipQty         = itemfg.q-ship-ptd .
            dOrderQty = 0.
            FOR EACH oe-ordl NO-LOCK 
                WHERE oe-ordl.company EQ cocode
                  AND oe-ordl.i-no EQ itemfg.i-no ,
                FIRST oe-ord OF oe-ordl 
                WHERE oe-ord.ord-date GE date(prmStartDate)
                      AND oe-ord.ord-date LE DATE(prmEndDate) NO-LOCK .
                dOrderQty = dOrderQty + oe-ordl.qty .
            END.
            dOrderQty = 0.
            FOR EACH ar-invl NO-LOCK 
                WHERE ar-invl.company EQ cocode
                  AND ar-invl.i-no EQ itemfg.i-no ,
                FIRST ar-inv WHERE ar-inv.company EQ cocode
                 AND ar-inv.inv-no EQ ar-invl.inv-no 
                 AND ar-inv.inv-date GE date(prmStartDate)
                 AND ar-inv.inv-date LE DATE(prmEndDate) NO-LOCK .
                dInvQty = dInvQty + ar-invl.inv-qty .
            END.
            IF prmStartDate NE "" AND prmEndDate NE "" THEN
                ASSIGN
                ttbrwsitemstatus.dOrderQty        = dOrderQty
                ttbrwsitemstatus.dShipQty         = dInvQty .

                v-count = v-count + 1.
                      IF v-count = 2000 THEN LEAVE MAIN-LOOP.
        
    END.   /*FOR EACH itemfg*/
  
  /*END.*/
END.
IF prmActItem = "BrwsItem" THEN DO:  
    v-count = 0.
    MAIN-LOOP:
     FOR EACH itemfg where itemfg.company = prmComp  AND 
          LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE "" NO-LOCK BY itemfg.i-no:
        create ttbrwsitemstatus.
        assign 
            ttbrwsitemstatus.vItemno          = itemfg.i-no
            ttbrwsitemstatus.vName            = itemfg.i-name
            ttbrwsitemstatus.vPartdsc         = itemfg.part-dscr1
            ttbrwsitemstatus.vCust            = itemfg.cust-no
            ttbrwsitemstatus.vStyle           = itemfg.style
            ttbrwsitemstatus.vProcat          = itemfg.procat
            ttbrwsitemstatus.vCode            = itemfg.i-code
            ttbrwsitemstatus.vEstno           = itemfg.est-no
            ttbrwsitemstatus.vCad             = itemfg.cad-no
            ttbrwsitemstatus.vSpc             = itemfg.spc-no
            ttbrwsitemstatus.vStocked         = itemfg.stocked
            ttbrwsitemstatus.vQonh            = itemfg.q-onh
            ttbrwsitemstatus.cCustPart        = itemfg.part-no
            ttbrwsitemstatus.cDie             = itemfg.die-no
            ttbrwsitemstatus.cReckey          = itemfg.rec_key
            .
            
            ASSIGN 
                ttbrwsitemstatus.dAllocQty        = itemfg.q-alloc
                ttbrwsitemstatus.dAvalQty         = itemfg.q-avail
                ttbrwsitemstatus.dOrderQty        = itemfg.q-ono
                ttbrwsitemstatus.dShipQty         = itemfg.q-ship-ptd .

                 v-count = v-count + 1.
                      IF v-count = 2000 THEN LEAVE MAIN-LOOP.
    END.   /*FOR EACH itemfg*/
     
END.





