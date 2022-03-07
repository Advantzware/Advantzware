
/*------------------------------------------------------------------------
    File        : NewEstimateBlank.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 27 02:04:24 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ip-rief     AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER op-rieb    AS ROWID      NO-UNDO.

DEFINE BUFFER bf-eb FOR eb. /*bf - buffer*/

DEFINE VARIABLE cPrev-cust          LIKE eb.cust-no NO-UNDO.
DEFINE VARIABLE cPrev-ship          LIKE eb.ship-id NO-UNDO.
DEFINE VARIABLE cPrev-style         LIKE eb.style NO-UNDO.

DEFINE VARIABLE cPart-no            AS CHARACTER NO-UNDO.
DEFINE VARIABLE li                  AS INTEGER   NO-UNDO. /*Count variable*/
DEFINE VARIABLE cPackCodeOverride   AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPriceBasedOnYield  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPriceBasedOnYield  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEstimateLocDefault AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND ef WHERE ROWID(ef) EQ ip-rief NO-LOCK NO-ERROR.

FIND FIRST ce-ctrl WHERE ce-ctrl.company = ef.company  
    AND ce-ctrl.loc     = ef.loc NO-LOCK NO-ERROR.

IF NOT AVAILABLE ef THEN RETURN.
RUN sys/ref/nk1look.p (ef.company, "CERequestYield", "C", NO, NO, "", "", OUTPUT cPriceBasedOnYield, OUTPUT lFound).

RUN sys/ref/nk1look.p (ef.company, "EstimateLocDefault", "C", NO, NO, "", "", OUTPUT cEstimateLocDefault, OUTPUT lFound).

CASE cPriceBasedOnYield:
    WHEN "RequestAlways" OR 
    WHEN "RequestNewOnly" THEN 
        lPriceBasedOnYield = NO.
    WHEN "YieldAlways" OR 
    WHEN "YieldNewOnly" THEN 
        lPriceBasedOnYield = YES.
END CASE.

IF cPriceBasedOnYield EQ "RequestNewOnly" OR cPriceBasedOnYield EQ "YieldNewOnly" THEN 
    FOR EACH eb
        WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        NO-LOCK
        BY eb.form-no  DESCENDING
        BY eb.blank-no DESCENDING:
        lPriceBasedOnYield = eb.yrprice.
        LEAVE.
    END.
    
FIND FIRST bf-eb
    WHERE bf-eb.company  EQ ef.company 
    AND bf-eb.est-no   EQ ef.est-no
    AND bf-eb.form-no  EQ 0
    AND bf-eb.blank-no EQ 0
    NO-LOCK NO-ERROR.
cPart-no = IF AVAILABLE bf-eb THEN bf-eb.part-no ELSE "".

FIND LAST bf-eb NO-LOCK
    WHERE bf-eb.company EQ ef.company
    AND bf-eb.est-no  EQ ef.est-no
    AND bf-eb.form-no NE 0
    USE-INDEX est-qty NO-ERROR.
IF AVAILABLE bf-eb THEN
    ASSIGN
        cPrev-cust  = bf-eb.cust-no
        cPrev-ship  = bf-eb.ship-id
        cPrev-style = eb.style.

FIND LAST bf-eb NO-LOCK
    WHERE bf-eb.company EQ ef.company
    AND bf-eb.est-no  EQ ef.est-no
    AND bf-eb.form-no EQ ef.form-no
    USE-INDEX est-qty NO-ERROR.
li = IF AVAILABLE bf-eb THEN bf-eb.blank-no ELSE 0.


CREATE eb. 
ASSIGN
    eb.est-type       = ef.est-type
    eb.company        = ef.company
    eb.loc            = ef.loc
    eb.e-num          = ef.e-num
    eb.est-no         = ef.est-no
    eb.est-int        = INT(ef.est-no)
    eb.eqty           = ef.eqty
    eb.form-no        = ef.form-no
    eb.blank-no       = li + 1
    eb.cas-no         = ce-ctrl.def-case
    eb.tr-no          = ce-ctrl.def-pal
    eb.cust-no        = cPrev-cust
    eb.ship-id        = cPrev-ship
    eb.i-pass         = 0
    eb.yrprice        = lPriceBasedOnYield     
    eb.part-no        = cPart-no + IF cPart-no NE "" THEN ("-" + STRING(eb.form-no) + "-" + STRING(eb.blank-no)) ELSE ""
    eb.style          = cPrev-style
    eb.cust-%         = INT(ef.est-type EQ 2) 
    eb.tr-cas         = 1
    eb.quantityPerSet = 1
    eb.yld-qty        = 1
    eb.tab-in         = YES
    eb.len            = 0
    eb.wid            = 0
    eb.dep            = 0
    eb.procat         = IF AVAILABLE bf-eb THEN bf-eb.procat ELSE ""
    eb.flute          = ef.flute
    eb.test           = ef.test.
 
IF cEstimateLocDefault NE "" THEN
    ASSIGN eb.loc = cEstimateLocDefault.
    
RUN est/packCodeOverride.p (INPUT eb.company, eb.cust-no, eb.style, OUTPUT cPackCodeOverride).
IF cPackCodeOverride GT "" THEN 
    eb.cas-no = cPackCodeOverride.
    
IF (ef.est-type EQ 6 OR ef.est-type EQ 2) THEN 
DO:
    IF cPart-no NE "" THEN
    DO:
        li = 1.
        FOR EACH bf-eb
            WHERE bf-eb.company  EQ ef.company
            AND bf-eb.est-no   EQ ef.est-no
            AND bf-eb.form-no  NE 0
            AND bf-eb.blank-no NE 0
            AND ROWID(bf-eb)   NE ROWID(eb)
            NO-LOCK
            BY bf-eb.form-no BY bf-eb.blank-no:
            li = li + 1.
        END.
        DO WHILE TRUE:
            IF NOT CAN-FIND(FIRST bf-eb
                WHERE bf-eb.company EQ ef.company
                AND bf-eb.est-no  EQ ef.est-no
                AND bf-eb.part-no EQ cPart-no + "-" + STRING(li)
                AND ROWID(bf-eb)  NE ROWID(eb))
                THEN 
            DO:
                eb.part-no = cPart-no + "-" + STRING(li).
                LEAVE.
            END.
            li = li + 1.
        END.
    END.
    
    FIND FIRST bf-eb
        WHERE bf-eb.company  EQ eb.company
        AND bf-eb.est-no   EQ eb.est-no 
        AND bf-eb.form-no  EQ 0
        AND bf-eb.blank-no EQ 0
        NO-LOCK NO-ERROR.
    IF AVAILABLE bf-eb THEN eb.procat = bf-eb.procat.
END.

FIND FIRST ITEM WHERE ITEM.company = ef.company
    AND ITEM.mat-type = "C"
    AND item.i-no EQ eb.cas-no NO-LOCK NO-ERROR.
    
IF AVAILABLE item THEN 
DO:
    FIND FIRST e-item
        WHERE e-item.company = item.company
        AND e-item.loc     = item.loc
        AND e-item.i-no    = item.i-no   NO-LOCK NO-ERROR.
    FIND FIRST itemfg
        WHERE itemfg.company = eb.company
        AND itemfg.i-no    = eb.stock-no   NO-LOCK NO-ERROR.
    IF AVAILABLE e-item THEN
        ASSIGN eb.cas-len = e-item.case-l
            eb.cas-wid = e-item.case-w
            eb.cas-dep = e-item.case-d
            eb.cas-wt  = e-item.avg-w
            eb.cas-pal = e-item.case-pall
            eb.cas-cnt = IF AVAILABLE itemfg THEN   itemfg.case-count ELSE e-item.box-case.

    IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
    IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
    IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
    IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
    IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
    IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
            IF AVAILABLE itemfg THEN itemfg.case-count ELSE item.box-case.
END.

op-rieb = ROWID(eb).
