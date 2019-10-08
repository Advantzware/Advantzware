
/*------------------------------------------------------------------------
    File        : fgpostProc.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jan 14 15:43:07 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ext-cost        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE type            AS ch        FORMAT "X" INITIAL "R".
DEFINE VARIABLE type-prt        AS ch        FORMAT "X(11)" INIT "".
DEFINE VARIABLE v-fg-qty        LIKE fg-rctd.t-qty.
DEFINE VARIABLE v-fg-cost       AS DECIMAL   FORMAT "->,>>>,>>9.99<<".
DEFINE VARIABLE v-tot-qty       AS INTEGER   FORMAT "->>>,>>>,>>9".
DEFINE VARIABLE v-tot-cost      AS DECIMAL   FORMAT "->>>,>>9.99<<".
DEFINE VARIABLE v-grd-tot-qty   AS INTEGER   FORMAT "->>>,>>>,>>9".
DEFINE VARIABLE v-grd-tot-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
DEFINE VARIABLE v-grd-tot-value AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
DEFINE VARIABLE v-tot-value     AS DECIMAL   FORMAT "->>,>>>,>>9.99".
DEFINE VARIABLE v-cum-tot       AS de.                                   
DEFINE VARIABLE v-tran-type     AS CHARACTER FORMAT "x(1)".      
DEFINE VARIABLE v-entrytype     AS CHARACTER INITIAL "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
DEFINE VARIABLE v-on            LIKE eb.num-up.
DEFINE VARIABLE v-qty-pallet    AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-whse          LIKE fg-rctd.loc.            
DEFINE VARIABLE v-one           AS INTEGER   FORMAT "->>,>>9" INIT 1.
DEFINE VARIABLE v-ftime         AS LOGICAL   INIT NO.
DEFINE VARIABLE v-dscr          LIKE account.dscr.
DEFINE VARIABLE v-disp-actnum   LIKE account.actnum.
DEFINE VARIABLE v-disp-amt      AS DECIMAL   FORMAT ">>,>>>,>>9.99cr".
DEFINE VARIABLE v-hdr           AS CHARACTER FORMAT "x(12)".
DEFINE VARIABLE v-postlst       AS cha       NO-UNDO.
DEFINE VARIABLE ll-wip          AS LOG       NO-UNDO.
/* DEFINE VARIABLE li              AS INTEGER   NO-UNDO. */
DEFINE VARIABLE li-loop         AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-time          AS CHARACTER FORMAT "X(5)" NO-UNDO.

DEFINE VARIABLE v-itm-lbl       AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-itm-dsh       AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-desc-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-Po-lbl        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-vend-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-desc-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-Po-dsh        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-vend-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-uom-lbl       AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-uom-dsh       AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-cstprt        AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-pr-tots2      LIKE v-pr-tots NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
PROCEDURE calc-partial:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.

        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = 0.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = 0.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * w-fg-rctd.partial / uom.mult.

                ELSE
                    v-fg-value = itemfg.sell-price * w-fg-rctd.partial / 1000.

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.partial * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.partial * itemfg.t-sqft)).

            v-msf[1] = w-fg-rctd.partial * itemfg.t-sqft.
        END.
    END. /* avail */


END PROCEDURE.

PROCEDURE calc-total:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.

        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / uom.mult).

                ELSE
                    v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / 1000).

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.t-qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - ((w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft)).

            v-msf[1] = (w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft.
        END.
    END. /* avail itemfg */


END PROCEDURE.

PROCEDURE orig:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.

        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / uom.mult.

                ELSE
                    v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / 1000.

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.t-qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.t-qty * itemfg.t-sqft)).

            v-msf[1] = w-fg-rctd.t-qty * itemfg.t-sqft.
        END.
    END. /* avail itemfg */

    ASSIGN
        v-msf[1] = v-msf[1] / 1000
        v-msf[2] = v-msf[2] / 1000.


END PROCEDURE.
