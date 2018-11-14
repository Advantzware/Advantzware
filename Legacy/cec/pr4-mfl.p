/* -------------------------------------------------- cec/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-vend-no LIKE e-item-vend.vend-no.
DEFINE INPUT PARAMETER v-add-to-est AS LOG.
DEFINE INPUT PARAMETER hld-qty AS DECIMAL NO-UNDO.

DEFINE SHARED VARIABLE cocode      AS cha       NO-UNDO.
DEFINE SHARED VARIABLE locode      AS cha       NO-UNDO.
DEFINE SHARED VARIABLE qty         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE j           AS INTEGER   NO-UNDO.
DEFINE        VARIABLE zzz         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE tmpstore    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE CALL_id     AS RECID     NO-UNDO.
DEFINE        VARIABLE deShrinkPct AS DECIMAL   NO-UNDO.

DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{cec/print4.i shared shared}

DEFINE BUFFER b-qty   FOR reftable.
DEFINE BUFFER b-cost  FOR reftable.
DEFINE BUFFER b-setup FOR reftable.

DEFINE VARIABLE rm-wt$    AS de        NO-UNDO.
DEFINE VARIABLE rm-wt%    AS de        NO-UNDO.
DEFINE VARIABLE rm-wt     AS de        NO-UNDO.
DEFINE VARIABLE mfl$      AS de        FORMAT ">>>>9.99" NO-UNDO.
DEFINE VARIABLE med-qty   AS de        NO-UNDO. 
DEFINE VARIABLE cumul     AS de        NO-UNDO.
DEFINE VARIABLE prev-nsh  AS de        NO-UNDO.
DEFINE VARIABLE prev-mach AS ch        NO-UNDO.
DEFINE VARIABLE mqty      AS de        NO-UNDO.
DEFINE VARIABLE m-waste   AS de        NO-UNDO.
DEFINE VARIABLE m-spo     AS de        NO-UNDO.
DEFINE VARIABLE v-on-f    AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-qty     LIKE med-qty NO-UNDO.
DEFINE VARIABLE vuom      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gqty      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE gcost     AS DECIMAL   NO-UNDO.

DEFINE TEMP-TABLE w-brd NO-UNDO LIKE brd 
    FIELD rm-qty LIKE v-qty.

DEFINE VARIABLE tot-qty      LIKE w-brd.qty NO-UNDO.
DEFINE VARIABLE tot-cst      LIKE w-brd.cost NO-UNDO.
DEFINE VARIABLE tot-c-m      LIKE w-brd.cost-m NO-UNDO.
DEFINE VARIABLE ld-rm-rate   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vqty         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-liner-qty  LIKE w-brd.qty NO-UNDO.
DEFINE VARIABLE v-medium-qty LIKE w-brd.qty NO-UNDO.
DEFINE VARIABLE v-n-out      AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

{cec/msfcalc.i}

{cec/rollfac.i}

FIND FIRST ce-ctrl {sys/look/ce-ctrlW.i} NO-LOCK NO-ERROR.

FIND FIRST est-op
    WHERE est-op.company EQ xest.company
    AND est-op.est-no  EQ xest.est-no
    AND est-op.qty     EQ v-op-qty
    AND est-op.line    GT 500
    AND est-op.dept    EQ "CR"
    NO-LOCK.

RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).

ASSIGN
    tot-qty = qty /* / xeb.num-up / v-on-f */
    mqty    = tot-qty - r-spo[xef.form-no] - spo
    m-spo   = r-spo[xef.form-no]
    m-waste = spo.
 
{sys/inc/roundup.i mqty}

item-bom-loop:
REPEAT:

    FIND NEXT item-bom NO-LOCK WHERE
        item-bom.company  EQ xef.company AND
        item-bom.parent-i EQ xef.board AND
        item-bom.line# LT 9
        NO-ERROR.

    IF NOT AVAIL item-bom THEN LEAVE item-bom-loop.

    /*** if xef.medium ne "" then  ***/ /* CTS */
    /** even lines are medium types **/
    IF (item-bom.line# MOD 2) EQ 0 THEN
    DO WITH NO-BOX NO-LABELS FRAME med1 STREAM-IO:
   
        FIND FIRST ITEM NO-LOCK 
            {sys/look/itemW.i} AND
            item.i-no = item-bom.i-no 
            NO-ERROR.
        IF AVAILABLE item THEN
            FIND FIRST e-item OF item 
            NO-LOCK NO-ERROR.
        IF NOT AVAIL item THEN LEAVE.

        ASSIGN
            mfl$         = 0
            b-msh        = 0
            v-medium-qty = 0
            adh-qty[1]   = 1.

        RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

        IF xeb.num-up * v-n-out NE 0 THEN
            v-medium-qty = hld-qty / (xeb.num-up * v-n-out).

        FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

        ASSIGN
            deShrinkPct = IF item-bom.shrink NE 100 THEN item-bom.shrink ELSE 0
            med-qty = IF v-corr THEN ((brd-l[3] * brd-w[3]) * v-medium-qty / (1 - (deShrinkPct / 100))) * .000007
                      ELSE ((brd-l[3] * brd-w[3]) * v-medium-qty / (1 - (deShrinkPct / 100))) / 144000
            fg-wt   = fg-wt + ((fg-qty / (1 - (item-bom.shrink / 100))) * item.basis-w)
            b-uom = IF AVAILABLE e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                    ELSE item.cons-uom.

        IF b-uom EQ "LF" THEN ASSIGN
            v-qty = (IF v-corr THEN (med-qty / .000007) ELSE (med-qty * 144000)) / brd-w[3] / 12.
        ELSE
            IF b-uom EQ "TON" THEN
                v-qty = med-qty * item.basis-w / 2000.
            ELSE
                IF b-uom EQ "MSF" THEN
                    v-qty = med-qty.
                ELSE
                    v-qty = med-qty * item.basis-w.

        {est/matcost.i v-qty mfl$ medium}

        CREATE w-brd.
        ASSIGN 
            b-msh          = mfl$
            mfl$           = (b-msh * v-qty) + lv-setup-medium
            w-brd.form-no  = xef.form-no
            w-brd.blank-no = 1
            w-brd.i-no     = item-bom.i-no
            w-brd.dscr     = xef.medium
            w-brd.basis-w  = item.basis-w
            w-brd.len      = brd-l[3] / (1 - (item-bom.shrink / 100))
            w-brd.wid      = brd-w[3]
            w-brd.cost     = mfl$
            w-brd.qty      = qty
            w-brd.cost-m   = v-qty
            w-brd.qty-uom  = "EA"
            w-brd.sc-uom   = b-uom.

        IF v-add-to-est THEN 
        DO:
            ASSIGN
                dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty ) * m-waste)
                dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000))
                dm-tot[5] = dm-tot[5] + mfl$
                /* add run spoil */
                dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000))
                dm-tot[5] = dm-tot[5] + ( m-spo * (mfl$ / mqty) ).

            DISPLAY
                item.i-name FORMAT "x(20)"
                item.basis-w 
                WHEN avail(item) TO 27
                item.cal 
                WHEN avail(item) FORMAT ">9.999<" TO 35
                "$" + string(b-msh,">>>9.99") TO 48 b-uom SPACE(0)
                lv-setup-medium 
                WHEN lv-setup-medium NE 0 FORMAT ">>>9.99" TO 59
                mfl$ / (save-qty / 1000) / v-sqft-fac FORMAT ">>>>9.99" TO 68
                mfl$ FORMAT ">>>>,>>9.99" TO 80 SKIP
                "Medium MR  Waste"
                m-waste FORMAT ">>>>>>9" TO 48 "Sht" SPACE(0)
                (mfl$ / mqty ) * m-waste FORMAT ">>>9.99" TO 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         "Medium RUN Waste"
         m-spo format ">>>>>>9" to 48 "Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
                                              format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty ) format ">>>>,>>9.99" to 80 skip.

            /* rm handling chg per cwt*/
            ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
            IF ld-rm-rate GT 0 THEN
                ASSIGN
                    rm-wt    = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
                          ((m-spo * brd-sf[3]) / 1000)) *
                          (IF avail(item) THEN item.basis-w ELSE 1)
                    rm-wt$   = (rm-wt / 100) * ld-rm-rate
                    ctrl2[3] = ctrl2[3] + rm-wt$.
     
        END.
    END.

    /*** if xef.flute ne "" then ***/
    /*** Odd number lines are liners **/
    IF (item-bom.line# MOD 2) NE 0 THEN
    DO WITH NO-BOX NO-LABELS FRAME flute STREAM-IO:
        ASSIGN
            mfl$        = 0
            b-msh       = 0
            v-liner-qty = 0.

        FIND FIRST item {sys/look/itemW.i} AND
                     item.i-no = item-bom.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN
            FIND FIRST e-item OF item NO-LOCK NO-ERROR.
        IF NOT(avail(item)) THEN LEAVE.

        RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

        IF xeb.num-up * v-n-out NE 0 THEN
            v-liner-qty = hld-qty / (xeb.num-up * v-n-out).

        FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

        ASSIGN
            adh-qty[2]  = 1
            deShrinkPct = IF item-bom.shrink NE 100 THEN item-bom.shrink ELSE 0
            med-qty     = IF v-corr THEN ((brd-l[3] * brd-w[3]) * v-liner-qty / (1 - (deShrinkPct / 100))) * .000007
                ELSE ((brd-l[3] * brd-w[3]) * v-liner-qty / (1 - (deShrinkPct / 100))) / 144000                    
            fg-wt       = fg-wt + (fg-qty * (IF avail(item) THEN item.basis-w ELSE 1))
            b-uom = IF AVAILABLE e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                    ELSE item.cons-uom.

        IF b-uom EQ "LF"  THEN
            v-qty = (IF v-corr THEN (med-qty / .000007)
            ELSE (med-qty * 144000)) / brd-w[3] / 12.
        ELSE
            IF b-uom EQ "TON" THEN
                v-qty = med-qty * item.basis-w / 2000.
            ELSE
                IF b-uom EQ "MSF" THEN
                    v-qty = med-qty.
                ELSE
                    v-qty = med-qty * item.basis-w.

        {est/matcost.i v-qty mfl$ flute}

        ASSIGN
            b-msh = mfl$
            mfl$  = (b-msh * v-qty) + lv-setup-flute.

        CREATE w-brd.
        ASSIGN 
            w-brd.form-no  = xef.form-no
            w-brd.blank-no = 1
            w-brd.i-no     = item-bom.i-no
            w-brd.dscr     = xef.medium
            w-brd.basis-w  = item.basis-w
            w-brd.len      = brd-l[3] / (1 - (item-bom.shrink / 100))
            w-brd.wid      = brd-w[3]
            w-brd.cost     = mfl$
            w-brd.qty      = qty
            w-brd.cost-m   = v-qty
            w-brd.qty-uom  = "EA"
            w-brd.sc-uom   = b-uom
            /*mfl$ = mfl$ / qty * mqty*/
            tmpstore       = "Liner ".

        IF v-add-to-est THEN 
        DO:
            ASSIGN
                dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste )
                dm-tot[4] = dm-tot[4] + (mfl$ / (save-qty / 1000))
                dm-tot[5] = dm-tot[5] + mfl$
                /* add run spoil */
                dm-tot[4] = dm-tot[4] + ( (m-spo * (mfl$ / mqty)) / (save-qty / 1000))
                dm-tot[5] = dm-tot[5] + (m-spo * (mfl$ / mqty)).
      
            DISPLAY
                item.i-name FORMAT "x(20)"
                item.basis-w 
                WHEN avail(item) TO 27
                item.cal 
                WHEN avail(item) FORMAT ">9.999<" TO 35
                "$" + string(b-msh,">>>9.99") TO 48 b-uom SPACE(0)
                lv-setup-flute 
                WHEN lv-setup-flute NE 0 FORMAT ">>>9.99" TO 59
                mfl$ / (save-qty / 1000) / v-sqft-fac FORMAT ">>>>9.99" TO 68
                mfl$ FORMAT ">>>>,>>9.99" TO 80 SKIP
                tmpstore + "MR  Waste" FORMAT "x(15)"
                m-waste FORMAT ">>>>>>9" TO 48 SPACE(0) " Sht" SPACE(0)
                (mfl$ / mqty) * m-waste FORMAT ">>>9.99" TO 59
         /* cost of waste amortized per 1000 of all blanks on this form */
         ((mfl$ / mqty) * m-waste) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         (mfl$ / mqty) * m-waste format ">>>>,>>9.99" to 80 skip
         tmpstore + "RUN Waste" format "x(15)"
         m-spo format ">>>>>>9" to 48 space(0) " Sht" space(0)
         /* cost of spoil amortized per 1000 of all blanks on this form */
         (m-spo * (mfl$ / mqty)) / (save-qty / 1000) / v-sqft-fac
            format ">>>>9.99" to 68
         m-spo * (mfl$ / mqty) format ">>>>,>>9.99" to 80 skip.

            /* rm handling chg per cwt*/
            ld-rm-rate = IF xeb.pur-man THEN rm-rate-f ELSE ctrl[3].
            IF ld-rm-rate GT 0 THEN
                ASSIGN
                    rm-wt    = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
                              ((m-spo * brd-sf[3]) / 1000)) *
                              (IF avail(item) THEN item.basis-w ELSE 1)
                    rm-wt$   = (rm-wt / 100) * ld-rm-rate
                    ctrl2[3] = ctrl2[3] + rm-wt$.
        END. /*end v-add-to-est*/
    END. /*end frame flute*/

END. /*end repeat*/

IF v-add-to-est AND
    xef.adh-code NE "" THEN 
DO WITH NO-BOX NO-LABELS FRAME adh-frame STREAM-IO:
    FIND FIRST item WHERE
        item.company = cocode AND
        item.i-no    = xef.adh-code
        NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
    DO:
        vuom = item.cons-uom.

        IF ITEM.sqin-lb NE 0 THEN
            gqty = xef.adh-sqin * est-op.num-sh / item.sqin-lb.

        FIND FIRST e-item OF item NO-LOCK NO-ERROR.

        IF AVAILABLE e-item THEN
        DO:
            EMPTY TEMP-TABLE tt-ei.
            CREATE tt-ei.
            DO j = 1 TO 10:
                ASSIGN
                    tt-ei.run-qty[j]  = e-item.run-qty[j]
                    tt-ei.run-cost[j] = e-item.run-cost[j].
            END.
         
            FIND FIRST b-qty WHERE
                b-qty.reftable = "blank-vend-qty" AND
                b-qty.company = e-item.company AND
                b-qty.CODE    = e-item.i-no
                NO-LOCK NO-ERROR.
         
            IF AVAILABLE b-qty THEN
            DO:
                FIND FIRST b-cost WHERE
                    b-cost.reftable = "blank-vend-cost" AND
                    b-cost.company = e-item.company AND
                    b-cost.CODE    = e-item.i-no
                    NO-LOCK NO-ERROR.
         
                DO j = 1 TO 10:
                    ASSIGN
                        tt-ei.run-qty[j + 10]  = b-qty.val[j]
                        tt-ei.run-cost[j + 10] = b-cost.val[j].
                END.
            END.
      
            DO j = 1 TO 20:
                IF tt-ei.run-qty[j] LT gqty THEN NEXT.
                gcost = gqty * tt-ei.run-cost[j].
                IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
                LEAVE.
            END.
        END.

        ELSE gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
            ELSE item.last-cost.

        ASSIGN
            dm-tot[4] = dm-tot[4] + (gcost / (save-qty / 1000) / v-sqft-fac)
            dm-tot[5] = dm-tot[5] + gcost.

        /* rm handling chg per cwt */
        IF xeb.pur-man THEN
            IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * rm-rate-f).
            ELSE.
        ELSE
            IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * ctrl[3]).
     
        /* rm handling pct. */
        IF xeb.pur-man THEN
            IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * hand-pct-f).
            ELSE.
        ELSE
            IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * ctrl[2]).

        ASSIGN
            vqty = STRING(gqty,">>>>>9.99")
            vqty = FILL(" ",9 - length(TRIM(vqty))) + trim(vqty).

        DISPLAY item.i-name
            vqty                     FORMAT "x(9)"           TO 48
            vuom
            gcost / (save-qty / 1000 / v-sqft-fac) FORMAT ">>>>9.99"  TO 68
            gcost                     FORMAT ">,>>>,>>9.99"   TO 80 SKIP.
    END.
END.

IF v-add-to-est AND
    xef.lam-code NE "" THEN 
DO WITH NO-BOX NO-LABELS FRAME lam-frame STREAM-IO:
    FIND FIRST item WHERE
        item.company = cocode AND
        item.i-no    = xef.lam-code
        NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN
    DO:
        ASSIGN
            vuom  = item.cons-uom
            gcost = 0.

        IF ITEM.sqin-lb NE 0 THEN
            gqty = xef.gsh-wid * xef.gsh-len * est-op.num-sh / item.sqin-lb.
        ELSE
            gqty = 0.

        FIND FIRST e-item OF item NO-LOCK NO-ERROR.

        IF AVAILABLE e-item THEN
        DO:
      
            EMPTY TEMP-TABLE tt-ei.
            CREATE tt-ei.
            DO j = 1 TO 10:
                ASSIGN
                    tt-ei.run-qty[j]  = e-item.run-qty[j]
                    tt-ei.run-cost[j] = e-item.run-cost[j].
            END.
         
            FIND FIRST b-qty WHERE
                b-qty.reftable = "blank-vend-qty" AND
                b-qty.company = e-item.company AND
                b-qty.CODE    = e-item.i-no
                NO-LOCK NO-ERROR.
         
            IF AVAILABLE b-qty THEN
            DO:
                FIND FIRST b-cost WHERE
                    b-cost.reftable = "blank-vend-cost" AND
                    b-cost.company = e-item.company AND
                    b-cost.CODE    = e-item.i-no
                    NO-LOCK NO-ERROR.
         
                DO j = 1 TO 10:
                    ASSIGN
                        tt-ei.run-qty[j + 10]  = b-qty.val[j]
                        tt-ei.run-cost[j + 10] = b-cost.val[j].
                END.
            END.

            DO j = 1 TO 20:
                IF tt-ei.run-qty[j] LT gqty THEN NEXT.
                gcost = gqty * tt-ei.run-cost[j].
                IF e-item.std-uom NE "" THEN vuom = e-item.std-uom.
                LEAVE.
            END.
        END.
        ELSE gcost = gqty * IF ce-ctrl.r-cost THEN item.avg-cost
            ELSE item.last-cost.

        ASSIGN
            dm-tot[4] = dm-tot[4] + (gcost / (save-qty / 1000) / v-sqft-fac)
            dm-tot[5] = dm-tot[5] + gcost.

        /* rm handling chg per cwt */
        IF xeb.pur-man THEN
            IF rm-rate-f NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * rm-rate-f).
            ELSE.
        ELSE
            IF ctrl[3] NE 0 THEN ctrl2[3] = ctrl2[3] + ((gqty / 100) * ctrl[3]).
     
        /* rm handling pct. */
        IF xeb.pur-man THEN
            IF hand-pct-f NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * hand-pct-f).
            ELSE.
        ELSE
            IF ctrl[2] NE 0 THEN ctrl2[2] = ctrl2[2] + (gcost * ctrl[2]).

        ASSIGN
            vqty = STRING(gqty,">>>>>9.99")
            vqty = FILL(" ",9 - length(TRIM(vqty))) + trim(vqty).

        DISPLAY item.i-name
            vqty  FORMAT "x(9)" TO 48
            vuom
            gcost / (save-qty / 1000 / v-sqft-fac) FORMAT ">>>>9.99" TO 68
            gcost FORMAT ">,>>>,>>9.99"   TO 80 SKIP.
    END.
END.

tot-qty = 0.

FOR EACH w-brd BREAK BY w-brd.form-no BY w-brd.i-no BY w-brd.len BY w-brd.wid:
    ASSIGN
        tot-qty = tot-qty + w-brd.qty
        tot-cst = tot-cst + w-brd.cost
        tot-c-m = tot-c-m + w-brd.cost-m.

    IF LAST-OF(w-brd.wid) THEN 
    DO:
        CREATE brd.
        BUFFER-COPY w-brd TO brd
            ASSIGN
            brd.cost   = tot-cst / tot-c-m
            brd.cost-m = tot-cst / (save-qty / 1000)
            brd.qty    = tot-qty
            tot-qty    = 0
            tot-cst    = 0
            tot-c-m    = 0.
    END.
END.

call_id = IF avail(item) THEN RECID(item) ELSE ?.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
