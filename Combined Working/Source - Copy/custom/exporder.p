/*custom/exporder.p */
DEF VAR v-complete AS cha NO-UNDO.
DEF VAR v-comma AS cha FORM "x" NO-UNDO.
DEF VAR li-prod-qty AS INT NO-UNDO.
DEF VAR v-ship-amt AS DEC NO-UNDO.
DEF VAR v-first-ship-dt AS date NO-UNDO.
DEF VAR v-last-ship-dt AS date NO-UNDO.
DEF VAR v-aging-dt AS date NO-UNDO.
DEF VAR v-ready-dt AS date NO-UNDO.
DEF VAR v-cust-dt AS date NO-UNDO.
DEF VAR li-qoh AS INT NO-UNDO.
DEF VAR v-onhand-amt AS DEC NO-UNDO.
DEF VAR v-cust-qty AS INT NO-UNDO.
DEF VAR v-rel-dt AS date NO-UNDO.
DEF VAR v-rel-qty AS INT NO-UNDO.
DEF VAR v-rel-po AS cha NO-UNDO.
DEF VAR v-cases-unit AS INT NO-UNDO.

DEF VAR cocode AS cha NO-UNDO.

FIND FIRST usercomp WHERE USER_id = "asi" AND company_default NO-LOCK NO-ERROR.
cocode = IF AVAIL usercomp THEN usercomp.company ELSE "".
{sys/inc/oeexport.i}

IF NOT oeexport-log THEN RETURN.


OUTPUT TO value(oeexport-char) /*c:\tmp\FCORINGTG.DAT.*/.

v-comma = "~t" /*","*/ .

FOR EACH oe-ord NO-LOCK WHERE oe-ord.company = cocode AND oe-ord.opened,
    EACH oe-ordl OF oe-ord NO-LOCK WHERE oe-ordl.opened:
    FIND FIRST itemfg WHERE itemfg.company = oe-ord.company
                        AND itemfg.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
    IF (AVAIL itemfg AND itemfg.procat = "ART") OR NOT AVAIL itemfg THEN NEXT.

    /*
    If the JOB is closed, but order is OPEN, then the flag will be COMPLETED = YES.
    If produced quantity is equal to or greater than order quantity then this will be COMPLETED = YES.
    If produced is less than the order quantity, then this will be NOT COMPLETED = NO.
    */
    
    ASSIGN li-prod-qty = 0
           li-qoh = 0.
    IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
    FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company   EQ cocode and
             fg-rcpth.job-no    EQ oe-ordl.job-no
        AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
        AND fg-rcpth.i-no      EQ oe-ordl.i-no
        AND fg-rcpth.rita-code EQ "R" USE-INDEX job,
      EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                         AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
      li-prod-qty = li-prod-qty + fg-rdtlh.qty.
    END.
    
    IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
       FIND FIRST job-hdr WHERE job-hdr.company = cocode
                            AND job-hdr.job-no = oe-ordl.job-no
                            AND job-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK NO-ERROR.
    IF AVAIL job-hdr AND NOT job-hdr.opened AND oe-ordl.opened THEN  v-complete = "YES".
    ELSE DO:
       IF li-prod-qty >= oe-ordl.qty THEN v-complete = "YES".
       ELSE v-complete = "NO".
    END.

    IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
    FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
        AND fg-bin.job-no  EQ oe-ordl.job-no
        AND fg-bin.job-no2 EQ oe-ordl.job-no2
        AND fg-bin.i-no    EQ oe-ordl.i-no:
        li-qoh = li-qoh + fg-bin.qty.
    END.

    ASSIGN v-first-ship-dt =  ?
           v-last-ship-dt = ?.
    /*
    FOR EACH ASI.oe-rel WHERE oe-rel.company EQ oe-ordl.company AND
                            oe-rel.ord-no  EQ oe-ordl.ord-no AND
                            oe-rel.i-no    EQ oe-ordl.i-no   AND
                            oe-rel.line    EQ oe-ordl.line NO-LOCK
                            BREAK BY OE-REL.rel-date :
        IF FIRST(oe-rel.rel-date) THEN 
           v-first-ship-dt = STRING(MONTH(oe-rel.rel-date)) +
                             STRING(DAY(oe-rel.rel-date)) +
                             STRING(year(oe-rel.rel-date)).
        IF last(oe-rel.rel-date) THEN 
           v-last-ship-dt = STRING(MONTH(oe-rel.rel-date)) +
                            STRING(DAY(oe-rel.rel-date)) +
                            STRING(year(oe-rel.rel-date)).

    END.
    */
    FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company   EQ cocode
                    AND fg-rcpth.job-no    EQ oe-ordl.job-no
                    AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                    AND fg-rcpth.i-no      EQ oe-ordl.i-no
                    AND fg-rcpth.rita-code EQ "S" 
                   BREAK BY fg-rcpth.trans-date:
        IF FIRST(fg-rcpth.trans-date) THEN v-first-ship-dt = fg-rcpth.trans-date.
        IF last(fg-rcpth.trans-date) THEN v-last-ship-dt = fg-rcpth.trans-date.
    END.

    v-aging-dt = ?.
    FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company   EQ cocode and
                        fg-rcpth.job-no    EQ oe-ordl.job-no
                    AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                    AND fg-rcpth.i-no      EQ oe-ordl.i-no
                    AND fg-rcpth.rita-code EQ "R" 
                   BREAK BY fg-rcpth.trans-date:
        v-aging-dt = /*STRING(MONTH(fg-rcpth.trans-date)) +
                     STRING(DAY(fg-rcpth.trans-date)) +
                     STRING(year(fg-rcpth.trans-date))*/ fg-rcpth.trans-date.
        LEAVE.
    END.

    ASSIGN v-ship-amt = oe-ordl.ship-qty * oe-ordl.price / 1000
           v-ready-dt = oe-ordl.prom-date
           v-cust-dt  = ?
           v-cust-qty = 0
           .
    v-cases-unit = 0 /*for now oe-ordl.cases-unit*/.

    FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company = cocode
                      AND fg-bin.job-no = oe-ordl.job-no
                      AND fg-bin.job-no2 = oe-ordl.job-no2
                      AND fg-bin.i-no = oe-ordl.i-no
                      AND fg-bin.cust-no = oe-ordl.cust-no NO-ERROR.
    v-cust-qty = IF AVAIL fg-bin THEN fg-bin.qty ELSE 0.
    IF v-cust-qty > 0 THEN 
       FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company   EQ cocode 
                    AND fg-rcpth.job-no    EQ oe-ordl.job-no
                    AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                    AND fg-rcpth.i-no      EQ oe-ordl.i-no
                    AND fg-bin.cust-no = oe-ordl.cust-no
                    AND fg-rcpth.rita-code EQ "R" 
                   BREAK BY fg-rcpth.trans-date:
            v-cust-dt = fg-rcpth.trans-date.
            LEAVE.
    END.

    v-onhand-amt = li-qoh * oe-ordl.price / 1000.
    PUT UNFORMATTED
        21800
        v-comma
        oe-ord.cust-name FORM "x(25)"
        v-comma
        oe-ord.cust-no FORM "x(15)"
        v-comma
        oe-ordl.i-no FORM "x(15)"
        v-comma
        oe-ordl.part-no FORM "x(20)"
        v-comma
        /*oe-ordl.part-dscr1 FORM "x(40)"*/
        itemfg.i-name FORM "x(40)"
        v-comma
        oe-ord.ord-no FORM ">>>>>>>>>9"
        v-comma
        oe-ordl.po-no FORM "x(30)"
        v-comma
        v-complete FORM "X(3)"  /* complete yes/no*/
        v-comma
        oe-ordl.qty FORM "->>>>>>>>9"
        v-comma
        oe-ordl.price FORM "->>>>9.99"
        v-comma
        oe-ordl.pr-uom
        v-comma
        oe-ordl.t-price FORM "->>>>>>9.99"
        v-comma
        li-prod-qty
        v-comma
        oe-ordl.ship-qty
        v-comma
        v-ship-amt FORM "->>>>>>9.99"
        v-comma
        v-first-ship-dt FORM "99/99/9999"
        v-comma
        v-last-ship-dt  FORM "99/99/9999"
        v-comma
        v-ready-dt   FORM "99/99/9999"
        v-comma
        li-qoh FORM "->>>>>>>9"
        v-comma
        v-onhand-amt FORM "->>>>>>9.99"
        v-comma
        v-aging-dt   FORM "99/99/9999"
        v-comma
        v-cases-unit FORM "->>>>>>>>9"  
        v-comma
        v-cust-qty FORM "->>>>>>>>9"
        v-comma
        v-cust-dt  FORM "99/99/9999"
        v-comma
        .

    FOR EACH ASI.oe-rel WHERE oe-rel.company EQ oe-ordl.company AND
        oe-rel.ord-no  EQ oe-ordl.ord-no AND
        oe-rel.i-no    EQ oe-ordl.i-no   AND
        oe-rel.line    EQ oe-ordl.line NO-LOCK:

        ASSIGN v-rel-dt = oe-rel.rel-date
               v-rel-qty = oe-rel.qty
               v-rel-po = oe-rel.po-no.
        PUT UNFORMATTED
          v-rel-dt FORM "99/99/9999" v-comma
          v-rel-qty FORM "->>>>>>>>9" v-comma
          v-rel-po FORM "x(15)"   v-comma
          .
    END.

    PUT SKIP.
END.

OUTPUT CLOSE.
