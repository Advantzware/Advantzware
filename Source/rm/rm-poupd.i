/* -------------------------------------------------- rm/rm-poupd.i 03/97 JLF */
/* raw materials - post transactions - update po                              */
/* -------------------------------------------------------------------------- */

IF int(rm-rctd.po-no) NE 0 THEN
update-po{1}: DO.
    /* Check to see if po entered through the purchasing module. If
       so, verify all charaters are number type because po number
       in purchasing is a integer field. */
    DO i = 1 TO LENGTH(rm-rctd.po-no):
        IF ASC(SUBSTRING(rm-rctd.po-no,i,1)) LT 48 OR
            asc(SUBSTRING(rm-rctd.po-no,i,1)) GT 57 THEN
            LEAVE update-po{1}.
    END.
    FIND FIRST po-ord EXCLUSIVE-LOCK
        WHERE po-ord.company EQ item.company
          AND po-ord.po-no   EQ int(rm-rctd.po-no)
        NO-WAIT NO-ERROR.
    IF NOT AVAILABLE po-ord AND LOCKED po-ord THEN DO:
        MESSAGE " Purchase Order Record " rm-rctd.po-no
            "is in use.  Can Not Update..."
            VIEW-AS ALERT-BOX.
        /* undo transblok, next transblok. */
        UNDO, NEXT.
    END.
    v-recid = ?.
    FOR EACH po-ordl NO-LOCK 
        WHERE po-ordl.company   EQ item.company
          AND po-ordl.i-no      EQ rm-rctd.i-no
          AND po-ordl.po-no     EQ int(rm-rctd.po-no)
          AND po-ordl.deleted   EQ NO
          AND po-ordl.item-type EQ YES
          AND po-ordl.job-no    EQ rm-rctd.job-no
          AND po-ordl.job-no2   EQ rm-rctd.job-no2
        USE-INDEX item-ordno
        BREAK BY po-ordl.s-num DESCENDING:
        v-recid = RECID(po-ordl).  
        IF LAST(po-ordl.s-num) OR po-ordl.s-num EQ rm-rctd.s-num THEN LEAVE.
    END.
    FIND po-ordl WHERE RECID(po-ordl) EQ v-recid EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAILABLE po-ordl THEN DO:
        ld-cvt-qty = rm-rctd.qty.
        IF rm-rctd.pur-uom NE po-ordl.cons-uom THEN
            RUN sys/ref/convquom.p (rm-rctd.pur-uom, po-ordl.cons-uom,
                item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                ld-cvt-qty, OUTPUT ld-cvt-qty).
        ASSIGN
            po-ord.received   = YES
            po-ordl.t-rec-qty = po-ordl.t-rec-qty + ld-cvt-qty
            .
        RUN rm/polclose.p (ROWID(po-ordl), rm-rctd.qty, rm-rctd.pur-uom).
        FIND CURRENT po-ordl EXCLUSIVE.
    END.  
    ELSE IF NOT AVAILABLE po-ordl THEN DO:
        IF LOCKED po-ordl THEN DO:
            MESSAGE " Purchase Order Line Record is in use.  Can Not Update..."
                VIEW-AS ALERT-BOX.
            /*    undo transblok, next transblok.*/
            UNDO, NEXT.
        END.
        ELSE DO:
            MESSAGE "Purchase Order Line Record not found.  Can Not Update..."
            VIEW-AS ALERT-BOX.
            UNDO, NEXT.
        END.
    END.
END. /* update-po{1} */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
