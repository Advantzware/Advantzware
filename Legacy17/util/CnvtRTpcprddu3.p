DEFINE BUFFER b-reftable FOR reftable.

FOR EACH reftable
    WHERE reftable.reftable EQ "pc/pcprddu3.p"
    NO-LOCK:
/*     FIND FIRST pc-prdd                                                             */
/*         WHERE RECID(pc-prdd) EQ INT(reftable.CODE)                                 */
/*         NO-LOCK NO-ERROR.                                                          */
/*     IF AVAIL pc-prdd THEN DO:                                                      */
/*         FIND b-reftable WHERE ROWID(b-reftable) EQ ROWID(reftable) EXCLUSIVE-LOCK. */
/*         b-reftable.CODE = pc-prdd.rec_key.                                         */
/*         FIND CURRENT b-reftable NO-LOCK.                                           */
/*     END.                                                                           */
    FIND FIRST fg-bin
        WHERE RECID(fg-bin) EQ INT(reftable.CODE2)
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN DO:
        FIND b-reftable WHERE ROWID(b-reftable) EQ ROWID(reftable) EXCLUSIVE-LOCK.
        b-reftable.CODE2 = fg-bin.rec_key.
        FIND CURRENT b-reftable NO-LOCK.
    END.
END.

MESSAGE "Reftable conversion process complete."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
