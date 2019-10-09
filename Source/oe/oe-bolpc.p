/* oe-bolpc.p                                               9/7/2012 */
/* Taken from oe-bolpc.i                                             */
/* Addresses the need to consider all tags when determining if p-c   */
/* of oe-boll                                                        */
/* Run from oe/oe-bolpc.i                                            */
DEF INPUT PARAMETER ipr-oe-boll AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcRunType AS CHAR NO-UNDO.  /* All or Single */
DEF BUFFER bf-oe-boll FOR oe-boll.
DEF BUFFER bf-upd-oe-boll FOR oe-boll.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
DEF BUFFER tmp-oe-boll FOR oe-boll.
DEF VAR v-sum-qty LIKE oe-boll.qty NO-UNDO.
DEF VAR v-ord-no LIKE oe-ordl.ord-no NO-UNDO.
DEF VAR v-i-no LIKE oe-ordl.i-no NO-UNDO.
DEF VAR v-company LIKE oe-boll.company  NO-UNDO.
DEF VAR v-p-c LIKE oe-boll.p-c NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR lFound AS LOG NO-UNDO.
DEF VAR dRelPost AS INT NO-UNDO.

FIND bf-oe-boll WHERE ROWID(bf-oe-boll) EQ ipr-oe-boll
                NO-LOCK NO-ERROR.
IF NOT AVAIL bf-oe-boll THEN
    RETURN.
dRelPost = 0.
v-ord-no = bf-oe-boll.ord-no.
v-i-no   = bf-oe-boll.i-no.
v-company = bf-oe-boll.company.
RUN sys/ref/nk1look.p (v-company, "RELPOST", "D", no, no, "", "", 
                          Output cReturn, output lFound).
IF lFound THEN
    dRelPost = DEC(cReturn).

FOR EACH bf-oe-boll WHERE bf-oe-boll.company EQ v-company
                      AND bf-oe-boll.ord-no  EQ v-ord-no
                      AND bf-oe-boll.i-no    EQ v-i-no
                      AND bf-oe-boll.p-c     EQ NO /*avoid switching C back to P -03101402*/
                      AND (IF ipcRunType EQ "All" THEN TRUE ELSE ROWID(bf-oe-boll) EQ ipr-oe-boll)
                    NO-LOCK.

    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ bf-oe-boll.company
          AND bf-oe-ordl.ord-no  EQ bf-oe-boll.ord-no
          AND bf-oe-ordl.i-no    EQ bf-oe-boll.i-no
        NO-ERROR.
    
    IF AVAIL bf-oe-ordl THEN DO:
        IF NOT dRelPost EQ 1 THEN DO:
            v-sum-qty = 0.
            FOR EACH tmp-oe-boll FIELDS(qty) NO-LOCK
                WHERE tmp-oe-boll.company EQ bf-oe-ordl.company
        		  AND tmp-oe-boll.ord-no  EQ bf-oe-ordl.ord-no
        		  AND tmp-oe-boll.i-no    EQ bf-oe-ordl.i-no 
                  AND tmp-oe-boll.line    EQ bf-oe-ordl.line
                  AND (tmp-oe-boll.rel-no LT bf-oe-boll.rel-no      OR
                     (tmp-oe-boll.rel-no EQ bf-oe-boll.rel-no AND
                      tmp-oe-boll.b-ord-no LE bf-oe-boll.b-ord-no))
        		  AND ROWID(tmp-oe-boll)  NE ROWID(bf-oe-boll)
                USE-INDEX ord-no:
                v-sum-qty = v-sum-qty + tmp-oe-boll.qty.
            END.
            v-p-c = bf-oe-boll.qty + v-sum-qty GE
                        (bf-oe-ordl.qty * (1 - (bf-oe-ordl.under-pct / 100))).
        END /*not iRelPost eq 1*/.
        ELSE v-p-c = YES.
        IF v-p-c NE bf-oe-boll.p-c THEN DO:
            FIND bf-upd-oe-boll WHERE ROWID(bf-upd-oe-boll) EQ ROWID(bf-oe-boll)
                              EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bf-upd-oe-boll THEN
                bf-upd-oe-boll.p-c = v-p-c.
            RELEASE bf-upd-oe-boll.
        END.
       
    END.
END.

