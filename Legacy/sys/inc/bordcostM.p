/* --------------------------------------------- sys/inc/bordcost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER v-job-no   LIKE job.job-no.
DEFINE INPUT  PARAMETER v-job-no2  LIKE job.job-no2.
DEFINE INPUT  PARAMETER v-i-no     LIKE itemfg.i-no.
DEFINE INPUT  PARAMETER  v-bol-no  LIKE oe-boll.bol-no.
DEFINE INPUT  PARAMETER v-qty      AS   INTEGER.
DEFINE INPUT  PARAMETER v-act-cost AS   LOGICAL.
DEFINE OUTPUT PARAMETER v-tot-cost AS   DECIMAL.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEFINE VARIABLE v-fgrec-qty  LIKE job-hdr.qty.
DEFINE VARIABLE v-cost       AS DECIMAL.
DEFINE VARIABLE lv-est-type  LIKE est.est-type.
DEFINE VARIABLE v-po-cost    AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-issued-qty AS INTEGER NO-UNDO .
DEFINE VARIABLE v-rec-qty    AS INTEGER NO-UNDO .
DEFINE VARIABLE v-sub-tot    AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-sub-qty    AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-req-qty    AS INTEGER NO-UNDO .


/* Find BOL for item. */
FOR EACH oe-boll NO-LOCK WHERE
    oe-boll.company EQ cocode AND
    oe-boll.bol-no EQ v-bol-no AND
    oe-boll.i-no EQ v-i-no :

    ASSIGN
        v-job-no    = oe-boll.job-no
        v-job-no2   = oe-boll.job-no2
        v-fgrec-qty = 0
        v-sub-tot   = 0
        v-req-qty   = 0
        v-sub-qty   = 0.


    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2
        NO-ERROR.

    IF AVAILABLE job THEN
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
              AND job-hdr.i-no    EQ v-i-no
              NO-ERROR.


    IF AVAILABLE job-hdr THEN
    DO:
        IF job.est-no NE "" THEN
            FIND FIRST est NO-LOCK
                WHERE est.company EQ job.company
                  AND est.est-no  EQ job.est-no
                  NO-ERROR.
        lv-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
        IF lv-est-type GT 4 THEN lv-est-type = lv-est-type - 4.

        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company   EQ cocode
              AND fg-rcpth.job-no    EQ job-hdr.job-no
              AND fg-rcpth.job-no2   EQ job-hdr.job-no2
              AND fg-rcpth.i-no      EQ job-hdr.i-no
              AND fg-rcpth.rita-code EQ "R",
             
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no:

            v-fgrec-qty = v-fgrec-qty + fg-rdtlh.qty.
        END.


        FOR EACH mat-act NO-LOCK
            WHERE mat-act.company EQ cocode
              AND mat-act.job     EQ job-hdr.job
              AND mat-act.job-no  EQ job-hdr.job-no
              AND mat-act.job-no2 EQ job-hdr.job-no2
              AND (mat-act.s-num  EQ job-hdr.frm OR lv-est-type EQ 2),

            FIRST job-mat NO-LOCK
            WHERE job-mat.company  EQ cocode
              AND job-mat.job      EQ mat-act.job
              AND job-mat.frm      EQ mat-act.s-num
              AND job-mat.blank-no EQ mat-act.b-num
              AND job-mat.i-no     EQ mat-act.i-no,

            FIRST ITEM NO-LOCK
            WHERE item.company EQ cocode
              AND item.i-no    EQ mat-act.i-no
              AND index("BA",item.mat-type) GT 0
            BREAK BY job-mat.i-no:

            FIND FIRST job-mch NO-LOCK
                 WHERE  job-mch.company EQ job.company
                   AND job-mch.job      EQ job.job
                   AND job-mch.job-no   EQ job.job-no
                   AND job-mch.job-no2  EQ job.job-no2
                 USE-INDEX line-idx NO-ERROR  .

            IF AVAILABLE job-mch  THEN
                v-req-qty = job-mch.run-qty .

            FOR EACH po-ordl NO-LOCK
                WHERE po-ordl.company   EQ cocode
                  AND po-ordl.i-no      EQ job-mat.i-no
                  AND po-ordl.job-no    EQ v-job-no
                  AND po-ordl.job-no2   EQ v-job-no2
                USE-INDEX item
                BY po-ordl.po-no DESCENDING:
                LEAVE.
            END.

            IF AVAILABLE po-ordl THEN
            DO:
                ASSIGN
                    v-po-cost    = po-ordl.cons-cost * 1000  /* po-ordl.t-cost */
                    v-issued-qty = po-ordl.t-rec-qty.

                v-sub-tot = v-sub-tot + (v-issued-qty / 1000 * v-po-cost ) .
                v-sub-qty = ( v-sub-tot / ( MIN( v-issued-qty / v-req-qty , 1) * job-hdr.qty ) * v-fgrec-qty ) .
                v-tot-cost = v-tot-cost + ( v-sub-qty / 1000 * oe-boll.qty ).
            END.
            ELSE
            DO:
                IF FIRST-OF(job-mat.i-no) THEN
                    ASSIGN v-tot-cost = v-tot-cost + oe-boll.qty / 1000 * job-mat.cost-m .
            END.
        END.  /* job-mat */
    END. /* avail job-hdr */
END.  /* for each oe-boll */



