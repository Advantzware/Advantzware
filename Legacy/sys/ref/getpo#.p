/* ------------------------------------------------------ sys/ref/getpo#.p JLF */
/* Find PO# for Order or Job Line Items                                        */
/* --------------------------------------------------------------------------- */

DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-form-no LIKE po-ordl.s-num NO-UNDO.
DEF OUTPUT PARAM op-po-no LIKE po-ordl.po-no NO-UNDO.

DEF VAR v-ord-no LIKE oe-ordl.ord-no NO-UNDO.


ASSIGN
 op-po-no = 0
 v-ord-no = 0.

FIND oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  ASSIGN
   v-ord-no = oe-ordl.ord-no
   op-po-no = oe-ordl.po-no-po.

  FIND FIRST job
      WHERE job.company EQ oe-ordl.company
        AND job.job-no  EQ oe-ordl.job-no
        AND job.job-no2 EQ oe-ordl.job-no2
      NO-LOCK NO-ERROR.  
END.

ELSE FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job THEN
FOR EACH job-mat
    WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
      AND job-mat.frm     EQ ip-form-no
    NO-LOCK,

    FIRST item
    WHERE item.company  EQ job.company
      AND item.i-no     EQ job-mat.rm-i-no
      AND INDEX("1234BPR",item.mat-type) GT 0
    NO-LOCK,

    FIRST job-hdr OF job NO-LOCK
    
    BY job-mat.blank-no
    BY job-mat.rm-i-no:

  op-po-no = job-mat.po-no.
  LEAVE.
END.

IF op-po-no EQ 0 THEN
FOR EACH po-ordl
    WHERE po-ordl.company   EQ job.company
      AND po-ordl.job-no    EQ job.job-no
      AND po-ordl.job-no2   EQ job.job-no2
      AND (po-ordl.s-num    EQ ip-form-no OR po-ordl.s-num EQ ?)
      AND po-ordl.item-type EQ YES
      AND LOOKUP(po-ordl.stat,"O,P,U") GT 0
    USE-INDEX job-no NO-LOCK,

    FIRST po-ord WHERE
    po-ord.company EQ po-ordl.company AND
    po-ord.po-no   EQ po-ordl.po-no AND
    LOOKUP(po-ord.stat,"N,O,R,U") GT 0
    NO-LOCK,

    FIRST item
    WHERE item.company  EQ po-ordl.company
      AND item.i-no     EQ po-ordl.i-no
      AND INDEX("1234BPR",item.mat-type) GT 0
    NO-LOCK:

  op-po-no = po-ordl.po-no.
  LEAVE.
END.
