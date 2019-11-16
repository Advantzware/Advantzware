
DEF INPUT PARAM v-recid    AS RECID.
DEF INPUT PARAM v-new-item AS CHAR.

{sys/inc/var.i SHARED}

DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-item FOR item.


FIND item WHERE RECID(item) EQ v-recid NO-ERROR.
IF NOT AVAIL item THEN RETURN.

IF v-new-item EQ "!" THEN
  v-new-item = CAPS(item.i-no).
ELSE
  FIND FIRST b-item
      WHERE b-item.company EQ item.company
        AND b-item.i-no    EQ v-new-item
      NO-LOCK NO-ERROR.

FOR EACH ap-invl
    WHERE ap-invl.company   EQ item.company
      AND ap-invl.item-no   EQ item.i-no
      AND ap-invl.item-type EQ YES
    NO-LOCK:

  {rm/updrmitm.i ap-invl item-no NO}
END.

IF NOT CAN-FIND(FIRST e-item
                WHERE e-item.company EQ item.company
                  AND e-item.i-no    EQ v-new-item) THEN
FOR EACH e-item
    WHERE e-item.company EQ item.company
      AND e-item.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i e-item i-no NO}
  LEAVE.
END.

FOR EACH e-item-vend
    WHERE e-item-vend.company EQ item.company
      AND e-item-vend.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i e-item-vend i-no NO}
END.

FOR EACH e-item-cust
    WHERE e-item-cust.company   EQ item.company
      AND e-item-cust.i-no      EQ item.i-no
      AND e-item-cust.item-type EQ YES
    NO-LOCK:

  {rm/updrmitm.i e-item-cust i-no NO}
END.

FOR EACH eb WHERE eb.company  EQ item.company NO-LOCK:
  IF eb.adhesive EQ item.i-no THEN DO:
    {rm/updrmitm.i eb adhesive NO 1}
  END.

  DO li = 1 TO EXTENT(eb.i-code):
    IF eb.i-code[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i eb i-code[li] NO 2}
    END.
  END.

  DO li = 1 TO EXTENT(eb.i-code2):
    IF eb.i-code2[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i eb i-code2[li] NO 3}
    END.
  END.

  IF eb.cas-no EQ item.i-no THEN DO:
    {rm/updrmitm.i eb cas-no NO 4}
  END.

  IF eb.tr-no EQ item.i-no THEN DO:
    {rm/updrmitm.i eb tr-no NO 5}
  END.
END.

FOR EACH ef WHERE ef.company EQ item.company NO-LOCK:
  IF ef.board EQ item.i-no THEN DO:
    {rm/updrmitm.i ef board NO 1}
  END.

  IF ef.medium EQ item.i-no THEN DO:
    {rm/updrmitm.i ef medium NO 2}
  END.

  IF ef.flute EQ item.i-no THEN DO:
    {rm/updrmitm.i ef flute NO 3}
  END.

  IF ef.lam-code EQ item.i-no THEN DO:
    {rm/updrmitm.i ef lam-code NO 4}
  END.

  IF ef.adh-code EQ item.i-no THEN DO:
    {rm/updrmitm.i ef adh-code NO 5}
  END.

  DO li = 1 TO EXTENT(ef.adder):
    IF ef.adder[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i ef adder[li] NO 6}
    END.
  END.

  DO li = 1 TO EXTENT(ef.leaf):
    IF ef.leaf[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i ef leaf[li] NO 7}
    END.
  END.

  DO li = 1 TO EXTENT(ef.spec-no):
    IF ef.spec-no[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i ef spec-no[li] NO 8}
    END.
  END.

  DO li = 1 TO EXTENT(ef.mis-rm-i-no):
    IF ef.mis-rm-i-no[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i ef mis-rm-i-no[li] NO 9}
    END.
  END.
END.

FOR EACH style WHERE style.company EQ item.company NO-LOCK:
  DO li = 1 TO EXTENT(style.material):
    IF style.material[li] EQ item.i-no THEN DO:
      {rm/updrmitm.i style material[li] NO}
    END.
  END.
END.

FOR EACH cust WHERE cust.company EQ item.company NO-LOCK:
  IF cust.pallet EQ item.i-no THEN DO:
    {rm/updrmitm.i cust pallet NO 1}
  END.

  IF cust.case-bundle EQ item.i-no THEN DO:
    {rm/updrmitm.i cust case-bundle NO 2}
  END.
END.

FOR EACH ce-ctrl WHERE ce-ctrl.company EQ item.company NO-LOCK:
  IF ce-ctrl.def-ink EQ item.i-no THEN DO:
    {rm/updrmitm.i ce-ctrl def-ink NO 1}
  END.
        
  IF ce-ctrl.def-coat EQ item.i-no THEN DO:
    {rm/updrmitm.i ce-ctrl def-coat NO 2}
  END.

  IF ce-ctrl.def-case EQ item.i-no THEN DO:
    {rm/updrmitm.i ce-ctrl def-case NO 3}
  END.
        
  IF ce-ctrl.def-pal EQ item.i-no THEN DO:
    {rm/updrmitm.i ce-ctrl def-pal NO 4}
  END.
END.

FOR EACH est-flm
    WHERE est-flm.company EQ item.company
      AND est-flm.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i est-flm i-no NO}
END.

IF TRIM(item.i-no) NE "" THEN
FOR EACH rm-bin
    WHERE rm-bin.company EQ item.company
      AND rm-bin.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i rm-bin i-no YES}
END.

FOR EACH loadtag
    WHERE loadtag.company   EQ item.company
      AND loadtag.i-no      EQ item.i-no
      AND loadtag.item-type EQ YES
    USE-INDEX i-no NO-LOCK:

  {rm/updrmitm.i loadtag i-no NO}
END.

FOR EACH wiptag
    WHERE wiptag.company   EQ item.company
      AND wiptag.rm-i-no      EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i wiptag rm-i-no NO}
END.

FOR EACH rm-rcpt
    WHERE rm-rcpt.company EQ item.company
      AND rm-rcpt.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i rm-rcpt i-no NO}
END.

FOR EACH rm-rcpth
    WHERE rm-rcpth.company EQ item.company
      AND rm-rcpth.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i rm-rcpth i-no NO}
END.

FOR EACH rm-rdtlh WHERE
    rm-rdtlh.company EQ ITEM.company AND
    rm-rdtlh.i-no EQ ITEM.i-no
    NO-LOCK:

    {rm/updrmitm.i rm-rdtlh i-no NO}
END.

FOR EACH rm-rctd
    WHERE rm-rctd.company EQ item.company
      AND rm-rctd.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i rm-rctd i-no NO}
END.

FOR EACH rm-receipts
    WHERE rm-receipts.company EQ item.company
      AND rm-receipts.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i rm-receipts i-no NO}
END.

FOR EACH job-mat
    WHERE job-mat.company EQ item.company
      AND job-mat.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i job-mat i-no NO 1}
END.

FOR EACH job-mat
    WHERE job-mat.company EQ item.company
      AND job-mat.rm-i-no EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i job-mat rm-i-no NO 2}
END.

FOR EACH job-brd
    WHERE job-brd.company EQ item.company
      AND job-brd.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i job-brd i-no NO}
END.

FOR EACH mat-act
    WHERE mat-act.company EQ item.company
      AND mat-act.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i mat-act i-no NO 1}
END.

FOR EACH mat-act
    WHERE mat-act.company EQ item.company
      AND mat-act.rm-i-no EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i mat-act rm-i-no NO 2}
END.

FOR EACH po-ordl
    WHERE po-ordl.company   EQ item.company
      AND po-ordl.i-no      EQ item.i-no
      AND po-ordl.item-type EQ YES
    NO-LOCK:

  FOR EACH po-all
      WHERE po-all.company EQ item.company
        AND po-all.po-no   EQ po-ordl.po-no
        AND po-all.line    EQ po-ordl.line
        AND po-all.i-no    EQ po-ordl.i-no:

    {rm/updrmitm.i po-all i-no NO}
  END.

  FOR EACH po-rcpts
      WHERE po-rcpts.company EQ item.company
        AND po-rcpts.po-no   EQ trim(string(po-ordl.po-no,">>>>>>>>>9"))
        AND po-rcpts.line    EQ po-ordl.line
        AND po-rcpts.i-no    EQ po-ordl.i-no:

    {rm/updrmitm.i po-rcpts i-no NO}
  END.

  {rm/updrmitm.i po-ordl i-no NO}
END.

FOR EACH item-bom
    WHERE item-bom.company  EQ item.company
      AND item-bom.parent-i EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i item-bom parent-i YES 1}
END.

FOR EACH item-bom
    WHERE item-bom.company EQ item.company
      AND item-bom.i-no    EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i item-bom i-no NO 2}
END.

FOR EACH item-spec
    WHERE item-spec.company   EQ item.company
      AND item-spec.i-no      EQ item.i-no
      AND item-spec.item-type EQ YES
    NO-LOCK:

  {rm/updrmitm.i item-spec i-no NO 2}
END.

FOR EACH itemfg-ink
    WHERE itemfg-ink.company EQ item.company
      AND itemfg-ink.rm-i-no EQ item.i-no
    NO-LOCK:

  {rm/updrmitm.i itemfg-ink rm-i-no NO}
END.

FOR EACH mach-part WHERE
    mach-part.company EQ ITEM.company AND
    mach-part.rm-part-code EQ ITEM.i-no
    NO-LOCK:

    {rm/updrmitm.i mach-part rm-part-code NO}
END.

/* gdm - 10150904 */
FOR EACH prep NO-LOCK
  WHERE prep.company EQ ITEM.company: 

  IF prep.i-no EQ ITEM.i-no THEN DO:
      {rm/updrmitm.i prep i-no NO}
  END.

END.

DO TRANSACTION:
  IF AVAIL b-item THEN DELETE item.
  
  ELSE DO:
    DISABLE TRIGGERS FOR LOAD OF item.
    item.i-no = v-new-item.
  END.
END.

IF AVAIL b-item THEN RUN rm/d-rmrqty.w (ROWID(b-item), YES).
