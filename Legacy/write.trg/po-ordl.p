&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME po-ordl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

{custom/globdefs.i}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ld-qty LIKE {&TABLENAME}.ord-qty NO-UNDO.
DEF VAR v-basis-w AS DEC NO-UNDO.
DEF VAR v-len LIKE {&TABLENAME}.s-len NO-UNDO.
DEF VAR v-wid LIKE {&TABLENAME}.s-wid NO-UNDO.
DEF VAR v-dep LIKE {&TABLENAME}.s-len NO-UNDO.
DEF VAR ll-error AS LOG NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR poPaperClip-int AS INT NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

RUN po/updordpo.p (BUFFER {&TABLENAME}).



IF {&TABLENAME}.job-no EQ "" THEN {&TABLENAME}.s-num = 0.

IF {&TABLENAME}.pr-uom EQ "" AND {&TABLENAME}.i-no NE "" THEN DO:
  IF {&TABLENAME}.item-type THEN DO:
    FIND FIRST item NO-LOCK
         WHERE item.company EQ {&TABLENAME}.company
           AND item.i-no    EQ {&TABLENAME}.i-no NO-ERROR.
    IF AVAIL item THEN
    FIND FIRST e-item OF item NO-LOCK NO-ERROR.
    IF AVAIL e-item THEN {&TABLENAME}.pr-uom = e-item.std-uom.
    IF {&TABLENAME}.pr-uom EQ "" THEN {&TABLENAME}.pr-uom = item.pur-uom.
    IF {&TABLENAME}.pr-uom EQ "" THEN {&TABLENAME}.pr-uom = item.cons-uom.
  END.

  ELSE DO:
    FIND FIRST itemfg
        WHERE itemfg.company EQ {&TABLENAME}.company
          AND itemfg.i-no    EQ {&TABLENAME}.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN {&TABLENAME}.pr-uom = itemfg.pur-uom.
  END.

  IF {&TABLENAME}.pr-uom EQ "" THEN {&TABLENAME}.pr-uom = "EA".
END.

IF {&TABLENAME}.pr-qty-uom EQ "" THEN {&TABLENAME}.pr-qty-uom = {&TABLENAME}.pr-uom.

IF {&TABLENAME}.pr-uom NE "" THEN DO:
  IF LOOKUP({&TABLENAME}.pr-uom,"L,LOT") GT 0 THEN
    {&TABLENAME}.t-cost = {&TABLENAME}.cost.

  ELSE DO:
    IF {&TABLENAME}.item-type THEN DO:
      FIND FIRST item NO-LOCK
           WHERE item.company EQ {&TABLENAME}.company
             AND item.i-no    EQ {&TABLENAME}.i-no NO-ERROR.
      IF AVAIL ITEM THEN
        ASSIGN
         v-basis-w = item.basis-w
         v-dep     = item.s-dep.
    END.

    ASSIGN
     v-len  = {&TABLENAME}.s-len
     v-wid  = {&TABLENAME}.s-wid
     ld-qty = {&TABLENAME}.ord-qty.

    IF {&TABLENAME}.pr-qty-uom NE {&TABLENAME}.pr-uom       AND
       ({&TABLENAME}.item-type                           OR
        LOOKUP({&TABLENAME}.pr-qty-uom,fg-uom-list) EQ 0 OR
        LOOKUP({&TABLENAME}.pr-uom,fg-uom-list)     EQ 0)   THEN
      RUN sys/ref/convquom.p({&TABLENAME}.pr-qty-uom, {&TABLENAME}.pr-uom,
                             v-basis-w, v-len, v-wid, v-dep,
                             ld-qty, OUTPUT ld-qty).

    {&TABLENAME}.t-cost = (ld-qty * {&TABLENAME}.cost) + {&TABLENAME}.setup.
  END.

  {&TABLENAME}.t-cost = ROUND({&TABLENAME}.t-cost * ((100 - {&TABLENAME}.disc) / 100),2).
END.

RELEASE item.
RELEASE e-item.
RELEASE itemfg.

IF ({&TABLENAME}.s-wid NE old-{&TABLENAME}.s-wid OR
    {&TABLENAME}.s-len NE old-{&TABLENAME}.s-len)       AND
   {&TABLENAME}.i-no NE ""                              AND
   {&TABLENAME}.job-no NE ""                            AND
   {&TABLENAME}.item-type                               AND
   CAN-FIND(FIRST item
            WHERE item.company  EQ {&TABLENAME}.company
              AND item.i-no     EQ {&TABLENAME}.i-no
              AND INDEX("BPRA1234",item.mat-type) GT 0) THEN DO:

  FIND FIRST job
      WHERE job.company EQ {&TABLENAME}.company
        AND job.job-no  EQ {&TABLENAME}.job-no
        AND job.job-no2 EQ {&TABLENAME}.job-no2
      NO-LOCK NO-ERROR.

  IF AVAIL job THEN
  FOR EACH job-mat
      WHERE job-mat.company  EQ job.company
        AND job-mat.job      EQ job.job
        AND job-mat.job-no   EQ job.job-no
        AND job-mat.job-no2  EQ job.job-no2
        AND job-mat.frm      EQ {&TABLENAME}.s-num
        AND CAN-FIND(FIRST item
                     WHERE item.company  EQ job-mat.company
                       AND item.i-no     EQ job-mat.i-no
                       AND INDEX("BPRA1234",item.mat-type) GT 0):
    ASSIGN
     job-mat.wid = {&TABLENAME}.s-wid
     job-mat.len = {&TABLENAME}.s-len.
  END.
END.

DISABLE TRIGGERS FOR LOAD OF po-ord.

FOR EACH po-ord
    WHERE po-ord.company EQ {&TABLENAME}.company
      AND po-ord.po-no   EQ {&TABLENAME}.po-no:

  IF old-{&TABLENAME}.po-no NE 0 AND
     {&TABLENAME}.stat NE "U"    AND
     {&TABLENAME}.stat NE "C"    AND
     po-ord.printed              AND
     po-ord.opened               THEN {&TABLENAME}.stat = "U".

  RUN po/po-total.p (RECID(po-ord)).
  
  IF {&tablename}.i-no NE OLD-{&tablename}.i-no THEN DO:
      /* If required, copy attachments from item master */
      RUN sys/ref/nk1look.p (g_company, "POPaperClip", "I", no, no, "", "", 
                            Output cReturn, output llRecFound).
      If llRecFound then
      	poPaperClip-INT = INT(cReturn).
    
      IF poPaperClip-INT EQ 1 THEN
          RUN po/copyItemAttach.p (INPUT "PO", INPUT "", INPUT ROWID(po-ord)).
  END.
  LEAVE.
END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
  {custom/fibreaud.i}
END.

/* Clear out any error-status from find with no-error that is false */
ll-error = YES NO-ERROR.
