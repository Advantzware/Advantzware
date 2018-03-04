&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ef

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 LIKE li NO-UNDO.
DEF VAR lv-type AS CHAR NO-UNDO.
DEF VAR li-line AS INT NO-UNDO.
DEF VAR ll-leafed AS LOG NO-UNDO.
DEF VAR ll-die-changed AS LOG NO-UNDO.
DEF VAR ll-same-layout AS LOG NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER op-lock FOR reftable.

{est/layout.i}

DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF est-flm.
DISABLE TRIGGERS FOR LOAD OF ef-nsh.
DISABLE TRIGGERS FOR LOAD OF op-lock.

cocode = {&TABLENAME}.company.

{sys/inc/f16to32.i}

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

IF {&TABLENAME}.est-type EQ ? THEN
  RUN est/resettyp.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.est-type).

IF {&TABLENAME}.est-type EQ 3 THEN {&TABLENAME}.est-type = 4.

IF {&TABLENAME}.n-out   EQ 0 THEN {&TABLENAME}.n-out   = 1.
IF {&TABLENAME}.n-out-l EQ 0 THEN {&TABLENAME}.n-out-l = 1.
IF {&TABLENAME}.n-out-d EQ 0 THEN {&TABLENAME}.n-out-d = 1.

IF {&TABLENAME}.est-type GE 5 AND v-cecscrn-char NE "Decimal" THEN
  ASSIGN
   {&TABLENAME}.gsh-len = ROUND({&TABLENAME}.gsh-len * li-16-32,0)
   {&TABLENAME}.gsh-len = {&TABLENAME}.gsh-len / li-16-32
   {&TABLENAME}.gsh-wid = ROUND({&TABLENAME}.gsh-wid * li-16-32,0)
   {&TABLENAME}.gsh-wid = {&TABLENAME}.gsh-wid / li-16-32
   {&TABLENAME}.gsh-dep = ROUND({&TABLENAME}.gsh-dep * li-16-32,0)
   {&TABLENAME}.gsh-dep = {&TABLENAME}.gsh-dep / li-16-32
   {&TABLENAME}.nsh-len = ROUND({&TABLENAME}.nsh-len * li-16-32,0)
   {&TABLENAME}.nsh-len = {&TABLENAME}.nsh-len / li-16-32
   {&TABLENAME}.nsh-wid = ROUND({&TABLENAME}.nsh-wid * li-16-32,0)
   {&TABLENAME}.nsh-wid = {&TABLENAME}.nsh-wid / li-16-32
   {&TABLENAME}.nsh-dep = ROUND({&TABLENAME}.nsh-dep * li-16-32,0)
   {&TABLENAME}.nsh-dep = {&TABLENAME}.nsh-dep / li-16-32
   {&TABLENAME}.trim-l  = ROUND({&TABLENAME}.trim-l * li-16-32,0)
   {&TABLENAME}.trim-l  = {&TABLENAME}.trim-l / li-16-32
   {&TABLENAME}.trim-w  = ROUND({&TABLENAME}.trim-w * li-16-32,0)
   {&TABLENAME}.trim-w  = {&TABLENAME}.trim-w / li-16-32
   {&TABLENAME}.trim-d  = ROUND({&TABLENAME}.trim-d * li-16-32,0)
   {&TABLENAME}.trim-d  = {&TABLENAME}.trim-d / li-16-32.
ELSE
IF {&TABLENAME}.n-cuts EQ 0 THEN {&TABLENAME}.n-cuts = {&TABLENAME}.n-out - 1.

IF {&TABLENAME}.n-cuts LT 0 THEN {&TABLENAME}.n-cuts = 0.

/* 12070513 WaitForApproval
IF {&TABLENAME}.n-out   NE old-{&TABLENAME}.n-out   OR
   {&TABLENAME}.n-out-l NE old-{&TABLENAME}.n-out-l THEN
  RUN est/estopout.p (ROWID({&TABLENAME}),
                      old-{&TABLENAME}.n-out,
                      old-{&TABLENAME}.n-out-l).
*/

ll-leafed = NO.

DO li = 1 TO EXTENT({&TABLENAME}.leaf):
  IF {&TABLENAME}.leaf[li]      NE old-{&TABLENAME}.leaf[li]      OR
     {&TABLENAME}.leaf-dscr[li] NE old-{&TABLENAME}.leaf-dscr[li] OR
     {&TABLENAME}.leaf-bnum[li] NE old-{&TABLENAME}.leaf-bnum[li] OR 
     {&TABLENAME}.leaf-w[li]    NE old-{&TABLENAME}.leaf-w[li]    OR
     {&TABLENAME}.leaf-l[li]    NE old-{&TABLENAME}.leaf-l[li]    THEN
    ll-leafed = YES.

  IF {&TABLENAME}.leaf[li] EQ "" THEN
    ASSIGN
     {&TABLENAME}.leaf-dscr[li] = ""
     {&TABLENAME}.leaf-snum[li] = 0
     {&TABLENAME}.leaf-bnum[li] = 0
     {&TABLENAME}.leaf-w[li]    = 0
     {&TABLENAME}.leaf-l[li]    = 0.
  ELSE
    {&TABLENAME}.leaf-snum[li] = {&TABLENAME}.form-no.
END.
IF ll-leafed THEN RUN est/leaf-upd.p (ROWID({&TABLENAME})).

RUN est/forminks.p (BUFFER {&TABLENAME}).

FOR EACH eb
    WHERE eb.company EQ {&TABLENAME}.company
      AND eb.est-no  EQ {&TABLENAME}.est-no
      AND eb.form-no EQ {&TABLENAME}.form-no
    BREAK BY eb.blank-no:

  IF {&TABLENAME}.est-type GE 5 THEN DO:
    IF {&TABLENAME}.flute NE "" THEN eb.flute = {&TABLENAME}.flute.
    IF {&TABLENAME}.test  NE "" THEN eb.test  = {&TABLENAME}.test.
  END.

  IF eb.blank-no EQ 1 THEN DO:
    IF {&TABLENAME}.f-col EQ 0                    OR
       (FIRST(eb.blank-no) AND LAST(eb.blank-no)) THEN
      ASSIGN
       {&TABLENAME}.f-col  = eb.i-col
       {&TABLENAME}.f-pass = eb.i-pass.
    IF {&TABLENAME}.f-coat EQ 0                   OR
       (FIRST(eb.blank-no) AND LAST(eb.blank-no)) THEN
      ASSIGN
       {&TABLENAME}.f-coat   = eb.i-coat
       {&TABLENAME}.f-coat-p = eb.i-coat-p.
  END.

  lv-die-in = eb.die-in.
  RUN est/upddiein.p (ROWID(eb)).
  FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK.
  IF lv-die-in NE b-eb.die-in THEN ll-die-changed = YES.
END.

IF ll-die-changed THEN RUN est/updefdie.p (ROWID({&TABLENAME})).

{&TABLENAME}.blank-qty = 0.
FOR EACH eb
    WHERE eb.company EQ {&TABLENAME}.company
      AND eb.est-no  EQ {&TABLENAME}.est-no
      AND eb.form-no EQ {&TABLENAME}.form-no
    NO-LOCK:
  {&TABLENAME}.blank-qty = {&TABLENAME}.blank-qty + 1.
END.

IF ROUND({&TABLENAME}.die-in,0) NE ROUND(old-{&TABLENAME}.die-in,0) THEN
FOR EACH est-prep
    WHERE est-prep.company  EQ {&TABLENAME}.company
      AND est-prep.est-no   EQ {&TABLENAME}.est-no
      AND est-prep.s-num    EQ {&TABLENAME}.form-no
      AND est-prep.mat-type EQ "R"
    EXCLUSIVE:

  est-prep.qty = {&TABLENAME}.die-in.

  IF est-prep.qty EQ 0 THEN DELETE est-prep.
END.

IF {&TABLENAME}.est-type EQ 4 OR
   {&TABLENAME}.est-type EQ 8 THEN DO:
  IF {&TABLENAME}.f-col + {&TABLENAME}.f-coat EQ 0 THEN
  FOR EACH est-prep
      WHERE est-prep.company  EQ {&TABLENAME}.company
        AND est-prep.est-no   EQ {&TABLENAME}.est-no
        AND est-prep.s-num    EQ {&TABLENAME}.form-no
        AND est-prep.mat-type EQ "P",
      
      FIRST prep
      WHERE prep.company EQ est-prep.company
        AND prep.code    EQ est-prep.code
        AND prep.dfault  EQ YES
      NO-LOCK:
    
    DELETE est-prep.  
  END.

  IF {&TABLENAME}.f-col    NE old-{&TABLENAME}.f-col    OR
     {&TABLENAME}.f-coat   NE old-{&TABLENAME}.f-coat   OR
     {&TABLENAME}.f-pass   NE old-{&TABLENAME}.f-pass   OR
     {&TABLENAME}.f-coat-p NE old-{&TABLENAME}.f-coat-p THEN
  FOR EACH est-prep
      WHERE est-prep.company EQ {&TABLENAME}.company
        AND est-prep.est-no  EQ {&TABLENAME}.est-no
        AND est-prep.s-num   EQ {&TABLENAME}.form-no
	    AND (est-prep.mat-type EQ "F" OR
             est-prep.mat-type EQ "P"),

      FIRST est
      WHERE est.company EQ est-prep.company
        AND est.est-no  EQ est-prep.est-no
      NO-LOCK:
      RUN est/GetPrepQty.p(INPUT ROWID(est),
                      INPUT est-prep.mat-type,
                      INPUT est-prep.s-num,
                      OUTPUT est-prep.qty).
/*     RUN sys/inc/flm-prep.p (RECID(est), est-prep.s-num, OUTPUT est-prep.qty). */
  END.
END.

IF {&TABLENAME}.board NE "" THEN
    FOR EACH ITEM NO-LOCK
        WHERE item.company EQ {&TABLENAME}.company
          AND item.i-no    EQ {&TABLENAME}.board:

      {&TABLENAME}.weight = item.basis-w.

  IF {&TABLENAME}.brd-dscr EQ "" THEN {&TABLENAME}.brd-dscr = item.i-name.
  
  IF {&TABLENAME}.est-type LE 4                   AND
     {&TABLENAME}.board NE old-{&TABLENAME}.board AND
     INDEX(PROGRAM-NAME(2),"ce/copyest") EQ 0 THEN DO:

    ASSIGN
/*      {&TABLENAME}.medium   = "" */
/*      {&TABLENAME}.flute    = "" */
     {&TABLENAME}.lam-code = ""
     {&TABLENAME}.adh-code = ""
     {&TABLENAME}.adh-sqin = 0
     {&TABLENAME}.trim-pen = 0.

    FOR EACH item-bom
        WHERE item-bom.company  EQ item.company
          AND item-bom.parent-i EQ item.i-no
          AND item-bom.line#    GE 1
          AND item-bom.line#    LE 5
        NO-LOCK:
        IF item-bom.i-no NE '' THEN DO:
            CASE item-bom.line#:
                WHEN 1 THEN {&TABLENAME}.medium   = item-bom.i-no.
                WHEN 2 THEN {&TABLENAME}.flute    = item-bom.i-no.
                WHEN 3 THEN {&TABLENAME}.lam-code = item-bom.i-no.
                WHEN 4 THEN ASSIGN
                    {&TABLENAME}.adh-code = item-bom.i-no
                    {&TABLENAME}.adh-sqin = {&TABLENAME}.gsh-len * {&TABLENAME}.gsh-wid.
                WHEN 5 THEN {&TABLENAME}.trim-pen = INT(item-bom.i-no).
            END CASE.
        END.
    END.
  END.
  ELSE
     IF {&TABLENAME}.est-type GT 4 AND
     ({&TABLENAME}.board NE old-{&TABLENAME}.board OR
      {&TABLENAME}.gsh-len NE old-{&TABLENAME}.gsh-len OR
      {&TABLENAME}.gsh-wid NE old-{&TABLENAME}.gsh-wid) AND
     INDEX(PROGRAM-NAME(2),"ce/copyest") EQ 0 THEN DO:

    
    IF {&TABLENAME}.gsh-len NE old-{&TABLENAME}.gsh-len OR
       {&TABLENAME}.gsh-wid NE old-{&TABLENAME}.gsh-wid THEN
       DO:
          {&TABLENAME}.adh-sqin = 0.
      
          FOR EACH item-bom
              WHERE item-bom.company  EQ item.company
                AND item-bom.parent-i EQ item.i-no
                AND item-bom.line#    LT 9
              NO-LOCK:
          
             IF item-bom.shrink NE 100 THEN
                {&TABLENAME}.adh-sqin = {&TABLENAME}.adh-sqin +
                                        ({&TABLENAME}.gsh-wid *
                                        ({&TABLENAME}.gsh-len / (1 - (item-bom.shrink / 100)))).
          END.
       END.

    IF {&TABLENAME}.board NE old-{&TABLENAME}.board THEN
    DO:
       ASSIGN
          {&TABLENAME}.lam-code = ""
          {&TABLENAME}.adh-code = "".
      
       FOR EACH item-bom
           WHERE item-bom.company  EQ item.company
             AND item-bom.parent-i EQ item.i-no
             AND item-bom.line#    GE 9
             AND item-bom.line#    LE 10
           NO-LOCK:
         CASE item-bom.line#:
            WHEN 9 THEN {&TABLENAME}.lam-code = item-bom.i-no.
            WHEN 10 THEN {&TABLENAME}.adh-code = item-bom.i-no.
         END CASE.
       END.
    END.
  END.

  LEAVE.
END.

DO li = 1 TO EXTENT({&TABLENAME}.mis-snum):
  IF {&TABLENAME}.mis-snum[li] NE 0 THEN
    {&TABLENAME}.mis-snum[li] = {&TABLENAME}.form-no.

  IF {&TABLENAME}.mis-cost[li] EQ "" THEN DO:
    DO li1 = 1 TO 4:
      lv-type = IF li1 EQ 1 THEN "MAT-QTY" ELSE
                IF li1 EQ 2 THEN "LAB-QTY" ELSE
                IF li1 EQ 3 THEN "MAT-CST" ELSE "LAB-CST".

      FOR EACH reftable
          WHERE reftable.reftable EQ "EST-MISC"
	        AND reftable.company  EQ {&TABLENAME}.company
	        AND reftable.loc      EQ {&TABLENAME}.loc
	        AND reftable.code     EQ TRIM({&TABLENAME}.est-no) +
                                     STRING({&TABLENAME}.form-no,"/99")
	        AND reftable.code2    EQ lv-type + STRING(li,"99")
          EXCLUSIVE-LOCK:
        DELETE reftable.
      END.
    END.

    ASSIGN
     {&TABLENAME}.mis-snum[li]  = 0
     {&TABLENAME}.mis-bnum[li]  = 0
     {&TABLENAME}.mis-matf[li]  = 0
     {&TABLENAME}.mis-labf[li]  = 0
     {&TABLENAME}.mis-matm[li]  = 0
     {&TABLENAME}.mis-labm[li]  = 0           
     {&TABLENAME}.mis-simon[li] = ""
     {&TABLENAME}.mis-mkup[li]  = 0.
  END.
END.

IF old-{&TABLENAME}.est-no NE ""              AND
   {&TABLENAME}.eqty NE old-{&TABLENAME}.eqty THEN
FOR EACH ef-nsh OF old-{&TABLENAME}:
  ef-nsh.eqty = {&TABLENAME}.eqty.
END.

IF NOT CAN-FIND(FIRST ef-nsh OF {&TABLENAME} WHERE ef-nsh.pass-no EQ 1) THEN DO:
  CREATE ef-nsh.
  BUFFER-COPY {&TABLENAME} EXCEPT rec_key TO ef-nsh
  ASSIGN
   ef-nsh.n-out-l = TRUNC({&TABLENAME}.gsh-len / {&TABLENAME}.nsh-len,0)
   ef-nsh.n-out-w = TRUNC({&TABLENAME}.gsh-wid / {&TABLENAME}.nsh-wid,0)
   ef-nsh.n-out-d = TRUNC({&TABLENAME}.gsh-dep / {&TABLENAME}.nsh-dep,0).
END.

ELSE FIND ef-nsh OF {&TABLENAME} NO-ERROR.

IF AVAIL ef-nsh THEN
  ASSIGN
   ef-nsh.pass-no  = 1
   ef-nsh.sheet-no = 1
   ef-nsh.dept     = "RC" 
   ef-nsh.len-in   = {&TABLENAME}.gsh-len
   ef-nsh.wid-in   = {&TABLENAME}.gsh-wid
   ef-nsh.dep-in   = {&TABLENAME}.gsh-dep
   ef-nsh.n-out-l  = {&TABLENAME}.n-out-l
   ef-nsh.n-out-w  = {&TABLENAME}.n-out
   ef-nsh.n-out-d  = {&TABLENAME}.n-out-d
   ef-nsh.len-out  = {&TABLENAME}.nsh-len
   ef-nsh.wid-out  = {&TABLENAME}.nsh-wid
   ef-nsh.dep-out  = {&TABLENAME}.nsh-dep.

FOR EACH reftable
    WHERE reftable.reftable EQ "EST-MISC"
	  AND reftable.company  EQ {&TABLENAME}.company
	  AND reftable.loc      EQ {&TABLENAME}.loc
	  AND reftable.code     EQ TRIM({&TABLENAME}.est-no) +
                               STRING({&TABLENAME}.form-no,"/99")
    BREAK BY reftable.code2
          BY RECID(reftable):
  IF NOT FIRST-OF(reftable.code2) THEN DELETE reftable.
END.

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  {est/op-lock.i est}
  
  IF NEW op-lock                                      OR
     {&TABLENAME}.company NE old-{&TABLENAME}.company OR
     {&TABLENAME}.op-lock NE old-{&TABLENAME}.op-lock THEN
    ASSIGN
     op-lock.val[1] = INT(NOT {&TABLENAME}.op-lock)
     op-lock.val[2] = op-lock.val[1].

  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.mod-date     = est.updated-date.
END.

IF TRIM({&TABLENAME}.rec_key) NE "" THEN DO:
  BUFFER-COMPARE old-{&TABLENAME} USING xgrain board brd-dscr i-code flute test cost-uom cost-msh weight fr-uom fr-msh nc gsh-wid gsh-len gsh-dep nsh-wid nsh-len nsh-dep trim-w trim-l trim-d n-out n-out-l n-out-d n-cuts die-in adder leaf leaf-dscr leaf-snum leaf-bnum leaf-w leaf-l TO {&TABLENAME}
      SAVE RESULT IN ll-same-layout.

  IF NOT ll-same-layout THEN DO:
    {custom/fibreaud.i}
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.
