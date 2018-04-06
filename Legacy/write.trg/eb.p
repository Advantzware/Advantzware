&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME eb

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER xeb FOR {&TABLENAME}.
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-est-op FOR est-op.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-style FOR style.

DEF VAR ll-tandem AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-hld-cust LIKE {&TABLENAME}.cust-no NO-UNDO.
DEF VAR lv-hld-ship LIKE {&TABLENAME}.ship-id NO-UNDO.
DEF VAR lv-hld-board LIKE ef.board NO-UNDO.
DEF VAR lv-die-in LIKE ef.die-in NO-UNDO.
DEF VAR ll-inked AS LOG NO-UNDO.
DEF VAR ll-web AS LOG NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR rec-list AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

DEF TEMP-TABLE w-itemfg-ink LIKE itemfg-ink
    FIELD old-rm-i-no LIKE itemfg-ink.rm-i-no
    FIELD old-pass    LIKE itemfg-ink.pass.


DISABLE TRIGGERS FOR LOAD OF est.
DISABLE TRIGGERS FOR LOAD OF ef.
DISABLE TRIGGERS FOR LOAD OF est-op.
DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.
DISABLE TRIGGERS FOR LOAD OF xeb.
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF itemfg-ink.

cocode = {&TABLENAME}.company.

{ce/msfcalc.i}
    
{sys/inc/f16to32.i}

{&TABLENAME}.est-no = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                      TRIM({&TABLENAME}.est-no).

IF {&TABLENAME}.est-type EQ ? THEN
  RUN est/resettyp.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.est-type).

IF {&TABLENAME}.est-type EQ 3 THEN {&TABLENAME}.est-type = 4.

find first b-style where b-style.company = cocode and
                         b-style.style = {&TABLENAME}.style
                         no-lock no-error.
  if avail b-style and b-style.type <> "F" THEN
        {&TABLENAME}.num-up = (IF {&TABLENAME}.num-wid EQ 0 THEN 1 ELSE {&TABLENAME}.num-wid) *
                      (IF {&TABLENAME}.num-len EQ 0 THEN 1 ELSE {&TABLENAME}.num-len) *
                      (IF {&TABLENAME}.num-dep EQ 0 THEN 1 ELSE {&TABLENAME}.num-dep).

IF {&TABLENAME}.est-type EQ 2 AND
   {&TABLENAME}.cust-% EQ 0   THEN {&TABLENAME}.cust-%  = 1.
ELSE
IF {&TABLENAME}.est-type GE 5 AND {&TABLENAME}.est-type LE 6 AND 
   {&TABLENAME}.quantityPerSet EQ 0  THEN {&TABLENAME}.quantityPerSet = 1.

IF {&TABLENAME}.part-no     NE "" AND
   old-{&TABLENAME}.part-no NE "" THEN RUN est/validset.p (BUFFER {&TABLENAME}).

IF {&TABLENAME}.part-no NE old-{&TABLENAME}.part-no AND
   old-{&tablename}.part-no NE "" THEN
   FOR EACH probeit WHERE
       probeit.company EQ {&tablename}.company AND
       probeit.est-no EQ {&tablename}.est-no AND
       probeit.part-no EQ old-{&tablename}.part-no:

       probeit.part-no = {&tablename}.part-no.
   END.


IF {&TABLENAME}.form-no EQ 0                                      AND
   {&TABLENAME}.stock-no NE ""                                    AND
   CAN-FIND(FIRST b-{&TABLENAME}
            WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
              AND b-{&TABLENAME}.est-no   EQ {&TABLENAME}.est-no
              AND b-{&TABLENAME}.stock-no EQ {&TABLENAME}.stock-no
              AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})) AND
   CAN-FIND(FIRST b-{&TABLENAME}
            WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
              AND b-{&TABLENAME}.est-no   EQ {&TABLENAME}.est-no
              AND b-{&TABLENAME}.stock-no NE {&TABLENAME}.stock-no
              AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})) THEN
FOR EACH itemfg NO-LOCK
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.est-no  EQ {&TABLENAME}.est-no
      AND itemfg.i-no    NE {&TABLENAME}.stock-no
      AND itemfg.isaset
    BREAK BY itemfg.rec_key DESC:
  IF LAST(itemfg.rec_key)                   OR
     itemfg.part-no EQ {&TABLENAME}.part-no THEN DO:
    IF AVAIL itemfg THEN {&TABLENAME}.stock-no = itemfg.i-no.
    LEAVE.
  END.
END.

IF INDEX(PROGRAM-NAME(5),"v-est3") EQ 0 AND
   INDEX(PROGRAM-NAME(3),"v-est3") EQ 0 AND
   index(program-name(2),"updest3") EQ 0 THEN
   DO:
      FOR EACH item FIELDS(case-l case-w case-d avg-w)
          WHERE item.company EQ {&TABLENAME}.company
            AND item.i-no    EQ {&TABLENAME}.cas-no
            AND item.i-code  EQ "R"
          NO-LOCK:
     
          ASSIGN
            {&TABLENAME}.cas-len = item.case-l
            {&TABLENAME}.cas-wid = item.case-w
            {&TABLENAME}.cas-dep = item.case-d
            {&TABLENAME}.cas-wt  = item.avg-w.
          LEAVE.
      END.
     
      FOR EACH item FIELDS(case-l case-w case-d) WHERE
          item.company EQ {&TABLENAME}.company AND
          item.i-no    EQ {&TABLENAME}.tr-no AND
          item.i-code  EQ "R"
          NO-LOCK:
     
          ASSIGN
             {&TABLENAME}.tr-len = item.case-l
             {&TABLENAME}.tr-wid = item.case-w
             {&TABLENAME}.tr-dep = item.case-d.
          LEAVE.
      END.
   END.

IF {&TABLENAME}.style   NE old-{&TABLENAME}.style   OR
   {&TABLENAME}.len     NE old-{&TABLENAME}.len     OR 
   {&TABLENAME}.wid     NE old-{&TABLENAME}.wid     OR
   {&TABLENAME}.dep     NE old-{&TABLENAME}.dep     OR
   {&TABLENAME}.t-len   NE old-{&TABLENAME}.t-len   OR 
   {&TABLENAME}.t-wid   NE old-{&TABLENAME}.t-wid   OR
   {&TABLENAME}.t-dep   NE old-{&TABLENAME}.t-dep   OR
   {&TABLENAME}.k-len   NE old-{&TABLENAME}.k-len   OR
   {&TABLENAME}.k-wid   NE old-{&TABLENAME}.k-wid   OR
   {&TABLENAME}.tuck    NE old-{&TABLENAME}.tuck    OR
   {&TABLENAME}.gluelap NE old-{&TABLENAME}.gluelap OR
   {&TABLENAME}.fpanel  NE old-{&TABLENAME}.fpanel  OR
   {&TABLENAME}.dust    NE old-{&TABLENAME}.dust    OR
   {&TABLENAME}.lock    NE old-{&TABLENAME}.lock    OR
   {&TABLENAME}.num-up  NE old-{&TABLENAME}.num-up  THEN DO:
  RUN est/upddiein.p (ROWID({&TABLENAME})).
  FIND CURRENT {&TABLENAME} NO-ERROR.
END.

IF {&TABLENAME}.est-type NE 4 AND
   {&TABLENAME}.est-type NE 8 THEN DO:
  IF {&TABLENAME}.i-col + {&TABLENAME}.i-coat EQ 0 THEN
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

  IF {&TABLENAME}.i-col    NE old-{&TABLENAME}.i-col    OR
     {&TABLENAME}.i-coat   NE old-{&TABLENAME}.i-coat   OR
     {&TABLENAME}.i-pass   NE old-{&TABLENAME}.i-pass   OR
     {&TABLENAME}.i-coat-p NE old-{&TABLENAME}.i-coat-p THEN
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

IF INT({&TABLENAME}.master-est-no) NE 0 AND
   {&TABLENAME}.stock-no           NE "" THEN
FOR EACH b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
      AND b-{&TABLENAME}.est-no   EQ {&TABLENAME}.master-est-no
      AND b-{&TABLENAME}.part-no  EQ {&TABLENAME}.part-no
      AND b-{&TABLENAME}.stock-no EQ "":
  b-{&TABLENAME}.stock-no = {&TABLENAME}.stock-no.
END.

FIND FIRST est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  ASSIGN
   est.updated-date = TODAY
   est.updated-id   = USERID("nosweat")
   est.mod-date     = est.updated-date.
      
  /*IF (est.est-type EQ 4 OR est.est-type EQ 8)           AND
     CAN-FIND(FIRST ef OF est
              WHERE ef.form-no NE {&TABLENAME}.form-no) THEN DO:
    RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).
    IF ll-tandem THEN {&TABLENAME}.yld-qty = {&TABLENAME}.bl-qty.
  END.*/
END.
  
FOR EACH ef
    WHERE ef.company EQ {&TABLENAME}.company
      AND ef.est-no  EQ {&TABLENAME}.est-no
      AND ef.form-no EQ {&TABLENAME}.form-no:

  IF {&TABLENAME}.blank-no NE 0                         AND
     old-{&TABLENAME}.blank-no NE 0                     AND
     {&TABLENAME}.blank-no NE old-{&TABLENAME}.blank-no THEN
  DO li = 1 TO EXTENT(ef.mis-cost):
    IF ef.mis-cost[li] NE ""                        AND
       ef.mis-bnum[li] EQ old-{&TABLENAME}.blank-no THEN
      ef.mis-bnum[li] = {&TABLENAME}.blank-no.
  END.

  IF {&TABLENAME}.est-type GE 5 THEN DO:
    IF {&TABLENAME}.flute NE "" THEN ef.flute = {&TABLENAME}.flute.
    IF {&TABLENAME}.test  NE "" THEN ef.test  = {&TABLENAME}.test.
  END.

  IF {&TABLENAME}.blank-no EQ 1 THEN DO:
    IF ef.f-col EQ 0 THEN
      ASSIGN
       ef.f-col  = {&TABLENAME}.i-col
       ef.f-pass = {&TABLENAME}.i-pass.
    IF ef.f-coat EQ 0 THEN
      ASSIGN
       ef.f-coat   = {&TABLENAME}.i-coat
       ef.f-coat-p = {&TABLENAME}.i-coat-p.
  END.

  RUN est/forminks.p (BUFFER ef).

  IF {&TABLENAME}.die-in NE old-{&TABLENAME}.die-in THEN DO:
    lv-die-in = ef.die-in.

    RUN est/updefdie.p (ROWID(ef)).

    FIND b-ef WHERE ROWID(b-ef) EQ ROWID(ef) NO-LOCK NO-ERROR.

    IF AVAIL b-ef AND ROUND(b-ef.die-in,0) NE ROUND(lv-die-in,0) THEN
    FOR EACH est-prep
        WHERE est-prep.company  EQ b-ef.company
          AND est-prep.est-no   EQ b-ef.est-no
          AND est-prep.s-num    EQ b-ef.form-no
          AND est-prep.mat-type EQ "R"
        EXCLUSIVE:

      est-prep.qty = b-ef.die-in.

      IF est-prep.qty EQ 0 THEN DELETE est-prep.
    END.
  END.

  IF {&TABLENAME}.pur-man NE old-{&TABLENAME}.pur-man THEN
    ef.nc = NOT {&TABLENAME}.pur-man.

  LEAVE.
END.

IF TRIM({&TABLENAME}.est-no) NE ""                       AND
   {&TABLENAME}.part-no      NE old-{&TABLENAME}.part-no AND
   old-{&TABLENAME}.part-no  NE ""                       THEN
FOR EACH est
    WHERE est.company EQ {&TABLENAME}.company
      AND est.est-no  EQ {&TABLENAME}.est-no
    NO-LOCK,
    EACH quotehd
    WHERE quotehd.company EQ est.company
      AND quotehd.loc     EQ est.loc
      AND quotehd.est-no  EQ est.est-no
    NO-LOCK,
    EACH quoteitm
    WHERE quoteitm.company EQ quotehd.company
      AND quoteitm.loc     EQ quotehd.loc
      AND quoteitm.q-no    EQ quotehd.q-no
      AND quoteitm.part-no EQ old-{&TABLENAME}.part-no:
  quoteitm.part-no = {&TABLENAME}.part-no.
END.

IF TRIM(old-{&TABLENAME}.est-no) NE "" AND
   {&TABLENAME}.eqty NE 0              THEN DO:
  IF {&TABLENAME}.est-type EQ 1 OR
     {&TABLENAME}.est-type EQ 5 OR
     {&TABLENAME}.est-type EQ 6 THEN
    IF NOT CAN-FIND(FIRST b-est-op
                    WHERE b-est-op.company EQ {&TABLENAME}.company
                      AND b-est-op.est-no  EQ {&TABLENAME}.est-no
                      AND b-est-op.qty     EQ {&TABLENAME}.eqty)
    THEN
    FOR EACH est-op
        WHERE est-op.company EQ {&TABLENAME}.company
          AND est-op.est-no  EQ {&TABLENAME}.est-no
          AND est-op.qty     EQ old-{&TABLENAME}.eqty:
      est-op.qty = {&TABLENAME}.eqty.
    END.

    ELSE.

  ELSE DO:
    FOR EACH est-qty
        WHERE est-qty.company EQ {&TABLENAME}.company
          AND est-qty.est-no  EQ {&TABLENAME}.est-no
        BY est-qty.eqty:
      est-qty.eqty = {&TABLENAME}.eqty.
      LEAVE.
    END.
    FOR EACH est-qty
        WHERE est-qty.company EQ {&TABLENAME}.company
          AND est-qty.est-no  EQ {&TABLENAME}.est-no
        BREAK BY est-qty.eqty:
      IF NOT FIRST(est-qty.eqty) THEN DELETE est-qty.
    END.
  END.
END.

IF {&TABLENAME}.est-type      LE 4  AND
   old-{&TABLENAME}.est-no    EQ "" AND
   {&TABLENAME}.est-no        NE "" AND
   {&TABLENAME}.master-est-no EQ "" THEN
  {&TABLENAME}.master-est-no = {&TABLENAME}.est-no.

IF {&TABLENAME}.company NE ""                           AND
   {&TABLENAME}.cust-no NE "TEMP"                       AND
   ({&TABLENAME}.cust-no NE old-{&TABLENAME}.cust-no OR
    {&TABLENAME}.ship-id NE old-{&TABLENAME}.ship-id)   THEN DO:

  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ {&TABLENAME}.company
        AND shipto.cust-no EQ {&TABLENAME}.cust-no
        AND shipto.ship-id EQ {&TABLENAME}.ship-id
      NO-ERROR.

  IF AVAIL shipto THEN
    ASSIGN
     {&TABLENAME}.ship-no      = shipto.ship-no
     {&TABLENAME}.ship-name    = shipto.ship-name
     {&TABLENAME}.ship-addr[1] = shipto.ship-addr[1]
     {&TABLENAME}.ship-addr[2] = shipto.ship-addr[2]
     {&TABLENAME}.ship-city    = shipto.ship-city
     {&TABLENAME}.ship-state   = shipto.ship-state
     {&TABLENAME}.ship-zip     = shipto.ship-zip.
END.

IF {&TABLENAME}.carrier EQ "" AND
   {&TABLENAME}.cust-no NE "" AND
   {&TABLENAME}.ship-id NE "" THEN DO:
  ASSIGN
   lv-hld-cust = {&TABLENAME}.cust-no
   lv-hld-ship = {&TABLENAME}.ship-id.
  IF AVAIL(ef) THEN
      lv-hld-board = ef.board.

  {ce/uship-id.i YES}
END.

DO li = 1 TO EXTENT({&TABLENAME}.i-code):
  IF {&TABLENAME}.i-code[li] EQ "" OR {&TABLENAME}.est-type LE 4 THEN
    ASSIGN
     {&TABLENAME}.i-code[li] = ""
     {&TABLENAME}.i-ps[li]   = 0
     {&TABLENAME}.i-dscr[li] = ""
     {&TABLENAME}.i-%[li]    = 0.
END.

DO li = 1 TO EXTENT({&TABLENAME}.i-code2):
  IF {&TABLENAME}.i-code2[li] EQ "" OR {&TABLENAME}.est-type GT 4 THEN
    ASSIGN
     {&TABLENAME}.i-code2[li] = ""
     {&TABLENAME}.i-ps2[li]   = 0
     {&TABLENAME}.i-dscr2[li] = ""
     {&TABLENAME}.i-%2[li]    = 0.
END.

IF cocode NE "" THEN DO:
  {sys/inc/fgcolors.i}

  IF fgcolors-log AND {&TABLENAME}.stock-no NE "" THEN DO:
    ll-inked = NO.

    IF {&TABLENAME}.est-type GT 4 THEN
    DO li = 1 TO EXTENT({&TABLENAME}.i-code):
      IF {&TABLENAME}.i-ps[li]   NE old-{&TABLENAME}.i-ps[li]   OR
         {&TABLENAME}.i-code[li] NE old-{&TABLENAME}.i-code[li] OR
         {&TABLENAME}.i-dscr[li] NE old-{&TABLENAME}.i-dscr[li] OR 
         {&TABLENAME}.i-%[li]    NE old-{&TABLENAME}.i-%[li]    THEN DO:
        ll-inked = YES.
        LEAVE.
      END.
    END.

    ELSE
    DO li = 1 TO EXTENT({&TABLENAME}.i-code2):
      IF {&TABLENAME}.i-ps2[li]   NE old-{&TABLENAME}.i-ps2[li]   OR
         {&TABLENAME}.i-code2[li] NE old-{&TABLENAME}.i-code2[li] OR
         {&TABLENAME}.i-dscr2[li] NE old-{&TABLENAME}.i-dscr2[li] OR 
         {&TABLENAME}.i-%2[li]    NE old-{&TABLENAME}.i-%2[li]    THEN DO:
        ll-inked = YES.
        LEAVE.
      END.
    END.

    IF ll-inked THEN DO:
      IF {&TABLENAME}.est-type GT 4 THEN
      DO li = 1 TO EXTENT({&TABLENAME}.i-code):
        IF {&TABLENAME}.i-code[li] NE "" THEN DO:
          CREATE w-itemfg-ink.
          ASSIGN
           w-itemfg-ink.rm-i-no     = {&TABLENAME}.i-code[li]
           w-itemfg-ink.dscr        = {&TABLENAME}.i-dscr[li]
           w-itemfg-ink.pass        = {&TABLENAME}.i-ps[li]
           w-itemfg-ink.cover%      = {&TABLENAME}.i-%[li]
           w-itemfg-ink.old-rm-i-no = old-{&TABLENAME}.i-code[li]
           w-itemfg-ink.old-pass    = old-{&TABLENAME}.i-ps[li].
        END.
      END.

      ELSE
      DO li = 1 TO EXTENT({&TABLENAME}.i-code2):
        IF {&TABLENAME}.i-code2[li] NE "" THEN DO:
          CREATE w-itemfg-ink.
          ASSIGN
           w-itemfg-ink.rm-i-no     = {&TABLENAME}.i-code2[li]
           w-itemfg-ink.dscr        = {&TABLENAME}.i-dscr2[li]
           w-itemfg-ink.pass        = {&TABLENAME}.i-ps2[li]
           w-itemfg-ink.cover%      = {&TABLENAME}.i-%2[li]
           w-itemfg-ink.old-rm-i-no = old-{&TABLENAME}.i-code2[li]
           w-itemfg-ink.old-pass    = old-{&TABLENAME}.i-ps2[li].
        END.
      END.

      FOR EACH w-itemfg-ink:
        FIND FIRST item
             WHERE item.company EQ {&TABLENAME}.company
               AND item.i-no    EQ w-itemfg-ink.rm-i-no
             NO-LOCK NO-ERROR.
        IF AVAIL item THEN DO:
          w-itemfg-ink.press-type = item.press-type.
          FIND FIRST itemfg-ink
              WHERE itemfg-ink.company EQ {&TABLENAME}.company
                AND itemfg-ink.i-no    EQ {&TABLENAME}.stock-no
                AND itemfg-ink.rm-i-no EQ w-itemfg-ink.old-rm-i-no
                AND itemfg-ink.pass    EQ w-itemfg-ink.old-pass
              NO-LOCK NO-ERROR.
          IF AVAIL itemfg-ink THEN w-itemfg-ink.in-out = itemfg-ink.in-out.
        END.
        ELSE DELETE w-itemfg-ink.
      END.

      FOR EACH w-itemfg-ink
          BREAK BY w-itemfg-ink.rm-i-no
                BY w-itemfg-ink.in-out:
        IF NOT FIRST-OF(w-itemfg-ink.in-out) THEN DELETE w-itemfg-ink.
      END.

      rec-list = "".

      FOR EACH w-itemfg-ink
          BREAK BY w-itemfg-ink.pass
                BY w-itemfg-ink.press-type:

        IF FIRST-OF(w-itemfg-ink.press-type) THEN
        FOR EACH itemfg-ink
            WHERE itemfg-ink.company EQ {&TABLENAME}.company
              AND itemfg-ink.i-no    EQ {&TABLENAME}.stock-no
              AND itemfg-ink.pass    EQ w-itemfg-ink.pass,
            FIRST item
            WHERE item.company    EQ itemfg-ink.company
              AND item.i-no       EQ itemfg-ink.rm-i-no
              AND item.press-type EQ w-itemfg-ink.press-type
            NO-LOCK:
          DELETE itemfg-ink.
        END.

        FIND FIRST itemfg-ink
            WHERE itemfg-ink.company EQ {&TABLENAME}.company
              AND itemfg-ink.i-no    EQ {&TABLENAME}.stock-no
              AND itemfg-ink.rm-i-no EQ w-itemfg-ink.rm-i-no
              AND itemfg-ink.in-out  EQ w-itemfg-ink.in-out
              AND itemfg-ink.pass    EQ w-itemfg-ink.pass
            NO-ERROR.

        IF NOT AVAIL itemfg-ink THEN CREATE itemfg-ink.

        RUN update-itemfg-ink.

        
        IF NOT CAN-DO(rec-list,itemfg-ink.rec_key) THEN
          ASSIGN
           rec-list        = TRIM(rec-list)                       +
                             (IF rec-list EQ "" THEN "" ELSE ",") +
                             itemfg-ink.rec_key
           itemfg-ink.occurs = 0.

        itemfg-ink.occurs = itemfg-ink.occurs + 1.
      END.
    END.
  END.
END.

IF {&TABLENAME}.stock-no NE "" THEN
FOR EACH b-itemfg NO-LOCK
    WHERE b-itemfg.company EQ {&TABLENAME}.company
      AND b-itemfg.i-no    EQ {&TABLENAME}.stock-no:

 FIND itemfg WHERE ROWID(itemfg) EQ ROWID(b-itemfg)
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

 IF AVAIL itemfg THEN DO:
  IF itemfg.est-no EQ "" THEN itemfg.est-no = {&TABLENAME}.est-no.

  IF itemfg.cust-no     EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.cust-no     = {&TABLENAME}.cust-no.
  IF itemfg.part-no     EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.part-no     = {&TABLENAME}.part-no.
  IF itemfg.i-name      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.i-name      = {&TABLENAME}.part-dscr1.
  IF itemfg.part-dscr1  EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.part-dscr1  = {&TABLENAME}.part-dscr2.
  IF itemfg.style       EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.style       = {&TABLENAME}.style.
  IF itemfg.procat      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN 
  DO:
    itemfg.procat      = {&TABLENAME}.procat.
    FIND FIRST procat WHERE procat.company = itemfg.company 
        AND procat.procat = itemfg.procat NO-LOCK NO-ERROR.
    IF AVAIL procat THEN itemfg.procat-desc = procat.dscr.
  END.
  IF itemfg.die-no      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.die-no      = {&TABLENAME}.die-no.
  IF itemfg.plate-no    EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.plate-no    = {&TABLENAME}.plate-no.
  IF itemfg.cad-no      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.cad-no      = {&TABLENAME}.cad-no.
  IF itemfg.upc-no      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.upc-no      = {&TABLENAME}.upc-no.
  IF itemfg.spc-no      EQ "" OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.spc-no      = {&TABLENAME}.spc-no.

  ASSIGN
   ld-len = itemfg.l-score[50]
   ld-wid = itemfg.w-score[50]
   ld-dep = itemfg.d-score[50].

  IF itemfg.l-score[50] EQ 0 OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.l-score[50] = {&TABLENAME}.len.
  IF itemfg.w-score[50] EQ 0 OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.w-score[50] = {&TABLENAME}.wid.
  IF itemfg.d-score[50] EQ 0 OR itemfg.est-no EQ {&TABLENAME}.est-no THEN
    itemfg.d-score[50] = {&TABLENAME}.dep.

  IF itemfg.l-score[50] NE ld-len OR
     itemfg.w-score[50] NE ld-wid OR
     itemfg.d-score[50] NE ld-dep THEN
    IF itemfg.isaset THEN RUN fg/updsetdm.p (RECID({&TABLENAME})).
    ELSE DO:
      {sys/inc/updfgdim.i {&TABLENAME}}
    END.
 END.
END.

IF {&TABLENAME}.stack-code NE "" AND {&TABLENAME}.stacks EQ 0 THEN DO:
  FIND FIRST stackPattern NO-LOCK
       WHERE stackPattern.stackCode EQ {&TABLENAME}.stack-code
      NO-ERROR.
  IF AVAIL stackPattern THEN {&TABLENAME}.stacks = stackPattern.stackCount.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.

PROCEDURE update-itemfg-ink.

  IF itemfg-ink.rec_key EQ "" THEN DO:
    {custom/rec_key.i itemfg-ink}
  END.

  BUFFER-COPY w-itemfg-ink EXCEPT rec_key TO itemfg-ink
  ASSIGN
   itemfg-ink.company = {&TABLENAME}.company
   itemfg-ink.i-no    = {&TABLENAME}.stock-no.

END PROCEDURE.
