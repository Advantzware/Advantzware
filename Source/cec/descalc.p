/* -------------------------------------------------- cec/descalc.p 11/95 BSM */
/* Box Design Calculation                                                     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-recid1 AS RECID NO-UNDO.
DEF INPUT PARAMETER v-recid2 AS RECID NO-UNDO.

{sys/inc/var.i shared}

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.

{sys/inc/f16to32.i}
{cec/descalc.i}

DEF VAR v-i    AS INT                  NO-UNDO.
DEF VAR v-j    AS INT                  NO-UNDO.
DEF VAR v-l    AS INT                  NO-UNDO.
DEF VAR v-k    AS INT                  NO-UNDO.
DEF VAR v-lscore-d     AS DEC EXTENT 30 FORMAT "999.99" NO-UNDO.
DEF VAR v-lscor2-d     AS DEC EXTENT 30 FORMAT "999.99" NO-UNDO.
DEF VAR v-wscore-d     AS DEC EXTENT 50 FORMAT "999.99" NO-UNDO.
DEF VAR v-wscor2-d     AS DEC EXTENT 50 FORMAT "999.99" NO-UNDO.
DEF VAR v-lcum-score-d AS DEC EXTENT 30 FORMAT "999.99" NO-UNDO.
DEF VAR v-lcum-scor2-d AS DEC EXTENT 30 FORMAT "999.99" NO-UNDO.
DEF VAR v-wcum-score-d AS DEC EXTENT 50 FORMAT "999.99" NO-UNDO.
DEF VAR v-lscore-fld-loc AS INT EXTENT 30 NO-UNDO.
DEF VAR v-lscore-fld-id  AS INT EXTENT 30 NO-UNDO.
DEF VAR v-lscore-fld-num AS INT INIT 1 NO-UNDO.
DEF VAR v-wscore-fld-num AS INT INIT 0 NO-UNDO.
DEF VAR v-lscore-width AS INT INIT 6 NO-UNDO.
DEF VAR v-lscore-ttl-width AS INT INIT 65 NO-UNDO.
DEF VAR v-code AS CHAR NO-UNDO.
DEF VAR v-formula AS CHAR NO-UNDO.
DEF VAR op AS CHAR EXTENT 40 NO-UNDO.
DEF VAR nextop AS INT NO-UNDO.
DEF VAR num AS DEC EXTENT 40.
DEF VAR curnum AS CHAR NO-UNDO.
DEF VAR kar AS CHAR FORMAT "x" NO-UNDO.  /* style formula kalk vars */
DEF VAR v-in-paren AS LOG INIT NO NO-UNDO.
DEF VAR v-score AS DEC EXTENT 30 NO-UNDO.

DEF VAR v-sq-box  AS   DEC NO-UNDO.
DEF VAR v-16-s    AS   DEC NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-entry AS INT NO-UNDO.
DEF VAR v-round   AS LOG NO-UNDO.
DEF VAR lv-max-loc AS INT NO-UNDO. /* not to have error if v-lscore-fld-num >= 10 */
DEF VAR v-lscore-d-dec-text AS CHAR NO-UNDO.
DEF VAR v-lcum-score-d-dec-text AS CHAR NO-UNDO.
DEF VAR v-wscore-d-dec-text AS CHAR NO-UNDO.
DEF VAR v-wcum-score-d-dec-text AS CHAR NO-UNDO.

DEF BUFFER reftable2 FOR reftable.

IF v-cecscrn-dec THEN
DO:

   DEF VAR v-decimal-list AS CHAR NO-UNDO.
   DEF VAR v-convert-dec-list AS CHAR NO-UNDO.
   ASSIGN
   v-decimal-list = ".000000,.015625,.031250,.046875,.062500,.078125,"
                  + ".093750,.109375,.125000,.140625,.156250,.171875,.187500,"
                  + ".203125,.218750,.234375,.250000,.265625,.281250,.296875,"
                  + ".312500,.328125,.343750,.359375,.375000,.390625,.406250,"
                  + ".421875,.437500,.453125,.468750,.484375,.500000,.515625,"
                  + ".531250,.546875,.562500,.578125,.593750,.609375,.625000,"
                  + ".640625,.656250,.671875,.687500,.703125,.718750,.734375,"
                  + ".750000,.765625,.781250,.796875,.812500,.828125,.843750,"
                  + ".859375,.875000,.890625,.906250,.921875,.937500,.953125,"
                  + ".968750,.984375"
   v-convert-dec-list = ".00,.01,.02,.03,.04,.05,.06,.07,.08,.09,"
                      + ".10,.11,.12,.13,.14,.15,.16,.17,.18,.19,"
                      + ".20,.21,.22,.23,.24,.25,.26,.27,.28,.29,"
                      + ".30,.31,.32,.33,.34,.35,.36,.37,.38,.39,"
                      + ".40,.41,.42,.43,.44,.45,.46,.47,.48,.49,"
                      + ".50,.51,.52,.53,.54,.55,.56,.57,.58,.59,"
                      + ".60,.61,.62,.63".
END.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ROUND"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ROUND"
   sys-ctrl.descrip = "Round Up Scoring Allowances?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
END.
v-round = sys-ctrl.log-fld.

FIND est WHERE RECID(est) EQ v-recid1 NO-LOCK.

{cec/boxdesu.i}

FIND eb WHERE RECID(eb) EQ v-recid2 NO-LOCK NO-ERROR.

IF AVAIL eb THEN
  FIND FIRST ef
      WHERE ef.company EQ eb.company
        AND ef.est-no  EQ eb.est-no
        AND ef.form-no EQ eb.form-no
      NO-LOCK NO-ERROR.

ELSE DO:
  FIND ef WHERE RECID(ef) EQ v-recid2 NO-LOCK.
  FIND FIRST eb
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no
      NO-LOCK NO-ERROR.
END.

IF AVAIL eb THEN
FIND FIRST item
    {sys/look/itemgsW.i}
      AND item.i-no EQ eb.adhesive
    NO-LOCK NO-ERROR.
IF AVAIL eb THEN
FIND FIRST style
    WHERE style.company EQ eb.company
      AND style.style   EQ eb.style
    NO-LOCK NO-ERROR.
IF AVAIL style THEN
FIND box-design-hdr
    WHERE box-design-hdr.company = style.company
      AND box-design-hdr.design-no EQ style.design-no
    NO-LOCK NO-ERROR.
IF AVAIL eb                           AND
   eb.len EQ eb.wid                   AND 
   NOT v-cecscrn-dec AND
   ( PROGRAM-NAME(2) MATCHES "*b-estitm*" OR
     program-name(2) MATCHES "*v-est*")
THEN DO:
    
  FIND FIRST reftable
      WHERE reftable.reftable EQ "STYFLU"
        AND reftable.company  EQ eb.style
        AND reftable.loc      EQ eb.flute
        AND reftable.code     EQ "DIM-FIT"
      NO-LOCK NO-ERROR.
  IF AVAIL reftable THEN v-sq-box = reftable.val[1] / 6.25 * k_frac.
END.

IF AVAIL box-design-hdr THEN DO:
  ASSIGN
   v-lscore-c     = box-design-hdr.lscore
   v-lcum-score-c = "".
  select count(*) into v-num-lines from box-design-line
      where box-design-line.design-no = box-design-hdr.design-no.
  v-num-lines = v-num-lines + 4.
END.

ELSE DO:
  v-lscore-c = "No Design exists for this Estimate/Form.".
  RETURN.
END.

FOR EACH w-box-design-line:
  DELETE w-box-design-line.
END.

v-lscore-ttl-width = LENGTH(v-lscore-c).

/** find field locations in length Score var **/
DO i = 1 TO v-lscore-ttl-width:
  IF substr(v-lscore-c, i, 1) EQ "[" THEN
    ASSIGN
     v-lscore-fld-loc[v-lscore-fld-num] = i
     v-lscore-fld-id[v-lscore-fld-num]  = IF SUBSTRING(v-lscore-c,i + 1,1) = "" THEN int(substr(v-lscore-c, i + 2, 2))
                                          ELSE int(substr(v-lscore-c, i + 1, 2)) 
                                    /* YSK before [ 02 ] (i + 2) -> now before + [02] (i + 1) */
     v-lscore-fld-num = v-lscore-fld-num + 1.
END.
v-lscore-fld-num = v-lscore-fld-num - 1.

i = 0.
FOR EACH box-design-line OF box-design-hdr NO-LOCK:
  IF box-design-line.wscore NE "" THEN DO:
    i = i + 1.
    CREATE w-box-design-line.
    ASSIGN
     w-box-design-line.line-no       = box-design-line.line-no
     w-box-design-line.wscore-c      = box-design-line.wscore
     w-box-design-line.wcum-score-c  = box-design-line.wcum-score
     w-box-design-line.wscore-fld-id = IF SUBSTRING(TRIM(box-design-line.wscore),2,1) = "" THEN int(substr(TRIM(box-design-line.wscore), 3, 2))
                                       ELSE int(substr(TRIM(box-design-line.wscore), 2, 2))   /* ysk not need space */
     v-wscore-fld-num                = v-wscore-fld-num + 1.

  END.
END.

DO v-j = 1 TO 2:
  v-score = 0.

  /**************************************************************
   Find Length Scores
   **************************************************************/
  IF v-j EQ 1 THEN DO:
    RELEASE reftable.
    v-code = "7".
    IF AVAIL item THEN
      IF item.mat-type EQ "G" THEN v-code = IF eb.tab-in THEN "3" ELSE "4".
      ELSE
      IF item.mat-type EQ "S" THEN v-code = IF eb.tab-in THEN "5" ELSE "6".

    FIND FIRST reftable
        WHERE reftable.reftable EQ "STYSCORE"
          AND reftable.company  EQ eb.style
          AND reftable.loc      EQ eb.flute
          AND reftable.code     EQ v-code
          AND reftable.code2    EQ ""
        NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
    DO i = 1 TO EXTENT(lv-k-len-scr-type):
      lv-k-len-scr-type[i] = substr(reftable.dscr,i,1).
    END.

    FIND FIRST reftable
        WHERE reftable.reftable EQ "STYFLU"
          AND reftable.company  EQ eb.style
          AND reftable.loc      EQ eb.flute
          AND reftable.code     EQ v-code
          AND reftable.code2    EQ ""
        NO-LOCK NO-ERROR.

    FIND FIRST reftable2
        WHERE reftable2.reftable EQ "STYFLU"
          AND reftable2.company  EQ eb.style
          AND reftable2.loc      EQ eb.flute
          AND reftable2.code     EQ v-code
          AND reftable2.code2    EQ "1"
        NO-LOCK NO-ERROR.

    v-formula = style.formula[2].
  END.

  ELSE DO:
  /**************************************************************
   Find Width Scores
   **************************************************************/

    FIND FIRST reftable
        WHERE reftable.reftable EQ "STYSCORE"
          AND reftable.company  EQ eb.style
          AND reftable.loc      EQ eb.flute
          AND reftable.code     EQ "2"
          AND reftable.code2    EQ ""
        NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
    DO i = 1 TO EXTENT(lv-k-wid-scr-type):
      lv-k-wid-scr-type[i] = substr(reftable.dscr,i,1).
    END.

    FIND FIRST reftable
        WHERE reftable.reftable EQ "STYFLU"
          AND reftable.company  EQ eb.style
          AND reftable.loc      EQ eb.flute
          AND reftable.code     EQ "2"
        NO-LOCK NO-ERROR.

    FIND FIRST reftable2
        WHERE reftable2.reftable EQ "STYFLU"
          AND reftable2.company  EQ eb.style
          AND reftable2.loc      EQ eb.flute
          AND reftable2.code     EQ "2"
          AND reftable2.code2    EQ "1"
        NO-LOCK NO-ERROR.
    v-formula = style.formula[1].
  END.

  IF AVAIL reftable THEN
  DO i = 1 TO 12: /* must be 12, total is in 13 */
    v-score[i] = reftable.val[i].
  END.

  IF AVAIL reftable2 THEN
  DO i = 1 TO 8: /* must be 8 (12 + 8 = 20) */
    v-score[12 + i] = reftable2.val[i].
  END.

  /**************************************************************
   Build Dimension Formula
   **************************************************************/
  /* get rid of any blank/space or invalid char */
  DO i = 1 TO LENGTH(v-formula):
   IF substr(v-formula, i, 1) EQ ""                                   OR
      index("0123456789.+-LWDTFJBOSI()",substr(v-formula, i ,1)) EQ 0 THEN
     v-formula = substr(v-formula, 1, i - 1) + substr(v-formula, i + 1).
  END.

  ASSIGN
   num    = 0     /* zero arrays */
   op     = ""
   nextop = 1.

  loop:
  DO i = 1 TO LENGTH(v-formula):
    kar = substr(v-formula, i, 1).
    IF INDEX("+-()",kar) GT 0 THEN DO:
      op[nextop] = kar.
    END.

    ELSE
    IF INDEX("LWDJTSFBOI",kar) GT 0 THEN DO:

      CASE kar:
          WHEN "L" THEN
             num[nextop] = eb.len.
          WHEN "W" THEN
             num[nextop] = eb.wid.
          WHEN "D" THEN
             num[nextop] = eb.dep.
          WHEN "J" THEN
             num[nextop] = eb.gluelap.
          WHEN "T" THEN
             num[nextop] = eb.tuck.
          WHEN "F" THEN
             num[nextop] = eb.dust.
          WHEN "B" THEN
             num[nextop] = eb.fpanel.
          WHEN "O" THEN
             num[nextop] = eb.lock.
          WHEN "I" THEN
             num[nextop] = style.dim-fit.
      END CASE.
      
      IF nextop GT 1 AND num[nextop - 1] NE 0 THEN
        ASSIGN
         num[nextop - 1] = num[nextop - 1] * num[nextop]
         num[nextop]     = 0
         nextop          = nextop - 1.
    END.
    ELSE DO:
      curnum = "".
      DO WHILE (KEYCODE(kar) GE 48 AND keycode(kar) LE 57) OR
               keycode(kar) EQ 46:
        ASSIGN
         curnum = curnum + kar
         i      = i + 1
         kar    = substr(v-formula,i,1).
      END.
      ASSIGN
       i           = i - 1
       num[nextop] = dec(curnum).
      IF nextop GT 1 AND num[nextop - 1] NE 0 THEN
        ASSIGN
         num[nextop - 1] = num[nextop - 1] * num[nextop]
         num[nextop]     = 0
         nextop          = nextop - 1.
    END.
    nextop = nextop + 1.
  END.

  /**************************************************************
   Calculate Length Dimensions
  **************************************************************/
  IF v-j EQ 1 THEN DO:
     ASSIGN
      v-in-paren = NO
      v-i        = 1.
     DO i = 1 TO 40:
       IF op[i] NE "" THEN DO:
         IF op[i] EQ "(" THEN v-in-paren = YES.
         IF op[i] EQ ")" THEN v-in-paren = NO.
         IF op[i] EQ "+" AND NOT v-in-paren THEN v-i = v-i + 1.
       END.
       ELSE
       IF num[i] NE 0 THEN DO:
         IF v-in-paren AND v-lscore-d[v-i] NE 0 THEN DO:
           IF op[i - 1] EQ "+" THEN v-lscore-d[v-i] = v-lscore-d[v-i] + num[i].
           IF op[i - 1] EQ "-" THEN v-lscore-d[v-i] = v-lscore-d[v-i] - num[i].
         END.
         ELSE v-lscore-d[v-i] = num[i].
       END.
     END.
     DO i = 1 TO EXTENT(eb.k-len-array2):
       IF eb.k-len-array2[i] EQ 0 THEN LEAVE.
    
       IF i EQ 1 THEN
         ASSIGN
          v-lscore-d = 0
          v-score    = 0.
    
       ASSIGN
        v-lscore-fld-num   = i
        v-lscore-d[i]      = eb.k-len-array2[i]
        v-lscore-fld-id[i] = i.
     END.
    
     IF v-lscore-fld-num + 1 NE v-i THEN DO:

       ASSIGN
        v-i              = v-lscore-fld-num + 1
        v-lscore-c       = ""
        v-lscore-fld-loc = 0.
    
       lv-max-loc = IF v-lscore-fld-num >= 20 THEN 210
                    ELSE IF v-lscore-fld-num >= 12 THEN 137  
                    ELSE IF v-lscore-fld-num >= 10 THEN 83
                    ELSE 65.
    
       DO i = 1 TO 30:      /*  was ((  ( 65 - ((60))  ) / 9 ) * 1)  error if v-lscore-fld-num >= 10 */
         v-lscore-fld-loc[i] = ROUND(((lv-max-loc - ((1 + v-lscore-fld-num) * 6)) / v-lscore-fld-num) * i,0) 
                               + ((i - 1) * 6).
       END.
     END.
    
     DO i = 1 TO v-lscore-fld-num:
    
       ASSIGN
        v-lscore-d[i] = v-lscore-d[i] + {sys/inc/k16bv.i v-score[i]}
        v-lscore-d[i] = v-lscore-d[i] * li-16-32.
    
       IF v-round THEN DO:
         {sys/inc/roundup.i v-lscore-d[i]}
       END.
       ELSE IF v-cecscrn-char NE "Decimal" THEN
         v-lscore-d[i] = TRUNCATE(v-lscore-d[i],0).
    
       ASSIGN
        v-lscore-d[i]     = v-lscore-d[i] / li-16-32
        v-lcum-score-d[i] = (IF i GT 1 THEN v-lcum-score-d[i - 1] ELSE 0) +
                             v-lscore-d[i].
     END.

     DO i = 1 TO v-lscore-fld-num:
       DO j = 1 TO v-lscore-fld-num:
         IF v-lscore-fld-id[j] EQ i THEN DO:
           ASSIGN
            v-lscor2-d[i]     = v-lscore-d[j]
            v-lcum-scor2-d[i] = v-lcum-score-d[j].
         END.
       END.
     END.
     dPanelsLength = v-lscor2-d.
     DO i = 1 TO v-lscore-fld-num:

       IF v-box-uom EQ "Inches"                      OR
          (v-box-uom EQ "Both" AND (NOT est.metric)) THEN
         ASSIGN
          v-lscore-d[i]     = {sys/inc/k16v.i v-lscor2-d[i]}
          v-lcum-score-d[i] = {sys/inc/k16v.i v-lcum-scor2-d[i]}.
       ELSE
         ASSIGN
          v-lscore-d[i]     = ROUND(v-lscor2-d[i]     * 25.4,0)
          v-lcum-score-d[i] = ROUND(v-lcum-scor2-d[i] * 25.4,0).
     END.
    
     IF NOT v-cecscrn-dec OR v-sc-fmt EQ "->>>>9" THEN
     DO i = 1 TO v-lscore-fld-num:
        IF i GT v-i THEN
           ASSIGN
              OVERLAY(v-lscore-c,    v-lscore-fld-loc[i],6) = FILL(" ",6)
              OVERLAY(v-lcum-score-c,v-lscore-fld-loc[i],6) = FILL(" ",6).
        ELSE
           ASSIGN
              OVERLAY(v-lscore-c,    v-lscore-fld-loc[i],6) = STRING(v-lscore-d[i],v-sc-fmt)
              OVERLAY(v-lcum-score-c,v-lscore-fld-loc[i],6) = STRING(v-lcum-score-d[i],v-sc-fmt).
     END.
     ELSE
     DO i = 1 TO v-lscore-fld-num:
        IF i GT v-i THEN
           ASSIGN
              OVERLAY(v-lscore-c,    v-lscore-fld-loc[i],6) = FILL(" ",6)
              OVERLAY(v-lcum-score-c,v-lscore-fld-loc[i],6) = FILL(" ",6).
        ELSE
        DO:
           ASSIGN
              v-index = INDEX(STRING(v-lscore-d[i],v-sc-fmt),".")
              v-lscore-d-dec-text = SUBSTRING(STRING(v-lscore-d[i],v-sc-fmt),v-index)
              v-entry = LOOKUP(v-lscore-d-dec-text,v-decimal-list).

           IF v-entry NE 0 THEN
              v-lscore-d-dec-text = SUBSTRING(STRING(v-lscore-d[i],v-sc-fmt),1,v-index - 1) + ENTRY(v-entry,v-convert-dec-list).
           ELSE
              v-lscore-d-dec-text = STRING(v-lscore-d[i],v-sc-fmt).

           ASSIGN
              v-index = INDEX(STRING(v-lcum-score-d[i],v-sc-fmt),".")
              v-lcum-score-d-dec-text = SUBSTRING(STRING(v-lcum-score-d[i],v-sc-fmt),v-index)
              v-entry = LOOKUP(v-lcum-score-d-dec-text,v-decimal-list).

           IF v-entry NE 0 THEN
              v-lcum-score-d-dec-text = SUBSTRING(STRING(v-lcum-score-d[i],v-sc-fmt),1,v-index - 1) + ENTRY(v-entry,v-convert-dec-list).
           ELSE
              v-lcum-score-d-dec-text = STRING(v-lcum-score-d[i],v-sc-fmt).

           ASSIGN
           OVERLAY(v-lscore-c,    v-lscore-fld-loc[i],6) = v-lscore-d-dec-text
           OVERLAY(v-lcum-score-c,v-lscore-fld-loc[i],6) = v-lcum-score-d-dec-text.
        END.
     END.

  END.
  /**************************************************************
   Calculate Width Dimensions
  **************************************************************/
  ELSE DO:
    v-in-paren = NO.
    v-i = 1.
    DO i = 1 TO 40:
      IF op[i] NE "" THEN DO:
        IF op[i] EQ "(" THEN v-in-paren = YES.
        ELSE IF op[i] EQ ")" THEN v-in-paren = NO.
        ELSE IF op[i] EQ "+" AND NOT v-in-paren THEN v-i = v-i + 1.
      END.
      ELSE
      IF num[i] NE 0 THEN DO:
        IF v-in-paren AND v-wscore-d[v-i] NE 0 THEN DO:
          IF op[i - 1] EQ "+" THEN v-wscore-d[v-i] = v-wscore-d[v-i] + num[i].
          ELSE IF op[i - 1] EQ "-" THEN v-wscore-d[v-i] = v-wscore-d[v-i] - num[i].
        END.
        ELSE v-wscore-d[v-i] = num[i].
      END.
    END.

    DO i = 1 TO EXTENT(eb.k-wid-array2):
      IF eb.k-wid-array2[i] EQ 0 THEN LEAVE.

      IF i EQ 1 THEN
        ASSIGN
         v-wscore-d = 0
         v-score    = 0.

      ASSIGN
       v-wscore-fld-num = i
       v-wscore-d[i]    = eb.k-wid-array2[i].
    END.
    v-wscore-d[1] = v-wscore-d[1] - v-sq-box.

    DO i = 30 TO 1 BY -1:
      IF v-wscore-d[i] NE 0 THEN DO:
        v-wscore-d[i] = v-wscore-d[i] - v-sq-box.
        LEAVE.
      END.
    END.
    IF v-wscore-fld-num + 1 NE v-i THEN DO:
      v-i = v-wscore-fld-num + 1.
      FIND LAST box-design-line OF box-design-hdr NO-LOCK NO-ERROR.
      
      IF AVAILABLE box-design-line THEN DO i = 1 TO 30:
        FIND FIRST w-box-design-line
            WHERE w-box-design-line.wscore-fld-id EQ i
            NO-ERROR.
        IF NOT AVAIL w-box-design-line THEN DO:
          CREATE w-box-design-line.
          w-box-design-line.wscore-fld-id = i.
        END.
        w-box-design-line.line-no =
                           ROUND((box-design-line.line-no - 1) / v-i * i,0) + 1.
      END.
    END.

    DO i = 1 TO v-wscore-fld-num:
      ASSIGN
       v-wscore-d[i] = {sys/inc/k16v.i  v-wscore-d[i]}
       v-wscore-d[i] = {sys/inc/k16bv.i v-wscore-d[i]}
       v-wscore-d[i] = v-wscore-d[i] + ({sys/inc/k16bv.i v-score[i]})
       v-wscore-d[i] = v-wscore-d[i] * li-16-32.
      
      IF v-round THEN DO:
        {sys/inc/roundup.i v-wscore-d[i]}
      END.
      ELSE IF v-cecscrn-char NE "Decimal" THEN
        v-wscore-d[i] = trunc(v-wscore-d[i],0).
        
      ASSIGN
       v-wscore-d[i]     = v-wscore-d[i] / li-16-32
       v-wcum-score-d[i] = (IF i GT 1 THEN v-wcum-score-d[i - 1] ELSE 0) +
                            v-wscore-d[i].

      FIND FIRST w-box-design-line
          WHERE w-box-design-line.wscore-fld-id EQ i
          NO-ERROR.

      IF AVAIL w-box-design-line THEN
        IF v-box-uom EQ "Inches"                      OR
           (v-box-uom EQ "Both" AND (NOT est.metric)) THEN
          ASSIGN
           w-box-design-line.wscore-d     = {sys/inc/k16v.i v-wscore-d[i]}
           w-box-design-line.wcum-score-d = {sys/inc/k16v.i v-wcum-score-d[i]}.
        ELSE
          ASSIGN
           w-box-design-line.wscore-d     = ROUND(v-wscore-d[i]     * 25.4,0)
           w-box-design-line.wcum-score-d = ROUND(v-wcum-score-d[i] * 25.4,0).
    END.
    dPanelsWidth = v-wscore-d.
    
    FIND FIRST w-box-design-line NO-ERROR.
    IF AVAIL w-box-design-line THEN
    DO:
       IF NOT v-cecscrn-dec OR v-sc-fmt EQ "->>>>9" THEN
       DO i = 1 TO v-wscore-fld-num:
          IF i GT v-i THEN
            ASSIGN
             w-box-design-line.wscore-c     = ""
             w-box-design-line.wcum-score-c = "".
          ELSE
            ASSIGN
             w-box-design-line.wscore-c     =
                                    STRING(w-box-design-line.wscore-d,v-sc-fmt)
             w-box-design-line.wcum-score-c =
                                    STRING(w-box-design-line.wcum-score-d,v-sc-fmt).
          
         
          FIND NEXT w-box-design-line NO-ERROR.
          IF NOT AVAIL w-box-design-line THEN LEAVE.
       END.
       ELSE
       DO i = 1 TO v-wscore-fld-num:
          IF i GT v-i THEN
            ASSIGN
             w-box-design-line.wscore-c     = ""
             w-box-design-line.wcum-score-c = "".
          ELSE
          DO:
             ASSIGN
              v-index = INDEX(STRING(w-box-design-line.wscore-d,v-sc-fmt),".")
              v-wscore-d-dec-text = SUBSTRING(STRING(w-box-design-line.wscore-d,v-sc-fmt),v-index)
              v-entry = LOOKUP(v-wscore-d-dec-text,v-decimal-list).

             IF v-entry NE 0 THEN
                v-wscore-d-dec-text = SUBSTRING(STRING(w-box-design-line.wscore-d,v-sc-fmt),1,v-index - 1) + ENTRY(v-entry,v-convert-dec-list).
             ELSE
                v-wscore-d-dec-text = STRING(w-box-design-line.wscore-d,v-sc-fmt).

             ASSIGN
                v-index = INDEX(STRING(w-box-design-line.wcum-score-d,v-sc-fmt),".")
                v-wcum-score-d-dec-text = SUBSTRING(STRING(w-box-design-line.wcum-score-d,v-sc-fmt),v-index)
                v-entry = LOOKUP(v-wcum-score-d-dec-text,v-decimal-list).

              IF v-entry NE 0 THEN
                 v-wcum-score-d-dec-text = SUBSTRING(STRING(w-box-design-line.wcum-score-d,v-sc-fmt),1,v-index - 1) + ENTRY(v-entry,v-convert-dec-list).
              ELSE
                 v-wcum-score-d-dec-text = STRING(w-box-design-line.wcum-score-d,v-sc-fmt).

              ASSIGN
                 w-box-design-line.wscore-c = v-wscore-d-dec-text
                 w-box-design-line.wcum-score-c = v-wcum-score-d-dec-text.
          END.
         
          FIND NEXT w-box-design-line NO-ERROR.
          IF NOT AVAIL w-box-design-line THEN LEAVE.
       END.

    END.
  END.
END.

/* end ---------------------------------- copr. 1995  advanced software, inc. */
