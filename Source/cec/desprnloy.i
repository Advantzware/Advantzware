/* -------------------------------------------------- cec/desprncap.i  */
/* Box Design Print                                                    */
/* ------------------------------------------------------------------- */

def input parameter v-est-id as recid no-undo.
DEF INPUT PARAMETER v-score AS CHAR NO-UNDO.

def var li-num-of-line as int no-undo.
def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.

{cecrep/jobtick2.i "shared"}

{cec/descalc.i new}
def var K_FRAC as dec init 6.25 no-undo.
def var v-line-text     like box-design-line.line-text.
def var v-int           as   int.
def var v-char          as   char.
def var v-hdr           as   char format "x(80)".
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR v-num AS DEC NO-UNDO.
DEF VAR v-wscore-metric AS DEC EXTENT 20 NO-UNDO.
DEF VAR v-wcum-metric AS DEC EXTENT 20 NO-UNDO.
DEF VAR lv-space AS CHAR NO-UNDO.
DEF VAR v-start-ext AS INT  EXTENT 20 NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-lscore-dec AS DEC EXTENT 20 NO-UNDO .
DEF VAR v-dec-count AS INT NO-UNDO.
DEF VAR v-prev-frac-len AS INT EXTENT 20 NO-UNDO.
DEF VAR v-cum-w AS DEC NO-UNDO.

def buffer b-bdl for box-design-line.
    /* for metric display */
DEF VAR lv-tmp AS cha NO-UNDO.
DEF VAR lv-tmp-val AS cha NO-UNDO.
DEF VAR lv-met-lsc AS cha NO-UNDO.
DEF VAR lv-met-lcum AS cha NO-UNDO.
DEF VAR ld-tmp-scr AS DEC NO-UNDO.
DEF VAR li-num-space AS INT NO-UNDO.
DEF VAR v-boxln AS INT NO-UNDO.
DEF VAR lv-str AS CHAR NO-UNDO.
DEF VAR lv-lscore-c-fraction AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR lv-lcum-score-c-fraction AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR lv-wscore-c-fraction AS CHAR NO-UNDO.
DEF VAR lv-wcum-score-c-fraction AS CHAR NO-UNDO.
DEF VAR v-lscore-c-metric AS CHAR NO-UNDO.
DEF VAR v-lcum-score-c-metric AS CHAR NO-UNDO.
DEF VAR v-lscore-c-metric-new AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-lcum-score-c-metric-new AS CHAR FORMAT "X(150)" NO-UNDO.
DEF VAR v-cumul AS DEC NO-UNDO.
DEF VAR v-count2 AS INT NO-UNDO.
DEF VAR v-lscore-dec-sum AS DEC NO-UNDO.
DEF VAR v-image AS LOG NO-UNDO.
DEF VAR v-skip-right-flap AS LOG NO-UNDO.
DEF VAR v-global-dec-count AS INT NO-UNDO.
DEF VAR v-spaces AS INT EXTENT 20 NO-UNDO.

form w-box-design-line.wcum-score-c             at 1
     w-box-design-line.wscore-c
     v-line-text           format "x(65)"  AT 23              SKIP
    with down frame f1 no-labels NO-BOX WIDTH 150.

form w-box-design-line.wscore-c                 at 1
     v-line-text                     format "x(65)"
     w-box-design-line.wcum-score-c                                         SKIP
    with down frame f2 no-labels NO-BOX WIDTH 120.

{sys/inc/f16to32.i}

find est where recid(est) eq v-est-id no-lock.

for each ef
    where ef.company = est.company 
      AND ef.est-no    eq est.est-no
      and (v-ef-recid eq ? or
           v-ef-recid eq recid(ef))
    no-lock,
    
    each eb
    where eb.company = ef.company
      AND eb.est-no   eq ef.est-no
      and eb.form-no eq ef.form-no
      AND (v-out1-id = ? or RECID(eb) = v-out1-id)
    no-lock:

  for each w-box-design-line:
    delete w-box-design-line.
  end.

  v-image = NO.

  find first item
      {sys/look/itemgs.w}
        and item.i-no eq eb.adhesive
      no-lock no-error.

  find FIRST box-design-hdr
      where box-design-hdr.design-no eq 0
        and box-design-hdr.company = eb.company 
        AND box-design-hdr.est-no    eq eb.est-no
        and box-design-hdr.form-no   eq eb.form-no
        and box-design-hdr.blank-no  eq eb.blank-no
      no-lock no-error.

  find first style
      where style.company eq eb.company
        and style.style   eq eb.style
      no-lock no-error.
  if (not avail box-design-hdr) and avail style then
  find FIRST box-design-hdr
      where box-design-hdr.design-no eq style.design-no
      no-lock no-error.

  if avail box-design-hdr          and
     box-design-hdr.design-no eq 0 then do:

    assign
     v-lscore-c     = box-design-hdr.lscore
     v-lcum-score-c = box-design-hdr.lcum-score.

    ASSIGN
       lv-tmp = ""
       lv-tmp-val = ""
       lv-met-lsc = ""
       lv-met-lcum = "".

    DO i = 1 TO LENGTH(box-design-hdr.lscore):
       lv-tmp-val = SUBSTRING(box-design-hdr.lscore,i,1).
       IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
       ELSE do:

          IF lv-tmp <> "" THEN 
            ASSIGN
               ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4
               lv-tmp = ""
               li-num-space = 0.
          
          ASSIGN
            li-num-space = li-num-space + 1
            lv-met-lsc = lv-met-lsc + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                         IF ld-tmp-scr = 0 THEN "" ELSE trim(string(ld-tmp-scr))
            ld-tmp-scr = 0.
       END.                                    
    END. 
    IF lv-tmp <> "" THEN
       ASSIGN
          ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4
          lv-met-lsc = lv-met-lsc + string(ld-tmp-scr).
    
    ASSIGN
       lv-tmp = ""
       lv-tmp-val = ""
       lv-met-lcum = ""
       ld-tmp-scr = 0
       li-num-space = 0.

    DO i = 1 TO LENGTH(box-design-hdr.lcum-score):
       lv-tmp-val = SUBSTRING(box-design-hdr.lcum-score,i,1).
       IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
       ELSE do:
          IF lv-tmp <> "" THEN
             ASSIGN
                ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4
                lv-tmp = ""
                li-num-space = 0.
          
          ASSIGN
             li-num-space = li-num-space + 1
             lv-met-lcum = lv-met-lcum + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                           IF ld-tmp-scr = 0 THEN "" ELSE trim(string(ld-tmp-scr))
             ld-tmp-scr = 0.
       END.                   
    END.
    IF lv-tmp <> "" THEN
       ASSIGN
        ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4
        lv-met-lcum = lv-met-lcum + string(ld-tmp-scr).
    
     IF est.metric THEN
        ASSIGN
           v-lscore-c     = lv-met-lsc
           v-lcum-score-c = lv-met-lcum.

     ASSIGN
       v-lscore-c-metric = lv-met-lsc
       v-lcum-score-c-metric = lv-met-lcum
       i = 0.

    for each box-design-line of box-design-hdr
        where box-design-line.wcum-score ne ""
        no-lock:

      i = i + 1.
      create w-box-design-line.
      assign
       w-box-design-line.line-no       = box-design-line.line-no
       w-box-design-line.wscore-c      = IF est.metric THEN string(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wscore
       w-box-design-line.wcum-score-c  = IF est.metric THEN string(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wcum-score.
    end.
  end.

  else do:
    run cec/descalc.p (recid(est), recid(eb)).
    if v-lscore-c begins "No Design" then return.
  end.
  
  assign
   li-num-of-line = 0.
 
  IF AVAIL eb AND eb.cad-no = "" THEN
     v-hdr = "                  " + 
             "Design #: " +
             trim(string(if avail style and box-design-hdr.design-no eq 0 then
                           style.design-no else box-design-hdr.design-no,">>>")) +
             "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
             IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal" .
  ELSE
     v-hdr = "                  " + 
             "Design #: " +
             trim(string(if avail style and box-design-hdr.design-no eq 0 then
                           style.design-no else box-design-hdr.design-no,">>>")) +
             "   " + box-design-hdr.DESCRIPTION.

  put {1} v-hdr skip.

  ASSIGN
     v-start-ext = 0
     v-lscore-dec = 0
     v-dec-count = 0
     v-lscore-dec-sum = 0
     v-prev-frac-len = 0.

  RUN convert-to-frac-proc(v-lscore-c, OUTPUT lv-lscore-c-fraction, YES).

  v-global-dec-count = v-dec-count.

  DO i = 1 TO v-dec-count:
         
     v-lscore-dec-sum = v-lscore-dec-sum + v-lscore-dec[i].

     IF v-lscore-dec-sum GT 0 THEN
        RUN sys/inc/dec-frac.p (v-lscore-dec-sum,16, OUTPUT lv-str).
     ELSE
        lv-str = "0".

     ASSIGN
        v-spaces[i] = v-start-ext[i] - LENGTH(lv-lcum-score-c-fraction) + 2
        lv-lcum-score-c-fraction = lv-lcum-score-c-fraction
                                 + FILL(" ",v-start-ext[i] - LENGTH(lv-lcum-score-c-fraction)) + lv-str.
     
  END.

  /*IF est.metric THEN DO:
     IF LENGTH(lv-lscore-c-fraction) <= 90 THEN
        put {1} space(7) "           "  lv-lscore-c-fraction  FORM "x(100)"  skip
                space(7) "           " lv-lcum-score-c-fraction FORM "x(100)"  skip.
     
     ELSE
        put {1} "<P12>" space(7) "           "  lv-lscore-c-fraction  FORM "x(135)"  skip
            space(7) "           " lv-lcum-score-c-fraction FORM "x(135)"  "<P12>" skip.
     
  END.
  ELSE DO:
     
     IF LENGTH(lv-lscore-c-fraction) <= 90 THEN
        put {1} space(3) lv-lscore-c-fraction  FORM "x(90)"  skip
                space(3) lv-lcum-score-c-fraction FORM "x(90)"  skip.
     ELSE
         put {1} "<P12>" space(3) lv-lscore-c-fraction  FORM "x(135)"  skip
               space(3) lv-lcum-score-c-fraction FORM "x(135)" "<P12>" skip.
  END.*/

  IF est.metric THEN DO:
    IF LENGTH(v-lscore-c) <= 90 THEN
       put {1} space(7) "           "  v-lscore-c  FORM "x(100)"  skip
              space(7) "           " v-lcum-score-c FORM "x(100)"  skip.
    
    ELSE
       put {1} "<P9>" space(7) "           "  v-lscore-c  FORM "x(135)"  skip
              space(7) "           " v-lcum-score-c FORM "x(135)" "<P12>" skip.
    
  END.   /* metric */
  ELSE DO:
     IF LENGTH(v-lscore-c) <= 90 THEN
        put {1} space(3) /* "     " */  v-lscore-c  FORM "x(90)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(90)"  skip.
     
     ELSE
        put {1} "<P9>" space(3) v-lscore-c  FORM "x(145)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(145)"  "<P12>" skip.
     
  END.

  ASSIGN
     v-lines = v-lines + 4
     v-wscore-metric = 0
     v-wcum-metric = 0
     v-count = 0.

  PUT SKIP(1).

  IF box-design-hdr.box-image = "" THEN DO:

    for each b-bdl of box-design-hdr
        no-lock
        WHERE b-bdl.line-text <> ""
        break by b-bdl.design-no:
   
      find first w-box-design-line
          where w-box-design-line.line-no eq b-bdl.line-no
          no-error.
   
      assign
       v-line-text = b-bdl.line-text.
       
      /* xprint bug : if print "-" more than 3 in a row, xprint draw rectangular and line*/
      DO i = 1 TO LENGTH(v-line-text):
         IF substring(v-line-text,i,1) = "-" THEN SUBSTRING(v-line-text,i,1) = "_".       
      END.

      IF AVAIL w-box-design-line AND
         v-count LE 20 THEN
         ASSIGN
            v-count = v-count + 1
            v-wscore-metric[v-count] = IF NOT est.metric THEN ({sys/inc/k16bv.i dec(w-box-design-line.wscore-c)}) * 25.4
                                       ELSE dec(w-box-design-line.wscore-c)
            v-wcum-metric[v-count]   = v-wscore-metric[v-count] + v-cum-w
            v-cum-w = v-cum-w + v-wscore-metric[v-count].

      IF AVAIL w-box-design-line THEN
      DO:
         RUN convert-to-frac-proc(w-box-design-line.wscore-c, OUTPUT lv-wscore-c-fraction, NO).
         RUN convert-to-frac-proc(w-box-design-line.wcum-score-c, OUTPUT lv-wcum-score-c-fraction, NO).
      END.

      /*display 
              TRIM(lv-wcum-score-c-fraction) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
              TRIM(lv-wscore-c-fraction)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
      down with frame f1.*/

      display 
              TRIM(w-box-design-line.wcum-score-c) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
              TRIM(w-box-design-line.wscore-c)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  /*AT 14 */ FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
      down  with frame f1.
   
      v-lines = v-lines + 1.
    
    end. /* for each b-bdl */
    PAGE.
  END. /* no box image */
  ELSE DO:     

     v-image = YES.
     FILE-INFO:FILE-NAME = box-design-hdr.box-image.
                                
     PUT unformatted "<C3><#30><R+40><C+85><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">".
     PUT UNFORMATTED "<=30>" SKIP.

     FOR EACH box-design-line OF box-design-hdr NO-LOCK:

         IF box-design-line.wscore <> "" AND v-count LE 20 THEN
            ASSIGN
               v-count = v-count + 1
               v-wscore-metric[v-count] = IF NOT est.metric THEN ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4
                                          ELSE dec(box-design-line.wscore)
               v-wcum-metric[v-count]   = v-wscore-metric[v-count] + v-cum-w
               v-cum-w = v-cum-w + v-wscore-metric[v-count].

         IF est.metric AND box-design-line.wscore <> "" THEN
            RUN convert-to-frac-proc(string( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4),
                                     OUTPUT lv-wscore-c-fraction, NO).
         ELSE
            RUN convert-to-frac-proc(box-design-line.wscore, OUTPUT lv-wscore-c-fraction, NO).

         IF est.metric AND box-design-line.wcum-score <> "" THEN
            RUN convert-to-frac-proc(string( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4),
                                     OUTPUT lv-wcum-score-c-fraction, NO).
         ELSE
            RUN convert-to-frac-proc(box-design-line.wcum-score, OUTPUT lv-wcum-score-c-fraction, NO).

         /*
         PUT "<C87>" TRIM(lv-wscore-c-fraction) FORMAT "X(9)" " "
                     TRIM(lv-wcum-score-c-fraction) FORMAT "X(9)" SKIP. */

         PUT "<C87>" IF est.metric AND box-design-line.wscore <> "" THEN string( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wscore " " 
                     IF est.metric AND box-design-line.wcum-score <> "" THEN string( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4) ELSE box-design-line.wcum-score SKIP.
     END.
  END. /* else box image */

  /*print metric*/

  ASSIGN
     lv-tmp = ""
     v-lscore-c-metric-new = ""
     v-lcum-score-c-metric-new = ""
     v-cumul = 0
     v-count2 = v-global-dec-count.

  IF AVAIL style AND
     INDEX(style.formula[2],"J") GT 0 THEN
     ASSIGN
        v-skip-right-flap = YES
        v-count2 = v-count2
        v-global-dec-count = v-global-dec-count.
  ELSE
     v-skip-right-flap = NO.

  DO i = LENGTH(v-lscore-c-metric) TO 1 BY -1 :
  
     lv-tmp-val = SUBSTRING(v-lscore-c-metric,i,1).

     IF lv-tmp-val <> " " THEN
        lv-tmp = lv-tmp-val + lv-tmp.
     ELSE
        IF lv-tmp NE "" THEN
        DO:
           IF NOT(v-count2 EQ v-global-dec-count and v-skip-right-flap) THEN
           DO:
              ASSIGN
              v-num = ROUND(DEC(lv-tmp),0)
              v-cumul = v-cumul + v-num
              v-lscore-c-metric-new = STRING(v-num) + v-lscore-c-metric-new
              v-lcum-score-c-metric-new = STRING(v-cumul) + v-lcum-score-c-metric-new
              v-lscore-c-metric-new = FILL(" ",v-spaces[v-count2])
                                    + v-lscore-c-metric-new
              v-lcum-score-c-metric-new = FILL(" ",v-spaces[v-count2])
                                        + v-lcum-score-c-metric-new.
           END.
        
           ASSIGN
             lv-tmp = ""
             v-count2 = v-count2 - 1.
        END.
  END. 
     
  IF lv-tmp NE "" THEN
     ASSIGN
        v-num = ROUND(DEC(lv-tmp),0)
        v-cumul = v-cumul + v-num
        v-lscore-c-metric-new = STRING(v-num) + v-lscore-c-metric-new
        v-lcum-score-c-metric-new = STRING(v-cumul) + v-lcum-score-c-metric-new
        v-lscore-c-metric-new = FILL(" ",v-spaces[1])
                              + v-lscore-c-metric-new
        v-lcum-score-c-metric-new = FILL(" ",v-spaces[1])
                                  + v-lcum-score-c-metric-new.

  IF v-image THEN
     ASSIGN
       v-lscore-c-metric-new = "<=30><R+24>" + v-lscore-c-metric-new
       v-lcum-score-c-metric-new = "<=30><R+25>" + v-lcum-score-c-metric-new.
  
  IF est.metric THEN DO:
     IF LENGTH(v-lscore-c-metric-new) <= 90 THEN
        put {1} space(7) "           "  v-lscore-c-metric-new  FORM "x(100)"  SKIP
                space(7) "           " v-lcum-score-c-metric-new FORM "x(100)"  skip.
     
     ELSE
        put {1} "<P12>" space(7) "           "  v-lscore-c-metric-new  FORM "x(135)"  SKIP
            space(7) "           " v-lcum-score-c-metric-new FORM "x(135)"  "<P12>" skip.
  END.
  ELSE DO:

     IF LENGTH(v-lscore-c-metric-new) <= 90 THEN
        put {1} space(3) v-lscore-c-metric-new  FORM "x(90)"  SKIP
                space(3) v-lcum-score-c-metric-new FORM "x(90)"  skip.
     
     ELSE
        put {1} "<P12>" SPACE(3) v-lscore-c-metric-new  FORM "x(135)"  SKIP
            space(3) v-lcum-score-c-metric-new FORM "x(135)" "<P12>" skip.
  END.

  /*print metric width*/

  PUT SKIP(1).

  DO i = 1 TO v-count:
     PUT SPACE(82) ROUND(v-wscore-metric[i],0) " " ROUND(v-wcum-metric[i],0) SKIP(3).
  END.

  PUT "<P10>" .
  PAGE.
  end.
  
PROCEDURE convert-to-frac-proc:

   DEFINE INPUT PARAM ip-string AS CHAR FORMAT "X(100)" NO-UNDO.
   DEFINE OUTPUT PARAM op-string AS CHAR FORMAT "X(100)" NO-UNDO.
   DEFINE INPUT PARAM ip-store-start-pos AS LOG NO-UNDO.

   DEF VAR v-count AS INT INIT 1 NO-UNDO.
   DEF VAR lv-tmp-prev AS CHAR NO-UNDO.

   ASSIGN
      lv-tmp = ""
      lv-space = "".

   DO i = 1 TO LENGTH(ip-string):
     lv-tmp-val = SUBSTRING(ip-string,i,1).
     IF lv-tmp-val <> " " THEN
        lv-tmp = lv-tmp + lv-tmp-val.
     ELSE
     DO:
        lv-space = lv-space + " ".

        IF lv-tmp <> "" THEN DO:
           
           lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).

           IF DEC(lv-tmp) GT 0 THEN
              RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
           ELSE
              lv-str = "0".

           IF ip-store-start-pos AND v-count LE 20 THEN
              v-lscore-dec[v-count] = DEC(lv-tmp).

           IF INDEX(lv-str,"-") <= 0 THEN
              lv-tmp = lv-str.
           ELSE
              lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                       SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).

           ASSIGN
             op-string = op-string + lv-space + lv-tmp.

           IF ip-store-start-pos AND v-count LE 20 THEN
           DO:
              v-prev-frac-len[v-count] = LENGTH(lv-tmp).

              IF v-count NE 1 THEN
                 v-start-ext[v-count] = v-start-ext[v-count - 1] + LENGTH(lv-space) + v-prev-frac-len[v-count - 1].
              ELSE
                 v-start-ext[v-count] = LENGTH(lv-space).

              v-count = v-count + 1.
           END.
           ASSIGN 
             lv-tmp = ""
             lv-space = "".
        END.
     END.
  END.

  IF lv-tmp <> "" THEN do:
     lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).

     IF DEC(lv-tmp) GE 0 THEN
        RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
     ELSE
        lv-str = "0".

     IF ip-store-start-pos AND v-count LE 20 THEN
        v-lscore-dec[v-count] = DEC(lv-tmp).

     IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
     ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                   SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).

     IF ip-store-start-pos AND v-count LE 20 AND v-count GE 2 THEN
        v-start-ext[v-count] = v-start-ext[v-count - 1] + LENGTH(lv-space) + v-prev-frac-len[v-count - 1].
     
     op-string = op-string + lv-space + lv-tmp.
  END.

  v-dec-count = v-count.

END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
