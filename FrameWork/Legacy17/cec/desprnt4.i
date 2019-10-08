/* -------------------------------------------------- cec/desprnt.i 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */
/* 06/10/01  YSK have box design on next page if box-line is more than 15 lines */

def input parameter v-est-id as recid no-undo.
def var li-num-of-line as int no-undo.
def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.
def var K_FRAC as dec init 6.25 no-undo.
/*{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
{cecrep/jobtick2.i "shared"}
{cec/descalc.i new}

def var v-line-text     like box-design-line.line-text.
def var v-int           as   int.
def var v-char          as   char.
def var v-first         as   int.
def var v-last          as   int.
def var v-frst-col      like box-design-line.line-no extent 500.
def var v-last-col      like box-design-line.line-no extent 500.
def var v-triad         like sys-ctrl.log-fld.
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

def buffer b-bdl for box-design-line.
/* for metric display */
DEF VAR lv-tmp AS cha NO-UNDO.
DEF VAR lv-tmp-val AS cha NO-UNDO.
DEF VAR lv-met-lsc AS cha NO-UNDO.
DEF VAR lv-met-lcum AS cha NO-UNDO.
DEF VAR ld-tmp-scr AS DEC NO-UNDO.
DEF VAR li-num-space AS INT NO-UNDO.
DEF VAR lv-null-cnt AS INT NO-UNDO.
DEF VAR v-fgitem AS cha FORM "x(70)" NO-UNDO.
DEF VAR v-boxln AS INT NO-UNDO.
DEF VAR lv-str AS CHAR NO-UNDO.
DEF VAR lv-lscore-c-fraction AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR lv-lcum-score-c-fraction AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR lv-wscore-c-fraction AS CHAR NO-UNDO.
DEF VAR lv-wcum-score-c-fraction AS CHAR NO-UNDO.
DEF VAR v-lscore-c-metric AS CHAR NO-UNDO.
DEF VAR v-lcum-score-c-metric AS CHAR NO-UNDO.
DEF VAR v-lscore-c-metric-new AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-lcum-score-c-metric-new AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-cumul AS DEC NO-UNDO.
DEF VAR v-count2 AS INT NO-UNDO.
DEF VAR v-lscore-dec-sum AS DEC NO-UNDO.
DEF VAR v-image AS LOG NO-UNDO.

form w-box-design-line.wcum-score-c             at 1
     w-box-design-line.wscore-c
     v-line-text           format "x(65)"  AT 23              SKIP
    with down frame f1 no-labels NO-BOX WIDTH 150.

form w-box-design-line.wscore-c                 at 1
     v-line-text                     format "x(65)"
     w-box-design-line.wcum-score-c                                         SKIP
    with down frame f2 no-labels NO-BOX WIDTH 120.
{sys/inc/f16to32.i}
{sys/inc/jobcard.i C}
v-triad = sys-ctrl.char-fld eq "Triad".

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
    no-lock:

  for each w-box-design-line:
    delete w-box-design-line.
  end.

  /*if avail ef then
  find first eb
      where eb.company = ef.company
        AND eb.est-no   eq ef.est-no
        and eb.form-no eq ef.form-no
      no-lock no-error.

  if avail eb then*/
  find first item
      {sys/look/itemgsW.i}
        and item.i-no eq eb.adhesive
      no-lock no-error.

  /*if avail eb then*/
  find box-design-hdr
      where box-design-hdr.design-no eq 0
        and box-design-hdr.company = eb.company 
        AND box-design-hdr.est-no    eq eb.est-no
        and box-design-hdr.form-no   eq eb.form-no
        and box-design-hdr.blank-no  eq eb.blank-no
      no-lock no-error.

  /*if avail eb then*/
  find first style
      where style.company eq eb.company
        and style.style   eq eb.style
      no-lock no-error.
  if (not avail box-design-hdr) and avail style then
  find box-design-hdr
      where box-design-hdr.design-no eq style.design-no
      no-lock no-error.

  if avail box-design-hdr          and
     box-design-hdr.design-no eq 0 then do:

    assign
     v-lscore-c     = box-design-hdr.lscore
     v-lcum-score-c = box-design-hdr.lcum-score.

    i = 0.
    for each box-design-line of box-design-hdr
        where box-design-line.wcum-score ne ""
        no-lock:

      i = i + 1.
      create w-box-design-line.
      assign
       w-box-design-line.line-no       = box-design-line.line-no
       w-box-design-line.wscore-c      = IF est.metric THEN string( ROUND({sys/inc/k16bv.i dec(box-design-line.wscore)} * 25.4,0)  ) ELSE box-design-line.wscore
       w-box-design-line.wcum-score-c  = IF est.metric THEN string( ROUND({sys/inc/k16bv.i dec(box-design-line.wscore)} * 25.4,0)) ELSE box-design-line.wcum-score.
     
    end.
  end.
  else do:
    run cec/descalc.p (recid(est), recid(eb)).
    if v-lscore-c begins "No Design" then return.
  end.

  IF est.metric THEN DO:
     ASSIGN lv-tmp = ""
            lv-tmp-val = ""
            lv-met-lsc = ""
            lv-met-lcum = "".

     DO i = 1 TO LENGTH(box-design-hdr.lscore):
        lv-tmp-val = SUBSTRING(box-design-hdr.lscore,i,1).
        IF lv-tmp-val <> " " AND lv-tmp-val <> "[" AND lv-tmp-val <> "]"
            THEN lv-tmp = lv-tmp + lv-tmp-val.
        ELSE do:
             IF lv-tmp <> "" THEN DO:
               ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
               lv-tmp = "".
               li-num-space = 0.
             END.
             li-num-space = li-num-space + 1.
             lv-met-lsc = lv-met-lsc + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                          IF ld-tmp-scr = 0 THEN "" ELSE (string(round(ld-tmp-scr,0),">>>>>9")).
             ld-tmp-scr = 0.
        END.                                    
     END. 
     IF lv-tmp <> "" THEN do:
         ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
         lv-met-lsc = lv-met-lsc + string(round(ld-tmp-scr,0),">>>>>9").
     END.
     ASSIGN lv-tmp = ""
         lv-tmp-val = ""
         lv-met-lcum = ""
         ld-tmp-scr = 0
         li-num-space = 0.

     DO i = 1 TO LENGTH(box-design-hdr.lcum-score):
        lv-tmp-val = SUBSTRING(box-design-hdr.lcum-score,i,1).
        IF lv-tmp-val <> " " AND lv-tmp-val <> "[" AND lv-tmp-val <> "]"
           THEN lv-tmp = lv-tmp + lv-tmp-val.
        ELSE do:
             IF lv-tmp <> "" THEN DO:
               ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
               lv-tmp = "".
               li-num-space = 0.
             END.
             li-num-space = li-num-space + 1.
             lv-met-lcum = lv-met-lcum + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "")  +
                            IF ld-tmp-scr = 0 THEN "" ELSE (string(round(ld-tmp-scr,0),">>>>>9")).
             ld-tmp-scr = 0.
        END.                   
     END.
     IF lv-tmp <> "" THEN do:
         ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
         lv-met-lcum = lv-met-lcum + string(round(ld-tmp-scr,0),">>>>>9").
     END.

     ASSIGN v-lscore-c     = lv-met-lsc
            v-lcum-score-c = lv-met-lcum.

     ASSIGN
       v-lscore-c-metric = lv-met-lsc
       v-lcum-score-c-metric = lv-met-lcum.
  END. /* for metric display */
  ELSE DO:  /* reduce spaces  have space upto 7 */
      ASSIGN lv-tmp = ""
             lv-null-cnt = 0
             lv-tmp-val = ""
             .
      DO i = 1 TO LENGTH(v-lscore-c):
         lv-tmp-val = SUBSTRING(v-lscore-c,i,1).
         IF lv-tmp-val <> " " THEN do:
            IF lv-null-cnt = 0  THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val.
            ELSE lv-tmp = lv-tmp +
                          (IF lv-null-cnt >= 7 THEN FILL(" ", lv-null-cnt - 3) ELSE FILL(" ",lv-null-cnt) ) +
                          lv-tmp-val.
            lv-null-cnt = 0.
         /*ELSE IF lv-null-cnt <= 7 THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val
                                             lv-null-cnt = lv-null-cnt + 1.       */   
         END.
         ELSE lv-null-cnt = lv-null-cnt + 1.         
      END.
      v-lscore-c = lv-tmp.      
      ASSIGN lv-tmp = ""
             lv-null-cnt = 0
             lv-tmp-val = "".
      DO i = 1 TO LENGTH(v-lcum-score-c):
         lv-tmp-val = SUBSTRING(v-lcum-score-c,i,1).
         IF lv-tmp-val <> " " THEN do:
            IF lv-null-cnt = 0  THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val.
            ELSE lv-tmp = lv-tmp + 
                          (IF lv-null-cnt >= 7 THEN FILL(" ", lv-null-cnt - 3) ELSE FILL(" ",lv-null-cnt) ) +
                          lv-tmp-val.
            lv-null-cnt = 0.
         /*ELSE IF lv-null-cnt <= 7 THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val
                                             lv-null-cnt = lv-null-cnt + 1.       */   
         END.
         ELSE lv-null-cnt = lv-null-cnt + 1.         
         /*IF lv-tmp-val <> " " THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val
                                         lv-null-cnt = 0.
         ELSE IF lv-null-cnt <= 7 THEN ASSIGN lv-tmp = lv-tmp + lv-tmp-val
                                              lv-null-cnt = lv-null-cnt + 1.          
         ELSE lv-null-cnt = lv-null-cnt + 1. */
      END.
      v-lcum-score-c = lv-tmp.      
  END.    /* reduce spaces */

/* PUT{1} 500. */

  assign
   v-frst-col = 0
   v-last-col = 0
   li-num-of-line = 0.
 
  for each b-bdl of box-design-hdr
      no-lock
      break by b-bdl.design-no:

    v-line-text = b-bdl.line-text.
    if v-line-text <> "" then li-num-of-line = li-num-of-line + 1.   /* ysk*/
    
    if v-line-text ne "" then
    do v-int = 1 to length(v-line-text):
      if substr(v-line-text,v-int,1) ne "" then do:
        v-last-col[v-int] = b-bdl.line-no.
        if v-frst-col[v-int] eq 0 then v-frst-col[v-int] = b-bdl.line-no.
      end.
    end.
  end.
  
  /*============
  if v-num-lines + line-counter gt page-size then page.
       ===========  not working  YSK */
  /* ============ mods for page skip  =============*/
  
  if li-num-of-line > 15 and v-out1-id <> ? and v-out2-id <> ?
  then do:
      PAGE {1}.
  end.
  
  /* ===== end of mods ==========*/
    v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
          "            " + 
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
          IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal".
  FIND FIRST ITEM WHERE ITEM.company = ef.company
                    AND ITEM.i-no = ef.board NO-LOCK NO-ERROR.
  IF eb.stock-no <> "" THEN
     FIND itemfg WHERE itemfg.company = ef.company AND
                       itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
  v-fgitem = "Board:" + ef.board + "," + ITEM.i-name + "     FG Item: " + eb.stock-no + 
             IF AVAIL itemfg THEN itemfg.i-name ELSE "".


  put {1} "        " v-fgitem SKIP
          v-hdr skip.

  ASSIGN
     v-start-ext = 0
     v-lscore-dec = 0
     v-dec-count = 0
     v-lscore-dec-sum = 0
     v-prev-frac-len = 0.

  RUN convert-to-frac-proc(v-lscore-c, OUTPUT lv-lscore-c-fraction, YES).

  DO i = 1 TO v-dec-count:
         
     v-lscore-dec-sum = v-lscore-dec-sum + v-lscore-dec[i].

     IF v-lscore-dec-sum GT 0 THEN
        RUN sys/inc/dec-frac.p (v-lscore-dec-sum,16, OUTPUT lv-str).
     ELSE
        lv-str = "0".

     lv-lcum-score-c-fraction = lv-lcum-score-c-fraction
                              + FILL(" ",v-start-ext[i] - LENGTH(lv-lcum-score-c-fraction)) + lv-str.
  END.

  if v-triad then
    put {1} space(6) v-lcum-score-c space(2) "Totals" skip.

  IF est.metric THEN DO: 
     IF LENGTH(lv-lscore-c-fraction) <= 90 THEN DO:
       if v-triad then
          put {1} lv-lscore-c-fraction    skip.
       else
         put {1} lv-lscore-c-fraction  FORM "x(100)"  skip
              lv-lcum-score-c-fraction FORM "x(100)"  skip.
     END.
     ELSE DO:
         if v-triad then
             put {1} lv-lscore-c-fraction    skip.
          else
            put {1} "<P12>" lv-lscore-c-fraction  FORM "x(135)"  skip
                 lv-lcum-score-c-fraction FORM "x(135)"  "<P12>" skip.
     END.
  END.
  ELSE DO:
     IF LENGTH(lv-lscore-c-fraction) <= 90 THEN DO:
        IF v-triad then
           put {1} lv-lscore-c-fraction    skip.
        else
          put {1} lv-lscore-c-fraction  FORM "x(90)"  skip
               lv-lcum-score-c-fraction FORM "x(90)"  skip.
     END.
     ELSE DO:
         IF v-triad THEN put {1} lv-lscore-c-fraction    skip.
        else
          put {1} "<P12>" lv-lscore-c-fraction  FORM "x(135)"  skip
               lv-lcum-score-c-fraction FORM "x(135)" "<P12>" skip.
     END.
  END. 

  ASSIGN
      v-lines = v-lines + 4
      v-wscore-metric = 0
      v-wcum-metric = 0
      v-count = 0    .

  IF box-design-hdr.box-image = "" THEN DO:

  for each b-bdl of box-design-hdr
      no-lock
      WHERE b-bdl.line-text <> ""
      break by b-bdl.design-no:
  

    find first w-box-design-line
        where w-box-design-line.line-no eq b-bdl.line-no
        no-error.

    assign
     v-line-text = b-bdl.line-text
     v-first     = 0
     v-last      = 3000.
    /* xprint bug : if print "-" more than 3 in a row, xprint draw rectangular and line*/
    DO i = 1 TO LENGTH(v-line-text):
       IF substring(v-line-text,i,1) = "-" THEN SUBSTRING(v-line-text,i,1) = "_".       
    END.

    IF AVAIL w-box-design-line AND
         v-count LE 20 THEN
         ASSIGN
            v-count = v-count + 1
            v-wscore-metric[v-count] = IF NOT est.metric THEN ({sys/inc/k16bv.i dec(w-box-design-line.wscore-c)}) * 25.4
                                       ELSE dec(w-box-design-line.wscore-c).

      IF AVAIL w-box-design-line THEN
      DO:
         RUN convert-to-frac-proc(w-box-design-line.wscore-c, OUTPUT lv-wscore-c-fraction, NO).
         RUN convert-to-frac-proc(w-box-design-line.wcum-score-c, OUTPUT lv-wcum-score-c-fraction, NO).
      END.

   /* if v-triad then do:
      display 
              (w-box-design-line.wscore-c)           when avail w-box-design-line
              v-line-text 
              (w-box-design-line.wcum-score-c) when avail w-box-design-line
                                                @ w-box-design-line.wcum-score-c
          with frame f2 STREAM-IO NO-BOX NO-LABELS .
      down  with frame f2 .
    end.

    else do:
      display 
              TRIM(w-box-design-line.wcum-score-c) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)"
              TRIM(w-box-design-line.wscore-c)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  AT 14 FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
      down  with frame f1.
    end. */

    if v-triad then do:

        display 
                lv-wscore-c-fraction          when avail w-box-design-line AT 8
                v-line-text 
                lv-wcum-score-c-fraction when avail w-box-design-line
                                                  @ w-box-design-line.wcum-score-c
            with frame f2 STREAM-IO NO-BOX NO-LABELS .
        down  with frame f2 .
    end.
   
    else do:

      display 
              TRIM(lv-wcum-score-c-fraction) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
              TRIM(lv-wscore-c-fraction)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
      down  with frame f1.
    end.

    v-lines = v-lines + 1.
    
  
  end. /* for each b-bdl */
  PAGE {1} .
  END. /* no box image */

  ELSE DO:     

     FILE-INFO:FILE-NAME = box-design-hdr.box-image.
     /*IF est.metric THEN PUT unformatted "<C+16><#30><R+25><C+65><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
         ELSE PUT unformatted "<C+13><#30><R+25><C+65><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     */
     PUT unformatted "<C3><#30><R+25><C+65><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     /* test To put text 
     PUT UNFORMATTED "<=30><R+6><C+10>IBM    DELL   COMPAQ   Corrugated Box" SKIP .  
     PUT UNFORMATTED "<IMAGEr:\ais_gui9\source\images\appl.bmp>" SKIP.
     to put text */
     PUT UNFORMATTED "<=30>" SKIP.
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
         IF box-design-line.wscore <> "" AND v-count LE 20 THEN
            ASSIGN
               v-count = v-count + 1
               v-wscore-metric[v-count] = IF NOT est.metric THEN ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4
                                          ELSE dec(box-design-line.wscore).

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

         PUT "<C68>" TRIM(lv-wscore-c-fraction) FORMAT "X(9)" " "
                     TRIM(lv-wcum-score-c-fraction) FORMAT "X(9)" SKIP.

         /*PUT  "<C66>" IF est.metric AND box-design-line.wscore <> "" THEN string( ROUND(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0)) ELSE TRIM(box-design-line.wscore) 
              IF est.metric AND box-design-line.wcum-score <> "" THEN string( ROUND(({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4,0)) ELSE TRIM(box-design-line.wcum-score) SKIP. */
     END.

     PAGE .
  END. /* else box image */

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
        lv-space = lv-space + "  ".

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
