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
DEF VAR v-fgitem-name AS cha FORM "x(70)" NO-UNDO.
DEF VAR v-boxln AS INT NO-UNDO.

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
  v-fgitem = "Board:" + string(ef.board) + "," + string(ITEM.i-name) + "   FG Item: " + string(eb.stock-no,"x(15)") .
  v-fgitem-name =   IF AVAIL itemfg THEN  itemfg.i-name ELSE "".


  put {1} "        " v-fgitem  SKIP
       space(length(v-fgitem) - 7)   v-fgitem-name skip  
          v-hdr skip.
  if v-triad then
    put {1} space(6) v-lcum-score-c space(2) "Totals" skip.
/* 
  IF est.metric THEN DO:
    if v-triad then
       put {1} /*"Score"*/ "   "  v-lscore-c    skip.
    else
      put {1} space(7) "   "  v-lscore-c  FORM "x(100)"  skip
              space(7) "   " v-lcum-score-c FORM "x(100)"  skip.
  END.
  ELSE DO:
    if v-triad then
       put {1} /*"Score"*/ "     "  v-lscore-c    skip.
    else
      put {1} space(7) "     "  v-lscore-c  FORM "x(70)"  skip
            space(6) "      " v-lcum-score-c FORM "x(70)"  skip.
  END.
*/
  

  IF est.metric THEN DO:
      if v-triad then
         put {1} v-lscore-c    skip.
      else
        put {1} v-lscore-c  FORM "x(100)"  skip
                v-lcum-score-c FORM "x(100)"  skip.
  END.
  ELSE DO:
      if v-triad then
         put {1} v-lscore-c    skip.
      else
        put {1} v-lscore-c  FORM "x(80)"  skip
                v-lcum-score-c FORM "x(80)"  skip.
  END.

  v-lines = v-lines + 4.

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
    if v-triad then do:
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
     PUT unformatted "<C1><#30><R+25><C+65><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     /* test To put text 
     PUT UNFORMATTED "<=30><R+6><C+10>IBM    DELL   COMPAQ   Corrugated Box" SKIP .  
     PUT UNFORMATTED "<IMAGEr:\ais_gui9\source\images\appl.bmp>" SKIP.
     to put text */
     PUT UNFORMATTED "<=30>" SKIP.
     ASSIGN i = 0 .
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
         i = i + 1 .
         PUT  "<C66>" IF est.metric AND box-design-line.wscore <> "" THEN string( ROUND(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0)) ELSE TRIM(box-design-line.wscore) 
              IF est.metric AND box-design-line.wcum-score <> "" THEN string( ROUND(({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4,0)) ELSE TRIM(box-design-line.wcum-score) SKIP. 
     END.
     IF i < 12 THEN PUT SKIP (8) .
     ELSE IF i < 18 THEN PUT SKIP(4) .
     PAGE .
  END. /* else box image */

  end.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
