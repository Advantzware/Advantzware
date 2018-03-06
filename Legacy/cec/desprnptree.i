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

/*{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
{cecrep/jobtick2.i "shared"}

{cec/descalc.i new}
def var K_FRAC as dec init 6.25 no-undo.
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
DEF VAR v-boxln AS INT NO-UNDO.

DEF VAR v-lscore-c-metric-new AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-lcum-score-c-metric-new AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-wscore-metric AS DEC EXTENT 20 NO-UNDO.
DEF VAR v-wcum-metric AS DEC EXTENT 20 NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
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
      AND (v-out1-id = ? or RECID(eb) = v-out1-id)
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
       w-box-design-line.wscore-c      =  box-design-line.wscore
       w-box-design-line.wcum-score-c  =  box-design-line.wcum-score.
    end.
  end.

  else do:
    run cec/descalc.p (recid(est), recid(eb)).
    if v-lscore-c begins "No Design" then return.
  end.
  

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
  
  IF AVAIL eb AND eb.cad-no = "" THEN
  v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
          "                  " + 
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
          IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal" .
  ELSE v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
          "                  " + 
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + box-design-hdr.DESCRIPTION.

  put {1} v-hdr skip.

  if v-triad then
       put {1} space(6) v-lcum-score-c space(2) "Totals" skip.
 
     IF LENGTH(v-lscore-c) <= 90 THEN DO:
        IF v-triad then
           put {1} /*space(13) "     " */ v-lscore-c    skip.
        else
          put {1} space(3) /* "     " */  v-lscore-c  FORM "x(90)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(90)"  skip.
     END.
     ELSE DO:
         IF v-triad THEN put {1} /*space(13) "     " */ v-lscore-c    skip.
        else
          put {1} "<P9>" space(3) /* "     " */  v-lscore-c  FORM "x(135)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(135)" "<P12>" skip.
     END.
  /*END.*/
  
  v-lines = v-lines + 4.
  v-wscore-metric = 0  .
  v-wcum-metric = 0.
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
     v-line-text = b-bdl.line-text
     v-first     = 0
     v-last      = 3000.
    /* xprint bug : if print "-" more than 3 in a row, xprint draw rectangular and line*/
    DO i = 1 TO LENGTH(v-line-text):
       IF substring(v-line-text,i,1) = "-" THEN SUBSTRING(v-line-text,i,1) = "_".       
    END.
    if v-triad then do:
      display 
              (w-box-design-line.wscore-c)           when avail w-box-design-line AT 8
              v-line-text 
              (w-box-design-line.wcum-score-c) when avail w-box-design-line
                                                @ w-box-design-line.wcum-score-c
          with frame f2 STREAM-IO NO-BOX NO-LABELS .
      down  with frame f2 .
    end.

    else do:
      display 
              TRIM(w-box-design-line.wcum-score-c) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
              TRIM(w-box-design-line.wscore-c)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  /*AT 14 */ FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
      down  with frame f1.
    end.

    v-lines = v-lines + 1.
    
  
  end. /* for each b-bdl */
  PAGE.
  END. /* no box image */
  ELSE DO:     

     FILE-INFO:FILE-NAME = box-design-hdr.box-image.
     
     PUT unformatted "<C3><#30><R+40><C+85><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     PUT UNFORMATTED "<=30>" SKIP.
    
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
 
         PUT  /*space(6)  */
              "<C87>"
               box-design-line.wscore " " 
               box-design-line.wcum-score SKIP.
     END.
   .
  END. /* else box image */

  PUT "<P10>" .
  PAGE.
  end.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
