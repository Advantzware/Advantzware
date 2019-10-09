/* -------------------------------------------------- cec/desprnt.i 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */
/* 06/10/01  YSK have box design on next page if box-line is more than 15 lines */
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{sys/inc/f16to32.i}
def input parameter v-est-id as recid no-undo.
def var li-num-of-line as int no-undo.
def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.
DEF VAR v-corrug AS cha FORM "x(12)" NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR lv-str AS cha NO-UNDO.
/*{sys/inc/var.i shared}
  {sys/form/s-top.f}
*/

{cecrep/jobpac1.i "shared"}

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
DEF VAR lv-tmp-cum AS cha NO-UNDO.
DEF VAR lv-tmp-val AS cha NO-UNDO.
DEF VAR lv-met-lsc AS cha NO-UNDO.
DEF VAR lv-met-lcum AS cha NO-UNDO.
DEF VAR ld-tmp-scr AS DEC NO-UNDO.
DEF VAR li-num-space AS INT NO-UNDO.

form w-box-design-line.wcum-score-c             at 1
     w-box-design-line.wscore-c
     v-line-text           format "x(65)"  AT 23              SKIP
    with down frame f1 no-labels NO-BOX WIDTH 150.

form w-box-design-line.wscore-c                 at 1
     v-line-text                     format "x(65)"
     w-box-design-line.wcum-score-c                                         SKIP
    with down frame f2 no-labels NO-BOX WIDTH 120.


{sys/inc/jobcard.i C}
v-triad = sys-ctrl.char-fld eq "Triad".

find est where recid(est) eq v-est-id no-lock.

for each ef where ef.company = est.company 
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
  find first eb where eb.company = ef.company
                  AND eb.est-no   eq ef.est-no
                  and eb.form-no eq ef.form-no no-lock no-error.

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
    
    IF est.metric THEN DO:
           ASSIGN lv-tmp = ""
                  lv-tmp-val = ""
                  lv-met-lsc = ""
                  lv-met-lcum = "".

           DO i = 1 TO LENGTH(box-design-hdr.lscore):
              lv-tmp-val = SUBSTRING(box-design-hdr.lscore,i,1).
              IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
              ELSE do:
                   IF lv-tmp <> "" THEN DO:
                     ld-tmp-scr = ROUND(({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4,0).
                     lv-tmp = "".
                     li-num-space = 0.
                   END.
                   li-num-space = li-num-space + 1.
                   lv-met-lsc = lv-met-lsc + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                                IF ld-tmp-scr = 0 THEN "" ELSE trim(string(ld-tmp-scr)).
                   ld-tmp-scr = 0.
              END.                                    
           END. 
           IF lv-tmp <> "" THEN do:
               ld-tmp-scr = ROUND(({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4,0).
               lv-met-lsc = lv-met-lsc + string(ld-tmp-scr).
           END.
           ASSIGN lv-tmp = ""
               lv-tmp-val = ""
               lv-met-lcum = ""
               ld-tmp-scr = 0
               li-num-space = 0.

           DO i = 1 TO LENGTH(box-design-hdr.lcum-score):
              lv-tmp-val = SUBSTRING(box-design-hdr.lcum-score,i,1).
              IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
              ELSE do:
                   IF lv-tmp <> "" THEN DO:
                     ld-tmp-scr = ROUND(({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4,0).
                     lv-tmp = "".
                     li-num-space = 0.
                   END.
                   li-num-space = li-num-space + 1.
                   lv-met-lcum = lv-met-lcum + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                                  IF ld-tmp-scr = 0 THEN "" ELSE trim(string(ld-tmp-scr)).
                   ld-tmp-scr = 0.
              END.                   
           END.
           IF lv-tmp <> "" THEN do:
               ld-tmp-scr = ROUND(({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4,0).
               lv-met-lcum = lv-met-lcum + string(ld-tmp-scr).
           END.

           ASSIGN v-lscore-c     = lv-met-lsc
                  v-lcum-score-c = lv-met-lcum.
    END. /* for metric display */

    i = 0.
    for each box-design-line of box-design-hdr
        where box-design-line.wcum-score ne ""
        no-lock:

      i = i + 1.
      create w-box-design-line.
      assign
       w-box-design-line.line-no       = box-design-line.line-no
       w-box-design-line.wscore-c      = IF est.metric THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0)) ELSE box-design-line.wscore
       w-box-design-line.wcum-score-c  = IF est.metric THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0)) ELSE box-design-line.wcum-score.
    end.
  end. /* avail box-design-hdr */

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
  end. /* for each b-bdl*/
   
  /*============
  if v-num-lines + line-counter gt page-size then page.
       ===========  not working  YSK */
  v-corrug = IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal".
  PUT "<#20><C1><FROM><C105><LINE><|3>" 
      "<=20><C90><FROM><R+31><LINE><|3>"
      "<=20><R+1><P10><B> SCORING INFORMATION"
      "<=20><R+1><C50>Corrugations: " v-corrug 
      SKIP.

  
   
  /* ===== end of mods ==========*/
    v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
          "                  " + 
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + box-design-hdr.DESCRIPTION.
        

  put {1} v-hdr skip.
/*
  if v-triad then
       put {1} space(6) v-lcum-score-c space(2) "Totals" skip.
*/
    
  
  PUT "</B><P8><=20><R+10><C95>Length Scores" SKIP
      "<=20><R+11><C93>Length      Cume" SKIP
      .
  ASSIGN lv-tmp = ""
         lv-tmp-val = ""
         lv-met-lsc = ""
         lv-met-lcum = "".

  li-cnt = 1.
  DO i = 1 TO LENGTH(v-lscore-c):
     lv-tmp-val = SUBSTRING(v-lscore-c,i,1).
     IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
     ELSE do:
          IF lv-tmp <> "" THEN DO:
             IF est.metric THEN DO:
                   /*lv-tmp = STRING( ROUND( 
                                    (({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4) ,4)
                           ). */
                    lv-tmp = lv-tmp.
             END.
             ELSE DO:
                lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).
                RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
                IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
                ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                         SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).
             END.
                     PUT UNFORMATTED "<=20><C93><R+" 12 + li-cnt ">" lv-tmp SKIP.
                     li-cnt = li-cnt + 1.
                     ASSIGN lv-tmp = "".                     
          END.                   
     END.                                    
  END. 
  IF lv-tmp <> "" THEN do:
     IF est.metric THEN DO:
                  /*lv-tmp = STRING( ROUND( 
                                   (({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4) ,4)
                          ). */
                   lv-tmp = lv-tmp.
     END.
     ELSE DO:
               lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).
               RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
               IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
               ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                        SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).
     END.
     PUT UNFORMATTED "<=20><C93><R+" 12 + li-cnt ">" lv-tmp SKIP.
  END.


  ASSIGN lv-tmp = ""
         lv-tmp-val = ""
         li-cnt = 1
         .         
  

  DO i = 1 TO LENGTH(v-lcum-score-c):
     lv-tmp-val = SUBSTRING(v-lcum-score-c,i,1).
     IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
     ELSE do:
           IF lv-tmp <> "" THEN DO:
              IF est.metric THEN DO:
                   /*lv-tmp = string( ROUND(
                       ( ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4),4) ) . */
                    lv-tmp = lv-tmp.
              END.
              ELSE DO:
                   lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).
                   RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
                   IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
                   ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                            SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).
              END.
              PUT UNFORMATTED "<=20><C100><R+" 12 + li-cnt ">"  lv-tmp   SKIP.
              li-cnt = li-cnt + 1.
              lv-tmp = "".                     
           END.           
     END.                   
  END.
  IF lv-tmp <> "" THEN do:
     IF est.metric THEN DO:
                  /*lv-tmp = STRING( ROUND( 
                                   (({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4) ,4)
                          ). */
                   lv-tmp = lv-tmp.
     END.
     ELSE DO:
               lv-tmp = STRING({sys/inc/k16bv.i DEC(lv-tmp)}).
               RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
               IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
               ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                        SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).
     END.
     PUT UNFORMATTED "<=20><C100><R+" 12 + li-cnt ">" lv-tmp SKIP.
  END.

  v-lines = v-lines + 4.
  PUT "<=20><R+2>" SKIP.

IF box-design-hdr.box-image = "" THEN DO:
   PUT "<=20><R+1><C95>Width Scores" SKIP
        "<=20><R+2><C93>Width      Cume" SKIP.
   li-cnt = 2.
      
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
    /*
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
    */    
      display 
              TRIM(w-box-design-line.wcum-score-c) when avail w-box-design-line
                   @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
              TRIM(w-box-design-line.wscore-c)     when avail w-box-design-line               
                   @ w-box-design-line.wscore-c   FORM "x(6)"
              v-line-text  /*AT 14 */ FORM "x(65)"
              with frame f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
         down  with frame f1.      
   /* end. */
    
      PUT "<#21>".
      IF AVAIL w-box-design-line AND w-box-design-line.wscore-c <> "" THEN DO:
             li-cnt = li-cnt + 1.
             lv-tmp = STRING({sys/inc/k16bv.i DEC(w-box-design-line.wscore-c)}).


             RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
             IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
             ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                      SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).

             lv-tmp-cum = STRING({sys/inc/k16bv.i DEC(w-box-design-line.wcum-score-c)}).
             RUN sys/inc/dec-frac.p (DEC(lv-tmp-cum),16, OUTPUT lv-str).
             IF INDEX(lv-str,"-") <= 0 THEN lv-tmp-cum = lv-str.
             ELSE lv-tmp-cum = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                      SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).

             PUT  UNFORMATTED "<=20><C93><R+" li-cnt ">" 
                  IF est.metric AND w-box-design-line.wscore-c <> "" 
                     THEN string( ROUND( ({sys/inc/k16bv.i dec(w-box-design-line.wscore-c)}) * 25.4,0))
                     ELSE /*box-design-line.wscore */ lv-tmp 
                  "<=20><C100><R+" li-cnt ">"
                  IF est.metric AND w-box-design-line.wcum-score-c <> "" 
                      THEN string( ROUND( ({sys/inc/k16bv.i dec(w-box-design-line.wcum-score-c)}) * 25.4,0)) 
                      ELSE /*box-design-line.wcum-score*/ lv-tmp-cum SKIP.
      END.
      PUT "<=21><R-1>".
      
    v-lines = v-lines + 1.    
    
  end. /* for each b-bdl */
  PUT SKIP(1).
  IF est.metric THEN DO:
   if v-triad then
      put {1} /*"Score"*/ "         "  v-lscore-c    skip.
   else
     put {1} space(7) "           "  v-lscore-c  FORM "x(100)"  skip
             space(7) "           " v-lcum-score-c FORM "x(100)"  skip.
 END.
 ELSE DO:
   if v-triad then
      put {1} /*"Score"*/ "     "  v-lscore-c    skip.
   else
      put {1} space(7) "     "  v-lscore-c  FORM "x(90)"  skip
              space(6) "      " v-lcum-score-c FORM "x(90)"  skip.
 END.

END. /* no box image */
ELSE DO:     

     FILE-INFO:FILE-NAME = box-design-hdr.box-image.
     
     IF est.metric THEN PUT unformatted skip(3) "<C+14><#30><R+35><C+75><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     ELSE PUT unformatted skip(3)
              "<C+10><#30><R+35><C+70><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     /* test To put text 
     PUT UNFORMATTED "<=30><R+6><C+10>IBM    DELL   COMPAQ   Corrugated Box" SKIP .  
     PUT UNFORMATTED "<IMAGEr:\ais_gui9\source\images\appl.bmp>" SKIP.
     to put text */
     PUT UNFORMATTED "<=30>" SKIP.
    
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
         PUT  
              IF est.metric AND box-design-line.wscore <> "" 
                 THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0))
                 ELSE box-design-line.wscore " " 
              IF est.metric AND box-design-line.wcum-score <> "" 
                     THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4,0)) ELSE box-design-line.wcum-score SKIP.
     END.
     PUT "<=20><R+1><C95>Width Scores" SKIP
         "<=20><R+2><C93>Width      Cume" SKIP.
     li-cnt = 2.
     FOR EACH box-design-line OF box-design-hdr NO-LOCK WHERE box-design-line.wscore <> "":
         li-cnt = li-cnt + 1.
         lv-tmp = STRING({sys/inc/k16bv.i DEC(box-design-line.wscore)}).
         RUN sys/inc/dec-frac.p (DEC(lv-tmp),16, OUTPUT lv-str).
         IF INDEX(lv-str,"-") <= 0 THEN lv-tmp = lv-str.
         ELSE lv-tmp = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                  SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).

         lv-tmp-cum = STRING({sys/inc/k16bv.i DEC(box-design-line.wcum-score)}).
         RUN sys/inc/dec-frac.p (DEC(lv-tmp-cum),16, OUTPUT lv-str).
         IF INDEX(lv-str,"-") <= 0 THEN lv-tmp-cum = lv-str.
         ELSE lv-tmp-cum = SUBSTRING(lv-str,1,INDEX(lv-str,"-") - 1) + " " +
                  SUBSTRING(lv-str,INDEX(lv-str,"-") + 1).
         
         
         PUT  UNFORMATTED "<=20><C93><R+" li-cnt ">" 
              IF est.metric AND box-design-line.wscore <> "" 
                 THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4,0))
                 ELSE /*box-design-line.wscore */ lv-tmp 
              "<=20><C100><R+" li-cnt ">" 
              IF est.metric AND box-design-line.wcum-score <> "" 
                  THEN string( ROUND( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4,0)) 
                  ELSE /*box-design-line.wcum-score*/ lv-tmp-cum SKIP.
     END.

  
  PUT "<=30><R+18><P10>" SKIP.
  IF est.metric THEN DO:
    if v-triad then
       put {1} /*"Score"*/ "         "  v-lscore-c    skip.
    else
      put {1} space(7) "           "  v-lscore-c  FORM "x(100)"  skip
              space(7) "           " v-lcum-score-c FORM "x(100)"  skip.
  END.
  ELSE DO:
    if v-triad then
       put {1} /*"Score"*/ "     "  v-lscore-c    skip.
    else
       put {1} space(7) "     "  v-lscore-c  FORM "x(90)"  skip
               space(6) "      " v-lcum-score-c FORM "x(90)"  skip.
  END.

  END. /* else box image */

  PUT "<P10>" .
  /*PAGE. */
  end.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
