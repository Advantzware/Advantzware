/* -------------------------------------------------- cec/desprntLa.i  */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */
/* 06/10/01  YSK have box design on next page if box-line is more than 15 lines */

def input parameter v-est-id as recid no-undo.
DEF INPUT PARAMETER v-eb-rowid AS ROWID NO-UNDO.

def var li-num-of-line as int no-undo.
def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.
def var K_FRAC as dec init 6.25 no-undo.
def   var v-type-score-len     as   char  no-undo.
def   var v-type-score-count     as   char  no-undo.
DEF  var v-type-score-wid     as   char  no-undo.
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
def var v-hdr           as   char format "x(120)".
DEF VAR v-count-get AS  CHAR EXTENT 30 NO-UNDO.

def buffer b-bdl for box-design-line.
    /* for metric display */
DEF VAR lv-tmp AS cha NO-UNDO.
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
      AND (v-eb-rowid EQ ROWID(eb) OR
           v-eb-rowid EQ ?)
    no-lock:

  for each w-box-design-line:
    delete w-box-design-line.
  end.

  find first item
      {sys/look/itemgsW.i}
        and item.i-no eq eb.adhesive
      no-lock no-error.

  find box-design-hdr
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
                     ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
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
               ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
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
                     ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
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
               ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
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
       w-box-design-line.wscore-c      = IF est.metric THEN string(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wscore
       w-box-design-line.wcum-score-c  = IF est.metric THEN string(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wcum-score.
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
   
  /*============
  if v-num-lines + line-counter gt page-size then page.
       ===========  not working  YSK */
  /* ============ mods for page skip  =============*/
  if li-num-of-line > 15 and v-out1-id <> ? and v-out2-id <> ?
  then PAGE.
  
                DO X = 1 TO EXTENT(eb.k-wid-array2):
                    IF eb.k-wid-scr-type2[X] EQ "" THEN 
                        ASSIGN
                           v-type-score-wid  = v-type-score-wid  + "@".
                    ELSE
                       ASSIGN
                           v-type-score-wid  = v-type-score-wid  + trim(eb.k-wid-scr-type2[X]).
                   END.
                   
                   DO X = 1 TO EXTENT(eb.k-len-array2):
                      IF eb.k-len-scr-type2[X] EQ "" THEN
                          ASSIGN
                           v-type-score-count  = v-type-score-count +  "@"   .
                      ELSE
                       ASSIGN
                           v-type-score-count  = v-type-score-count +  trim(eb.k-len-scr-type2[X])   .
                  END.
                  
                  ASSIGN
                      lv-tmp-val = ""
                      i = 1 .
                  
                  
                  DO X = 1 TO LENGTH(v-lscore-c) :

                      lv-tmp-val = SUBSTRING(v-lscore-c,X,1).

                      IF lv-tmp-val <> "" THEN v-count-get[i] = v-count-get[i] + lv-tmp-val.
                     /* ELSE
                          v-type-score-len = v-type-score-len + " " .*/
                       
                       IF X <> 1 THEN
                       IF lv-tmp-val = "" AND SUBSTRING(v-lscore-c,X - 1,1) <> ""  THEN
                           i = i + 1 .
                  END.
                  DEF VAR v-char-count AS INT NO-UNDO.
                  ASSIGN i = 1 .
                   DO X = 1 TO LENGTH(v-lscore-c) :

                      lv-tmp-val = SUBSTRING(v-lscore-c,X,1).

                      IF lv-tmp-val <> "" THEN do:
                           v-type-score-len = v-type-score-len + SUBSTRING(v-type-score-count,i,1).
                      IF length(v-count-get[i]) = 4 THEN
                          ASSIGN
                          X  = X + 3 
                          v-type-score-len = v-type-score-len + "   ".
                       IF length(v-count-get[i]) = 5 THEN
                          ASSIGN
                          X  = X + 4 
                          v-type-score-len = v-type-score-len + "    ".
                       IF length(v-count-get[i]) = 6 THEN
                          ASSIGN
                          X  = X + 5 
                          v-type-score-len = v-type-score-len + "     ".
                      END.

                      ELSE
                          v-type-score-len = v-type-score-len + " " .
                       
                       IF X <> 1 THEN
                       IF lv-tmp-val = "" AND SUBSTRING(v-lscore-c,X - 1,1) <> ""  THEN
                           i = i + 1 .
                  END.
                  
                   
  /* ===== end of mods ==========*/
  ASSIGN
    v-hdr = 
          "                  " + 
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
          IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal"
   v-hdr = v-hdr +
           "  Style: " + eb.style /*+ ", " + IF AVAIL style THEN style.dscr ELSE ""*/ .

  ASSIGN 
      v-type-score-len = REPLACE(v-type-score-len,"@"," ") 
      .

  put {1} v-hdr skip.
  if v-triad then
       put {1} space(6) v-lcum-score-c space(2) "Totals" skip.

  IF est.metric THEN DO:
    IF LENGTH(v-lscore-c) <= 90 THEN DO:
       if v-triad THEN put {1} /*"Score"*/ "         "  v-lscore-c    skip
            space(3) v-type-score-len FORMAT "X(200)" SKIP.
       ELSE put {1} space(7) "           "  v-lscore-c  FORM "x(100)"  skip
              space(7) "           " v-lcum-score-c FORM "x(100)"  skip
           space(3) v-type-score-len FORMAT "X(200)" SKIP.
       
    END.
    ELSE DO:
       if v-triad THEN put {1} /*"Score"*/ "         "  v-lscore-c    skip
            space(18) v-type-score-len FORMAT "X(200)" SKIP.
       ELSE put {1} "<P9>" space(7) "           "  v-lscore-c  FORM "x(135)"  skip
              space(7) "           " v-lcum-score-c FORM "x(135)" "<P12>" skip
           space(3) v-type-score-len FORMAT "X(200)" SKIP.
       
    END.
  END.   /* metric */
  ELSE DO:
     IF LENGTH(v-lscore-c) <= 90 THEN DO:
        if v-triad THEN put {1} /*space(13) "     " */ v-lscore-c    skip
            space(3) v-type-score-len FORMAT "X(200)" SKIP.
        ELSE put {1} space(3) /* "     " */  v-lscore-c  FORM "x(90)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(90)"  skip
           space(3)  v-type-score-len FORMAT "X(200)" SKIP
            .
        
        
     END.
     ELSE DO:
        if v-triad THEN put {1} /*space(13) "     " */ v-lscore-c    skip
             space(3) v-type-score-len FORMAT "X(200)" SKIP.
        ELSE put {1} "<P9>" space(3) v-lscore-c  FORM "x(145)"  skip
               space(3) /*"      " */ v-lcum-score-c FORM "x(145)"  "<P12>" skip
           space(3) v-type-score-len FORMAT "X(200)" SKIP.
        
     END.
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
     PUT unformatted "<C3><#30><R+25><C+80><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
     PUT UNFORMATTED "<=30>" SKIP.
     i = 1 .
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
         /*PUT  /*space(6)  */
              "<C81>"*/
         ASSIGN 
             v-type-score-wid = REPLACE(v-type-score-wid,"@"," ") .

              IF est.metric AND box-design-line.wscore <> "" THEN DO: 
                PUT "<C81>"  string( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4)  
                      SUBSTRING(v-type-score-wid,i,1) .
                      i = i + 1 .
              END.
              ELSE do: 
                   PUT "<C81>" box-design-line.wscore " " .
                   IF  box-design-line.wscore <> "" THEN do:
                       PUT SUBSTRING(v-type-score-wid,i,1)  FORMAT "x(2)" .
                     i = i + 1 .
                   END.
              END.
               
              IF est.metric AND box-design-line.wcum-score <> "" THEN PUT string( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4). ELSE  PUT box-design-line.wcum-score. PUT SKIP.
              

     END.

    
     
  END. /* else box image */
  PUT "<P10>" .
 /* PAGE. */
  end.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
