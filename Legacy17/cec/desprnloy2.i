/* -------------------------------------------------- cec/desprncap.i  */
/* Box Design Print                                                    */
/* ------------------------------------------------------------------- */

def input parameter v-est-id as recid no-undo.
DEF OUTPUT PARAMETER op-score AS CHAR NO-UNDO.

def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.

{cecrep/jobtick2.i "shared"}

{cec/descalc.i new}
def var K_FRAC as dec init 6.25 no-undo.

def var v-int           as   int.
def var v-char          as   char.
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
DEF VAR lv-str AS CHAR NO-UNDO.
DEF VAR v-wscore-c AS CHAR NO-UNDO.

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
       
       ASSIGN lv-tmp = ""
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
  END.

  else do:

    run cec/descalc.p (recid(est), recid(eb)).

    if v-lscore-c begins "No Design" then return.
  end.
  
  IF box-design-hdr.box-image = "" THEN DO:
  
    for each b-bdl of box-design-hdr
        no-lock
        WHERE b-bdl.line-text <> ""
        break by b-bdl.design-no:
   
      find first w-box-design-line
          where w-box-design-line.line-no eq b-bdl.line-no
          no-error.
   
      IF AVAIL w-box-design-line THEN
         v-wscore-c = v-wscore-c + " " + w-box-design-line.wscore-c.
    
    end. /* for each b-bdl */
    
  END. /* no box image */
  ELSE     
     FOR EACH box-design-line OF box-design-hdr NO-LOCK:
  
         IF est.metric AND box-design-line.wscore <> "" THEN
            v-wscore-c = v-wscore-c + " " + string( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4).
         ELSE
            v-wscore-c = v-wscore-c + " " + box-design-line.wscore.
     END.
  
  
  IF ef.xgrain EQ "S" OR ef.xgrain = "B" THEN
     op-score = v-lscore-c.
  ELSE
     op-score = v-wscore-c.
END.


/* end ---------------------------------- copr. 1997  advanced software, inc. */
