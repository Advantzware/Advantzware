/* -------------------------------------------------- cec/desprnt.i 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */

def input parameter v-est-id as recid no-undo.
def var li-num-of-line as int no-undo.
def {2} shared var v-out1-id       as   recid          no-undo.
def {2} shared var v-out2-id       as   recid          no-undo.
def var v-start-compress as cha no-undo.
def var v-end-compress as cha no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

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

form w-box-design-line.wcum-score-c             at 1
     w-box-design-line.wscore-c
     v-line-text                                format "x(65)"              skip

    with down frame f1 no-labels no-box.

form w-box-design-line.wscore-c                 at 1
     v-line-text                                format "x(65)"
     w-box-design-line.wcum-score-c                                         skip

    with down frame f2 no-labels no-box.


{sys/inc/jobcard.i C}
v-triad = sys-ctrl.char-fld eq "Triad".

find est where recid(est) eq v-est-id no-lock.

for each ef
    where ef.e-num    eq est.e-num
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
      where eb.e-num   eq ef.e-num
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
        and box-design-hdr.e-num     eq eb.e-num
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
       w-box-design-line.wscore-c      = box-design-line.wscore
       w-box-design-line.wcum-score-c  = box-design-line.wcum-score.
    end.
  end.

  else do:
    run cec/descalc.p (recid(est), recid(eb)).
    if v-lscore-c begins "No Design" then return.
  end.

  if v-num-lines + line-counter gt page-size then page.

  v-hdr = (if v-triad then "           " else "Totals  Score     ") +
          "Design #: " +
          trim(string(if avail style and box-design-hdr.design-no eq 0 then
                        style.design-no else box-design-hdr.design-no,">>>")) +
          "   " + "Description: " + box-design-hdr.description.

  put {1} v-hdr skip.

  if v-triad then
    put {1} space(6) v-lcum-score-c space(2) "Totals" skip.

  assign
   v-frst-col = 0
   v-last-col = 0
   . 
  for each b-bdl of box-design-hdr
      no-lock
      break by b-bdl.design-no:

    v-line-text = b-bdl.line-text.
    
    if v-line-text ne "" then
    do v-int = 1 to length(v-line-text):
      if substr(v-line-text,v-int,1) ne "" then do:
        v-last-col[v-int] = b-bdl.line-no.
        if v-frst-col[v-int] eq 0 then v-frst-col[v-int] = b-bdl.line-no.
      end.
    end.
  end.
   

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

    if v-line-text ne "" then
    do v-int = 1 to length(v-line-text):
      if substr(v-line-text,v-int,1) ne "" then do:
        v-last = v-int.
        if v-first eq 0 then v-first = v-int.
      end.
    end.

    if v-first ne 0 then
    do v-int = v-first to v-last:
      v-char = substr(v-line-text,v-int,1).

      if index("-|",v-char) ne 0 then
        substr(v-line-text,v-int,1) = if b-bdl.line-no eq v-frst-col[v-int]
                                                                then chr(220)
                                      else
                                      if b-bdl.line-no eq v-last-col[v-int]
                                                                then chr(223)
                                      else
                                      if v-int eq v-first       or
                                         v-int eq v-last        then chr(219)
                                      else
                                      if v-char eq "-"          then chr(196)
                                                                else chr(179).
      else
      if v-char eq "#" then do:
        substr(v-line-text,v-int,1) = " ".

        if b-bdl.line-no eq v-frst-col[v-int] or
           b-bdl.line-no eq v-last-col[v-int] then do:

          if b-bdl.line-no eq v-frst-col[v-int] then
            assign
             substr(v-line-text,v-int - 1,1) =
                         if substr(v-line-text,v-int - 1,1) ne "" then chr(220)
                                                                  else chr(222)
             substr(v-line-text,v-int + 1,1) =
                         if substr(v-line-text,v-int + 1,1) ne "" then chr(220)
                                                                  else chr(221).

          else
            assign
             substr(v-line-text,v-int - 1,1) =
                         if substr(v-line-text,v-int - 1,1) ne "" then chr(223)
                                                                  else chr(222)
             substr(v-line-text,v-int + 1,1) =
                         if substr(v-line-text,v-int + 1,1) ne "" then chr(223)
                                                                  else chr(221).
        end.

        else
          assign
           substr(v-line-text,v-int - 1,1) = chr(222)
           substr(v-line-text,v-int + 1,1) = chr(221).
      end.
    end.

    if v-triad then do:
      display {1}
              w-box-design-line.wscore-c           when avail w-box-design-line
              v-line-text
              trim(w-box-design-line.wcum-score-c) when avail w-box-design-line
                                                @ w-box-design-line.wcum-score-c
          with frame f2.
      down {1} with frame f2.
    end.

    else do:
      display {1}
              w-box-design-line.wcum-score-c when avail w-box-design-line
              w-box-design-line.wscore-c     when avail w-box-design-line
              v-line-text
          with frame f1.
      down {1} with frame f1.
    end.

    v-lines = v-lines + 1.
  end.

  if v-triad then
    put {1} space(1) "Score"  v-lscore-c     skip.
  else
    put {1} space(8) "Score"  v-lscore-c     skip
            space(7) "Totals" v-lcum-score-c skip.

  v-lines = v-lines + 4.
end.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
