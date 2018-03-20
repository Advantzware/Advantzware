/* --------------------------------------------- sys/ref/mksetest.p 06/00 JLF */
/* Create estimate for a set                                                  */
/* -------------------------------------------------------------------------- */

def input parameter ip-rowid as ROWID.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-itemfg for itemfg.
def buffer b-est    for est.
def buffer b-qty    for est-qty.
def buffer b-ef     for ef.
def buffer b-eb     for eb.
def buffer b-ref    for reftable.
def buffer b-prep   for est-prep.
def buffer b-op     for est-op.
def buffer b-inst   for est-inst.
def buffer b-flm    for est-flm.
def buffer b-boxh   for box-design-hdr.
def buffer b-boxl   for box-design-line.
def buffer b-notes  for notes.

def var v-est-no        like est.est-no NO-UNDO.
def var v-qty           like fg-set.part-qty NO-UNDO.
def var choice          as   LOG NO-UNDO.
DEF VAR v-rowid         AS   ROWID NO-UNDO.
DEF VAR v               AS   INT NO-UNDO.
DEF VAR lv-part-no      LIKE itemfg.part-no NO-UNDO.
DEF VAR v-first-blank AS LOG INIT YES NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.
DEF VAR char-val2 AS CHAR NO-UNDO.
DEF VAR date-val AS DATE NO-UNDO.
DEF VAR date-val2 AS DATE NO-UNDO.

{fg/fullset.i NEW}

DEF TEMP-TABLE w-part NO-UNDO
    FIELD w-part-no LIKE itemfg.part-no
    FIELD w-i-no    LIKE itemfg.i-no.

SESSION:SET-WAIT-STATE ("general").
 
find fg-set where ROWID(fg-set) eq ip-rowid no-lock.
    
find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq fg-set.set-no
    no-lock no-error.
          
if avail itemfg and itemfg.isaset then do transaction:
  CREATE w-part.
  ASSIGN
   w-part-no = itemfg.part-no
   w-i-no    = itemfg.i-no.

  RUN fg/fullset.p (ROWID(itemfg)).
    
  for each tt-fg-set,
        
      first b-itemfg
      where b-itemfg.company eq cocode
        and b-itemfg.i-no    eq tt-fg-set.part-no
        and b-itemfg.est-no  ne "" 
      no-lock,    
        
      first eb
      where eb.company  eq cocode
        AND eb.est-no   EQ 
          fill(" ",8 - length(trim(b-itemfg.est-no))) + trim(b-itemfg.est-no)
        and eb.stock-no eq b-itemfg.i-no
        and eb.form-no  ne 0
      no-lock:

    lv-part-no = b-itemfg.part-no.
    FIND FIRST w-part WHERE w-part-no EQ lv-part-no NO-ERROR.
    IF AVAIL w-part THEN DO:
      lv-part-no = eb.part-no.
      FIND FIRST w-part WHERE w-part-no EQ lv-part-no NO-ERROR.
    END.
    IF AVAIL w-part THEN DO:
      MESSAGE "Duplicate Customer Part#: " + TRIM(lv-part-no) +
              ", cannot build set estimate..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.

    CREATE w-part.
    ASSIGN
     w-part-no = lv-part-no
     w-i-no    = b-itemfg.i-no.
  END.

  assign
   choice   = no
   v-est-no = fill(" ",8 - length(trim(itemfg.est-no))) + trim(itemfg.est-no).

  find first b-est
      where b-est.company  eq cocode
        and b-est.est-no   eq v-est-no
        and b-est.est-type eq 6
      no-error.
  
  if avail b-est then do on endkey undo, leave:
    v-est-no = "".
     
    for each tt-fg-set,
        
        first b-itemfg
        where b-itemfg.company eq cocode
          and b-itemfg.i-no    eq tt-fg-set.part-no
          and b-itemfg.est-no  ne "" 
        no-lock:

      if v-est-no eq "" then v-est-no = trim(b-itemfg.est-no).
      else
      if trim(v-est-no) ne trim(b-itemfg.est-no) then do:
        v-est-no = "".
        leave.
      end.
    end.

    IF v-est-no EQ "" THEN
      MESSAGE "Estimate exists, delete estimate and rebuild?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE choice.
    ELSE
      MESSAGE "Set estimate already exists, cannot rebuild"
          VIEW-AS ALERT-BOX ERROR.
  end.

  else
  do on error undo, leave:
    find first ce-ctrl {sys/look/ce-ctrlW.i} exclusive-lock.
    assign
     choice        = yes
     ce-ctrl.e-num = ce-ctrl.e-num + 1
     i             = ce-ctrl.e-num.
  end.
  
  if choice then do:
    if avail b-est then do:
      i = int(b-est.est-no).
      DELETE b-est.
    end.
    
    find current itemfg exclusive.

    create b-est.
    assign
     b-est.est-type = 6
     b-est.company  = cocode
     b-est.loc      = locode
     b-est.est-no   = string(i,">>>>>>>>")
     b-est.est-date = today
     b-est.mod-date = ?
     itemfg.est-no  = b-est.est-no.
     
    find current itemfg no-lock.

    b-est.form-qty = 0.

    create b-qty.
    assign
     b-qty.company  = cocode
     b-qty.est-no   = b-est.est-no.

    create b-eb.
    assign
     b-eb.company    = cocode
     b-eb.loc        = locode
     b-eb.est-type   = b-est.est-type
     b-eb.est-no     = b-est.est-no
     b-eb.form-no    = 0
     b-eb.blank-no   = 0
     b-eb.est-int    = int(b-est.est-no)
     b-eb.stock-no   = itemfg.i-no
     b-eb.part-no    = itemfg.part-no
     b-eb.part-dscr1 = itemfg.i-name
     b-eb.part-dscr2 = itemfg.part-dscr1.
   
    if b-eb.part-no eq "" then b-eb.part-no = b-eb.stock-no.
    
    /*for each fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq itemfg.i-no
        no-lock:
        
      create w-part.
      assign
       w-part.part-no  = fg-set.part-no
       w-part.part-qty = fg-set.part-qty.
    end.

    do while true:
      find first w-part
          where w-part.part-qty ge 1
            and can-find(first b-itemfg
                         where b-itemfg.company eq cocode
                           and b-itemfg.i-no    eq w-part.part-no
                           and b-itemfg.isaset  eq yes)
            and can-find(first fg-set
                         where fg-set.company eq cocode
                           and fg-set.set-no  eq w-part.part-no
                           and fg-set.part-no ne w-part.part-no)
          no-error.
      if not avail w-part then leave.
      
      for each w-part
          where w-part.part-qty ge 1
            and can-find(first b-itemfg
                         where b-itemfg.company eq cocode
                           and b-itemfg.i-no    eq w-part.part-no
                           and b-itemfg.isaset  eq yes):
                           
        for each fg-set
            where fg-set.company eq cocode
              and fg-set.set-no  eq w-part.part-no
              and fg-set.part-no ne w-part.part-no
            no-lock
            break by fg-set.set-no:
          
          create w-part1.
          assign
           w-part1.part-no  = fg-set.part-no
           w-part1.part-qty = fg-set.part-qty * w-part.part-qty.
           
          if last(fg-set.set-no) then delete w-part.
        end.
      end.
      
      for each w-part1:
        create w-part.
        buffer-copy w-part1 to w-part.
        delete w-part1.
      end.
    end.
    
    for each w-part break by w-part.part-no:
      if first-of(w-part.part-no) then v-qty = 0.
    
      v-qty = v-qty + w-part.part-qty.
      
      if last-of(w-part.part-no) then w-part.part-qty = v-qty.
      
      else delete w-part.
    end.

    for each w-part,
        
        first b-itemfg
        where b-itemfg.company eq cocode
          and b-itemfg.i-no    eq w-part.part-no
          and b-itemfg.est-no  gt ""*/
    FOR EACH tt-fg-set,

        first b-itemfg
        where b-itemfg.company eq cocode
          and b-itemfg.i-no    eq tt-fg-set.part-no
          and b-itemfg.est-no  gt ""
        no-lock,
        
        first est
        where est.company eq cocode
          and est.est-no  eq 
            fill(" ",8 - length(trim(b-itemfg.est-no))) + trim(b-itemfg.est-no)
        no-lock,    
        
        first eb
        where eb.company  eq est.company
          AND eb.est-no   EQ est.est-no
          and eb.stock-no eq b-itemfg.i-no
          and eb.form-no  ne 0
        no-lock,
        
        first ef
        where ef.company eq eb.company
          AND ef.est-no  EQ eb.est-no
          and ef.form-no eq eb.form-no
        no-lock:

      FIND FIRST w-part WHERE w-i-no EQ b-itemfg.i-no NO-ERROR.

      ASSIGN
       lv-part-no     = IF AVAIL w-part           THEN w-part-no
                        ELSE
                        IF b-itemfg.part-no NE "" THEN b-itemfg.part-no
                        ELSE                           eb.part-no
       b-est.form-qty = b-est.form-qty + 1.  
  
      create b-eb.
             
      buffer-copy eb except rec_key to b-eb
      assign 
       b-eb.company      = cocode
       b-eb.loc          = locode
       b-eb.est-type     = b-est.est-type
       b-eb.est-no       = b-est.est-no
       b-eb.est-int      = int(b-est.est-no)
       b-eb.form-no      = b-est.form-qty
       b-eb.blank-no     = 1
       b-eb.part-no      = lv-part-no
       b-eb.quantityPerSet      = tt-fg-set.part-qty.
     
      IF v-first-blank THEN
      DO:
         run est/estqtyd.w (RECID(b-qty), recid(b-eb),"0",
                            output char-val,
                            output char-val2,
                            output date-val,
                            output date-val2).

         IF char-val <> "?" THEN
         DO:
            ASSIGN
            b-qty.eqty = INTEGER(entry(1,char-val))
            b-qty.qty[1] = b-qty.eqty
            b-qty.qty[2] = integer(entry(2,char-val))
            b-qty.qty[3] = integer(entry(3,char-val))
            b-qty.qty[4] = integer(entry(4,char-val))
            b-qty.qty[5] = integer(entry(5,char-val))
            b-qty.qty[6] = integer(entry(6,char-val))
            b-qty.qty[7] = integer(entry(7,char-val))
            b-qty.qty[8] = integer(entry(8,char-val))
            b-qty.qty[9] = integer(entry(9,char-val))
            b-qty.qty[10] = integer(entry(10,char-val))
            b-est.est-qty[1] = b-qty.eqty
            b-est.est-qty[2] = b-qty.qty[2]
            b-est.est-qty[3] = b-qty.qty[3]
            b-est.est-qty[4] = b-qty.qty[4].                  
         END.

         if char-val2 <> "?" THEN
            assign b-qty.qty[11] = integer(entry(1,char-val2))
                   b-qty.qty[12] = integer(entry(2,char-val2))
                   b-qty.qty[13] = integer(entry(3,char-val2))
                   b-qty.qty[14] = integer(entry(4,char-val2))
                   b-qty.qty[15] = integer(entry(5,char-val2))
                   b-qty.qty[16] = integer(entry(6,char-val2))
                   b-qty.qty[17] = integer(entry(7,char-val2))
                   b-qty.qty[18] = integer(entry(8,char-val2))
                   b-qty.qty[19] = integer(entry(9,char-val2))
                   b-qty.qty[20] = integer(entry(10,char-val2)).
      END.

      ASSIGN
         b-eb.eqty         = b-qty.eqty
         v-first-blank = NO.

      create b-ef.
            
      buffer-copy ef except rec_key to b-ef
      assign
       b-ef.company  = cocode
       b-ef.loc      = locode
       b-ef.est-type = b-est.est-type
       b-ef.est-no   = b-est.est-no
       b-ef.eqty     = b-eb.eqty
       b-ef.form-no  = b-est.form-qty.

      for each reftable {ce/est-mrpl.i ef} no-lock:
        create b-ref.
        buffer-copy reftable except rec_key to b-ref
        assign
         b-ref.code = trim(b-ef.est-no) + string(b-ef.form-no,"/99").
      end.

      for each est-prep
          where est-prep.company eq eb.company
            AND est-prep.est-no  EQ eb.est-no
            AND est-prep.s-num   EQ eb.form-no
            and (est-prep.b-num  eq 0 or
                 est-prep.b-num  eq eb.blank-no)
          no-lock:

        find last b-prep
            where b-prep.company eq b-eb.company
              and b-prep.est-no  eq b-eb.est-no
              and b-prep.eqty    eq b-eb.eqty
            no-lock no-error.
        v = if avail b-prep then b-prep.line else 0.
        
        create b-prep.
        
        buffer-copy est-prep except rec_key to b-prep
        assign
         b-prep.est-no = b-eb.est-no
         b-prep.eqty   = b-eb.eqty
         b-prep.s-num  = b-eb.form-no
         b-prep.line   = v + 1.
      
        if est-prep.b-num gt 0 then b-prep.b-num = b-eb.blank-no.
      end.
    
      for each est-op
          where est-op.company eq eb.company
            AND est-op.est-no  EQ eb.est-no
            AND est-op.s-num   EQ eb.form-no
            and (est-op.b-num  eq 0 or
                 est-op.b-num  eq eb.blank-no)
          no-lock:
        
        create b-op.
        
        buffer-copy est-op except rec_key to b-op
        assign
         b-op.est-no = b-eb.est-no
         b-op.eqty   = b-eb.eqty
         b-op.s-num  = b-eb.form-no.
      
        if est-op.b-num gt 0 then b-op.b-num = b-eb.blank-no.
      end.
    
      for each est-flm
          where est-flm.company eq eb.company
            AND est-flm.est-no  EQ eb.est-no
            AND est-flm.snum    EQ eb.form-no
            and (est-flm.bnum   eq 0 or
                 est-flm.bnum   eq eb.blank-no)
          no-lock:
               
        create b-flm.
        
        buffer-copy est-flm except rec_key to b-flm
        assign
         b-flm.est-no = b-eb.est-no
         b-flm.eqty   = b-eb.eqty
         b-flm.snum   = b-eb.form-no.
      
        if est-flm.bnum gt 0 then b-flm.bnum = b-eb.blank-no.
      end.
      
      for each est-inst
          WHERE est-inst.company EQ eb.company
            AND est-inst.est-no  eq eb.est-no
            and est-inst.line    eq eb.form-no
          no-lock:
               
        create b-inst.
        
        buffer-copy est-inst except rec_key to b-inst
        assign
         b-inst.est-no = b-eb.est-no
         b-inst.eqty   = b-eb.eqty
         b-inst.line   = b-eb.form-no.
      end.
      
      for each box-design-hdr
          where box-design-hdr.company  eq eb.company
            AND box-design-hdr.est-no   eq eb.est-no
            and box-design-hdr.form-no  eq eb.form-no
            and box-design-hdr.blank-no eq eb.blank-no
          no-lock:
        
        buffer-copy box-design-hdr except rec_key to b-boxh
        assign
         b-boxh.est-no   = b-eb.est-no
         b-boxh.eqty     = b-eb.eqty
         b-boxh.form-no  = b-eb.form-no
         b-boxh.blank-no = b-eb.blank-no.
         
        for each box-design-line of box-design-hdr no-lock:
          buffer-copy box-design-line except rec_key to b-boxl
          assign
           b-boxl.est-no   = b-eb.est-no
           b-boxl.eqty     = b-eb.eqty
           b-boxl.form-no  = b-eb.form-no
           b-boxl.blank-no = b-eb.blank-no.
        end.
      end.   

      FOR EACH notes WHERE notes.rec_key EQ est.rec_key:
        CREATE b-notes.
        BUFFER-COPY notes TO b-notes
        ASSIGN
         b-notes.rec_key = b-est.rec_key. 
      END.
    end.
    
    v-rowid = ROWID(b-est).
  end.  
end. /* do transaction */

SESSION:SET-WAIT-STATE ("").

find est where ROWID(est) eq v-rowid no-lock no-error.
if avail est then do:
  /* used to run estimate maintenance (cec/est.p) */
  MESSAGE "You have created estimate " + TRIM(est.est-no) + "..."
          VIEW-AS ALERT-BOX.
end.



