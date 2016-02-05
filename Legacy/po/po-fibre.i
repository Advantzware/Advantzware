/* -------------------------------------------------- po/po-fibre.i 11/00 JLF */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Fibre                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

page {1}. 

assign
 v-bottom              = int("{1}" eq "") + 1
 v-tax[v-bottom]       = po-ord.tax 
 v-po-tot[v-bottom]    = po-ord.t-cost
 v-tot-sqft[v-bottom]  = 0
 v-t-freight[v-bottom] = po-ord.t-freight.

hide {1} frame po-tots.
view {1} frame po-cont.

for each po-ordl WHERE
    po-ordl.company EQ po-ord.company AND
    po-ordl.po-no EQ po-ord.po-no AND
    (v-printde-po or
          (not po-ordl.deleted))
    by po-ordl.line with frame po-line:

  find first item
      where item.company eq cocode
        and item.i-no    eq po-ordl.i-no
        and po-ordl.item-type
      no-lock no-error.
       
  assign
   v-print-lines = 5 + int(avail item)
   v-job         = fill(" ",6 - length(trim(po-ordl.job-no))) +
                   trim(po-ordl.job-no)
   xg-flag       = no.
   
  if v-job ne "" then
  find last oe-ordl
       where oe-ordl.company eq cocode
         and oe-ordl.job-no  eq v-job
         and oe-ordl.job-no2 eq po-ordl.job-no2
       use-index job no-lock no-error.

  /*
  ASSIGN v-inst = ""
           v-tmp-lines = 0
           j = 0
           K = 0
           lv-got-return = 0.

  FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
       DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= 70 THEN ASSIGN j = i
                                      lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / 70.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return.

           IF k < 5 THEN v-inst[k] = v-inst[k] +
                                     IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                                     ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
       END.
  END.*/

  FOR EACH tt-formtext:
    DELETE tt-formtext.
  END.

  lv-text = "".
  FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
  END.

  DO i = 1 TO EXTENT(v-inst):
    CREATE tt-formtext.
    ASSIGN
     tt-line-no = i
     tt-length  = 80.
  END.

  RUN custom/formtext.p (lv-text).

  ASSIGN
   i      = 0
   v-inst = "".

  FOR EACH tt-formtext:
    i = i + 1.
    IF i LE EXTENT(v-inst) THEN v-inst[i] = tt-formtext.tt-text.      
  END.
    
  v-inst-lines = 0.
  do i = 1 to EXTENT(v-inst):
    if v-inst[i] ne "" then v-inst-lines = v-inst-lines + 1.
  end.
  if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.
  v-print-lines = v-print-lines + v-inst-lines + 1.
  if line-counter {2} - 1 + v-print-lines gt page-size {2} + 1 then page {1}.
  
  v-sqft = 0.

  IF po-ordl.pr-qty-uom EQ "EA"                    OR
     (NOT po-ordl.item-type AND
      LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) GT 0) THEN
    v-sqft = po-ordl.ord-qty.

  ELSE
    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                           (IF AVAIL item THEN item.basis-w ELSE 0),
                           po-ordl.s-len,
                           po-ordl.s-wid,
                           (IF AVAIL item THEN item.s-dep ELSE 0),
                           po-ordl.ord-qty,
                           OUTPUT v-sqft).
  
  v-sqft = (IF v-corr THEN
              (po-ordl.s-len * po-ordl.s-wid * .007)
            ELSE
              (po-ordl.s-len * po-ordl.s-wid / 144)) * v-sqft / 1000.

  IF po-ordl.pr-qty-uom EQ "ROLL" THEN v-sqft = v-sqft * (12 / po-ordl.s-len).

  v-tot-sqft[v-bottom] = v-tot-sqft[v-bottom] + v-sqft.
     
  assign
   v-wid    = po-ordl.s-wid - trunc(po-ordl.s-wid,0)
   v-wid    = ( v-wid * 16 ) / 100
   v-wid    = trunc(po-ordl.s-wid,0) + v-wid
   v-len    = po-ordl.s-len - trunc(po-ordl.s-len,0)
   v-len    = ( v-len * 16 ) / 100
   v-len    = trunc(po-ordl.s-len,0) + v-len
   v-tax[1] = if po-ordl.tax then po-ord.tax * po-ordl.t-cost / v-tax[3] else 0
   v-adder  = ""
   xg-flag  = no.

  release job-mat. 

  if avail item and item.mat-type eq "B" then do:
    find first job
        where job.company eq cocode
          and job.job-no  eq fill(" ",6 - length(trim(po-ordl.job-no))) +
                                  trim(po-ordl.job-no)
          and job.job-no2 eq po-ordl.job-no2
        no-lock no-error.
        
    if avail job then do:
      find first est
          where est.company eq job.company
            and est.est-no  eq job.est-no
          no-lock no-error.
      
      for each job-mat
          where job-mat.company  eq cocode
            and job-mat.job      eq job.job
            and job-mat.job-no   eq job.job-no
            and job-mat.job-no2  eq job.job-no2
            and job-mat.i-no     eq po-ordl.i-no
            and job-mat.frm      eq po-ordl.s-num
          use-index job no-lock
          break by job-mat.blank-no desc:
        if last(job-mat.blank-no)            or
           job-mat.blank-no eq po-ordl.b-num then leave.
      end.

      if avail job-mat then do:
        find first ef
            where ef.e-num   eq job.e-num
              and ef.form-no eq job-mat.frm
            no-lock no-error.
   
        assign
         xg-flag   = avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B")
         v-num-add = 0.
         
        for each xjob-mat
            where xjob-mat.company  eq cocode
              and xjob-mat.job      eq job-mat.job
              and xjob-mat.job-no   eq job-mat.job-no
              and xjob-mat.job-no2  eq job-mat.job-no2
              and xjob-mat.frm      eq job-mat.frm
              and xjob-mat.blank-no eq job-mat.blank-no
              and xjob-mat.i-no     ne job-mat.i-no
            no-lock,
              
            first xitem
            where xitem.company  eq cocode 
              and xitem.i-no     eq xjob-mat.i-no
              and xitem.mat-type eq "A"
            no-lock:
              
          assign
           v-num-add          = v-num-add + 1
           v-adder[v-num-add] = xitem.i-no.
             
          if v-num-add ge 3 then leave.
        end.
      end.
    end.    
  end.
  
  assign
   v-ord-qty = if po-ordl.ord-qty - trunc(po-ordl.ord-qty,0) gt 0
               then string(po-ordl.ord-qty,">>>9.9<<<")
               else string(po-ordl.ord-qty,">>>>>9")
   v-ord-qty = fill(" ",6 - length(trim(v-ord-qty))) + trim(v-ord-qty).
  
  if v-job ne "" then v-job = trim(v-job) + "-" + string(po-ordl.job-no2,"99").
    
  {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}
    
  display {1} v-ord-qty
              po-ordl.i-name
              v-adder[1]
              v-job
              po-ordl.cost
              po-ordl.pr-uom
              v-sqft
              po-ordl.dscr[1]
              v-adder[2]
              v-mach[1]
              v-mach[2]
              po-ordl.due-date
              po-ordl.t-cost
              po-ordl.dscr[2]
              v-adder[3]
              "" @ v-tax[1]
              v-tax[1] when po-ordl.tax.
              
  if po-ordl.actnum ne "" then put {1} po-ordl.actnum at 8 skip.
              
  IF AVAIL ITEM AND index("1,2,3,4,A,G,J,L,R,T,V,W,Z,9,M,I,O,X,Y,7,8",ITEM.mat-type) > 0 THEN .
  ELSE if avail item then
    put {1} "W: " at 8 v-wid space(2) "L: " v-len
            space(2) "Flute: " item.flute space(2) item.reg-no format "x(6)".
            
  run po/po-ordls.p (recid(po-ordl)).
            
  {po/poprints.i}
            
      if not v-test-scr then
        put {1}
            skip
            space(7)
            "Score: "
            len-score format "x(66)".
          
      else
      if dec(trim(len-score)) ne v-wid then
        put {1}
            skip
            space(7)
            "Score: "
            len-score format "x(66)".
    end.
    END.
  end.
            
  if v-inst-lines gt 0 then do:
    put {1} skip(1).
    do i = 1 to EXTENT(v-inst):
      if v-inst[i] ne "" then
        put {1} v-inst[i] skip.
    end.
  end.
  
  put {1} skip fill("-",80) format "x(80)" skip.
end. /* for each po-ordl record */

/*
ASSIGN v-inst = ""
         v-tmp-lines = 0
         j = 0
         K = 0
         lv-got-return = 0.
  
FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
    DO i = 1 TO LENGTH(notes.note_text) :        
              IF i - j >= 70 THEN ASSIGN j = i
                                         lv-got-return = lv-got-return + 1.
                    
              v-tmp-lines = ( i - j ) / 70.
              {SYS/INC/ROUNDUP.I v-tmp-lines}
              k = v-tmp-lines + lv-got-return.

              IF k < 5 THEN v-inst[k] = v-inst[k] + 
                   IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(notes.note_text,i,1)
                                  ELSE "".
                        

              IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
              THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
              END.

    END.
end.*/

FOR EACH tt-formtext:
  DELETE tt-formtext.
END.

lv-text = "".
FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
  lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
END.

DO i = 1 TO EXTENT(v-inst):
  CREATE tt-formtext.
  ASSIGN
   tt-line-no = i
   tt-length  = 80.
END.

RUN custom/formtext.p (lv-text).

ASSIGN
 i      = 0
 v-inst = "".

FOR EACH tt-formtext:
  i = i + 1.
  IF i LE EXTENT(v-inst) THEN v-inst[i] = tt-formtext.tt-text.      
END.

DO i = 1 TO EXTENT(v-inst):
   if v-inst[i] ne "" then v-inst-lines = v-inst-lines + 1.
end.
 
if v-inst-lines gt 0 then do:
  v-inst-lines = v-inst-lines + 1.

  if line-counter {2} - 1 + v-inst-lines gt page-size {2} + 1 then page {1}.

  put {1} skip(1).
  do i = 1 to EXTENT(v-inst):
    if v-inst[i] ne "" THEN DO:
       put {1} v-inst[i] skip.
    END.
  end.
end.

hide {1} frame po-cont.
view {1} frame po-tots.

/* end ----------------------------------- Copr. 2000  Advanced Software Inc. */
