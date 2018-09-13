/* -------------------------------------------------- po/po-loveten.i */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Fibre                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */
/* Task : 01030706 Display buyer signature  Real Drouin (RD1) 01/04/07 */

IF FIRST-OF(po-ord.po-no) THEN RUN pr-header (FIRST(po-ord.po-no)).
     
ASSIGN
 v-bottom              = INT("{1}" EQ "") + 1
 v-tax[v-bottom]       = po-ord.tax 
 v-po-tot[v-bottom]    = po-ord.t-cost
 v-tot-sqft[v-bottom]  = 0
 v-t-freight[v-bottom] = po-ord.t-freight
 v-tax[3] = 0.

FOR EACH po-ordl FIELDS(t-cost) NO-LOCK
    WHERE po-ordl.company EQ po-ord.company
      AND po-ordl.po-no   EQ po-ord.po-no
      AND po-ordl.tax     EQ YES
      AND (v-printde-po OR NOT po-ordl.deleted):
      
  v-tax[3] = v-tax[3] + po-ordl.t-cost.  
END.    

FOR EACH po-ordl
    WHERE po-ordl.company EQ po-ord.company
      AND po-ordl.po-no   EQ po-ord.po-no
      AND (v-printde-po OR NOT po-ordl.deleted)
    BY po-ordl.line:

  find first item
      where item.company eq cocode
        and item.i-no    eq po-ordl.i-no
        and po-ordl.item-type
      no-lock no-error.
       
  assign
   v-print-lines = 6
   v-job         = fill(" ",6 - length(trim(po-ordl.job-no))) +
                   trim(po-ordl.job-no) 
   xg-flag       = no.

  if v-job ne "" then
  find last oe-ordl
       where oe-ordl.company eq po-ord.company
         and oe-ordl.job-no  eq v-job
         and oe-ordl.job-no2 eq po-ordl.job-no2
       use-index job no-lock no-error.

  FOR EACH tt-formtext:
    DELETE tt-formtext.
  END.

  lv-text = "".
  FOR EACH notes FIELDS(note_text) WHERE
      notes.rec_key EQ po-ordl.rec_key NO-LOCK:
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

  assign
    v-print-lines = v-print-lines + v-inst-lines
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
  /* v-wid     = po-ordl.s-wid - trunc(po-ordl.s-wid,0)
   v-wid     = ( v-wid * 16 ) / 100
   v-wid     = trunc(po-ordl.s-wid,0) + v-wid
   v-len     = po-ordl.s-len - trunc(po-ordl.s-len,0)
   v-len     = ( v-len * 16 ) / 100
   v-len     = trunc(po-ordl.s-len,0) + v-len*/
   v-tax[1]  = if po-ordl.tax AND v-tax[3] <> 0 then po-ord.tax * po-ordl.t-cost / v-tax[3] else 0
   v-adder   = ""
   xg-flag   = no
   v-basis-w = 0
   v-dep = IF po-ordl.s-dep GT 0 THEN po-ordl.s-dep
           ELSE IF AVAIL ITEM AND item.mat-type = "C" THEN ITEM.case-d
           ELSE IF AVAIL ITEM THEN item.s-dep
           ELSE 0
   .
      RUN sys\inc\decfrac2.p(INPUT po-ordl.s-wid, INPUT 32, OUTPUT v-wid).
      RUN sys\inc\decfrac2.p(INPUT po-ordl.s-len, INPUT 32, OUTPUT v-len).
      RUN sys\inc\decfrac2.p(INPUT v-dep, INPUT 32, OUTPUT v-dep2).

  /*IF v-dep NE 0 THEN
      
    ASSIGN v-dep = v-dep2 - TRUNC(v-dep2,0)
           v-dep = (v-dep * 16 ) / 100
           v-dep = TRUNC(v-dep2,0) + v-dep.*/


  IF AVAIL item THEN
    ASSIGN
     v-basis-w = item.basis-w.

  v-cost = po-ordl.t-cost - po-ordl.setup.

  release job-mat. 

  if avail item and item.mat-type eq "B" then do:
    find first job
        where job.company eq po-ord.company
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
          where job-mat.company  eq job.company
            and job-mat.job      eq job.job
            and job-mat.job-no   eq job.job-no
            and job-mat.job-no2  eq job.job-no2
            and job-mat.i-no     eq po-ordl.i-no
            and (job-mat.frm      eq po-ordl.s-num OR
                 po-ordl.s-num EQ ?)
          use-index job no-lock
          break by job-mat.blank-no desc:
        if last(job-mat.blank-no)            or
           job-mat.blank-no eq po-ordl.b-num then leave.
      end.

      if avail job-mat then do:
        find first ef
            where ef.company eq job.company
              and ef.est-no  eq job.est-no
              and ef.form-no eq job-mat.frm
            no-lock no-error.
   
        assign
         xg-flag   = avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B")
         v-num-add = 0.
         
        IF AVAIL ef THEN
        DO v-count = 1 TO 6:

           IF ef.adder[v-count] NE "" THEN
           for FIRST xjob-mat
               where xjob-mat.company  eq job-mat.company
                 and xjob-mat.job      eq job-mat.job
                 and xjob-mat.job-no   eq job-mat.job-no
                 and xjob-mat.job-no2  eq job-mat.job-no2
                 and xjob-mat.frm      eq job-mat.frm
                 and xjob-mat.blank-no eq job-mat.blank-no
                 and xjob-mat.i-no     ne job-mat.i-no
                 AND xjob-mat.i-no     EQ ef.adder[v-count]
               no-lock,
               first xitem
               where xitem.company  eq xjob-mat.company
                 and xitem.i-no     eq xjob-mat.i-no
                 and xitem.mat-type eq "A"
               NO-LOCK:

             assign
              v-num-add          = v-num-add + 1
              v-adder[v-num-add] = xitem.i-no.
           end.
        END.
        ELSE
           for each xjob-mat
               where xjob-mat.company  eq job-mat.company
                 and xjob-mat.job      eq job-mat.job
                 and xjob-mat.job-no   eq job-mat.job-no
                 and xjob-mat.job-no2  eq job-mat.job-no2
                 and xjob-mat.frm      eq job-mat.frm
                 and xjob-mat.blank-no eq job-mat.blank-no
                 and xjob-mat.i-no     ne job-mat.i-no
               no-lock,
               first xitem
               where xitem.company  eq xjob-mat.company
                 and xitem.i-no     eq xjob-mat.i-no
                  and xitem.mat-type eq "A"
               NO-LOCK:
             assign
                v-num-add          = v-num-add + 1
                v-adder[v-num-add] = xitem.i-no.
             
             if v-num-add ge 6 then leave.
        end.


      end.
    end.    
  end.

  assign
   v-ord-qty = if po-ordl.ord-qty - trunc(po-ordl.ord-qty,0) gt 0
               then string(po-ordl.ord-qty,">>>9.9<<<")
               else string(po-ordl.ord-qty,">>>>>9")
   v-ord-qty = fill(" ",6 - length(trim(v-ord-qty))) + trim(v-ord-qty).
  
  if v-job ne "" then
  DO:
     v-job = trim(v-job) + "-" + string(po-ordl.job-no2,"99").
      
     IF po-ordl.s-num NE ? THEN
        v-job = v-job + "." + STRING(po-ordl.s-num,"99").
  END.

  {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}

  IF AVAIL ITEM AND index("1,2,3,4,A,G,J,L,R,T,V,W,Z,9,M,I,O,X,Y,7,8",ITEM.mat-type) > 0 THEN .
  ELSE if avail item THEN v-print-lines = v-print-lines + 1.
            
  {po/poprints.i}

      IF AVAIL item AND item.mat-type EQ "B" THEN
        v-print-lines = v-print-lines + 1.
    END.     
    END.
  END.

  IF LINE-COUNTER {2} + 10 + v-print-lines GT PAGE-SIZE{2} + 1 THEN
    RUN pr-header (NO).

  IF v-cost GT 0 THEN
     v-setup = "Setup: " + STRING(po-ordl.setup,">>,>>9.99").
  ELSE
     v-setup = "".

  IF v-sqft LT 10000 THEN
     vsTmp-v-sqft = STRING(v-sqft,">,>>9.9<<").
  ELSE IF v-sqft LT 100000 THEN
     vsTmp-v-sqft = STRING(v-sqft,">>>>9.9<<").
  ELSE
     vsTmp-v-sqft = STRING(v-sqft,">>>>>9.9<").

  ASSIGN
    vsTmpLine = po-ordl.pr-uom + vsTmp-v-sqft
    v-line-2 = "".

  IF AVAIL ITEM AND ITEM.mat-type = "B" AND ITEM.industry = "2" THEN
  DO:
     RUN sys\inc\decfrac2.p(INPUT po-ordl.s-wid, INPUT 32, OUTPUT v-wid-frac).
     RUN sys\inc\decfrac2.p(INPUT po-ordl.s-len, INPUT 32, OUTPUT v-len-frac).
     v-line-2 = "W: " + v-wid-frac + " L: " + v-len-frac.
  END.
  ELSE
     v-line-2 = po-ordl.i-name.
     
  PUT {1} v-ord-qty FORMAT "X(6)" TO 6
          v-line-2 FORMAT "X(28)" AT 8
          v-adder[1] FORMAT "X(9)" AT 37
          v-job AT 47
          po-ordl.cost format ">>,>>9.99<<<" AT 60
          space(1) vsTmpLine
          po-ordl.dscr[1] FORMAT "X(28)" AT 8
          v-adder[2] FORMAT "X(9)" AT 37
          v-mach[1] FORMAT "X(6)" AT 50
          v-mach[2] FORMAT "X(1)" AT 57
          po-ordl.due-date FORMAT "99/99/99" AT 60
          v-cost FORMAT "->,>>>,>>9.99<<<" TO 80
          po-ordl.dscr[2] format "x(28)" at 8.

  DO v-count = 3 TO 6:
     IF v-adder[v-count] <> "" THEN
     DO:
        v-print-lines = v-print-lines + 1.
        PUT {1} v-adder[v-count] AT 37 SKIP.
     END.
  END.

  DISPLAY {1}
          string(po-ordl.over-pct,">9.99%") + "/" + string(po-ordl.under-pct,">9.99%") @ v-overrun
          v-setup WITH FRAME po-line-2.

  IF v-wid NE "" THEN
    PUT {1} "W: " at 8 v-wid space(2).

  IF v-len NE "" THEN
    PUT {1} "L: " v-len space(2).

  IF v-dep2 NE "" THEN
    PUT {1} "D: " v-dep SPACE(2).

  IF AVAIL ITEM AND ITEM.mat-type = "B" AND ITEM.industry EQ "2" THEN
    PUT {1} "Flute: " item.flute space(2) item.reg-no format "x(6)".

  run po/po-ordls.p (recid(po-ordl)).
            
  {po/poprints.i}

      IF AVAIL item AND item.mat-type EQ "B" THEN
        PUT {1}
            SKIP
            SPACE(7)
            "Score: "
            len-score FORMAT "x(66)".
    END.

      
    END.
  end.
            
  if v-inst-lines gt 0 then do:
    put {1} skip(1).
    do i = 1 to EXTENT(v-inst):
      if v-inst[i] ne "" then
        put {1} v-inst[i] skip.
    end.
  end.
  IF lCustCode THEN DO:
   put {1} SKIP.
   PUT {1} po-ordl.cust-no FORM "x(8)"  SKIP.
  END.
  
  put {1} skip "<C1><R+.5><FROM><C82><LINE><||3>" SKIP. 
end. /* for each po-ordl record */

FOR EACH tt-formtext:
  DELETE tt-formtext.
END.

lv-text = "".                         /*po-ord.rec_key*/
FOR EACH notes FIELDS(note_text) WHERE notes.rec_key EQ vend.rec_key
                 AND notes.note_group = "Vendor Notes" NO-LOCK:
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
DO i = 1 TO EXTENT(v-inst):
   if v-inst[i] ne "" then v-inst-lines = v-inst-lines + 1.
end.

if v-inst-lines gt 0 then do:
  v-inst-lines = v-inst-lines + 1.
  if LINE-COUNTER {2} + 10 + v-inst-lines GT PAGE-SIZE{2} + 1 THEN
    RUN pr-header (NO).

  put {1} skip(1).
  do i = 1 to EXTENT(v-inst):
    if v-inst[i] ne "" THEN DO:
       put {1} v-inst[i]  skip.
    END.
  end.
end.

   ASSIGN v-text = "".
   FOR EACH notes FIELDS(note_text) NO-LOCK
                  WHERE notes.rec_key = po-ord.rec_key :
      ASSIGN v-text = v-text +
                     (IF v-text <> "" THEN CHR(10) ELSE "") +
                      notes.note_text.
   END. /* FOR EACH notes NO-LOCK */
   ASSIGN v-text = FNformat(v-text,60).

   if LINE-COUNTER {2} + 10 + NUM-ENTRIES(v-text,"`") GT PAGE-SIZE{2} + 1 THEN
      RUN pr-header (NO).

   DO i = 1 TO NUM-ENTRIES(v-text,"`"):
      PUT {1} ENTRY(i,v-text,"`") FORMAT "X(60)" SKIP. 
   END.  
  
  v-line-cnt = LINE-COUNTER {2} .
  IF v-lines-per-page - v-line-cnt > 10 THEN
  DO i = 1 TO v-lines-per-page - v-LINE-Cnt - 10:
     PUT {1}  " " SKIP.
  END.
  

  
  v-sig-image = "signature\" + po-ord.buyer + ".jpg". /*RD1*/
  FILE-INFO:FILE-NAME = v-sig-image.
  IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
     v-sig-image = "signature\" + po-ord.buyer + ".bmp".     
     FILE-INFO:FILE-NAME = v-sig-image.
  END.
  v-sig-image = IF file-info:FULL-PATHNAME <> ? THEN FILE-INFO:FULL-PATHNAME + ">" ELSE "".

  FIND FIRST users WHERE users.USER_id = po-ord.buyer NO-LOCK NO-ERROR.
  IF NOT AVAIL users THEN
     FIND FIRST buyer WHERE buyer.company = po-ord.company
                        AND buyer.buyer = po-ord.buyer NO-LOCK NO-ERROR.
  v-username = IF AVAIL users THEN users.USER_name
               ELSE IF AVAIL buyer THEN buyer.buyer-n
               ELSE po-ord.buyer.
         
  PUT {1}
      "Freight:"             to 66 
     v-t-freight[2]         to 80   format ">,>>>,>>9.999"
     skip (1)
     "MSF:"                 to 66
     v-tot-sqft[2]          to 80   format ">,>>>,>>9.999" SKIP
      "TVS:"                 to 66
    /* v-tot-sqft[2]          to 80   format ">,>>>,>>9.999"*/
     SKIP.

   PUT {1}
     "Autorisé Per ________________________________"       at 2
     "TVQ:"                 to 66   
     v-tax[2]               to 80   format ">>,>>>,>>9.99"
     v-username FORM "x(30)" at 2
     "GRAND TOTAL:"         to 66  
     v-po-tot[2]            to 80   format ">>,>>>,>>9.99".
   PUT {1} SKIP(1)
     "Vérifié Per _________________________________"       at 2.

   PUT {1}
     "<B>FUNDS payable in " + string(cCurCode,"x(3)") +  ".</B>" AT 59 FORMAT "x(200)" .
  
  v-signature = IF v-sig-image <> "" THEN
                   "<C16><#2><R-4.5><C+40><Image#2=" + v-sig-image        
                ELSE "".

  PUT {1} 
      v-signature FORM "x(100)"
     SKIP.  


  PUT {1}
      "<R63.5><C50><P10> QF-38 Purchase Order Rev.2, 10/09".
