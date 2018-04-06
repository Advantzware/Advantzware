/* -------------------------------------------------- po/po-prexp.p 05/25 YSK */
/*                                                                            */
/* Pratt's export PO                                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-format as char no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xjob-mat for job-mat.
def buffer xitem for item.
def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-adder like item.i-no extent 6 no-undo.
def var xg-flag as log init no no-undo.
def var v-instr as char no-undo.
def var v-ord-qty like po-ordl.ord-qty extent 4 no-undo.
def var v-ord-cst like po-ordl.cost no-undo.
def var v-setup like e-item-vend.setup no-undo.
def var v-outfile as char extent 4 no-undo.
def var v-mach as char extent 4 no-undo.
def var v-line as char no-undo.
DEF VAR li-style AS INT NO-UNDO.
DEF VAR v-totalLine AS INT NO-UNDO.

DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

{sys/inc/pratt.i}

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

find first cust
    where cust.company eq cocode
      and cust.active  eq "X"
    no-lock no-error.

if avail cust and pratt-log and pratt-dir ne "" then
print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST po-ord
    WHERE RECID(po-ord) EQ report.rec-id
      AND CAN-FIND(FIRST po-ordl
                   WHERE po-ordl.company   EQ po-ord.company
                     AND po-ordl.po-no     EQ po-ord.po-no
                     AND po-ordl.item-type EQ YES
                     AND (v-printde-po OR NOT po-ordl.deleted)),

    FIRST vend
    WHERE vend.company EQ po-ord.company
      AND vend.vend-no EQ po-ord.vend-no
      AND (vend.po-export EQ "Pratt" OR
           (poexport-cha  EQ "Pratt" AND vend.an-edi-vend))
    NO-LOCK
                   
    BY po-ord.po-no.

  if opsys eq "UNIX" and substr(pratt-dir,1,1) ne v-slash then
    pratt-dir = v-slash + pratt-dir.

  if substr(pratt-dir,length(pratt-dir),1) eq v-slash then
    substr(pratt-dir,length(pratt-dir),1) = "".
    
  assign
   v-outfile[1] = trim(pratt-dir) + v-slash + "dataxfer" +
                  v-slash + "in" + v-slash
   v-outfile[2] = v-outfile[1] + string(time,"99999999")
   v-outfile[3] = "poPrat" /*+ trim(v-format) + "philcor"*/ +
                  substr(string(year(today),"9999"),3,2) +
                  string(month(today),"99") +
                  string(day(today),"99") +
                  substr(string(time,"HH:MM:SS"),1,2) +
                  substr(string(time,"HH:MM:SS"),4,2) +
                  substr(string(time,"HH:MM:SS"),7,2) + ".dat"
   v-outfile[4] = v-outfile[1] + v-outfile[3].

  output to value(v-outfile[2]).

  if po-ord.stat eq "N" then po-ord.stat = "O".

  assign
   v-sname    = cust.name
   v-saddr[1] = cust.addr[1]
   v-saddr[2] = cust.addr[2]
   v-scity    = cust.city
   v-sstate   = cust.state
   v-szip     = cust.zip.
 
  if po-ord.type eq "D" then
    assign
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.

  find first carrier
      where carrier.company eq po-ord.company
        and carrier.carrier eq po-ord.carrier
      no-lock no-error.

  /* Order Download Specification */

  /* total # of records*/
  v-totalline = 3 /* # of header lines */.
  FOR EACH po-ordl
      WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted),
      
      FIRST item
      WHERE item.company  EQ cocode
        AND item.i-no     EQ po-ordl.i-no
        AND item.mat-type EQ "B"
      NO-LOCK
      
      BY po-ordl.line:
      v-totalline = v-totalline + 5.
  END.

  put string(v-totalLine,"999") FORM "x(3)"  fill(' ',125) form "x(125)" .
  


  /* H1 */
  
    put 
  /* CUSTOMER # */
    "20500"                                     format "x(5)"
  
      /* 01 */
      "01"                                        format "x(2)"
  /* 000000 */
   "000000"                                    format "x(6)"
  /* CUSTOMER PHONE # */
   cust.area-code                              format "999"
      "-"                                         format "x"
      cust.phone                                  format "999-9999"
  /* CUSTOMER BILLING ZIP */
   cust.zip                                    format "x(12)"
  /* SHIP VIA */
   string(if avail carrier then carrier.dscr else po-ord.carrier) format "x(15)"  

  /* FREIGHT */
   IF po-ord.fob-code EQ "DEST" THEN "DESTINATION" ELSE "ORIGIN"
                                                  format "x(15)"

  /* P.O. DATE */
  po-ord.po-date                              format "99/99/99" 

  /* 000 */
   fill(" ",3)                                 format "x(3)"
  /* CUST # */
  "20500" /*cust.cust-no*/                    format "x(5)"
  /* 45 blank spaces */
   fill(" ",45)                                format "x(45)"  
   .


  /* H2 */
    
  /* CUST BILLING NAME */
  IF cust.addr[2] <> "" THEN
     put cust.name                                   format "x(30)".
  ELSE put fill(" ",30) FORM "x(30)".

  /* CUST BILLING ADDRESS */
  IF cust.addr[2] <> "" THEN
     put cust.addr[1]                                format "x(30)".
  ELSE put cust.name                                 format "x(30)".

  /* CUST BILLING ADDRESS */
  put cust.addr[2]                                format "x(30)".
    
  /* CUST BILLING CITY, ST */
  put trim(cust.city) + " " + trim(cust.state)    format "x(30)".

  /* 8 blank spaces */
  put fill(" ",8)                                 format "x(8)"       /*skip*/.

  /* H3 */

  /* CUST SHIP TO NAME */
  IF v-saddr[2] <> "" THEN
     put v-sname                                     format "x(30)".
  ELSE PUT   fill(" ",30) FORM "x(30)".

  /* CUST SHIP TO ADDRESS */
  IF v-saddr[2] <> "" THEN
     put v-saddr[1]                                  format "x(30)".
  ELSE put v-sname                                   format "x(30)".  

  /* CUST SHIP TO ADDRESS */
  put v-saddr[2]                                  format "x(30)".
    
  /* CUST SHIP TO CITY, ST */
  put trim(v-scity) + " " + trim(v-sstate)        format "x(30)".

  /* 8 blank spaces */
  put fill(" ",8)                                 format "x(8)"      /* skip*/ .


  FOR EACH po-ordl
      WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted),
      
      FIRST item
      WHERE item.company  EQ cocode
        AND item.i-no     EQ po-ordl.i-no
        AND item.mat-type EQ "B"
      NO-LOCK
      
      BY po-ordl.line:
      
    assign
     xg-flag = no
     v-adder = "".
    
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
         xg-flag = avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B")
         i       = 0.
         
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
            no-lock,
            
            FIRST reftable
            WHERE reftable.reftable EQ "util/b-pratt-x.w"
              AND reftable.company  EQ xitem.company
              AND reftable.code2    EQ xitem.i-no
            NO-LOCK:
              
          assign
           i          = i + 1
           v-adder[i] = STRING(INT(reftable.code),"9999").
             
          if i ge 6 then leave.
        end.
      end.
    end.

    /* Order Download Specification */
    
    /* D1 */

    /* CUSTOMER # */
    put "20500" /*cust.cust-no*/                    format "x(5)".

    /* 01 */
    put "01"                                        format "x(2)".
    
    /* PURCHASE ORDER # */
    put po-ord.po-no                                format "999999".

    /* A */
    put "A"                                         format "x(1)".
    
    /* PURCHASE ORDER # */
    put po-ordl.line                                format "99".
    
    /* PURCHASE ORDER # */
    put po-ord.po-no                                format "999999".

    /* A */
    put "A"                                         format "x(1)".
    
    /* PURCHASE ORDER # */
    put po-ordl.line                                format "99".
    
    /* MESSAGE CODE #1 */
    put fill(" ",2)                                 format "xx".
    
    /* MESSAGE CODE #2 */
    put fill(" ",2)                                 format "xx".
    
    /* MESSAGE CODE #3 */
    put fill(" ",2)                                 format "xx".
    
    /* 7 blank spaces */
    put fill(" ",7)                                 format "x(7)".
    
    /* BY */
    put "BY"                                        format "x(5)".
    
    /* DUE DATE */
    put substr(string(year(po-ordl.due-date),"9999"),3,2)  +
        string(month(po-ordl.due-date),"99") +
        string(day(po-ordl.due-date),"99")          format "x(6)".
    
    /* OVERRUN PERCENTAGE */
    put po-ord.over-pct                             format "99".
    
    /* UNDERRUN PERCENTAGE */
    put po-ord.under-pct                            format "99".
        
    /* INTEGER OF WIDTH */
    put trunc(po-ordl.s-wid,0)                      format "999999".

    /* NUMERATOR OF WIDTH */
    put (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                                                    format "99".
    /* DENOMINATOR OF WIDTH */
    put 16                                          format "99".
        
    /* WIDTH */
    put trunc(po-ordl.s-wid,0)                      format "999"
        ":"                                         format "x"
        (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                                                    format "99".
        
    /* INTEGER OF LENGTH */
    put trunc(po-ordl.s-len,0)                      format "999999".

    /* NUMERATOR OF LENGTH */
    put (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16
                                                    format "99".
    /* DENOMINATOR OF LENGTH */
    put 16                                          format "99".
        
    /* LENGTH */
    put trunc(po-ordl.s-len,0)                      format "999"
        ":"                                         format "x"
        (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16
                                                    format "99".
    
    /* STYLE NUMBER */
    run po/po-ordls.p (recid(po-ordl)).
    
    {po/po-ordls.i}
 
    li-style = IF AVAIL b-ref1 OR AVAIL b-ref2 THEN 1 ELSE 2.

    put li-style                                    format "9999".
    
    /* STYLE DESCRIPTION */
    put (IF li-style EQ 1 THEN "SCORED" ELSE "TRIMMED") + " SHEET"
                                                    format "x(14)".
    
    /* WEIGHT OF BOARD */
    put item.basis-w                                format "9999".
        
    /* QUANTITY SHEETS */
    v-ord-qty[1] = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "EA" then
      run sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty[1], output v-ord-qty[1]).
                           
    if v-ord-qty[1] - trunc(v-ord-qty[1],0) gt 0 then
      v-ord-qty[1] = trunc(v-ord-qty[1],0) + 1.

    v-ord-qty[2] = v-ord-qty[1].

    IF v-ord-qty[1] GT 99999999 THEN v-ord-qty[1] = 99999999.
    
    put v-ord-qty[1]                                format "99999999".

    /* 13 blank spaces */
    put fill(" ",13)                                format "x(13)"      /*skip*/.
    
    /* D2 */
    
    /* FLUTE */
    put item.flute                                  format "x(3)".
    
    /* PRICE PER MSF */
    v-ord-cst = po-ordl.cost.
    
    if po-ordl.pr-uom ne "MSF" then
      run sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-cst, output v-ord-cst).

    IF v-ord-cst GT 9999.99 THEN v-ord-cst = 9999.99.
                           
    put /* not for Pratt
          v-ord-cst                                   format "9999.99"*/
        FILL(" ",125) FORM "x(125)" .
        
    /* SETUP CHARGE */
    v-setup = 0.

    release e-item.
    release e-item-vend.

    find first e-item of item no-lock no-error.

    if avail e-item then
    find first e-item-vend of e-item
        where e-item-vend.vend-no   eq po-ord.vend-no
          and e-item-vend.item-type eq yes
        no-lock no-error.
    
    if avail e-item-vend then do:
      v-ord-qty[3] = po-ordl.ord-qty.

      if po-ordl.pr-qty-uom ne e-item.std-uom then
        run sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
                               item.basis-w, po-ordl.s-len,
                               po-ordl.s-wid, item.s-dep,
                               v-ord-qty[3], output v-ord-qty[3]).

      EMPTY TEMP-TABLE tt-eiv.
      CREATE tt-eiv.
      DO i = 1 TO 10:
         ASSIGN
            tt-eiv.run-qty[i] = e-item-vend.run-qty[i]
            tt-eiv.setups[i] = e-item-vend.setups[i].
      END.

            
      IF AVAIL e-item-vend THEN
      DO:
               
         DO i = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
               tt-eiv.setups[i + 10] = e-item-vend.setupsXtra[i].
         END.
      END.
                           
      do i = 1 to 20:
        if v-ord-qty[3] le tt-eiv.run-qty[i] then do:
           v-setup = tt-eiv.setups[i].
           leave.
        end.
      end.
    end.

    IF v-setup GT 999.99 THEN v-setup = 999.99.
    
    /* not for Pratt
    put v-setup                                     format "999.99".

    /* "001.00" */
    put 1                                           format "999.99".

    /* "00000000.0000" */
    put 0                                           format "99999999.9999".
    */    

    /* ORDERED SF */
    v-ord-qty[3] = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "SF" then
      run sys/ref/convquom.p(po-ordl.pr-qty-uom, "SF",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty[3], output v-ord-qty[3]).
                           
    if v-ord-qty[3] - trunc(v-ord-qty[3],0) gt 0 then
      v-ord-qty[3] = trunc(v-ord-qty[3],0) + 1.

    v-ord-qty[4] = v-ord-qty[3].

    IF v-ord-qty[3] GT 9999999 THEN v-ord-qty[3] = 9999999.
  /* not for Pratt  
    put v-ord-qty[3]                                format "9999999".

    /* 46 blank spaces */
    put fill(" ",46)                                format "x(46)".

    /* "X" */
    put "X"                                         format "x".

    /* "X" */
    put "X"                                         format "x".

    /* DESCRIPTION TEXT */
    put po-ordl.i-name                              format "x(30)".

    /* 8 blank spaces */
    put fill(" ",8)                                 format "x(8)"       skip.
 */

    /* D3 */
    /* 128 blank spaces  board description */
    put fill(" ",128)                               format "x(128)"     /*skip*/.
    
    /* D4 */    
    /* SCORE */
    do i = 1 to 9:
      if avail b-ref1 and b-ref1.val[i] ne 0 then 
        put trunc(b-ref1.val[i],0)                  format ">>>"
            ":"                                     format "x"
            (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100
                                                    format "99"
            substr(b-ref1.dscr,i,1)                 format "x".
            
      else put "       "                            format "x(7)".
    end.

    /* NUMBER UP */
    put 1                                           format "999.99".

    /* TRIM */
    put fill(" ",2)                                 format "xx".

    /* 1 OUT WIDTH, NO TRIM */
    put trunc(po-ordl.s-wid,0)                      format "999"
        ":"                                         format "x"
        (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                                                    format "99".

    /* BLANK WIDTH (MULT OUT) */
    put trunc(po-ordl.s-wid,0)                      format "999"
        ":"                                         format "x"
        (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                                                    format "99".

    /* (SF OR SM) PER M */
    v-ord-qty[2] = v-ord-qty[4] / (v-ord-qty[2] / 1000).

    IF v-ord-qty[2] GT 99999999 THEN v-ord-qty[2] = 99999999.

    put v-ord-qty[2]                                format "99999999".

    /* EXT'D (SF OR SM) ORDERED */
    IF v-ord-qty[4] GT 99999999 THEN v-ord-qty[4] = 99999999.

    put v-ord-qty[4]                                format "99999999".

    /* BASE BOARD GRADE CODE   position 100-103 */
    FIND FIRST reftable
        WHERE reftable.reftable EQ "util/b-hrms-x.w"
          AND reftable.company  EQ po-ordl.company
          AND reftable.code2    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
    if avail reftable then
      put int(reftable.code)                        format "9999".    
    else
      put 0                                         format "9999".
    
    /* ADDERS */
    do i = 1 to 6:
      put v-adder[i]                                format "x(4)".
    end.

    /* 1 blank space */
    put fill(" ",1)                                 format "x(1)"       /*skip*/.
    
    /* D5 */

    /* SPECIAL INSTRUCTIONS */
    v-instr = "".

    for each notes where notes.rec_key eq po-ordl.rec_key no-lock:
      v-instr = v-instr + " " + trim(notes.note_text).
    end.

    for each notes where notes.rec_key eq po-ord.rec_key no-lock:
      v-instr = v-instr + " " + trim(notes.note_text).
    end.

    put v-instr                                     format "x(64)".

    /* Job# */
    if po-ordl.job-no ne "" then
      put string(po-ordl.job-no,"x(6)") + "-" +
          string(po-ordl.job-no2,"99")              format "x(9)".
    else
      put fill(" ",9)                               format "x(9)".

    /* 64 blank spaces */
    put fill(" ",55)                                format "x(55)"       /*skip*/.
    
  end. /* for each po-ordl record */


  po-ord.printed = yes.
  if search(v-outfile[2]) ne ? then do:
    
    output close.   
    /*===
    if opsys eq "unix" then
      unix silent quoter -c 1-3000 value(v-outfile[2]) >
                                   value(v-outfile[2] + ".quo").
    else
      dos  silent quoter -c 1-3000 value(v-outfile[2]) >
                                   value(v-outfile[2] + ".quo").
                                   
    input from value(v-outfile[2] + ".quo").
    
    output to value(v-outfile[4]).
    
    repeat:
      v-line = fill(" ",128).
      import v-line.
      put v-line format "x(128)" /*skip*/.
    end.


    output close.
    
    input close.
    ===*/
    OS-COPY VALUE(v-outfile[2]) VALUE(v-outfile[4]).

    if opsys eq "unix" then
      unix silent rm value(v-outfile[2] + "*.*").
    else
      dos silent del value(v-outfile[2] + "*.*").
  

    RUN po/ftppo.p (v-outfile[4],"Pratt"). 

  end.

  PAUSE 1 NO-MESSAGE.
end. /* for each po-ord record */

/* end ----------------------------------- Copr. 2004  Advanced Software Inc. */
