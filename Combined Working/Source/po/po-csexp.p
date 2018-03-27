/* -------------------------------------------------- po/po-hrexp.p 02/02 JLF */
/*                                                                            */
/* Harry Rohde export PO                                                      */
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
DEF VAR v-job-no AS CHAR NO-UNDO.
DEF VAR lv-cust-no AS CHAR NO-UNDO.
DEF VAR lv-cust-list AS CHAR INIT "360,360,5,71" NO-UNDO.
DEF VAR lv-char-list AS CHAR INIT ",CSC,SouthPak,TrePaper" NO-UNDO.


DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

{sys/inc/corsuply.i}

lv-cust-no = ENTRY(LOOKUP(corsuply-cha,lv-char-list),lv-cust-list).

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

find first cust
    where cust.company eq cocode
      and cust.active  eq "X"
    no-lock no-error.

if avail cust and corsuply-log and corsuply-dir ne "" then
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
    WHERE vend.company    EQ po-ord.company
      AND vend.vend-no    EQ po-ord.vend-no
      AND (vend.po-export EQ "CorSuply" OR
           (poexport-cha  EQ "CorSuply" AND vend.an-edi-vend))
    NO-LOCK

    BREAK BY po-ord.po-no.

  if first(po-ord.po-no) then do:
    if opsys eq "UNIX" and substr(corsuply-dir,1,1) ne v-slash then
      corsuply-dir = v-slash + corsuply-dir.

    if substr(corsuply-dir,length(corsuply-dir),1) eq v-slash then
      substr(corsuply-dir,length(corsuply-dir),1) = "".
    
    assign
     v-outfile[1] = trim(corsuply-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + string(time,"99999999")
     v-outfile[3] = "po_" + trim(REPLACE(v-format, " ","")) + "corsuply" +
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat"
     v-outfile[4] = v-outfile[1] + v-outfile[3].
    
    output to value(v-outfile[2]).
  end.

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
            no-lock:
              
          assign
           i          = i + 1
           v-adder[i] = xitem.i-no.
             
          if i ge 6 then leave.
        end.
      end.
    end.

    /* Order Download Specification */

    /* H1 */

    /* CUSTOMER # */
    put lv-cust-no                                  format "x(5)".

    /* 01 */
    put "01"                                        format "x(2)".

    /* 000000 */
    put "000000"                                    format "x(6)".

    /* CUSTOMER PHONE # */
    put cust.area-code                              format "999"
        "-"                                         format "x"
        cust.phone                                  format "999-9999".

    /* CUSTOMER BILLING ZIP */
    put cust.zip                                    format "x(12)".

    /* SHIP VIA */
    put string(if avail carrier then carrier.dscr else po-ord.carrier)
                                                    format "x(15)".

    /* FREIGHT */
    put IF po-ord.fob-code EQ "DEST" THEN "DESTINATION" ELSE "ORIGIN"
                                                    format "x(15)".

    /* P.O. DATE */
    put po-ord.po-date                              format "99999999".

    /* 000 */
    put fill(" ",3)                                 format "x(3)".

    /* CUST # */
    put lv-cust-no                                  format "x(5)".

    /* 45 blank spaces */
    put fill(" ",45)                                format "x(45)"      skip.

    /* H2 */
    
    /* CUST BILLING NAME */
    put cust.name                                   format "x(30)".
    
    /* CUST BILLING ADDRESS */
    put cust.addr[1]                                format "x(30)".
    
    /* CUST BILLING ADDRESS */
    put cust.addr[2]                                format "x(30)".
    
    /* CUST BILLING CITY, ST */
    put trim(cust.city) + " " + trim(cust.state)    format "x(30)".

    /* 8 blank spaces */
    put fill(" ",8)                                 format "x(8)"       skip.

    /* H3 */
    
    /* CUST SHIP TO NAME */
    put v-sname                                     format "x(30)".
    
    /* CUST SHIP TO ADDRESS */
    put v-saddr[1]                                  format "x(30)".
    
    /* CUST SHIP TO ADDRESS */
    put v-saddr[2]                                  format "x(30)".
    
    /* CUST SHIP TO CITY, ST */
    put trim(v-scity) + " " + trim(v-sstate)        format "x(30)".

    /* 8 blank spaces */
    put fill(" ",8)                                 format "x(8)"       skip.
    
    /* D1 */

    /* CUSTOMER # */
    put lv-cust-no                                  format "x(5)".

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

    /* Job# */
    v-job-no = "".

    find first job
        where job.company eq cocode 
          and job.job-no eq string(fill(" ",6 - length(trim(po-ordl.job-no)))) +
                            trim(po-ordl.job-no) 
          and job.job-no2 eq po-ordl.job-no2
        no-lock no-error.

    if avail job then do:
      FOR EACH job-mch
          where job-mch.company  eq cocode
            and job-mch.job      eq job.job
            and job-mch.job-no   eq job.job-no
            and job-mch.job-no2  eq job.job-no2
            and job-mch.frm     eq po-ordl.s-num
          BY job-mch.LINE /*pass BY job-mch.dept*/:
        v-job-no = SUBSTRING(job-mch.m-code,1,1).
        LEAVE.
      END.
      IF v-job-no = "" THEN
      FOR EACH job-mch
          where job-mch.company  eq cocode
            and job-mch.job      eq job.job
            and job-mch.job-no   eq job.job-no
            and job-mch.job-no2  eq job.job-no2
          NO-LOCK
          BY job-mch.LINE /*job-mch.frm BY job-mch.blank-no BY job-mch.pass BY job-mch.dept*/:
        v-job-no = SUBSTRING(job-mch.m-code,1,1).
        LEAVE.
      END.

      v-job-no = (IF v-job-no <> "" THEN v-job-no + "-" ELSE "") +
                 trim(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2,"99").

      IF po-ordl.job-no = "" THEN v-job-no = "".

      IF v-job-no = "-" THEN v-job-no = "".
    END.
    
    put v-job-no                                    format "x(13)".
    
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
    put fill(" ",13)                                format "x(13)"      skip.
    
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
                           
    put v-ord-cst                                   format "9999.99".
        
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
                               v-ord-qty[2], output v-ord-qty[2]).

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
    
    put v-setup                                     format "999.99".

    /* "001.00" */
    put 1                                           format "999.99".

    /* "00000000.0000" */
    put 0                                           format "99999999.9999".
        
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
    
    /* D3 */

    /* BOARD CODE */
    put po-ordl.i-no                                format "x(15)".   
    
    /* ADDERS */
    do i = 1 to 6:
      put v-adder[i]                                format "x(10)".
    end.

    /* 53 blank spaces */
    put fill(" ",53)                                format "x(53)"     skip.
    
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

    /* 4 blank spaces */
    put fill(" ",33)                                format "x(33)"      skip.
    
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

    /* 64 blank spaces */
    put fill(" ",64)                                format "x(64)"       skip.
  end. /* for each po-ordl record */

  po-ord.printed = yes.
  
  if last(po-ord.po-no)        and
     search(v-outfile[2]) ne ? then do:

    output close.
    
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
      put v-line format "x(128)" skip.
    end.
    
    output close.
    
    input close.
    
    if opsys eq "unix" then
      unix silent rm value(v-outfile[2] + "*.*").
    else
      dos silent del value(v-outfile[2] + "*.*").
      
    RUN po/ftppo.p (v-outfile[4],"CorSuply"). 
    message "Corrugated Supplies file:" trim(v-outfile[3]) "has been created" 
            view-as alert-box.
  end.
end. /* for each po-ord record */

/* end ----------------------------------- Copr. 2004  Advanced Software Inc. */
