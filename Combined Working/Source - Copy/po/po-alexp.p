/* -------------------------------------------------- po/po-alexp.p 01/03 JLF */
/*                                                                            */
/* Alliance export PO                                                        */
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
def var v-ord-qty like po-ordl.ord-qty no-undo.
def var v-ord-cst like po-ordl.cost no-undo.
def var v-outfile as char extent 4 no-undo.
def var v-mach as char extent 4 no-undo.
def var v-line as char no-undo.
def var v-char as char no-undo.


{sys/inc/alliance.i}

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

if alliance-log and alliance-dir ne "" then
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
      AND (vend.po-export EQ "Alliance" OR
           (poexport-cha  EQ "Alliance" AND vend.an-edi-vend))
    NO-LOCK

    BREAK BY po-ord.po-no:

  if first(po-ord.po-no) then do:
    if opsys eq "UNIX" and substr(alliance-dir,1,1) ne v-slash then
      alliance-dir = v-slash + alliance-dir.

    if substr(alliance-dir,length(alliance-dir),1) eq v-slash then
      substr(alliance-dir,length(alliance-dir),1) = "".
    
    assign
     v-outfile[1] = trim(alliance-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + string(time,"99999999")
     v-outfile[3] = "po_" + trim(REPLACE(v-format, " ","")) + "alliance" +
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat"
     v-outfile[4] = v-outfile[1] + v-outfile[3].
    
    output to value(v-outfile[2]).

    /* Order Download Specification - BEGIN */
    put "$BEGIN$"
        skip.
  end.

  if po-ord.stat eq "N" then po-ord.stat = "O".

  assign
   v-sname    = company.name
   v-saddr[1] = company.addr[1]
   v-saddr[2] = company.addr[2]
   v-scity    = company.city
   v-sstate   = company.state
   v-szip     = company.zip.
 
  if po-ord.type eq "D" then
    assign
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.
  
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
      
      BY po-ordl.line WITH FRAME po-line:
      
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
    
    /* ORDER RECORD */
    
    /* TYPE */
    v-char = if po-ordl.deleted then "D" else string(po-ord.printed,"C/A").
    put v-char format "x".
    
    /* GRADE */
    put po-ordl.i-name format "x(25)".

    /* FLUTE */
    put item.flute  format "x(10)".

    /* TEST */
    put item.reg-no format "x(10)".

    /* DUE DATE */
    put month(po-ordl.due-date)                           format "99"
        day(po-ordl.due-date)                             format "99"
        substr(string(year(po-ordl.due-date),"9999"),3,2) format "99".
        
    /* WIDTH & LENGTH */
    put trunc(po-ordl.s-wid,0)                          format "9999"
        (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16   format "99".
        
    put trunc(po-ordl.s-len,0)                          format "9999"
        (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16   format "99".

    /* QUANTITY */
    v-ord-qty = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "EA" then
      run sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty, output v-ord-qty).
                           
    if v-ord-qty - trunc(v-ord-qty,0) gt 0 then
      v-ord-qty = trunc(v-ord-qty,0) + 1.
    
    put v-ord-qty   format "999999".
    
    /* MAX OVER % */
    put po-ord.over-pct   format "999".
    
    /* UNDER % */
    put po-ord.under-pct   format "999".

    /* BLANK CODES (Adders) */
    do i = 1 to 6:
      put v-adder[i]    format "x(10)".
    end.

    /* CUST PO# */
    put po-ord.company  format "xxx"
        po-ord.po-no    format "999999999"
        po-ordl.line    format "999".
    
    /* BILL TO CUST ADDRESS NAME */
    put company.name    format "x(30)".
    
    /* BILL TO CUST ADDRESS1 */
    put company.addr[1] format "x(30)".
    
    /* BILL TO CUST ADDRESS2 */
    put company.addr[2] format "x(30)".
    
    /* BILL TO CUST CITY */
    put company.city    format "x(20)".
    
    /* BILL TO CUST STATE */
    put company.state   format "x(2)".
    
    /* BILL TO CUST ZIP */
    put company.zip     format "x(10)".

    /* SHIP TO CUST ADDRESS NAME */
    put v-sname     format "x(30)".
    
    /* SHIP TO CUST ADDRESS1 */
    put v-saddr[1]  format "x(30)".
    
    /* SHIP TO CUST ADDRESS2 */
    put v-saddr[2]  format "x(30)".
    
    /* SHIP TO CUST CITY */
    put v-scity     format "x(20)".
    
    /* SHIP TO CUST STATE */
    put v-sstate    format "x(2)".

    /* SHIP TO CUST ZIP */
    put v-szip      format "x(10)".
    
    /* CUST ITEM */
    put po-ordl.i-no    format "x(25)".

    /* FIRST MACHINE CODE & INITIAL */
    {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}
    
    put v-mach[1]   format "x(5)"
        v-mach[2]   format "x(1)".

    put po-ordl.vend-i-no format "x(15)".

    put skip.

    /* SCORES RECORD */

    run po/po-ordls.p (recid(po-ordl)).
    
    {po/po-ordls.i}    
    
    /* TYPE */
    put "X".
    
    do i = 1 to 12:    
      /* SCORE1 to 12 */
      if avail b-ref1 then
        put "02"
            trunc(b-ref1.val[i],0)                          format "9999"
            (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100  format "99".
            
      else put "00000000".
    end.
    
    do i = 1 to 5:     
      /* SCORE13 to 17 */
      if avail b-ref2 then
        put "02"
            trunc(b-ref2.val[i],0)                          format "9999"
            (b-ref2.val[i] - trunc(b-ref2.val[i],0)) * 100  format "99".
            
      else put "00000000".
    end.

    PUT TRIM(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2,"99") FORMAT "x(9)".

    /* FILLER */
    put space(109).
    
    put skip.
    
    /* SPECIAL INSTRUCTIONS RECORD */

    v-instr = "".
    
    do i = 1 to 4:
      if po-ord.spec-i[i] ne "" then
        v-instr = v-instr + trim(po-ord.spec-i[i]) + " ".
    end.

    /* TYPE */
    put "Z".

    /* SPECIAL INSTRUCTIONS */
    put v-instr     format "x(80)".

    /* FILLER */
    put space(177).

    put skip.
  end. /* for each po-ordl record */

  po-ord.printed = yes.
  
  if last(po-ord.po-no)        and
     search(v-outfile[2]) ne ? then do:
    /* Order Download Specification - END */
    put "$END$"
        skip.

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
      v-line = "".
      import v-line.
      if v-line eq "" then put skip(1).
      else put unformatted v-line skip.
    end.
    
    output close.
    
    input close.
    
    if opsys eq "unix" then
      unix silent rm value(v-outfile[2] + "*.*").
    else
      dos silent del value(v-outfile[2] + "*.*").
      
    message "Alliance file:" trim(v-outfile[3]) "has been created"
            view-as alert-box.
  end.
end. /* for each po-ord record */

/* end ----------------------------------- Copr. 2002  Advanced Software Inc. */
