/* -------------------------------------------------- po/po-ckexp.p 08/05 JLF */
/*                                                                            */
/* Corr-U-Kraft II export PO                                                  */
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
def var v-text as char no-undo.
def var v-instr as char no-undo.
DEF VAR v-inst AS cha FORMAT "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
def var v-ord-qty like po-ordl.ord-qty no-undo.
def var v-ord-cst like po-ordl.cost no-undo.
def var v-outfile as char extent 4 no-undo.
def var v-mach as char extent 4 no-undo.
def var v-line as char no-undo.
DEF VAR li AS INT NO-UNDO.

&SCOPED-DEFINE for-each-po-ordl                       ~
FOR EACH po-ordl                                      ~
      WHERE po-ordl.company   EQ po-ord.company       ~
        AND po-ordl.po-no     EQ po-ord.po-no         ~
        AND po-ordl.item-type EQ YES                  ~
        AND (v-printde-po OR NOT po-ordl.deleted),    ~
                                                      ~
      FIRST item                                      ~
      WHERE item.company  EQ cocode                   ~
        AND item.i-no     EQ po-ordl.i-no             ~
        AND item.mat-type EQ "B"                      ~
      NO-LOCK


{sys/inc/corkraft.i}

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

find first cust
    where cust.company eq cocode
      and cust.active  eq "X"
    no-lock no-error.

if avail cust and corkraft-log and corkraft-dir ne "" then
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
      AND (vend.po-export EQ "Corr-U-KraftII" OR
           (poexport-cha  EQ "Corr-U-KraftII" AND vend.an-edi-vend))
    NO-LOCK
                   
    BREAK BY po-ord.po-no.
    
  if first(po-ord.po-no) then do:
    if opsys eq "UNIX" and substr(corkraft-dir,1,1) ne v-slash then
      corkraft-dir = v-slash + corkraft-dir.

    if substr(corkraft-dir,length(corkraft-dir),1) eq v-slash then
      substr(corkraft-dir,length(corkraft-dir),1) = "".
    
    assign
     v-outfile[1] = trim(corkraft-dir) + v-slash + "dataxfer" +
                    v-slash + "in" + v-slash
     v-outfile[2] = v-outfile[1] + string(time,"99999999")
     v-outfile[3] = "po_" + trim(v-format) + "amcor" +
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

  /* IDGRP */
  PUT "058" FORMAT "x(3)".
    
  /* IDCMPNY */
  PUT "058" FORMAT "x(3)".

  /* IDCUST */
  PUT (IF corkraft-int EQ 0 THEN 77777 ELSE corkraft-int) FORMAT "99999".
    
  /* IDLOC */
  PUT "0" + STRING(po-ord.TYPE EQ "D","1/2") FORMAT "x(2)".
  
  li = 0.
  {&for-each-po-ordl}:
    li = li + 1.
  END.
    
  /* IDCNTO */
  PUT li FORMAT "999".
    
  /* Not Used */
  PUT 0 FORMAT "999999".
    
  /* Filler */
  PUT FILL(" ",490) format "x(490)".

  PUT SKIP.
  
  {&for-each-po-ordl}
      
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
            no-lock,
            
            FIRST reftable
            WHERE reftable.reftable EQ "util/b-hrms-x.w"
              AND reftable.company  EQ xitem.company
              AND reftable.code2    EQ xitem.i-no
            NO-LOCK:
              
          assign
           i          = i + 1
           v-adder[i] = reftable.code.
             
          if i ge 6 then leave.
        end.
      end.
    end.
    
    /* OTS Order Detail Record */
    
    /* RIRECT */
    put "O" format "x".
    
    /* RICUS# */
    PUT (IF corkraft-int EQ 0 THEN 77777 ELSE corkraft-int) FORMAT "99999".              
    
    /* RIRUNW */
    put "N" format "x".
        
    /* RIPO## */
    put po-ord.company format "xxx"
        po-ord.po-no   format "9999999999"
        po-ordl.line   format "999".
        
    /* RIQTYO */
    v-ord-qty = po-ordl.ord-qty.
    
    if po-ordl.pr-qty-uom ne "EA" then
      run sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                             item.basis-w, po-ordl.s-len,
                             po-ordl.s-wid, item.s-dep,
                             v-ord-qty, output v-ord-qty).
                           
    {sys/inc/roundup.i v-ord-qty}
    
    put v-ord-qty format "9999999".

    /* RISQFC */
    put " " format "x".
    
    /* RIMXOV */
    put po-ord.over-pct format "99".
    
    /* RIMXUN */
    put po-ord.under-pct format "99".
        
    /* RIWIDW */
    put trunc(po-ordl.s-wid,0) format "999".

    /* RIWIDN */
    put (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16 format "99".

    /* RIWIDD */
    put 16 format "99".

    /* RILGHW */    
    put trunc(po-ordl.s-len,0) format "999".

    /* RILGHN */
    put (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16 format "99".

    /* RILGHD */
    put 16 format "99".
    
    /* RITEST */
    put item.reg-no format "x(4)".
    
    /* RIFLUT */
    put item.flute format "x(2)".
    
    /* RILINR */
    put "    " format "x(4)".
    
    /* RIWRA */
    put " " format "x".
    
    /* RI#69 */
    put " " format "x".
    
    /* RIWAX */
    put " " format "x".
    
    /* RIMIK */
    put " " format "x".
    
    /* RI33# */
    put " " format "x".
    
    /* RIWILC */
    put " " format "x".

    run po/po-ordls.p (recid(po-ordl)).
    
    {po/po-ordls.i}

    do i = 1 to 12:
      if avail b-ref1 then do:

        /* RISC[i]W */
        put trunc(b-ref1.val[i],0) format "999".

        /* RISC[i]N */
        put (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100 format "99".

        /* RISC[i]D */
        put 16 format "99".

        /* RISC[i]T */
        put substr(b-ref1.dscr,i,1) format "x".
      end.
            
      else put "0000000 ".
    end.
    
    /* RIDUDT */
    put month(po-ordl.due-date)                           format "99"
        "/"                                               format "x"
        day(po-ordl.due-date)                             format "99"
        "/"                                               format "x"
        substr(string(year(po-ordl.due-date),"9999"),3,2) format "99".
    
    /* RIDUEM */
    put fill(" ",16) format "x(16)".
    
    /* RIMISC */
    put fill(" ",9) format "x(9)".
    
    /* RISHNM */
    put v-sname format "x(25)".
    
    /* RISHAD */
    put trim(v-saddr[1]) + 
        (if v-saddr[2] ne "" then ", " else "") +
        trim(v-saddr[2])                          format "x(25)".
    
    /* RISHCY */
    put v-scity format "x(16)".
    
    /* RISHST */
    put v-sstate format "x(2)".
    
    /* RISHZP */
    put v-szip format "x(5)".

    /* RISINS */
    v-instr = "".
    for each notes where notes.rec_key eq po-ord.rec_key no-lock:
      v-text = notes.note_text.
      do i = 1 to length(v-text):
        if substr(v-text,i,1) eq chr(10) then substr(v-text,i,1) = "".
      end.
      v-instr = v-instr + " " + trim(v-text).
    end.
    put trim(v-instr) format "x(72)".

    /* RISPEC */
    v-instr = "".
    for each notes where notes.rec_key eq po-ordl.rec_key no-lock:
      v-text = notes.note_text.
      do i = 1 to length(v-text):
        if substr(v-text,i,1) eq chr(10) then substr(v-text,i,1) = "".
      end.
      v-instr = v-instr + " " + trim(notes.note_text).
    end.
    put trim(v-instr) format "x(72)".

    /* RIBYER */
    put fill(" ",10) format "x(10)".
    
    /* RIADDR */
    do i = 1 to 6:
      put v-adder[i] format "x(5)".
    end.
    put fill(" ",15) format "x(15)".

    /* RIFILL */
    put fill(" ",46) format "x(46)".

    put skip.
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
      
    RUN po/ftppo.p (v-outfile[4],"Corr-U-KraftII"). 
    message "Corr-U-Kraft II file:" trim(v-outfile[3]) "has been created"
            view-as alert-box.
  end.
end. /* for each po-ord record */

/* end ----------------------------------- Copr. 2005  Advanced Software Inc. */
