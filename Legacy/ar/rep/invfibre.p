/* ---------------------------------------------- ar/rep/invfibre.p 11/00 JLF */
/* PRINT Fibre Invoice                                                        */
/* -------------------------------------------------------------------------- */

DEF VAR v-prt-control-list AS cha NO-UNDO.
DEF VAR v-prt-control AS cha NO-UNDO.
DEF VAR v-cnt AS INT NO-UNDO.
v-prt-control-list = "27,67,51".
DO v-cnt = 1 TO 3:
   v-prt-control = v-prt-control + CHR(INT(ENTRY(v-cnt,v-prt-control-list))).
END.
PUT CONTROL v-prt-control.


{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-rh for rm-rcpth.
def buffer b-rd for rm-rdtlh.

def var save_id as recid.

{ar/rep/invoice.i}

def var v-bot-lab    as   char format "x(20)" extent 2 NO-UNDO.
def var v-tot-inv    as   dec  format "->>>>>>9.99" NO-UNDO.

def var v-part-dscr  as   char format "x(25)"NO-UNDO.
def var v-lines      as   INT NO-UNDO.

def var v-po-no      like ar-invl.po-no NO-UNDO.
def var v-sman-no    as   CHAR NO-UNDO.
def var v-fob        as   CHAR NO-UNDO.
def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 like shipto.ship-name NO-UNDO.
def var v-cust-addr3 like shipto.ship-name NO-UNDO.
def var v-rel-qty    like ar-invl.inv-qty NO-UNDO.
def var v-p-c        as   CHAR NO-UNDO.
DEF VAR v-bol-no     AS INT NO-UNDO.

def workfile w-lin
    field w-par like ar-invl.i-name init ""
    field w-lin as   int.
    
find first ar-inv no-lock no-error.
find first ar-invl no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(3)
       trim(string(ar-inv.inv-no,">>>>>>"))     at 73
       skip(1)
       ar-inv.inv-date      format "99/99/99"   at 73
       skip(1)
       trim(string(v-bol-no,">>>>>>"))   at 73
       skip(3)
       v-po-no              format "x(15)"      at 3
       v-sman-no            format "x(3)"       at 19
       ar-inv.cust-no       format "x(8)"       at 22
       carrier.dscr         format "x(11)"      at 31   when avail carrier
       v-fob                format "x(11)"      at 46
       ar-inv.terms-d       format "x(23)"      at 62   
       skip(2)
       cust.name            format "x(47)"      at 3
       v-ship-name          format "x(33)"      at 52
       cust.addr[1]         format "x(47)"      at 3
       v-ship-addr[1]       format "x(33)"      at 52
       cust.addr[2]         format "x(47)"      at 3
       v-ship-addr[2]       format "x(33)"      at 52
       v-cust-addr3         format "x(47)"      at 3
       v-ship-addr3         format "x(33)"      at 52
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.
    
form ar-invl.qty            format "->>>>>>>"       to 8
     ar-invl.ord-no         format ">>>>>>"         at 11   
     w-par                  format "x(30)"          at 19
     ar-invl.inv-qty        format "->>>>>>>"       to 57
     v-p-c                  format "X"            at 59
     ar-invl.unit-pr        format "->>>>>9.99<<<"  to 69
     ar-invl.pr-qty-uom     format "x(4)"           at 71
     ar-invl.amt            format "->>>>9.99"      to 84

    with frame inv-mid1 down no-box no-labels stream-io width 85.

form header
     " "
     skip(5)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]                               to 84
     v-bot-lab[2]                               to 84
     skip(1)
     v-tot-inv                                  to 84
     skip(1)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.
    
    
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame inv-top.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock
    
    break by ar-inv.cust-no
          by ar-inv.inv-no:

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  if avail shipto then
    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-city    = shipto.ship-city
     v-ship-state   = shipto.ship-state
     v-ship-zip     = shipto.ship-zip
     v-ship-addr3   = v-ship-city + ", " +
                      v-ship-state + "  " +
                      v-ship-zip.
                        
  assign
   v-po-no   = ""
   v-sman-no = ""
   v-bol-no  = 0.
    
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      by ar-invl.i-no:
    if v-po-no eq "" and ar-invl.po-no ne "" then v-po-no = ar-invl.po-no.
    IF v-bol-no EQ 0 AND ar-invl.bol-no NE 0 THEN v-bol-no = ar-invl.bol-no.
    
    if v-sman-no eq "" then
    do i = 1 to 3:
      if ar-invl.sman[i] ne "" then do:
        v-sman-no = ar-invl.sman[i].
        leave.
      end.  
    end.
  end.

  assign
   v-fob        = if ar-inv.fob-code begins "ORIG" 
                  then "ORIGIN" else "DESTINATION"
   v-cust-addr3 = cust.city + ", " +
                  cust.state + "  " +
                  cust.zip.

  if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
  if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

  page.

  hide frame inv-bot2.
  view frame inv-bot1.
  
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      by ar-invl.misc
      by ar-invl.i-no:

    v-lines = 0.
    
    if ar-invl.part-no ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if ar-invl.i-name ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-name
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if ar-invl.i-dscr ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-dscr
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.

   if ar-invl.part-dscr1 ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-dscr1
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if ar-invl.part-dscr2 ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-dscr2
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
          
    if ar-invl.i-no ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if ar-invl.po-no ne "" then do:
      create w-lin.
      assign
       w-par   = "PO#: " + ar-invl.po-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.

    IF ar-invl.job-no NE "" THEN DO:
      RELEASE reftable.
      FIND FIRST job NO-LOCK
          WHERE job.company EQ ar-invl.company
            AND job.job-no  EQ ar-invl.job-no
            AND job.job-no2 EQ ar-invl.job-no2
          NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ar-invl.i-no
          USE-INDEX reftable NO-ERROR.
      IF NOT AVAIL reftable THEN
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ ar-invl.company
            AND job-hdr.job-no  EQ ar-invl.job-no
            AND job-hdr.job-no2 EQ ar-invl.job-no2
            AND job-hdr.i-no    EQ ar-invl.i-no
          NO-ERROR.

      IF AVAIL reftable OR AVAIL job-hdr THEN
      FOR EACH rm-rcpth NO-LOCK
          WHERE rm-rcpth.company   EQ ar-invl.company
            AND rm-rcpth.job-no    EQ ar-invl.job-no
            AND rm-rcpth.job-no2   EQ ar-invl.job-no2
            AND rm-rcpth.rita-code EQ "I"
          USE-INDEX job,
          EACH rm-rdtlh NO-LOCK
          WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
            AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
            AND rm-rdtlh.s-num     EQ (IF AVAIL reftable THEN reftable.val[12]
                                                         ELSE job-hdr.frm)
            AND rm-rdtlh.tag       NE "",
          EACH b-rd NO-LOCK
          WHERE b-rd.company   EQ rm-rdtlh.company
            AND b-rd.tag       EQ rm-rdtlh.tag
            AND b-rd.loc       EQ rm-rdtlh.loc
            AND b-rd.loc-bin   EQ rm-rdtlh.loc-bin
            AND b-rd.rita-code EQ "R"
            AND b-rd.tag2      NE ""
          USE-INDEX tag,
          FIRST b-rh NO-LOCK
          WHERE b-rh.r-no      EQ b-rd.r-no
            AND b-rh.rita-code EQ b-rd.rita-code
            AND b-rh.i-no      EQ rm-rcpth.i-no:
        IF NOT CAN-FIND(FIRST w-lin WHERE w-lin.w-par EQ rm-rdtlh.tag2) THEN DO:
          FIND FIRST w-lin WHERE w-lin.w-par EQ "" NO-ERROR.
          IF NOT AVAIL w-lin THEN CREATE w-lin.
          ASSIGN
           w-lin.w-par = b-rd.tag2
           v-lines     = v-lines + 1
           w-lin       = v-lines.
        END.
      END.
    END.
    
    do v-lines = v-lines + 1 to 2:
      if v-lines eq 1 or ar-invl.est-no ne "" then do:
        create w-lin.
        w-lin = v-lines.
      end.  
    end.
    
    v-lines = v-lines + 1.
    
    if line-counter - 1 + v-lines gt page-size + 1 then page.
    
    for each w-lin by w-lin with frame inv-mid1:
      display w-par.
      
    v-p-c = "P".
    
    FOR EACH oe-boll WHERE
        oe-boll.company EQ ar-invl.company AND
        oe-boll.bol-no eq ar-invl.bol-no AND
        oe-boll.i-no eq ar-invl.i-no AND
        oe-boll.ord-no EQ ar-invl.ord-no
        no-lock:
        
        if oe-boll.p-c then v-p-c = "C".
    end.

      if w-lin eq 1 then
        display ar-invl.inv-qty
                ar-invl.qty
                ar-invl.ord-no
                w-par
                v-p-c
                ar-invl.unit-pr
                ar-invl.pr-qty-uom
                ar-invl.amt.
                
      else
      if w-lin eq 2 then
        display int(ar-invl.est-no) @ ar-invl.ord-no.
        
      down.
      
      delete w-lin.
    end.  

    put skip(1).
  end.
  
  hide frame inv-bot1.
  view frame inv-bot2.
    
  assign
   v-bot-lab[1]   = if ar-inv.freight eq 0 or not ar-inv.f-bill then "" else
                    (" FREIGHT: " + string(ar-inv.freight,"->>>>>9.99"))
   v-bot-lab[2]   = if ar-inv.tax-amt eq 0 then "" else
                    (" TAXABLE: " + string(ar-inv.tax-amt,"->>>>>9.99"))
   v-tot-inv      = /*ar-inv.due*/ ar-inv.gross
   ar-inv.printed = yes.
end. /* for each ar-inv */

hide frame inv-top no-pause.
page.
PUT CONTROL v-prt-control.
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
