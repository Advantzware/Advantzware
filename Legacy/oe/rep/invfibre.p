/* ---------------------------------------------- oe/rep/invfibre.p 11/00 JLF */
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

def buffer xinv-head for inv-head.
def buffer b-rh for rm-rcpth.
def buffer b-rd for rm-rdtlh.

{oe/rep/invoice.i}

def var v-inv-no     like inv-head.inv-no.
def var v-bot-lab    as   char format "x(20)" extent 2.
def var v-tot-inv    as   dec  format "->>>>>>9.99".

def var v-part-dscr  as   char format "x(25)".
def var v-lines      as   int. 

def var v-po-no      like inv-line.po-no.
def var v-sman-no    as   char.
def var v-fob        as   char.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 like shipto.ship-name.
def var v-cust-addr3 like shipto.ship-name.
def var v-p-c        as   char.

def workfile w-lin
    field w-par like inv-line.i-name init ""
    field w-lin as   int.

find first inv-head no-lock no-error.
find first inv-line no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(3)
       trim(string(v-inv-no,">>>>>>"))          at 73
       skip(1)
       inv-head.inv-date    format "99/99/99"   at 73
       skip(1)
       trim(string(inv-head.bol-no,">>>>>>"))   at 73
       skip(3)
       v-po-no              format "x(15)"      at 3
       v-sman-no            format "x(3)"       at 19
       inv-head.cust-no     format "x(8)"       at 22
       carrier.dscr         format "x(11)"      at 31   when avail carrier
       v-fob                format "x(11)"      at 46
       inv-head.terms-d     format "x(23)"      at 62   
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

form oe-ordl.qty            format "->>>>>>>"       to 8
     inv-line.ord-no        format ">>>>>>"         at 11   
     w-par                  format "x(30)"          at 19
     inv-line.inv-qty       format "->>>>>>>"       to 57
     v-p-c                  format "x"              at 59
     inv-line.price         format "->>>>>9.99<<<"  to 69
     inv-line.pr-uom        format "x(4)"           at 71
     inv-line.t-price       format "->>>>9.99"      to 84

    with frame inv-mid1 down no-box no-labels stream-io width 85.

form inv-misc.charge                            at 19
     inv-misc.dscr          format "x(30)"
     inv-misc.amt           format "->>>>>9.99" to 84

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 85.

form header
     " "
     skip(5)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]                   to 84
     v-bot-lab[2]                   to 84
     skip(1)
     v-tot-inv                      to 84
     skip(1)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame inv-top.

for each report where report.term-id eq v-term-id no-lock,

    first inv-head where recid(inv-head) eq report.rec-id no-lock,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq inv-head.cust-no
    no-lock

    break by report.key-01
          by report.key-02:

  IF NOT v-reprint OR inv-head.inv-no EQ 0 THEN DO TRANSACTION:
    RUN oe/get-inv#.p (ROWID(inv-head)).
  END.

  FIND xinv-head WHERE ROWID(xinv-head) EQ ROWID(inv-head) NO-LOCK.

  ASSIGN
   v-inv-no       = xinv-head.inv-no
   v-ship-name    = inv-head.sold-name
   v-ship-addr[1] = inv-head.sold-addr[1]
   v-ship-addr[2] = inv-head.sold-addr[2]
   v-ship-city    = inv-head.sold-city
   v-ship-state   = inv-head.sold-state
   v-ship-zip     = inv-head.sold-zip.

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq inv-head.carrier
      no-lock no-error.

  find first oe-bolh
      where oe-bolh.company eq cocode
        and oe-bolh.bol-no  eq inv-head.bol-no
      use-index bol-no no-lock no-error.

  if avail oe-bolh then do:
    find first shipto
        where shipto.company eq cocode
          and shipto.cust-no eq oe-bolh.cust-no
          and shipto.ship-id eq oe-bolh.ship-id
        no-lock no-error.

    if avail shipto then
      assign
       v-ship-name    = shipto.ship-name
       v-ship-addr[1] = shipto.ship-addr[1]
       v-ship-addr[2] = shipto.ship-addr[2]
       v-ship-city    = shipto.ship-city
       v-ship-state   = shipto.ship-state
       v-ship-zip     = shipto.ship-zip.
  end. /* avail oe-bolh */
    
  assign
   v-po-no   = ""
   v-sman-no = "".
    
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      by inv-line.i-no:
    if v-po-no eq "" and inv-line.po-no ne "" then v-po-no = inv-line.po-no.
    
    if v-sman-no eq "" then
    do i = 1 to 3:
      if inv-line.sman[i] ne "" then do:
        v-sman-no = inv-line.sman[i].
        leave.
      end.  
    end.
  end.

  assign
   v-fob          = if inv-head.fob-code begins "O" 
                    then "ORIGIN" else "DESTINATION"
   v-ship-addr3   = v-ship-city + ", " +
                    v-ship-state + "  " +
                    v-ship-zip
   v-cust-addr3   = cust.city + ", " +
                    cust.state + "  " +
                    cust.zip.

  if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
  if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
    
  page.
    
  hide frame inv-bot2.
  view frame inv-bot1.
  
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      by inv-line.i-no:
      
    release oe-ordl.
      
    if inv-line.ord-no ne 0 then
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq inv-line.ord-no
          and oe-ordl.i-no    eq inv-line.i-no
          and oe-ordl.line    eq inv-line.line
        no-lock no-error.
      
    v-p-c = "P".
    
    for each oe-boll
        where oe-boll.b-no eq inv-line.b-no
          and oe-boll.i-no eq inv-line.i-no
          and oe-boll.line eq inv-line.line
        no-lock:
        
      if oe-boll.p-c then v-p-c = "C".
    end.
    
    v-lines = 0.
    
    if inv-line.part-no ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.part-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if inv-line.i-name ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.i-name
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if inv-line.part-dscr1 ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.part-dscr1
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if inv-line.part-dscr2 ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.part-dscr2
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
          
    if inv-line.i-no ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.i-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if inv-line.po-no ne "" then do:
      create w-lin.
      assign
       w-par   = "PO#: " + inv-line.po-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.

    IF inv-line.job-no NE "" THEN DO:
      RELEASE reftable.
      FIND FIRST job NO-LOCK
          WHERE job.company EQ inv-line.company
            AND job.job-no  EQ inv-line.job-no
            AND job.job-no2 EQ inv-line.job-no2
          NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ inv-line.i-no
          USE-INDEX reftable NO-ERROR.
      IF NOT AVAIL reftable THEN
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ inv-line.company
            AND job-hdr.job-no  EQ inv-line.job-no
            AND job-hdr.job-no2 EQ inv-line.job-no2
            AND job-hdr.i-no    EQ inv-line.i-no
          NO-ERROR.

      IF AVAIL reftable OR AVAIL job-hdr THEN
      FOR EACH rm-rcpth NO-LOCK
          WHERE rm-rcpth.company   EQ inv-line.company
            AND rm-rcpth.job-no    EQ inv-line.job-no
            AND rm-rcpth.job-no2   EQ inv-line.job-no2
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
           v-lines = v-lines + 1
           w-lin   = v-lines.
        END.
      END.
    END.
    
    do v-lines = v-lines + 1 to 2:
      if v-lines eq 1 or inv-line.est-no ne "" then do:
        create w-lin.
        w-lin = v-lines.
      end.  
    end.
    
    v-lines = v-lines + 1.
    
    if line-counter - 1 + v-lines gt page-size + 1 then page.
        
    for each w-lin by w-lin with frame inv-mid1:
      display w-par.
      
      if w-lin eq 1 then
        display oe-ordl.qty when avail oe-ordl
                inv-line.inv-qty when not avail oe-ordl @ oe-ordl.qty
                inv-line.ord-no
                inv-line.inv-qty
                v-p-c
                inv-line.price
                inv-line.pr-uom
                inv-line.t-price.
                
      else
      if w-lin eq 2 then
        display int(inv-line.est-no) @ inv-line.ord-no.
        
      down with frame inv-mid1.  
        
      delete w-lin.  
    end.
    
    put skip(1).
  end.  
  
  FOR EACH inv-misc NO-LOCK
      WHERE inv-misc.company EQ cocode
        AND inv-misc.r-no    EQ inv-head.r-no
        AND inv-misc.bill    EQ "Y"
      BREAK BY inv-misc.charge:

    v-lines = 2 + INT(inv-misc.po-no NE "").

    IF LINE-COUNTER - 1 + v-lines GT PAGE-SIZE + 1 THEN PAGE.

    DISPLAY inv-misc.charge
            inv-misc.dscr
            inv-misc.amt

        WITH FRAME inv-mid2.
    DOWN WITH FRAME inv-mid2.

    IF v-lines EQ 3 THEN DO:
      DISPLAY "PO#:" + TRIM(inv-misc.po-no) @ inv-misc.dscr

          WITH FRAME inv-mid2.
      DOWN WITH FRAME inv-mid2.
    END.

    PUT SKIP(1).
  END. /* each inv-misc */
  
  hide frame inv-bot1.
  view frame inv-bot2.

  assign
   v-bot-lab[1] = if inv-head.t-inv-freight ne 0 and inv-head.f-bill then
                    (" FREIGHT: " +
                     string(inv-head.t-inv-freight,"->>>>>9.99")) else ""
   v-bot-lab[2] = if inv-head.t-inv-tax     ne 0 then
                    (" TAXABLE: " +
                     string(inv-head.t-inv-tax,    "->>>>>9.99")) else ""

   v-tot-inv    = inv-head.t-inv-rev.
end. /* for each inv-head */

PAGE.

PUT CONTROL v-prt-control.
/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
