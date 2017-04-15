/* ---------------------------------------------- oe/rep/cocxprnt.p 07/06 YSK */
/* Print Xprint COC (Certificate of Compliance)                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.

{oe/rep/oe-lad.i}

def var v-fob               as   char format "x(12)".
def var v-ord-bol           as   char format "x(15)".
def var v-ord-date          like oe-ord.ord-date.
def var v-part-qty          as   dec.
def var v-po-no             like oe-bolh.po-no extent 2.
def var v-bol-qty           like oe-boll.qty.
def var v-qty-alf           as   char format "x(5)".
def var v-bin               as   char format "x(22)" extent 4.
def var v-ship-i            as   char extent 2 format "x(60)".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-carrier-dscr LIKE carrier.dscr NO-UNDO.

ASSIGN ls-image1 = "images\apc.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".


def workfile w1
  field part-no like fg-set.part-no
  field rec-id  as   recid.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first report no-lock no-error.
/*
form header
     skip(9)
     cust.name                    at 17
     v-ship-name                  at 63
     cust.addr[1]                 at 17
     v-ship-addr[1]               at 63
     cust.addr[2]                 at 17
     v-ship-addr[2]               at 63
     v-cust-addr3                 at 17
     v-ship-addr3                 at 63
     skip(1)
     v-po-no[1]                   at 33
     v-po-no[2]                   at 77
     skip(2)
     v-ord-date                   at 9
     oe-bolh.bol-date             at 23
     carrier.dscr                 at 37 format "x(20)" when avail carrier
     v-fob                        at 58
     v-ord-bol                    at 78
     skip(4)
    with frame coc-top page-top no-box no-underline width 102.
*/
form v-qty-alf                    at 7
     space(4)
     itemfg.i-name
     skip(1)
     
    with frame coc-mid no-box no-underline no-labels down width 102.

form header
     skip(5)
     v-ship-i[1]                  at 19 
     v-ship-i[2]                  at 8     
    with frame coc-tot page-bottom no-box no-underline no-labels down width 102.

/*view frame coc-top.*/

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

{sa/sa-sls01.i}

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id no-lock:

  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.b-no    eq oe-bolh.b-no
      no-lock:

    release oe-rel.

    find first oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-boll.r-no
          AND oe-rell.ord-no  EQ oe-boll.ord-no
          and oe-rell.i-no    eq oe-boll.i-no
          and oe-rell.line    eq oe-boll.line
         no-lock no-error.
    if avail oe-rell then do:
      find first oe-relh of oe-rell no-lock.
      find first oe-rel
          where oe-rel.company eq cocode
            and oe-rel.ord-no  eq oe-relh.ord-no
            and oe-rel.line    eq oe-rell.line
            and oe-rel.link-no eq oe-rell.r-no
            and oe-rel.ship-no eq oe-relh.ship-no
            and oe-rel.i-no    eq oe-rell.i-no
          no-lock no-error.
      if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-relh.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
    end.

    create xreport.
    assign
     xreport.term-id = v-term-id
     xreport.key-01  = report.key-01
     xreport.key-02  = report.key-02
     xreport.key-03  = report.key-03
     xreport.key-04  = report.key-04
     xreport.key-05  = if avail oe-rel then oe-rel.po-no else ""
     xreport.key-06  = oe-boll.i-no
     xreport.rec-id  = recid(oe-boll).
  end.

  delete report.
end.

for each report   where report.term-id eq v-term-id no-lock,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-05
          by report.key-06

    with frame coc-mid:

  assign
   v-po-no[1] = report.key-04
   v-po-no[2] = report.key-05.

  if first-of(report.key-05) then do:
    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-fob      = ""
     v-ord-bol  = trim(string(oe-bolh.ord-no,">>>>>9")) + " / " +
                  trim(string(oe-bolh.bol-no,">>>>>9"))
     v-ord-date = oe-bolh.bol-date.

    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-bolh.ord-no
        no-lock no-error.

    if avail oe-ord then do:
      if not avail carrier then
      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.

      assign
       v-fob      = if oe-ord.fob-code begins "ORIG" then "Origin"
                                                     else "Destination"
       v-ord-date = oe-ord.ord-date.
    end.
    {oe/rep/cocxprnt.i}
    /*page.
      hide frame coc-bot no-pause.
    */
    assign
     v-ship-i[1] = oe-bolh.ship-i[3] 
     v-ship-i[2] = oe-bolh.ship-i[4].
     
    view frame coc-bot.
  end.
  
  v-bol-qty = v-bol-qty + oe-boll.qty.
  
  if last-of(report.key-06) then do:
    v-qty-alf = trim(string(v-bol-qty,">>>>9")).

    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-bolh.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no
          and oe-ordl.line    eq oe-boll.line
        no-lock no-error.

    display v-qty-alf
            itemfg.i-name
              oe-ordl.i-name when avail oe-ordl and oe-ordl.i-name ne ""
              @ itemfg.i-name.

    assign
     i     = 0
     j     = 0
     v-bin = "".

    find first fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq itemfg.i-no
        no-lock no-error.

    if itemfg.isaset then do while avail fg-set:
      i = i + 1.
     
      if i gt 8 then do:
        put skip(3).

        assign
         i     = 1
         j     = 0
         v-bin = "".
      end.

      {sys/inc/part-qty.i v-part-qty fg-set}

      if i modulo 4 eq 1 or i eq 1 then put space(6).

      put trim(string(v-bol-qty * v-part-qty,">>>>9")) format "x(5)"
          space(1)
          fg-set.part-no
          space(1).

      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.i-no    eq fg-set.part-no
          no-lock no-error.

      if avail fg-bin then
        v-bin[if i modulo 4 eq 0 then 4 else (i modulo 4)] = fg-bin.loc-bin.

      if i modulo 4 eq 0 then do:
        put skip
            space(6)
            v-bin
            skip.

        j = j + 2.
      end.
    
      find next fg-set
          where fg-set.company eq cocode
            and fg-set.set-no  eq itemfg.i-no
          no-lock no-error.
    end.

    if i modulo 4 ne 0 then do:
      put skip
          space(6)
          v-bin
          skip.

      j = j + 2.
    end.

    do j = (j + 1) to 5:
      put skip(1).
    end.
    
    v-bol-qty = 0.
  end.
end. /* for each report */

hide frame coc-top no-pause.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

