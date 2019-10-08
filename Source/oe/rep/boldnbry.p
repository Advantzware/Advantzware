/* ---------------------------------------------- oe/rep/boldnbry.p 01/99 JLF */
/* PRINT Danbury BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.

{oe/rep/oe-lad.i}

def var v-bol-no            like oe-bolh.bol-no.
def var v-bol-qty           like oe-boll.qty.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   log  format "Complete/".
def var v-part-info         as   char format "x(52)".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".


find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(2)
       oe-bolh.bol-date             at 61
       skip(3)
       carrier.dscr                 at 62 format "x(19)" when avail carrier
       skip(4)
       cust.name                    at 7
       v-ship-name                  at 51
       cust.addr[1]                 at 7
       v-ship-addr[1]               at 51
       cust.addr[2]                 at 7
       v-ship-addr[2]               at 51
       v-cust-addr3                 at 7
       v-ship-addr3                 at 51
       skip(5)

    with frame bol-top page-top no-box no-underline stream-io width 80.

form v-bol-qty                      to 11 format "->>,>>>,>>9"
     oe-boll.i-no                   at 13
     itemfg.i-name                  at 29 format "x(30)"
     oe-boll.ord-no                 to 68
     v-part-comp                    at 72

    with frame bol-mid down no-box no-labels stream-io width 80.

form header
     skip(4)
     trim(string(v-bol-no,">>>>>>>9")) at 19
     skip(1)

    with frame bol-bot page-bottom no-box no-underline stream-io width 80.


{sa/sa-sls01.i}

view frame bol-top.
view frame bol-bot.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each xreport  where xreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)  eq xreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq oe-bolh.company
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
  end. /* first-of(oe-bolh.bol-no) */

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = string(oe-boll.ord-no,"999999")
     report.key-02   = oe-boll.i-no
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
    page.

    for each report where report.term-id eq v-term-id,

        first oe-boll where recid(oe-boll) eq report.rec-id,

        first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

        first itemfg
        where itemfg.company eq oe-boll.company
          and itemfg.i-no    eq oe-boll.i-no
        no-lock

        break by report.key-01
              by report.key-02:

      v-bol-qty = v-bol-qty + oe-boll.qty.

      if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
        find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
        if not avail w2 then create w2.
        assign
         w2.cas-cnt = oe-boll.qty-case
         w2.cases   = w2.cases + oe-boll.cases.
      end.

      if oe-boll.partial ne 0 then do:
        find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
        if not avail w2 then create w2.
        assign
         w2.cas-cnt = oe-boll.partial
         w2.cases   = w2.cases + 1.
      end.

      if last-of(report.key-02) then do:
        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-boll.ord-no
              and oe-ordl.i-no    eq oe-boll.i-no
              and oe-ordl.line    eq oe-boll.line
            no-lock no-error.

        release oe-rel.
        find first oe-rell
            where oe-rell.company eq oe-boll.company
              and oe-rell.r-no    eq oe-boll.r-no
              AND oe-rell.ord-no  EQ oe-boll.ord-no
              and oe-rell.i-no    eq oe-boll.i-no
              and oe-rell.line    eq oe-boll.line
             no-lock no-error.
        if avail oe-rell then do:
          find first oe-relh of oe-rell no-lock.
          find first oe-rel
              where oe-rel.company eq oe-relh.company
                and oe-rel.ord-no  eq oe-rell.ord-no
                and oe-rel.line    eq oe-rell.line
                and oe-rel.link-no eq oe-rell.r-no
                and oe-rel.ship-no eq oe-relh.ship-no
                and oe-rel.i-no    eq oe-rell.i-no
              no-lock no-error.

          if not avail oe-rel then
          find first oe-rel
              where oe-rel.company  eq oe-relh.company
                and oe-rel.ord-no   eq oe-rell.ord-no
                and oe-rel.line     eq oe-rell.line
                and oe-rel.rel-date eq oe-relh.rel-date
                and oe-rel.ship-no  eq oe-relh.ship-no
                and oe-rel.i-no     eq oe-rell.i-no
              no-lock no-error.
        end.

        v-part-comp = not avail oe-ordl or
                      oe-ordl.ship-qty + v-bol-qty ge oe-ordl.qty.

        display v-bol-qty
                oe-boll.i-no
                itemfg.i-name
                oe-boll.ord-no when oe-boll.ord-no ne 0
                v-part-comp

           with frame bol-mid.
        down with frame bol-mid.

        put "PO#:" at 8.
        if avail oe-rel and oe-rel.po-no ne "" then put oe-rel.po-no  at 13.
                                               else put oe-bolh.po-no at 13.

        v-part-info = "".
        for each w2 break by w2.cases desc:
          v-part-info = v-part-info + trim(string(w2.cases,">>>9")) + "@" +
                                      trim(string(w2.cas-cnt,">>>>>9")) + "  ".
        end.

        v-part-info = substr(v-part-info,1,length(v-part-info) - 1).

        put v-part-info at 29 skip.

        if avail oe-ordl and oe-ordl.part-no ne "" then
          put "Your Part #: " at 13
              oe-ordl.part-no skip.

        put skip(1).

        v-bol-qty = 0.

        for each w2:
          delete w2.
        end.
      end.

      delete report.
    end. /* for each report */

    v-bol-no = oe-bolh.bol-no.
  end.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

hide frame bol-top no-pause.
page.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
