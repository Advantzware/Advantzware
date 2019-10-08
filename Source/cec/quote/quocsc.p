/* --------------------------------------------- cec/quote/quocsc.p 11/97 FWK */
/* print quotes in 10 Pitch format                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.

{est/printquo.i}

def var k_frac as dec init "6.25" no-undo.
def var numfit as int no-undo.
def var comfit as int no-undo.
def var linfit as int no-undo.
def var sold as ch extent 5 no-undo.
def var bill as ch extent 5 no-undo.
def var ship as ch extent 5 no-undo.
def var tot as de no-undo.
def var v-over-under as char no-undo.
def var v-comp-name like company.name extent 4.
def var trim-size like xqitm.size no-undo.
def var temp-trim-size like xqitm.size no-undo.
def var cc as int no-undo.
def var v-printline as int initial 0 no-undo.
def var v-first-q-no like quote.q-no no-undo.
def var v-line like xqitm.line no-undo.

def var v-part              like xqitm.part-no                          no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
DEF VAR style-name LIKE style.dscr NO-UNDO.
DEF VAR v-style AS cha NO-UNDO.

tmpstore = fill("-",130).

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.
FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

format xquo.est-no        to 5
       xqitm.part-no      to 31   format "x(25)"
       trim-size          to 57   format "x(24)"
       xqqty.qty          to 64   format ">>>>>>9"
       xqqty.price        to 75   format ">>,>>9.99"
       xqqty.uom          to 80

      header "Est # Description                Siz/Styl/Brd/C" +
             "olors         QTY      Price  UOM" format "x(80)"
      with frame item-10p no-box no-labels down width 80 STREAM-IO.

  find first est where est.company = xquo.company
                   AND est.est-no eq xquo.est-no no-lock no-error.
  find first sman
      where sman.company eq cocode
        and sman.sman    eq xquo.sman
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq xquo.carrier
      no-lock no-error.
  find first terms
      where terms.company eq cocode
        and terms.t-code  eq xquo.terms
      no-lock no-error.
  find first cust
      where cust.company eq xquo.company
        and cust.cust-no eq xquo.cust-no
      no-lock no-error.

  if avail cust then
    v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                   trim(string(cust.under-pct,">>9.9<%")).

  assign
   sold[5] = trim(string(xquo.sold-no))
   ship[5] = trim(string(xquo.ship-id))
   bill[5] = trim(string(xquo.cust-no)).

  do i = 1 to 4:
    assign
     sold[i] = xquo.soldto[i]
     ship[i] = xquo.shipto[i]
     bill[i] = xquo.billto[i].
  end.

  if (xquo.shipto[1] eq xquo.soldto[1] and
      xquo.shipto[2] eq xquo.soldto[2] and
      xquo.shipto[3] eq xquo.soldto[3] and
      xquo.shipto[4] eq xquo.soldto[4]) then
    assign
     ship[1] = ""
     ship[2] = ""
     ship[3] = ""
     ship[4] = ""
     ship[5] = "SAME".

  find first company where company.company eq cocode no-lock no-error.
  if v-log then do:
    i = 0.
    if company.name ne "" then
      assign
       i              = i + 1
       v-comp-name[i] = company.name.

    if company.addr[1] ne "" then
      assign
       i              = i + 1
       v-comp-name[i] = company.addr[1].

    if company.addr[2] ne "" then
      assign
       i              = i + 1
       v-comp-name[i] = company.addr[2].

    if company.city ne "" then
      assign
       i              = i + 1
       v-comp-name[i] = company.city + ", " + company.st + "  " + company.zip.
  end.

  format header
     skip(2)
     v-comp-name[1] skip
     v-comp-name[2] skip
     v-comp-name[3] skip
     v-comp-name[4] skip
     skip(1)
     "Quote"                    to 56
     "Date"                     to 66
     v-first-q-no                  to 56 format ">>>>9"
     xquo.quo-date              to 68
     skip(1)
     "Sold To"
     "Ship To"                  at 45
     ship[5] skip
     sold[1] format "x(30)"
     ship[1] format "x(30)"     at 45
     sold[2] format "x(30)"
     ship[2] format "x(30)"     at 45
     sold[3] format "x(30)"
     ship[3] format "x(30)"     at 45
     sold[4] format "x(30)"
     ship[4] format "x(30)"     at 45
     skip(1)
     "ATTN:"
     xquo.contact
     skip(1)
     "Salesmn"
     "Ship Via"                 at 10
     "FOB"                      at 20
     "Terms"                    at 28
     "Over-Under %"             at 69
     sman.sman                  at 3
     carrier.carrier            at 10
     cust.fob-code VIEW-AS TEXT at 20
     terms.dscr                 at 28   format "x(30)"
     v-over-under               at 69   format "x(12)"
     skip(1)
  with frame quote page-top width 80 no-labels no-box STREAM-IO.
    
  v-first-q-no = xquo.q-no.

  view frame quote.

  if (not ch-multi) then do:
    {cec/quote/quocsc.i 1}

    release est.
    if v-prt-box then
      find first est
          where est.company eq xquo.company
            and est.est-no  eq xquo.est-no
          no-lock no-error.
    if avail est then do:
      put skip(2).
      run cec/desprnt.p (input recid(est)).
    end.
  end.

  else do:
   for each report where report.term-id eq v-term-id,
       first xquo  where recid(xquo) eq report.rec-id
       no-lock
       break by report.key-01
             by report.key-02
             by report.key-03
       transaction:

    find first est where est.company = xquo.company
                   AND est.est-no eq xquo.est-no no-lock no-error.
    find first sman
        where sman.company eq cocode
          and sman.sman    eq xquo.sman
        no-lock no-error.
    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq xquo.carrier
        no-lock no-error.
    find first terms
        where terms.company eq cocode
          and terms.t-code  eq xquo.terms
        no-lock no-error.
    find first cust
        where cust.company eq xquo.company
          and cust.cust-no eq xquo.cust-no
        no-lock no-error.

    if avail cust then
      v-over-under = trim(string(cust.over-pct,">>9.9<%")) + "-" +
                     trim(string(cust.under-pct,">>9.9<%")).

    assign
     sold[5] = trim(string(xquo.sold-no))
     ship[5] = trim(string(xquo.ship-id))
     bill[5] = trim(string(xquo.cust-no)).

    do i = 1 to 4:
      assign
       sold[i] = xquo.soldto[i]
       ship[i] = xquo.shipto[i]
       bill[i] = xquo.billto[i].
    end.

    if first-of(report.key-01) then do:
      page.
      v-first-q-no = xquo.q-no.
    end.

    v-last = last-of(report.key-01).

    {cec/quote/quocsc.i 2}
    delete report.
   end.
  end.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
