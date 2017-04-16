/* ------------------------------------------- cec/quote/prtquo10.p 11/00 JLF */
/* print quotes in Fibre format                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.

{est/printquo.i}

def var sold                as   char                       extent 5    no-undo.
def var bill                as   char                       extent 5    no-undo.
def var ship                as   char                       extent 5    no-undo.
def var tot                 as   dec                                    no-undo.
def var v-over-under        as   char                                   no-undo.
def var v-comp-name         as   char   format "x(80)"      extent 4    no-undo.
def var v-hdr               as   char   format "x(80)"                  no-undo.
def var v-note              like xquo.comment                           no-undo.
def var v-part              like quoteit.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var trim-size           like quoteit.size                           no-undo.
def var cc                  as   int                                    no-undo.

def var v-first-q-no like quotehd.q-no no-undo.
def var numfit as int no-undo.
def var v-last as log initial no no-undo.
DEF VAR style-name LIKE style.dscr NO-UNDO.
DEF VAR v-fob AS CHAR FORMAT "x(5)" NO-UNDO.


assign
 tmpstore = fill("-",130)
 .
         
FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.

FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.


format xquo.est-no        to 8    format "x(8)"
       xqitm.part-no      to 28   format "x(19)"
       trim-size          to 47   format "x(18)"
       xqqty.qty          to 55   format ">>>>>>9"
       xqqty.price        to 65   format ">>,>>9.99"
       xqqty.uom          to 69
       xxx                to 80   format ">>>,>>9.99"

      with frame detail no-box no-labels down width 80 STREAM-IO.


v-hdr = "Est #    Description         Siz/Styl/Br" +
        "d/Color     QTY     Price UOM      Price".

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

  if avail cust then v-over-under =
    trim(string(cust.over-pct,">>9.9<%"))   + "-" +
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

  FORM header
       skip(2)
       v-comp-name[1] skip
       v-comp-name[2] skip
       v-comp-name[3] skip
       v-comp-name[4] skip
       skip(1)
       "Quote"                    to 56
       "Date"                     to 66
       "Your RFQ"                 to 80
       v-first-q-no               to 56 format ">>>>9"
       xquo.quo-date              to 68
       xquo.rfq                   to 80 format "x(8)"
       skip(1)
       "Sold To"
       sold[5]
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
       cust.contact
       skip(1)
       "Salesmn"
       "Ship Via"                 at 10
       "FOB"                      at 20
       "Terms"                    at 28
       "Del.Date"                 at 59
       "Over-Under %"             at 69
       sman.sman                  at 3
       carrier.carrier            at 10
       v-fob                      at 20
       terms.dscr                 at 28   format "x(30)"
       xquo.del-date              at 59
       v-over-under               at 69   format "x(12)"
       skip(1)
       v-hdr
       fill("-",80)                       format "X(80)"

    with frame quote page-top width 80 no-labels no-box STREAM-IO.
    
  assign
   v-first-q-no = xquo.q-no
   v-fob        = cust.fob-code.

  view frame quote.

  if (not ch-multi) then do:
    {cec/quote/prtquo10.i 1}

    release est.
    if v-prt-box then
      find first est
          where est.company eq xquo.company
            and est.loc     eq xquo.loc
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

    find first est
        where est.company eq xquo.company
          AND est.est-no  EQ xquo.est-no
        no-lock no-error.
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

    if avail cust then v-over-under =
      trim(string(cust.over-pct,">>9.9<%"))   + "-" +
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

    if first-of(report.key-01) OR s-sep-page then do:
      page.
      v-first-q-no = xquo.q-no.
    end.

    v-last = last-of(report.key-01).

    {cec/quote/prtquo10.i 2}
   end.
  end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
