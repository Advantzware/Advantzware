/* ------------------------------------------- cec/quote/quounipk.p 05/05 YSK */
/* print quotes - Xprint format for Unipak                                               */
/* -------------------------------------------------------------------------- */


{sys/inc/var.i shared}

def shared buffer xquo for quotehd.
def buffer xqitm for quoteitm.
def buffer xqqty for quoteqty.
def buffer xqchg for quotechg.
def buffer b-qi for quoteitm.
def buffer x-qi for quoteitm.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{est/printquo.i}
def var numfit as int no-undo.
def var sold as ch extent 5 FORM "x(30)" no-undo.
def var bill as ch extent 5 FORM "x(30)" no-undo.
def var ship as ch extent 5 FORM "x(30)" no-undo.
def var tot as de no-undo.
def var v-over-under as char no-undo.
def var v-comp-name like company.name extent 4.
def var trim-size like quoteitm.size no-undo.
def var temp-trim-size like quoteitm.size no-undo.
def var cc as int no-undo.
def var v-printline as int initial 0 no-undo.
def var v-first-q-no like quotehd.q-no no-undo.
def var v-line like quoteitm.line no-undo.
def var v-rels as int NO-UNDO.
DEF VAR lv-yes-printed AS LOG NO-UNDO.
def var v-part              like quoteitm.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\unipak.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR style-dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-est-no AS cha NO-UNDO.
DEF VAR lv-two-box AS LOG NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-uom LIKE xqqty.uom NO-UNDO.
DEF VAR lv-chg-amt LIKE quotechg.amt NO-UNDO.
assign
 tmpstore = fill("-",130).

find first company where company.company = cocode no-lock no-error.

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.
FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

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

  ASSIGN
     v-first-q-no = xquo.q-no
     v-line-total = 0
     v-printline = 0.

PUT "<FMS Mincho>". 


if (not ch-multi) then do:
    {cec/quote/quouni1.i}    
    {cec/quote/quounipk.i 1}
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
         
    PUT "<FBook Antiqua><R58><C1><P12><B> TERMS AND CONDITIONS: </B> <P8> " 
      "<R59><C1>  The pricing on this quote is good for 60 days.  All prices have been based on current material costs." 
      "<R60><C1>  In the event of an increase in materials or changes to the above specifications, prices may be adjusted." 
      "<R62><C1>  <B>Unipak <U>appreciates your valuable business</U> and we are committed to"
      "  providing assistance at any time to meet your needs.</B>        <B><P12>Thank You!</B><P9>" .
    v-printline = v-printline + 6.
    IF v-printline < 50 THEN PUT SKIP(60 - v-printline).

    release est.
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

    if s-sep-page OR first-of(report.key-01) THEN
       ASSIGN
        v-first-q-no = xquo.q-no
        v-printline = 0.
    
    IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
       page.
       {cec/quote/quouni1.i}
    END.

    IF s-sep-page OR FIRST-OF(report.key-02) THEN DO:
        {cec/quote/quouni1.i}
    END. /* key-02 */

    ASSIGN
      v-last = last-of(report.key-01)
      v-line-total = 0.
  
    {cec/quote/quounipk.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
 
    IF  (ch-multi and (v-last OR s-sep-page)) or (not ch-multi) then do:
       PUT "<FBook Antiqua><R58><C1><P12><B> TERMS AND CONDITIONS: </B> <P8> " 
           "<R59><C1>  The pricing on this quote is good for 60 days.  All prices have been based on current material costs." 
           "<R60><C1>  In the event of an increase in materials or changes to the above specifications, prices may be adjusted." 
           "<R62><C1>  <B>Unipak <U>appreciates your valuable business</U> and we are committed to"
           "  providing assistance at any time to meet your needs.</B>        <B><P12>Thank You!</B><P9>" .
             
     v-printline = v-printline + 6.
     IF v-printline < 50 THEN PUT SKIP(60 - v-printline).
    END.
   end. /* for each */
  end.  /* else */

/* end ---------------------------------- copr. 2000  advanced software, inc. */
