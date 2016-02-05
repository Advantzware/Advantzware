/* ------------------------------------------- cec/quote/quopacif.p 10/02 YSK */
/* print quotes in ABOX - Xprint format                                               */
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
DEF VAR v-lines AS INT NO-UNDO.
def var v-rels as int.
DEF VAR lv-ext-price AS DEC NO-UNDO.

def var v-part              like quoteitm.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
DEF VAR lv-out-cost AS DEC NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.

ASSIGN ls-image1 = "images\aboxlogo.jpg"
       ls-image2 = "images\aboxlogo.jpg".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
/*FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".
*/

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
DEF VAR lv-chg-amt LIKE quotechg.amt NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-uom LIKE xqqty.uom NO-UNDO.
DEF VAR lv-two-box AS LOG NO-UNDO.
def NEW shared var v-out1-id       as   recid          no-undo.
def NEW shared var v-out2-id       as   recid          no-undo.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
{cecrep/jobtick2.i "new shared"}

assign
 tmpstore = fill("-",130)
 .

find first company where company.company = cocode no-lock no-error.
/*ASSIGN v-comp-add1 = company.addr[1]
           v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
           v-comp-add3 = "Phone: 604.533.2545" 
           v-comp-add4 = "Fax  : 604.533.2633".
*/
FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.
FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAIL xquo THEN RETURN.

/*
format xquo.est-no      to 5
       xqitm.part-no    to 31   format "x(25)"
       trim-size        to 55   format "x(23)"
       xqqty.qty        to 63   format ">>>>>>9"
       xqqty.rels       to 67   format ">>9"
       xqqty.price      to 77   format ">>,>>9.99"
       xqqty.uom       to 80

      header "Est # Description               Siz/Styl/Brd/Co" +
             "lors         QTY Rel     PriceUOM" format "x(80)"
      with frame item-10p no-box no-labels down width 80 STREAM-IO.
*/
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

  v-first-q-no = xquo.q-no.
 
  v-line-total = 0.
  v-printline = 0.
  PUT "<Farial>". 


if (not ch-multi) then do:
    {cec/quote/quoabox2.i}
    {cec/quote/quoabox.i 1}
    PUT "<FArial><R52><C1><#11><P12><B> </B> <P9> " 
        "<=11><R+1>  All item quantities are subject to an over / under of 10%. If you have any further questions or concerns" 
        "<=11><R+2>  please do not hesitate to call 800-366-0488 or email us at abox@foldingcarton.com." 
        "<=11><R+3>  Visit our website at  www.foldingcarton.com." 
        "<=11><R+10><P8><C20>706 Rand Rd.    P.O.Box 203     Kaufman, Texas 75142      800.366.0488    Fax:972.9324612"
        "<=11><R+11><C17>3816 Binz Engleman Rd., Suite B-105     San Antonio, Texas 78219    210.922.6900     Fax:210.922.9202"
        .

    release est.
    if v-prt-box THEN  DO:
      find first est where est.company eq xquo.company
            and est.loc     eq xquo.loc
            and est.est-no  eq xquo.est-no
          no-lock no-error.
      if avail est then do:
        /*put skip(2).
        run cec/desprnt2.p (input recid(est)). */
        PUT "<FCourier New>" SKIP.
        FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
        run cec/desprnt2.p (?,
                           input-output v-lines,
                           recid(xest)).
      end.
    END.
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

    if s-sep-page OR first-of(report.key-01) then do:
    /*  page.  */
      v-first-q-no = xquo.q-no.
      v-printline = 0.
    end.
    IF s-sep-page OR FIRST-OF(report.key-02) THEN DO:
       {cec/quote/quoabox2.i}
    END. /* key-02 */
    v-last = last-of(report.key-01).
    v-line-total = 0.
  
    {cec/quote/quoabox.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

    IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
       page.
       {cec/quote/quoabox2.i}
    END.

   IF  (ch-multi and (v-last OR s-sep-page)) or (not ch-multi) then do:
       PUT "<FArial><R52><C1><#11><P12><B> </B> <P9> " 
        "<=11><R+1>  All item quantities are subject to an over / under of 10%. If you have any further questions or concerns" 
        "<=11><R+2>  please do not hesitate to call 800-366-0488 or email us at abox@foldingcarton.com." 
        "<=11><R+3>  Visit our website at  www.foldingcarton.com." 
        "<=11><R+10><P8><C20>706 Rand Rd.    P.O.Box 203     Kaufman, Texas 75142      800.366.0488    Fax:972.9324612"
        "<=11><R+11><C17>3816 Binz Engleman Rd., Suite B-105     San Antonio, Texas 78219    210.922.6900     Fax:210.922.9202"
        .
     v-printline = v-printline + 6.
     IF v-printline < 50 THEN PUT SKIP(60 - v-printline).
 END.
  if v-prt-box THEN DO:
     find first est
          where est.company eq xquo.company
            and est.loc     eq xquo.loc
            and est.est-no  eq xquo.est-no
          no-lock no-error.
     if avail est then do:
       /*put skip(2).
        run cec/desprnt2.p (input recid(est)). */
        PUT "<FCourial New><P9>" SKIP.
        FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
         run cec/desprnt2.p (?,
                           input-output v-lines,
                           recid(xest)).
     end.
  END.
 end. /* for each report */
end. /* else */

/* end ---------------------------------- copr. 2000  advanced software, inc. */
