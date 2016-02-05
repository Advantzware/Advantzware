/* ------------------------------------------- cec/quote/quoprfrm.p 11/05 YSK */
/* print quotes in Xprint Marketing(APC) format                                               */
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
def var v-rels as int.

def var v-part              like quoteitm.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
DEF VAR lv-fg# AS cha NO-UNDO.
DEF VAR li-cline AS INT NO-UNDO.

DEF VAR v-style AS CHAR NO-UNDO.
DEF VAR col-dscr AS CHAR NO-UNDO.
DEF VAR brd-dscr AS CHAR NO-UNDO.
DEF VAR brd-no AS CHAR NO-UNDO.
DEF VAR i-dscr2 AS CHAR FORMAT "x(30)" NO-UNDO.

ASSIGN ls-image1 = "images\Performance logo.jpg"
       /*ls-image2 = "images\pacific2.bmp"*/.

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
DEF VAR lv-two-box AS LOG NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-uom LIKE xqqty.uom NO-UNDO.
DEF VAR lv-chg-amt LIKE quotechg.amt NO-UNDO.
DEF VAR v-prep-prt-list AS CHAR NO-UNDO.
DEF VAR v-prep-printed AS LOG NO-UNDO.
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

     PUT "<C3><R2><#1><R+13><C+55><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
         "<FCourier New>"
         "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
   
     v-printline = v-printline + 15.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
         "<R4><C62><FROM><R6><C62><LINE>" SKIP
         "<R6><C65><FROM><R8><C65><LINE>" SKIP
         .
    
   PUT "<FArial><P12><=#3><R-2> <B>Quotation#: " v-first-q-no "</B><P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/ FORM "x(20)"
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

 

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C38><FROM><R27><C38><LINE>" SKIP
   "<R23><C52><FROM><R27><C52><LINE>" SKIP
   "<R23><C70><FROM><R27><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " xquo.quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C6><FROM><R30><C6><LINE>" SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP
             "<R28><C45><FROM><R30><C45><LINE>" SKIP
             "<R28><C55><FROM><R30><C55><LINE>" SKIP
             "<R28><C62><FROM><R30><C62><LINE>" SKIP
             "<R28><C72><FROM><R30><C72><LINE>" SKIP
             .
   PUT "<FArial><=5><R+1> Est#      Description                    Item/Style/Color/Board                                     Quantity    Release                 Price         UOM " SKIP(1).
  PUT "<FCourier New>".
    {cec/quote/quoprfrm.i 1}
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
/*  no total in quote
        PUT "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
            "<=8><R+1> Sub Total  :" v-line-total FORM "->>,>>9.99"
            "<=8><R+2> " v-bot-lab[1] 
            "<=8><R+3> " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                        v-bot-lab[2]
            "<=8><R+4> Grand Total:" v-quo-total FORM "->>,>>9.99" .
*/
        
        PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " SKIP.
        li-cline = 1.
        do i = 1 to 5:
           if xquo.comment[i] ne "" THEN DO: 
              put "<C1><R" string(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
              li-cline = li-cline + 1.
           END.
        END.
        v-printline = v-printline + 6.
        IF v-printline < 50 THEN PUT SKIP(60 - v-printline).

    release est.
  /* not done yet
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
    */
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

    if first-of(report.key-01) OR s-sep-page then do:
    /*  page.  */
      v-first-q-no = xquo.q-no.
      v-printline = 0.
    end.
    
    IF FIRST-OF(report.key-01) OR s-sep-page THEN DO:
    PUT "<C3><R2><#1><R+13><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
        "<FCourier New><P12>"
        "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
        SPACE(5) bill[1]  shipto[1] AT 55 skip
        SPACE(5) bill[2]  shipto[2] AT 55 SKIP
        SPACE(5) bill[3]  shipto[3] AT 55 SKIP
        SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
   
     v-printline = v-printline + 15.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
         "<R4><C62><FROM><R6><C62><LINE>" SKIP
         "<R6><C65><FROM><R8><C65><LINE>" SKIP
         .
    
   PUT "<FArial><P12><=#3><P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

 

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C38><FROM><R27><C38><LINE>" SKIP
   "<R23><C52><FROM><R27><C52><LINE>" SKIP
   "<R23><C70><FROM><R27><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " xquo.quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C6.2><FROM><R30><C6.2><LINE>" SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP
             "<R28><C45><FROM><R30><C45><LINE>" SKIP
             "<R28><C55><FROM><R30><C55><LINE>" SKIP
             "<R28><C62><FROM><R30><C62><LINE>" SKIP
             "<R28><C72><FROM><R30><C72><LINE>" SKIP 
             .
   PUT "<FArial><=5><R28.1> Est# " SKIP(1).
   PUT "<FArial><=5><R+1> Quote#  Description                   Item/Style/Color/Board                                     Quantity    Release                 Price         UOM " SKIP(1).
  PUT "<FCourier New>".


  END. /* key-02 */
    v-last = last-of(report.key-01).
    /*v-line-total = 0.*/
    {cec/quote/quoprfrm.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

 IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
    page.
      PUT "<C3><R2><#1><R+13><C+25><IMAGE#1=" ls-full-img1 SKIP /* pacific package */ 
         "<FCourier New><P12>"
         "Bill To:"  space(40) "Ship To:"  shipto[5] SKIP
         SPACE(5) bill[1]  shipto[1] AT 55 skip
         SPACE(5) bill[2]  shipto[2] AT 55 SKIP
         SPACE(5) bill[3]  shipto[3] AT 55 SKIP
         SPACE(5) bill[4]  shipto[4] AT 55 SKIP.
   
     v-printline = v-printline + 15.
     PUT "<|10><R4><C50><#3><FROM><R8><C80><RECT>" SKIP.
     PUT "<R6><C50><FROM><R6><C80><LINE>" SKIP      
         "<R4><C62><FROM><R6><C62><LINE>" SKIP
         "<R6><C65><FROM><R8><C65><LINE>" SKIP
         .
    
   PUT "<FArial><P12><=#3><R-2> <B>Quotation#: " v-first-q-no "</B><P10>" SKIP
   "<=#3> Customer ID             Contact"
   "<=#3><R+2> Telephone                       Fax <FCourier New>" 
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact /*cust.contact*/
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .

 

   PUT "<|10><R23><C1><#4><FROM><R27><C80><RECT>" SKIP
   "<R25><C1><FROM><R25><C80><LINE>" SKIP    
   "<R23><C11><FROM><R27><C11><LINE>" SKIP
   "<R23><C22><FROM><R27><C22><LINE>" SKIP
   "<R23><C38><FROM><R27><C38><LINE>" SKIP
   "<R23><C52><FROM><R27><C52><LINE>" SKIP
   "<R23><C70><FROM><R27><C70><LINE>" SKIP
   .
   PUT "<FArial><=4><R+1> Quote Date         FOB                     Ship Via                                  Terms                      Sales Person                       Over-Under %" SKIP
   "<FCourier New><=4><R+3> " xquo.quo-date FORM "99/99/9999" space(2)
   cust.fob-code FORM "x(11)" SPACE(2)
   carrier.dscr FORM "x(20)" SPACE(1)
   terms.dscr FORM "x(15)" space(1) sman.sname space(2) v-over-under SKIP.


   PUT "<|10><R28><C1><#5><FROM><R30><C80><RECT>" SKIP    
             "<R28><C6><FROM><R30><C6><LINE>" SKIP
             "<R28><C20><FROM><R30><C20><LINE>" SKIP
             "<R28><C45><FROM><R30><C45><LINE>" SKIP
             "<R28><C55><FROM><R30><C55><LINE>" SKIP
             "<R28><C62><FROM><R30><C62><LINE>" SKIP
             "<R28><C72><FROM><R30><C72><LINE>" SKIP
             .
   PUT "<FArial><=5><R28.1> Est# " SKIP(1).
   PUT "<FArial><=5><R+1> Quote#  Description                   Item/Style/Color/Board                                     Quantity    Release                 Price         UOM " SKIP(1).
  PUT "<FCourier New>".
 END.

 IF  (ch-multi and (v-last OR s-sep-page)) OR
     (not ch-multi) then do:
          PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " SKIP.
          li-cline = 1.
          do i = 1 to 5:
             if xquo.comment[i] ne "" THEN DO: 
                put "<C1><R" string(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
                li-cline = li-cline + 1.
             END.
          END.

     v-printline = v-printline + 6.
     IF v-printline < 50 THEN PUT SKIP(60 - v-printline).
 END.
   end.
  end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
