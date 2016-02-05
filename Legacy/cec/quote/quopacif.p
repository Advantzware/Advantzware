/* ------------------------------------------- cec/quote/quopacif.p 10/02 YSK */
/* print quotes in Pacific - Xprint format                                               */
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
DEF VAR idummy AS INT NO-UNDO.
def var v-part              like quoteitm.part-no                        no-undo.
def var v-board             as   char                                   no-undo.
def var v-last as log initial no no-undo.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR lv-part-dscr2 AS cha NO-UNDO.
DEF VAR lv-i-coldscr AS cha NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>>9.9<<<<" NO-UNDO.
DEF TEMP-TABLE tt-qty FIELD tt-recid AS RECID
                      FIELD qty AS INT
                      INDEX qty IS PRIMARY qty DESCENDING.                      

/* no image for pacific
ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
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
DEF VAR v-prep-printed AS LOG NO-UNDO.
DEF VAR v-prep-prt-list AS CHAR NO-UNDO.
DEF VAR lv-fg# AS cha NO-UNDO.

{cecrep/jobtick2.i "new shared"}

ASSIGN tmpstore = fill("-",130).

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

  v-first-q-no = xquo.q-no.
  v-line-total = 0.
  v-printline = 0.

PUT "<Farial>". 

if (not ch-multi) then do:
   {cec/quote/quopaci2.i}
   {cec/quote/quopacif.i 1}
   v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].


       PUT "<FArial><R53><C25><P9> We hope the above proposal meets your satisfaction."
           "<R54><C23> If you have any questions or require additional information,"
           "<R55><C24> Please do not hesitate to call your sales representative."
           "<R56><C27> We thank you for your continued support."
           "<R58><C15> Visit our website for a complete listing of our products or email us at info@actionbox.ca"
           "<R60><C15><P12><B> Terms </B> <P9> " 
           "<R62><C17> All custom orders are subject to an over / under of 10%. Net 30, on approved credit:"
           "<R63><C20> Prices do not include taxes. Prices subject to change without notice.".

        v-printline = v-printline + 11.
        IF v-printline < 50 THEN PUT SKIP(60 - v-printline).

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
       {cec/quote/quopaci2.i}
 /*
        /* PUT "<C+25><#1><R+5><C+25><IMAGE#1=" ls-full-img1 SKIP. /* pacific package */ */
     PUT "<C+25><#1>".
     PUT "<=1>" SKIP. 
     /*PUT "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */

     PUT "<C1><#2>"
         "<P10><=2><R+9>"
          v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP(1)
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
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact 
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .
    
   /* same as request - no space between hd & line
   PUT "<R11><#4><R17><C80><RECT>" SKIP
   "<R13><C1><FROM><R13><C80><LINE>" SKIP
   "<R15><C1><FROM><R15><C80><LINE>" SKIP

   "<R11><C20><FROM><R15><C20><LINE>" SKIP
   "<R11><C30><FROM><R15><C30><LINE>" SKIP
   "<R11><C40><FROM><R15><C40><LINE>" SKIP
   "<R11><C50><FROM><R15><C55><LINE>" SKIP
   "<R11><C60><FROM><R15><C60><LINE>" SKIP
   "<R11><C70><FROM><R15><C70><LINE>" SKIP
   .
   */  

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
   */
  END. /* key-03 */
    v-last = last-of(report.key-01).
    v-line-total = 0.
  
    {cec/quote/quopacif.i 2}  
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
/*  no total in quote
        PUT "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
         "<=8><R+1> Sub Total  :" v-line-total FORM "->>,>>9.99"
         "<=8><R+2> " v-bot-lab[1] 
         "<=8><R+3> " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
                     v-bot-lab[2]
         "<=8><R+4> Grand Total:" v-quo-total FORM "->>,>>9.99" .
*/
 IF LINE-COUNTER > PAGE-SIZE - 10 THEN DO:
    page.
     PUT "<C+25><#1>".
     PUT "<=1>" SKIP. 
     /*PUT "<C1><#2><R+10><C+35><IMAGE#2=" ls-full-img2 SKIP  /* company image */ */

     PUT "<C1><#2>"
         "<P10><=2><R+9>"
          v-comp-add1 AT 8 SKIP
          v-comp-add2 AT 8 SKIP
          v-comp-add3 AT 8 SKIP
          v-comp-add4 AT 8 SKIP(1)
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
   "<=3><R+1> " xquo.cust-no  space(6) xquo.contact 
   "<=3><R+3> " cust.area-code + cust.phone format "(999)999-9999"  space(5) cust.fax
   .
    
   /* same as request - no space between hd & line
   PUT "<R11><#4><R17><C80><RECT>" SKIP
   "<R13><C1><FROM><R13><C80><LINE>" SKIP
   "<R15><C1><FROM><R15><C80><LINE>" SKIP

   "<R11><C20><FROM><R15><C20><LINE>" SKIP
   "<R11><C30><FROM><R15><C30><LINE>" SKIP
   "<R11><C40><FROM><R15><C40><LINE>" SKIP
   "<R11><C50><FROM><R15><C55><LINE>" SKIP
   "<R11><C60><FROM><R15><C60><LINE>" SKIP
   "<R11><C70><FROM><R15><C70><LINE>" SKIP
   .
   */  

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
 END.

 IF  (ch-multi and (v-last OR s-sep-page)) or (not ch-multi) then do:
     /*
          PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " SKIP
             "  All item quantities are subject to an over / under of 10%. If you have any further questions or concerns" SKIP
             "  please do not hesitate to call 604.533.2545 or email us at sales@packaging.bc.ca." SKIP
             "  Check out our website at  www.pacificpackaging.ca" SKIP.
     */
     PUT "<FArial><R53><C25><P9> We hope the above proposal meets your satisfaction."
           "<R54><C23> If you have any questions or require additional information,"
           "<R55><C24> Please do not hesitate to call your sales representative."
           "<R56><C27> We thank you for your continued support."
           "<R58><C15> Visit our website for a complete listing of our products or email us at info@actionbox.ca"
           "<R60><C15><P12><B> Terms </B> <P9> " 
           "<R62><C17> All custom orders are subject to an over / under of 10%. Net 30, on approved credit:"
           "<R63><C20> Prices do not include taxes. Prices subject to change without notice.".

     v-printline = v-printline + 11.
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
   end.
  end.

PROCEDURE printHeader:
  DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInitVar AS INTEGER NO-UNDO.

  IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN DO:
    PAGE.
    {cec/quote/quopacif.i}
    opInitVar = 0.
  END.
END PROCEDURE.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
