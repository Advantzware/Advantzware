
/*------------------------------------------------------------------------
    File        : QuoteDetail.p
    Purpose     : QuoteDetail

    Syntax      :

    Description : Return a Dataset of all Quote Inquiry

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{QuoteDetail.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Part-no AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteDetail.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VARIABLE quotedate      AS DATE.
ASSIGN quotedate = CalcQuoteDate().
DEFINE VARIABLE custx          AS CHARACTER.
DEFINE VAR quotehdrowid AS ROWID.

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

find first itemfg where
     itemfg.company EQ prmComp AND
     itemfg.part-no = Part-no
     NO-LOCK NO-ERROR.

if available itemfg then do:
FOR EACH quotehd WHERE
    quotehd.company EQ prmComp AND
    quotehd.cust-no = itemfg.cust-no no-lock:
    ASSIGN quotehdrowid = ROWID(quotehd).
END.

find first quotehd where ROWID(quotehd) =  quotehdrowid    NO-LOCK.
if available quotehd then do:
         create ttQuote .
         assign
          ttQuote.q-no = quotehd.q-no 
          ttQuote.billto = quotehd.billto[1] + "" + quotehd.billto[2] + "" + quotehd.billto[3] + "" + quotehd.billto[4]
          ttQuote.shipto = quotehd.shipto[1] + "" + quotehd.shipto[2] + "" + quotehd.shipto[3] + "" + quotehd.shipto[4].
          
          find first sman where
               sman.company EQ prmComp AND
               sman.sman = quotehd.sman
               no-lock.

          assign ttQuote.sname = sman.sname.
          IF quotehd.quo-date < quotedate  THEN
             assign ttQuote.quo-date =  quotehd.quo-date.
          ELSE 
             assign ttQuote.quo-date =  quotehd.quo-date.
          
        find first est where est.company = quotehd.company 
                           and est.loc     = quotehd.loc
                           and est.est-no = quotehd.est-no no-lock.
          if available est then do:
             assign
                ttQuote.estDate = est.ord-date
                ttQuote.Part-no = itemfg.part-no.
        
             FOR EACH quoteitm WHERE
                 quoteitm.company EQ prmComp AND
                 quoteitm.q-no = quotehd.q-no
                 NO-LOCK
                 BY quoteitm.part-no BY quoteitm.q-no BY quoteitm.qty:

                 FIND FIRST eb WHERE eb.company = prmComp AND
                      eb.est-no = quoteitm.est-no AND
                      eb.part-no = quoteitm.part-no
                      NO-LOCK NO-ERROR.
                 IF AVAILABLE eb THEN
                 DO:
                    find first ef WHERE ef.company = prmComp AND ef.est-no   eq est.est-no and ef.form-no eq eb.form-no no-lock no-error.
                    v-board = if avail ef then
                              ef.board    + " - " +
                              ef.adder[1] + " " + ef.adder[2] + " " + ef.adder[3] + " " +
                              ef.adder[4] + " " + ef.adder[5] + " " + ef.adder[6]
                              else "-".
                    if substr(v-board,length(trim(v-board)),1) eq "-" then
                       substr(v-board,length(trim(v-board)),1) = "".
                    
                    if v-board eq "" then v-board = quoteitm.i-dscr.
                    
                    assign 
                    ttQuote.qty = string(quoteitm.qty)
                    ttQuote.uom = string(quoteitm.uom)
                    ttQuote.price = string(quoteitm.price,">>>,>>>,>>9.99")
                    ttQuote.part_no = quoteitm.part-no
                    ttQuote.style = quoteitm.style
                    ttQuote.colord = quoteitm.i-coldscr
                    ttQuote.size = quoteitm.SIZE
                    ttQuote.material = v-board.
                 END.
             end.
          END.
 end.     
end.
