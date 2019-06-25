
/*------------------------------------------------------------------------
    File        : quote_qtchg.p   Copy of(est/b-qtchg.w)
    Purpose     : Quote qty

    Syntax      :

    Description : Return a Dataset of all Quote Items

    Author(s)   : 
    Created     : Aug 24  2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQuoteQtChgItems NO-UNDO 
    FIELD qtqty-snum    AS INT
    FIELD qtqty-bnum    AS INT       
    FIELD qtqty-bill    AS CHARACTER 
    FIELD qtqty-charge  AS CHARACTER 
    FIELD qtqty-prepqty AS DECIMAL 
    FIELD qtqty-cost    AS DECIMAL 
    FIELD qtqty-mkup    AS DECIMAL 
    FIELD qtqty-amtz    AS DECIMAL 
    FIELD qtqty-amt     AS DECIMAL 
    FIELD qtqty-matf    AS DECIMAL 
    FIELD qtqty-matm    AS DECIMAL 
    FIELD qtqty-labf    AS DECIMAL
    FIELD qtqty-labm    AS DECIMAL
    FIELD qtqty-simon   AS CHAR
    FIELD qtqty-reckey  AS CHAR
    
    FIELD qtqty-quote   AS INT
    FIELD qtqty-partno  AS CHAR
    FIELD qtqty-dscr    AS CHAR
    FIELD qtqty-qty     AS INT

    .

DEFINE DATASET dsQuoteQtChgItems FOR ttQuoteQtChgItems.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmQty       AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmLine      AS INT  NO-UNDO.

DEFINE INPUT PARAMETER prmSnum      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmBnum      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmBill      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCharge    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPrepQty   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCost      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmMkup      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmAmtz      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmAmt       AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmMatF      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmMatM      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmLabF      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmLabM      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmSimon     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteQtChgItems .
DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

IF prmUser       = ? THEN ASSIGN prmUser        = "".
IF prmAction     = ? THEN ASSIGN prmAction      = "".
IF prmQuote      = ? THEN ASSIGN prmQuote       = 0.
IF prmQty        = ? THEN ASSIGN prmQty         = 0.
IF prmLine       = ? THEN ASSIGN prmLine        = 0.
IF prmSnum       = ? THEN ASSIGN prmSnum        = 0.
IF prmBnum       = ? THEN ASSIGN prmBnum        = 0.
IF prmBill       = ? THEN ASSIGN prmBill        = "".
IF prmCharge     = ? THEN ASSIGN prmCharge      = "".
IF prmPrepQty    = ? THEN ASSIGN prmPrepQty     = 0.
IF prmCost       = ? THEN ASSIGN prmCost        = 0.
IF prmMkup       = ? THEN ASSIGN prmMkup        = 0.
IF prmAmtz       = ? THEN ASSIGN prmAmtz        = 0.
IF prmAmt        = ? THEN ASSIGN prmAmt         = 0.
IF prmMatF       = ? THEN ASSIGN prmMatF        = 0.
IF prmMatM       = ? THEN ASSIGN prmMatM        = 0.
IF prmLabF       = ? THEN ASSIGN prmLabF        = 0.
IF prmLabM       = ? THEN ASSIGN prmLabM        = 0.
IF prmSimon      = ? THEN ASSIGN prmSimon       = "".
IF prmReckey     = ? THEN ASSIGN prmReckey      = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
 
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 prmLoc   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN
    cocode = prmComp 
    locode = prmLoc .
 
IF prmAction = "TopInfo" THEN DO:
     FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no 
            AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty   NO-LOCK :
         
          create ttQuoteQtChgItems .
            ASSIGN 
                ttQuoteQtChgItems.qtqty-quote     =  quoteitm.q-no  
                ttQuoteQtChgItems.qtqty-partno    =  quoteitm.part-no
                ttQuoteQtChgItems.qtqty-dscr      =  quoteitm.part-dscr1
                ttQuoteQtChgItems.qtqty-qty       =  quoteqty.qty    .
        END.
    END.

END.



IF prmAction = "Select" THEN DO:   
MESSAGE "prmAction" prmQuote prmAction prmLine.
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no 
            AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FIND FIRST quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty NO-LOCK NO-ERROR.
            FOR EACH quotechg WHERE quotechg.company eq prmComp
                AND quotechg.loc eq quoteqty.loc AND quotechg.q-no eq quoteqty.q-no
                AND ((ASI.quotechg.line eq quoteqty.line AND quotechg.qty eq quoteqty.qty) OR 
                     (ASI.quotechg.LINE eq 0                 AND quotechg.qty eq 0               )) NO-LOCK:

            create ttQuoteQtChgItems .
            ASSIGN 
                ttQuoteQtChgItems.qtqty-snum     =  quotechg.s-num   
                ttQuoteQtChgItems.qtqty-bnum     =  quotechg.b-num
                ttQuoteQtChgItems.qtqty-bill     =  quotechg.bill
                ttQuoteQtChgItems.qtqty-charge   =  quotechg.charge
                ttQuoteQtChgItems.qtqty-prepqty  =  quotechg.prep-qty
                ttQuoteQtChgItems.qtqty-cost     =  quotechg.cost
                ttQuoteQtChgItems.qtqty-mkup     =  quotechg.mkup
                ttQuoteQtChgItems.qtqty-amtz     =  quotechg.amtz
                ttQuoteQtChgItems.qtqty-amt      =  quotechg.amt
                ttQuoteQtChgItems.qtqty-matf     =  quotechg.matf
                ttQuoteQtChgItems.qtqty-matm     =  quotechg.matm
                ttQuoteQtChgItems.qtqty-labf     =  quotechg.labf
                ttQuoteQtChgItems.qtqty-labm     =  quotechg.labm
                ttQuoteQtChgItems.qtqty-simon    =  quotechg.simon
                ttQuoteQtChgItems.qtqty-reckey   =  quotechg.rec_key   .
        END.
    END.    
END.

/*********create item***********************************/


IF prmAction = "Add" THEN DO:
   FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FIND FIRST quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty  NO-LOCK NO-ERROR.
        
        CREATE quotechg .

        ASSIGN
            quotechg.company = quoteqty.company
            quotechg.loc     = quoteqty.loc
            quotechg.q-no    = quoteqty.q-no
            quotechg.line    = quoteqty.line 
            quotechg.qty     = quoteqty.qty
            quotechg.quote-date = quoteqty.quote-date 

            quotechg.s-num   =  prmSnum   
            quotechg.b-num   =  prmBnum   
            quotechg.bill    =  prmBill   
            quotechg.charge  =  prmCharge 
            quotechg.prep-qty = prmPrepQty
            quotechg.cost    =  prmCost   
            quotechg.mkup    =  prmMkup   
            quotechg.amtz    =  prmAmtz   
            quotechg.amt     =  prmAmt    
            quotechg.matf    =  prmMatF   
            quotechg.matm    =  prmMatM   
            quotechg.labf    =  prmLabF   
            quotechg.labm    =  prmLabM   
            quotechg.simon   =  prmSimon   .
            


        ASSIGN
            prmReckey = quotechg.rec_key 
            prmAction = "View" .

    END.
END.

/******************update****************************************/

IF prmAction = "Update" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
      FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no 
            AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FIND FIRST quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty NO-LOCK NO-ERROR.
            FOR EACH quotechg WHERE quotechg.company eq prmComp
                AND quotechg.loc eq quoteqty.loc AND quotechg.q-no eq quoteqty.q-no
                AND quotechg.rec_key = prmReckey EXCLUSIVE-LOCK:

            ASSIGN
                quotechg.s-num   =  prmSnum   
                quotechg.b-num   =  prmBnum   
                quotechg.bill    =  prmBill   
                quotechg.charge  =  prmCharge 
                quotechg.prep-qty = prmPrepQty
                quotechg.cost    =  prmCost   
                quotechg.mkup    =  prmMkup   
                quotechg.amtz    =  prmAmtz   
                quotechg.amt     =  prmAmt    
                quotechg.matf    =  prmMatF   
                quotechg.matm    =  prmMatM   
                quotechg.labf    =  prmLabF   
                quotechg.labm    =  prmLabM   
                quotechg.simon   =  prmSimon   .
        END.
        
    END.

    ASSIGN
        prmAction = "View" .

END.
/******************* delete*********************/
IF prmAction = "Delete" THEN DO:
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
     FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no 
            AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FIND FIRST quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty NO-LOCK NO-ERROR.
            FOR EACH quotechg WHERE quotechg.company eq prmComp
                AND quotechg.loc eq quoteqty.loc AND quotechg.q-no eq quoteqty.q-no
                AND quotechg.rec_key = prmReckey exclusive-LOCK:
            DELETE quotechg .
        END.
    END.

END.   /* end of delete*/  


IF prmAction = "View" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
       FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no 
            AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FIND FIRST quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND quoteqty.qty = prmQty NO-LOCK NO-ERROR.
            FOR EACH quotechg WHERE quotechg.company eq prmComp
                AND quotechg.loc eq quoteqty.loc AND quotechg.q-no eq quoteqty.q-no
                AND quotechg.rec_key = prmReckey NO-LOCK:
            create ttQuoteQtChgItems .
            ASSIGN 
                ttQuoteQtChgItems.qtqty-snum     =  quotechg.s-num   
                ttQuoteQtChgItems.qtqty-bnum     =  quotechg.b-num
                ttQuoteQtChgItems.qtqty-bill     =  quotechg.bill
                ttQuoteQtChgItems.qtqty-charge   =  quotechg.charge
                ttQuoteQtChgItems.qtqty-prepqty  =  quotechg.prep-qty
                ttQuoteQtChgItems.qtqty-cost     =  quotechg.cost
                ttQuoteQtChgItems.qtqty-mkup     =  quotechg.mkup
                ttQuoteQtChgItems.qtqty-amtz     =  quotechg.amtz
                ttQuoteQtChgItems.qtqty-amt      =  quotechg.amt
                ttQuoteQtChgItems.qtqty-matf     =  quotechg.matf
                ttQuoteQtChgItems.qtqty-matm     =  quotechg.matm
                ttQuoteQtChgItems.qtqty-labf     =  quotechg.labf
                ttQuoteQtChgItems.qtqty-labm     =  quotechg.labm
                ttQuoteQtChgItems.qtqty-simon    =  quotechg.simon
                ttQuoteQtChgItems.qtqty-reckey   =  quotechg.rec_key   .
        END.
    END.    
END.

