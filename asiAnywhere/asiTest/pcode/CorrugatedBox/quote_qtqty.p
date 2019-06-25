
/*------------------------------------------------------------------------
    File        : quote_qtqty.p   Copy of(est/b-qtqty.w)
    Purpose     : Quote qty

    Syntax      :

    Description : Return a Dataset of all Quote Items

    Author(s)   : Sunil
    Created     : Mar 24 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQuoteQtQtyItems NO-UNDO 
    FIELD qtqty-qty       AS INT
    FIELD qtqty-price     AS DECIMAL       
    FIELD qtqty-uom       AS CHARACTER 
    FIELD qtqty-profit    AS DECIMAL 
    FIELD qtqty-rels      AS INT 
    FIELD qtqty-matcost   AS DECIMAL 
    FIELD qtqty-labcost   AS DECIMAL 
    FIELD qtqty-focost    AS DECIMAL 
    FIELD qtqty-vocost    AS DECIMAL 
    FIELD qtqty-msf       AS DECIMAL 
    FIELD qtqty-reckey    AS CHAR 
    FIELD qtqty-date      AS DATETIME
    FIELD qtqty-user      AS CHARACTER
    FIELD qtqty-line      AS INT
    FIELD qtqty-quote      AS INT
    FIELD qtqty-partno      AS CHAR
    FIELD qtqty-dscr      AS CHAR
    FIELD qtqty-style      AS CHAR

    .

DEFINE DATASET dsQuoteQtQtyItems FOR ttQuoteQtQtyItems.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmQty       AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmPrice     AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmUom       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmProfit    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmRels      AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmMatCost   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmLabCost   AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmFoCost    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmVoCost    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmTotLab    AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmDate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLine      AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmRePrice   AS CHAR NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteQtQtyItems .
DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

IF prmUser       = ? THEN ASSIGN prmUser       = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmQuote      = ? THEN ASSIGN prmQuote      = 0.
IF prmPrice      = ? THEN ASSIGN prmPrice      = 0.
IF prmUom        = ? THEN ASSIGN prmUom        = "".
IF prmProfit     = ? THEN ASSIGN prmProfit     = 0.
IF prmRels       = ? THEN ASSIGN prmRels       = 0.
IF prmMatCost    = ? THEN ASSIGN prmMatCost    = 0.
IF prmLabCost    = ? THEN ASSIGN prmLabCost    = 0.
IF prmFoCost     = ? THEN ASSIGN prmFoCost     = 0.
IF prmVoCost     = ? THEN ASSIGN prmVoCost     = 0.
IF prmTotLab     = ? THEN ASSIGN prmTotLab     = 0.
IF prmDate       = ? THEN ASSIGN prmDate       = "".
IF prmLine       = ? THEN ASSIGN prmLine       = 0.


DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
  DEF VAR lv-uom LIKE uom.uom NO-UNDO.
  DEF VAR uom-list AS CHAR INIT "M,C,EA,CS" NO-UNDO.
  DEF NEW SHARED VAR g_company AS CHAR NO-UNDO .
  DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO .

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
    locode = prmLoc
    g_company = prmComp 
    g_loc   = prmLoc .
 

FUNCTION tot-msf RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

IF AVAIL quoteqty THEN RETURN round(quoteqty.tot-lbs / 1000,2).
ELSE RETURN 0.00.   /* Function return value. */

END FUNCTION.

IF prmAction = "TopInfo" THEN DO:
     FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
          create ttQuoteQtQtyItems .
            ASSIGN 
                ttQuoteQtQtyItems.qtqty-quote     =  quoteitm.q-no  
                ttQuoteQtQtyItems.qtqty-partno    =  quoteitm.part-no
                ttQuoteQtQtyItems.qtqty-dscr      =  quoteitm.part-dscr1
                ttQuoteQtQtyItems.qtqty-style     =  quoteitm.style   .
    END.

END.



IF prmAction = "Select" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.line NO-LOCK :
            create ttQuoteQtQtyItems .
            ASSIGN 
                ttQuoteQtQtyItems.qtqty-qty       =  quoteqty.qty     
                ttQuoteQtQtyItems.qtqty-price     =  quoteqty.price
                ttQuoteQtQtyItems.qtqty-uom       =  quoteqty.uom
                ttQuoteQtQtyItems.qtqty-profit    =  quoteqty.profit
                ttQuoteQtQtyItems.qtqty-rels      =  quoteqty.rels
                ttQuoteQtQtyItems.qtqty-matcost   =  quoteqty.mat-cost
                ttQuoteQtQtyItems.qtqty-labcost   =  quoteqty.lab-cost
                ttQuoteQtQtyItems.qtqty-focost    =  quoteqty.fo-cost
                ttQuoteQtQtyItems.qtqty-vocost    =  quoteqty.vo-cost
                ttQuoteQtQtyItems.qtqty-msf       =  tot-msf()
                ttQuoteQtQtyItems.qtqty-reckey    =  string(rowid(quoteqty))
                ttQuoteQtQtyItems.qtqty-date      =  quoteqty.quote-date
                ttQuoteQtQtyItems.qtqty-user      =  quoteqty.quote-user
                ttQuoteQtQtyItems.qtqty-line      =  quoteqty.LINE  .
            
        END.
    END.    
END.

/*********create item***********************************/

IF prmAction =  "AddValdate" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
     
   RUN sys/ref/uom-fg.p (NO ,OUTPUT uom-list).
    ASSIGN
     prmUom  = CAPS(prmUom )
     lv-uom = prmUom .

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      cError =  "UOM must be " + TRIM(uom-list) .
      RETURN .
    END.
 

END.

IF prmAction = "Add" THEN DO:
    
   FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
        
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        CREATE quoteqty .

        assign
            quoteqty.company = quoteitm.company
            quoteqty.loc = quoteitm.loc
            quoteqty.q-no = quoteitm.q-no
            quoteqty.line = quoteitm.line
            quoteqty.quote-date = TODAY
            quoteqty.quote-user = prmUser  
            quoteqty.qty        = prmQty
            quoteqty.price      = prmPrice
            quoteqty.uom        = prmUom
            quoteqty.profit     = prmProfit
            quoteqty.rels       = prmRels
            quoteqty.mat-cost   = prmMatCost
            quoteqty.lab-cost   = prmLabCost
            quoteqty.fo-cost    = prmFoCost
            quoteqty.vo-cost    = prmVoCost
            quoteqty.quote-date = DATETIME(prmDate)    .
          

        ASSIGN
            prmLine = quoteitm.line
            prmAction = "View" .

    END.
END.

/******************update****************************************/

IF prmAction =  "UpdateValdate" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
     

    RUN sys/ref/uom-fg.p (NO ,OUTPUT uom-list).
    ASSIGN
     prmUom  = CAPS(prmUom )
     lv-uom = prmUom .

    IF LOOKUP(lv-uom,uom-list) LE 0 THEN DO:
      cError =  "UOM must be " + TRIM(uom-list) .
      RETURN .
    END.
 

END.

IF prmAction = "Update" THEN DO:
     
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
      FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND ROWID(quoteqty) = to-rowid(prmReckey) exclusive-LOCK :

            ASSIGN
                quoteqty.qty        = prmQty
                quoteqty.price      = prmPrice
                quoteqty.uom        = prmUom
                quoteqty.profit     = prmProfit
                quoteqty.rels       = prmRels
                quoteqty.mat-cost   = prmMatCost
                quoteqty.lab-cost   = prmLabCost
                quoteqty.fo-cost    = prmFoCost
                quoteqty.vo-cost    = prmVoCost
                quoteqty.quote-date = DATETIME(prmDate)
                quoteqty.quote-user = prmUser     .

        END.
        /*IF NOT AVAILABLE quotehd OR quotehd.rfq EQ '' THEN RETURN.
            {custom/rfq-qty.i}*/
    END.

    ASSIGN
        prmAction = "View" .

END.
/******************* delete*********************/
IF prmAction = "Delete" THEN DO:
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
      FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND rowid(quoteqty) = to-rowid(prmReckey) exclusive-LOCK :
            DELETE quoteqty .
        END.
    END.

END.   /* end of delete*/

/**************reprice************************/

IF prmAction = "RePrice" THEN DO:

  DEF VAR lv-factor AS DEC NO-UNDO.
  DEF VAR ls-reprice-to AS cha NO-UNDO.
  DEF VAR lv-cas-cnt LIKE eb.cas-cnt NO-UNDO.
  DEF BUFFER bf-qty FOR quoteqty.
       
       FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
        IF AVAIL quotehd THEN DO:
        FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
    /*RUN est/g-qtrprc.w (OUTPUT ls-reprice-to).*/
        ASSIGN
           ls-reprice-to = prmRePrice .
  
    FOR EACH quotehd OF quoteitm NO-LOCK ,
        EACH bf-qty
        WHERE bf-qty.company EQ quoteitm.company 
          AND bf-qty.loc     EQ quoteitm.loc
          AND bf-qty.q-no    EQ quoteitm.q-no
          AND bf-qty.line    EQ quoteitm.line
          AND bf-qty.uom     NE ls-reprice-to :

      FIND FIRST eb
          WHERE eb.company EQ quotehd.company
            AND eb.est-no  EQ quotehd.est-no
            AND eb.part-no EQ quoteitm.part-no
          NO-LOCK NO-ERROR.
     
     /* FIND CURRENT  bf-qty EXCLUSIVE-LOCK NO-ERROR.*/
      ASSIGN 
       lv-cas-cnt   = IF AVAIL eb AND eb.cas-cnt NE 0 THEN eb.cas-cnt ELSE 1
       bf-qty.price = IF bf-qty.uom EQ "CS" THEN
                        (bf-qty.price * bf-qty.qty / lv-cas-cnt)
                      ELSE
                      IF bf-qty.uom EQ "EA" THEN
                        (bf-qty.price * bf-qty.qty)
                      ELSE
                      IF bf-qty.uom EQ "MSF" THEN
                        (bf-qty.price * bf-qty.tot-lbs / 1000)
                      ELSE
                      IF bf-qty.uom EQ "M" THEN
                        (bf-qty.price * bf-qty.qty / 1000)
                      ELSE /*L*/
                         0

       bf-qty.uom   = ls-reprice-to 
       bf-qty.price = IF bf-qty.uom EQ "CS" THEN
                        (bf-qty.price / (bf-qty.qty / lv-cas-cnt))
                      ELSE
                      IF bf-qty.uom EQ "EA" THEN
                        (bf-qty.price / bf-qty.qty)
                      ELSE
                      IF bf-qty.uom EQ "MSF" THEN
                        (bf-qty.price / (bf-qty.tot-lbs / 1000))
                      ELSE
                      IF bf-qty.uom EQ "M" THEN
                        (bf-qty.price / (bf-qty.qty / 1000))
                      ELSE /*L*/
                         0.

      IF bf-qty.qty EQ quoteitm.qty THEN DO:
        FIND CURRENT quoteitm EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL quoteitm THEN
          ASSIGN
           quoteitm.uom   = bf-qty.uom
           quoteitm.price = bf-qty.price.
        FIND CURRENT quoteitm NO-LOCK NO-ERROR.
      END.
      
    END. /* each quotehd */
    
  /*  /* connect to rfq database - start */
    FIND FIRST asi.module NO-LOCK WHERE module.module EQ 'rfq' NO-ERROR.
    IF AVAILABLE module AND module.is-used THEN DO:
      IF module.expire-date EQ ? OR module.expire-date GE TODAY THEN DO:
        IF NOT CONNECTED('rfq') AND SEARCH('addon\rfq.pf') NE ? THEN
        CONNECT -pf VALUE(SEARCH('addon\rfq.pf')) NO-ERROR.
      END. /* expire-date */
    END. /* avail module */
    /* connect to rfq database - end */
    
    IF CONNECTED('rfq') THEN
    FOR EACH quotehd OF quoteitm NO-LOCK,
        EACH bf-qty NO-LOCK
        WHERE bf-qty.company EQ quoteitm.company 
          AND bf-qty.loc     EQ quoteitm.loc
          AND bf-qty.q-no    EQ quoteitm.q-no
          AND bf-qty.line    EQ quoteitm.line:
      IF quotehd.rfq NE '' THEN
      RUN custom/rfq-qty.p (quotehd.company,quotehd.loc,quotehd.est-no,
                            quotehd.rfq,quoteitm.part-no,bf-qty.qty,
                            bf-qty.price,bf-qty.uom,TODAY,bf-qty.rels).
    END. /* each quotehd */
    
    /* disconnect to rfq database */
    IF CONNECTED('rfq') THEN DISCONNECT rfq.*/

 END.
END.


IF prmAction = "View" THEN DO:   

    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    IF AVAIL quotehd THEN DO:
      FIND FIRST quoteitm WHERE quoteitm.company EQ prmComp AND quoteitm.q-no = quotehd.q-no AND quoteitm.LINE = prmLine NO-LOCK NO-ERROR.
        FOR EACH quoteqty WHERE quoteqty.company = quoteitm.company
            AND quoteqty.loc = quoteitm.loc AND quoteqty.q-no = quoteitm.q-no 
            AND quoteqty.line = quoteitm.LINE AND rowid(quoteqty) = TO-ROWID(prmReckey) NO-LOCK :
            create ttQuoteQtQtyItems .
            ASSIGN 
                ttQuoteQtQtyItems.qtqty-qty       =  quoteqty.qty     
                ttQuoteQtQtyItems.qtqty-price     =  quoteqty.price
                ttQuoteQtQtyItems.qtqty-uom       =  quoteqty.uom
                ttQuoteQtQtyItems.qtqty-profit    =  quoteqty.profit
                ttQuoteQtQtyItems.qtqty-rels      =  quoteqty.rels
                ttQuoteQtQtyItems.qtqty-matcost   =  quoteqty.mat-cost
                ttQuoteQtQtyItems.qtqty-labcost   =  quoteqty.lab-cost
                ttQuoteQtQtyItems.qtqty-focost    =  quoteqty.fo-cost
                ttQuoteQtQtyItems.qtqty-vocost    =  quoteqty.vo-cost
                ttQuoteQtQtyItems.qtqty-msf       =  tot-msf()
               ttQuoteQtQtyItems.qtqty-reckey    =  string(rowid(quoteqty))
                ttQuoteQtQtyItems.qtqty-date      =  quoteqty.quote-date
                ttQuoteQtQtyItems.qtqty-user      =  quoteqty.quote-user
                ttQuoteQtQtyItems.qtqty-line      =  quoteqty.LINE  .
        END.
    END.    
END.
