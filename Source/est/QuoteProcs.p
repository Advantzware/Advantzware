
/*------------------------------------------------------------------------
    File        : QuoteProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thue Feb 23 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE gcQuoPriceChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE gcCePrepPrice  AS CHARACTER NO-UNDO .
DEFINE VARIABLE gcCePrep       AS CHARACTER NO-UNDO .

DEFINE TEMP-TABLE w-probeit LIKE probeit
    FIELD mat-cost   LIKE probe.mat-cost
    FIELD lab-cost   LIKE probe.lab-cost
    FIELD vo-cost    LIKE probe.vo-cost
    FIELD fo-cost    LIKE probe.fo-cost
    FIELD probe-date LIKE probe.probe-date
    .    

{est/ttQuoteMaster.i}    
{est/ttQuoteHeader.i}
{est/ttQuoteItem.i}
{est/ttQuoteQuantity.i}
{est/ttQuoteMisc.i}
        
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */



/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddQuoteHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-quotehd FOR quotehd.
    
    IF NOT AVAILABLE ipbf-quotehd THEN
        RETURN.
    
    FIND FIRST ttQuoteHeader
         WHERE ttQuoteHeader.riQuotehd EQ ROWID(ipbf-quotehd) 
         NO-ERROR.
    IF AVAILABLE ttQuoteHeader THEN
        RETURN.
           
    CREATE ttQuoteHeader.
    ASSIGN
        ttQuoteHeader.riQuotehd  = ROWID(ipbf-quotehd)
        ttQuoteHeader.company    = ipbf-quotehd.company
        ttQuoteHeader.locationID = ipbf-quotehd.loc
        ttQuoteHeader.quoteID    = ipbf-quotehd.q-no
        ttQuoteHeader.estimateID = ipbf-quotehd.est-no
        ttQuoteHeader.customerID = ipbf-quotehd.cust-no
        ttQuoteHeader.quoteDate  = ipbf-quotehd.quo-date
        ttQuoteHeader.expireDate = ipbf-quotehd.expireDate
        ttQuoteHeader.salesMan   = ipbf-quotehd.sman
        ttQuoteHeader.terms      = ipbf-quotehd.terms
        ttQuoteHeader.carrier    = ipbf-quotehd.carrier
        .
END PROCEDURE.

PROCEDURE pAddQuoteItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-quoteitm FOR quoteitm.
    
    IF NOT AVAILABLE ipbf-quoteitm THEN
        RETURN.
    
    FIND FIRST ttQuoteItem
         WHERE ttQuoteItem.riQuoteitm EQ ROWID(ipbf-quoteitm) 
         NO-ERROR.
    IF AVAILABLE ttQuoteItem THEN
        RETURN.
           
    CREATE ttQuoteItem.
    ASSIGN
        ttQuoteItem.riQuoteitm = ROWID(ipbf-quoteitm)
        ttQuoteItem.company    = ipbf-quoteitm.company
        ttQuoteItem.locationID = ipbf-quoteitm.loc
        ttQuoteItem.quoteID    = ipbf-quoteitm.q-no
        ttQuoteItem.lineID     = ipbf-quoteitm.line
        ttQuoteItem.partID     = ipbf-quoteitm.part-no
        ttQuoteItem.itemID     = ipbf-quoteitm.i-no
        ttQuoteItem.style      = ipbf-quoteitm.style
        .
END PROCEDURE.

PROCEDURE pAddQuoteMisc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-quotechg FOR quotechg.
    
    IF NOT AVAILABLE ipbf-quotechg THEN
        RETURN.
    
    FIND FIRST ttQuoteMisc
         WHERE ttQuoteMisc.riQuotechg EQ ROWID(ipbf-quotechg) 
         NO-ERROR.
    IF AVAILABLE ttQuoteMisc THEN
        RETURN.
           
    CREATE ttQuoteMisc.
    ASSIGN
        ttQuoteMisc.riQuotechg = ROWID(ipbf-quotechg)
        ttQuoteMisc.company    = ipbf-quotechg.company
        ttQuoteMisc.locationID = ipbf-quotechg.loc
        ttQuoteMisc.quoteID    = ipbf-quotechg.q-no
        ttQuoteMisc.lineID     = ipbf-quotechg.line
        ttQuoteMisc.quantity   = ipbf-quotechg.qty        
        .
END PROCEDURE.

PROCEDURE pAddQuoteQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-quoteqty FOR quoteqty.
    
    IF NOT AVAILABLE ipbf-quoteqty THEN
        RETURN.
    
    FIND FIRST ttQuoteQuantity
         WHERE ttQuoteQuantity.riQuoteqty EQ ROWID(ipbf-quoteqty) 
         NO-ERROR.
    IF AVAILABLE ttQuoteQuantity THEN
        RETURN.
           
    CREATE ttQuoteQuantity.
    ASSIGN
        ttQuoteQuantity.riQuoteqty = ROWID(ipbf-quoteqty)
        ttQuoteQuantity.company    = ipbf-quoteqty.company
        ttQuoteQuantity.locationID = ipbf-quoteqty.loc
        ttQuoteQuantity.quoteID    = ipbf-quoteqty.q-no
        ttQuoteQuantity.lineID     = ipbf-quoteqty.line
        ttQuoteQuantity.quantity   = ipbf-quoteqty.qty
        .

END PROCEDURE.

PROCEDURE pBuildQuote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttQuoteMaster FOR ttQuoteMaster.    
    
    DEFINE BUFFER bf-quotehd  FOR quotehd.
    DEFINE BUFFER bf-quoteitm FOR quoteitm.
    DEFINE BUFFER bf-quoteqty FOR quoteqty.
    DEFINE BUFFER bf-quotechg FOR quotechg.
    
    IF NOT AVAILABLE ipbf-ttQuoteMaster THEN
        RETURN.
        
    FOR EACH bf-quotehd NO-LOCK
        WHERE bf-quotehd.company EQ ipbf-ttQuoteMaster.company
          AND bf-quotehd.cust-no GE ipbf-ttQuoteMaster.beginCustomerID
          AND bf-quotehd.cust-no LE ipbf-ttQuoteMaster.endCustomerID
          AND bf-quotehd.loc     EQ ipbf-ttQuoteMaster.locationID
          AND bf-quotehd.q-no    GE ipbf-ttQuoteMaster.beginQuoteID
          AND bf-quotehd.q-no    LE ipbf-ttQuoteMaster.endQuoteID:
        RUN pAddQuoteHeader ( BUFFER bf-quotehd).  
        
        FOR EACH bf-quoteitm OF bf-quotehd NO-LOCK:
            RUN pAddQuoteItem (BUFFER bf-quoteitm).
            
            FOR EACH bf-quoteqty OF bf-quoteitm NO-LOCK:
                RUN pAddQuoteQuantity (BUFFER bf-quoteqty).

                FOR EACH bf-quotechg NO-LOCK
                    WHERE bf-quotechg.company EQ bf-quoteqty.company
                      AND bf-quotechg.loc     EQ bf-quoteqty.loc
                      AND bf-quotechg.q-no    EQ bf-quoteqty.q-no
                      AND bf-quotechg.line    EQ bf-quoteqty.line
                      AND bf-quotechg.qty     EQ bf-quoteqty.qty:
                    RUN pAddQuoteMisc (BUFFER bf-quotechg).
                END.
            END.
        END.   
    END.      
END PROCEDURE.

PROCEDURE pAddQuoteMaster PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBeginCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEndCustomerID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBeginQuoteID    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEndQuoteID      AS INTEGER   NO-UNDO.
    
    CREATE ttQuoteMaster.
    ASSIGN
        ttQuoteMaster.company         = ipcCompany
        ttQuoteMaster.locationID      = ipcLocationID
        ttQuoteMaster.beginCustomerID = ipcBeginCustomerID
        ttQuoteMaster.endCustomerID   = ipcEndCustomerID
        ttQuoteMaster.beginQuoteID    = ipiBeginQuoteID
        ttQuoteMaster.endQuoteID      = ipiEndQuoteID
        .
END PROCEDURE.

PROCEDURE Quote_BuildQuote:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuoteID AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteMaster.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteHeader.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteItem.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteQuantity.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteMisc.
 
    DEFINE BUFFER bf-quotehd FOR quotehd.
    
    FIND FIRST bf-quotehd NO-LOCK
         WHERE bf-quotehd.company EQ ipcCompany
           AND bf-quotehd.q-no    EQ ipiQuoteID
         NO-ERROR.
    IF NOT AVAILABLE bf-quotehd THEN
        RETURN.
     
    RUN Quote_BuildQuotes (
        INPUT  bf-quotehd.company,
        INPUT  bf-quotehd.loc,
        INPUT  bf-quotehd.cust-no,
        INPUT  bf-quotehd.cust-no,
        INPUT  bf-quotehd.q-no,
        INPUT  bf-quotehd.q-no,
        INPUT-OUTPUT TABLE ttQuoteMaster,
        INPUT-OUTPUT TABLE ttQuoteHeader,
        INPUT-OUTPUT TABLE ttQuoteItem,
        INPUT-OUTPUT TABLE ttQuoteQuantity,
        INPUT-OUTPUT TABLE ttQuoteMisc
        ).
END PROCEDURE.

PROCEDURE Quote_BuildQuotes:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcLocationID      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcBeginCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcEndCustomerID   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBeginQuoteID    AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiEndQuoteID      AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteMaster.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteHeader.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteItem.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteQuantity.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttQuoteMisc.
        
    EMPTY TEMP-TABLE ttQuoteHeader.
    EMPTY TEMP-TABLE ttQuoteItem.
    EMPTY TEMP-TABLE ttQuoteMaster.
    EMPTY TEMP-TABLE ttQuoteMisc.
    EMPTY TEMP-TABLE ttQuoteQuantity.
    
    RUN pAddQuoteMaster(ipcCompany , ipcLocationID, ipcBeginCustomerID, ipcEndCustomerID, ipiBeginQuoteID, ipiEndQuoteID).
    
    FOR EACH ttQuoteMaster:
        RUN pBuildQuote (BUFFER ttQuoteMaster).
    END.
END PROCEDURE.

PROCEDURE Quote_CreateQuoteFromEst:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    
    RUN pQuoteAutoCreateFromEst(iprwRowid, ipcCompany, ipcEstimateNo).
    
END.    


PROCEDURE pQuoteAutoCreateFromEst PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Internal Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCustomer AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-eb FOR eb.
    FIND FIRST est NO-LOCK
        WHERE est.company EQ ipcCompany
        AND est.est-no  EQ ipcEstimateNo
        NO-ERROR. 
    IF NOT AVAILABLE est THEN RETURN.
    
    FIND FIRST bf-eb NO-LOCK WHERE bf-eb.company EQ ipcCompany
        AND bf-eb.est-no  EQ est.est-no
        AND bf-eb.cust-no GT ""
        NO-ERROR.
    IF AVAILABLE bf-eb THEN
        cCustomer = bf-eb.cust-no.
    ELSE
        cCustomer = "".
          
    RUN pSetGlobalSettings(ipcCompany,cCustomer ).
    
    RUN pCreateProbeit( BUFFER est ,ipcCompany ).
    
    RUN pCreateQuoteFromProbe(BUFFER est,iprwRowid).     
        
   
END PROCEDURE.

PROCEDURE pCreateProbeit PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER opbf-est FOR est.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER xprobe FOR probe.
    DEFINE BUFFER bf-eb  FOR eb.
    
    FOR EACH w-probeit:
        DELETE w-probeit.
    END. /*FOR EACH w-probeit*/ 
    
    FOR EACH xprobe
        WHERE xprobe.company  EQ opbf-est.company
        AND xprobe.est-no   EQ opbf-est.est-no          :

          
        FIND FIRST bf-eb
            WHERE bf-eb.company   EQ opbf-est.company 
            AND bf-eb.est-no    EQ opbf-est.est-no
            AND (bf-eb.form-no  NE 0 OR
            (bf-eb.form-no EQ 0 AND opbf-est.est-type EQ 6))
            AND bf-eb.blank-no  EQ 1
                                 
            NO-LOCK NO-ERROR.

        IF AVAILABLE bf-eb THEN 
        DO:   
            CREATE w-probeit.
            ASSIGN
                w-probeit.company      = bf-eb.company
                w-probeit.est-no       = bf-eb.est-no
                w-probeit.cust-no      = bf-eb.cust-no
                w-probeit.part-no      = bf-eb.part-no
                w-probeit.bl-qty       = xprobe.est-qty
                w-probeit.sell-price   = xprobe.sell-price
                w-probeit.prof-on      = xprobe.prof-on
                w-probeit.net-profit   = xprobe.net-profit
                w-probeit.gross-profit = xprobe.gross-profit
                w-probeit.mat-cost     = xprobe.mat-cost
                w-probeit.lab-cost     = xprobe.lab-cost
                w-probeit.vo-cost      = xprobe.vo-cost
                w-probeit.fo-cost      = xprobe.fo-cost
                w-probeit.tot-lbs      = xprobe.tot-lbs
                w-probeit.freight      = xprobe.freight
                w-probeit.probe-date   = xprobe.probe-date.
        END.
    END.

END PROCEDURE.


PROCEDURE pCreatePriceMatrixForQuote PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 define input PARAMETER ipcCompany AS CHARACTER NO-UNDO.
 define input PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
 define input PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
 define input PARAMETER ipcItemNo AS CHARACTER NO-UNDO.
 
 FOR EACH quotehd NO-LOCK 
        WHERE quotehd.company EQ ipcCompany
        AND quotehd.est-no EQ ipcEstimate
        ,
        EACH quoteitm OF quotehd EXCLUSIVE-LOCK 
        WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.part-no EQ ipcPartNo:
    ASSIGN quoteitm.i-no = ipcItemNo .
    LEAVE.
 END.
 RELEASE quoteitm.
 IF AVAIL quotehd THEN
 RUN oe/updprmtx2.p (ROWID(quotehd), "", 0, "", 0, "Q").

END PROCEDURE.


PROCEDURE pCreateQuoteFromProbe PRIVATE:
    
    DEFINE PARAMETER BUFFER opbf-est FOR est.
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE BUFFER bf-ef FOR ef.
    
    DEFINE VARIABLE lNewQuote   AS LOGICAL INIT YES NO-UNDO.
    DEFINE VARIABLE iQuoteNo    LIKE quotehd.q-no NO-UNDO.
    DEFINE VARIABLE cNotes      LIKE quotehd.comment NO-UNDO.
    DEFINE VARIABLE iLineNo     AS INTEGER NO-UNDO .  /* for quoteitm.line */
    DEFINE VARIABLE iFirstQty   AS INTEGER NO-UNDO.  /* first qty for quoteitm */
    DEFINE VARIABLE li-prep-qty LIKE quotechg.prep-qty NO-UNDO.
    DEFINE VARIABLE v-tmp-int   AS INTEGER NO-UNDO.
    DEFINE VARIABLE j           AS INTEGER NO-UNDO.
    DEFINE VARIABLE ld-cost     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE li-value    AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-cnt      AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-tot-mat   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-tot-lab   AS DECIMAL NO-UNDO.
    
    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ iprwRowid NO-ERROR.
    
    FOR EACH w-probeit
        BREAK BY w-probeit.cust-no
        BY w-probeit.part-no
        BY w-probeit.bl-qty:

        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ w-probeit.company
            AND bf-eb.est-no  EQ w-probeit.est-no
            AND bf-eb.cust-no EQ w-probeit.cust-no
            AND bf-eb.part-no EQ w-probeit.part-no
            AND bf-eb.form-no NE 0
            NO-ERROR.

        IF NOT AVAILABLE bf-eb THEN 
        DO:
            FIND FIRST bf-eb NO-LOCK
                WHERE bf-eb.company EQ w-probeit.company
                AND bf-eb.est-no  EQ w-probeit.est-no
                AND bf-eb.form-no EQ 0
                NO-ERROR.
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ w-probeit.company
                AND eb.est-no  EQ w-probeit.est-no
                AND eb.form-no NE 0
                NO-ERROR.
        END.

        FIND FIRST quotehd
            WHERE quotehd.company EQ opbf-est.company
            AND quotehd.loc     EQ opbf-est.loc
            AND quotehd.q-no    EQ iQuoteNo
            NO-ERROR.

        IF FIRST-OF(w-probeit.cust-no) THEN 
        DO:
            IF lNewQuote OR NOT FIRST(w-probeit.cust-no) OR NOT AVAILABLE quotehd THEN 
            DO:
                CREATE quotehd.
                quotehd.quo-date = TODAY.
        
            END.
           
            ASSIGN
                iQuoteNo           = quotehd.q-no  /* from create trigger */
                quotehd.e-num      = opbf-est.e-num
                quotehd.est-no     = opbf-est.est-no
                quotehd.cust-no    = bf-eb.cust-no
                quotehd.ship-no    = bf-eb.ship-no
                quotehd.ship-id    = bf-eb.ship-id
                quotehd.sold-no    = 1
                quotehd.part-dscr1 = bf-eb.part-dscr1
                quotehd.upd-date   = TODAY
                quotehd.quo-date   = TODAY
                quotehd.upd-user   = USERID("ASI").

            {custom/getrfq.i}
      
            FIND FIRST cust
                WHERE (cust.company EQ opbf-est.company)
                AND cust.cust-no EQ quotehd.cust-no
                NO-LOCK NO-ERROR.
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ opbf-est.company
                AND shipto.cust-no EQ quotehd.cust-no
                AND shipto.ship-id EQ quotehd.ship-id
                NO-ERROR.
            FOR EACH soldto NO-LOCK
                WHERE soldto.company EQ opbf-est.company
                AND soldto.cust-no EQ quotehd.cust-no
          
                BREAK BY soldto.sold-no DESCENDING:
                IF soldto.sold-no EQ 1 OR LAST-OF(soldto.sold-no) THEN LEAVE.
            END.

            ASSIGN
                quotehd.sman     = cust.sman  /* bf-eb.sman */
                quotehd.carrier  = eb.carrier  /*bf-eb.carrier */
                quotehd.del-zone = eb.dest-code  /* bf-eb.dest-code */
                quotehd.terms    = cust.terms
                quotehd.contact  = cust.contact.

            IF cust.cust-no EQ "TEMP" THEN
                ASSIGN
                    quotehd.shipto[1] = eb.ship-name
                    quotehd.shipto[2] = eb.ship-addr[1]
                    quotehd.shipto[3] = eb.ship-addr[2]
                    quotehd.shipto[4] = eb.ship-city + ", " + eb.ship-state +
                             " " + eb.ship-zip
                    quotehd.billto[1] = quotehd.shipto[1]
                    quotehd.billto[2] = quotehd.shipto[2]
                    quotehd.billto[3] = quotehd.shipto[3]
                    quotehd.billto[4] = quotehd.shipto[4] 
                    quotehd.soldto[1] = quotehd.shipto[1]
                    quotehd.soldto[2] = quotehd.shipto[2]
                    quotehd.soldto[3] = quotehd.shipto[3]
                    quotehd.soldto[4] = quotehd.shipto[4].

            ELSE
                ASSIGN
                    quotehd.billto[1] = cust.name
                    quotehd.billto[2] = cust.addr[1]
                    quotehd.billto[3] = cust.addr[2]
                    quotehd.billto[4] = cust.city + ", " + cust.state + " " + cust.zip
                    quotehd.shipto[1] = shipto.ship-name
                    quotehd.shipto[2] = shipto.ship-addr[1]
                    quotehd.shipto[3] = shipto.ship-addr[2]
                    quotehd.shipto[4] = shipto.ship-city + ", " + shipto.ship-state +
                             " " + shipto.ship-zip
                    quotehd.soldto[1] = soldto.sold-name
                    quotehd.soldto[2] = soldto.sold-addr[1]
                    quotehd.soldto[3] = soldto.sold-addr[2]
                    quotehd.soldto[4] = soldto.sold-city + ", " + soldto.sold-state +
                             " " + soldto.sold-zip.
               
            /* copy notes from old quotehd */
            IF lNewQuote THEN 
            DO:
                RUN est/GetQuoteDefNotes.p (INPUT quotehd.company,
                    OUTPUT cNotes).
                /*          FIND FIRST bf-qhd NO-LOCK NO-ERROR. */
                /*          IF AVAILABLE bf-qhd THEN                */
                ASSIGN  
                    quotehd.comment[1] = cNotes[1]
                    quotehd.comment[2] = cNotes[2]
                    quotehd.comment[3] = cNotes[3]
                    quotehd.comment[4] = cNotes[4]
                    quotehd.comment[5] = cNotes[5].
            END.

            IF opbf-est.est-type GE 7 THEN 
            DO:

                FOR EACH quotechg OF quotehd:
                    DELETE quotechg.
                END.
            END.
        END.
        
        FIND FIRST quoteitm
            WHERE quoteitm.company EQ quotehd.company
            AND quoteitm.loc     EQ quotehd.loc
            AND quoteitm.q-no    EQ quotehd.q-no
            AND quoteitm.part-no EQ w-probeit.part-no
            NO-ERROR.
                  
        IF FIRST-OF(w-probeit.part-no) THEN 
        DO:
            FIND FIRST bf-ef NO-LOCK
                WHERE bf-ef.company  EQ opbf-est.company
                AND bf-ef.est-no   EQ opbf-est.est-no
                AND (bf-ef.form-no EQ bf-eb.form-no OR opbf-est.est-type EQ 6)
                NO-ERROR.
              
            IF lNewQuote OR NOT AVAILABLE quoteitm THEN 
            DO:
                FOR EACH quoteitm NO-LOCK
                    WHERE quoteitm.company EQ quotehd.company
                    AND quoteitm.loc     EQ quotehd.loc
                    AND quoteitm.q-no    EQ quotehd.q-no
            
                    BY quoteitm.line DESCENDING:
                    iLineNo = quoteitm.line + 1.
                    LEAVE.
                END.
                CREATE quoteitm.
            END.
            ELSE iLineNo = quoteitm.line.
      
            ASSIGN
                quoteitm.company    = quotehd.company
                quoteitm.loc        = quotehd.loc
                quoteitm.q-no       = quotehd.q-no
                quoteitm.est-no     = quotehd.est-no
                quoteitm.line       = iLineNo
                quoteitm.style      = bf-eb.style
                quoteitm.part-no    = bf-eb.part-no
                quoteitm.part-dscr1 = bf-eb.part-dscr1
                quoteitm.part-dscr2 = bf-eb.part-dscr2
                quoteitm.i-coldscr  = IF opbf-est.est-type EQ 6 THEN eb.i-coldscr
                                                  ELSE bf-eb.i-coldscr
                quoteitm.i-dscr     = bf-ef.brd-dscr
                quoteitm.qty        = w-probeit.bl-qty
                quoteitm.uom        = gcQuoPriceChar
                quoteitm.price      = w-probeit.sell-price
                quoteitm.upd-date   = TODAY
                quoteitm.upd-user   = USERID("ASI")
                /*RCO400 only */
                quoteitm.i-no       = bf-eb.stock-no.

            RUN sys/inc/calcsize.p (IF opbf-est.est-type EQ 6 THEN ROWID(bf-eb) ELSE ROWID(eb),
                OUTPUT quoteitm.size).

            IF quoteitm.uom NE "M" THEN
                RUN sys/ref/convcuom.p("M",quoteitm.uom, 0, 0, 0, 0,
                    quoteitm.price, OUTPUT quoteitm.price).
   
            IF bf-ef.brd-dscr EQ '' THEN 
            DO:
                FIND FIRST ITEM NO-LOCK
                    WHERE item.company EQ opbf-est.company
                    AND item.i-no    EQ bf-ef.board
                    NO-ERROR.
                IF AVAILABLE ITEM THEN
                    quoteitm.i-dscr = IF item.i-name   GT "" THEN item.i-name   ELSE
                        IF item.est-dscr GT "" THEN item.est-dscr ELSE
                        item.i-dscr.
            END. /* IF brd-dscr */
        END.
           
        FIND FIRST quoteqty
            WHERE quoteqty.company EQ quoteitm.company
            AND quoteqty.loc     EQ quoteitm.loc
            AND quoteqty.q-no    EQ quoteitm.q-no
            AND quoteqty.LINE    EQ quoteitm.line
            AND quoteqty.qty     EQ w-probeit.bl-qty
            AND quoteqty.rels    EQ INTEGER(w-probeit.freight)
            NO-ERROR.

        IF lNewQuote OR NOT AVAILABLE quoteqty THEN CREATE quoteqty.

        ASSIGN
            quoteqty.company    = quoteitm.company
            quoteqty.loc        = quoteitm.loc
            quoteqty.q-no       = quoteitm.q-no
            quoteqty.line       = quoteitm.line
            quoteqty.qty        = w-probeit.bl-qty
            quoteqty.uom        = gcQuoPriceChar
            quoteqty.price      = w-probeit.sell-price
            quoteqty.rels       = w-probeit.freight
            quoteqty.quote-date = /*IF lNewQuote THEN TODAY ELSE */ w-probeit.probe-date
            quoteqty.quote-user = USERID("ASI")
            quoteqty.prof-on    = w-probeit.prof-on
            quoteqty.mat-cost   = w-probeit.mat-cost
            quoteqty.lab-cost   = w-probeit.lab-cost
            quoteqty.vo-cost    = w-probeit.vo-cost
            quoteqty.fo-cost    = w-probeit.fo-cost
            quoteqty.tot-lbs    = w-probeit.tot-lbs
            quoteqty.profit     = IF w-probeit.prof-on EQ "Net" THEN w-probeit.net-profit
                                                         ELSE w-probeit.gross-profit.
      
        IF quoteqty.uom NE "M" THEN
            RUN sys/ref/convcuom.p("M",quoteqty.uom, 0, 0, 0, 0,
                quoteqty.price, OUTPUT quoteqty.price).
               
    /* update rfqitem qty - start */
    &SCOPED-DEFINE getrfq
        {custom/rfq-qty.i}
        /* update rfqitem qty - end */
               
        IF LAST-OF(w-probeit.cust-no) OR opbf-est.est-type LT 7 THEN 
        DO:
            iFirstQty = IF opbf-est.est-type GE 7 THEN 0 ELSE w-probeit.bl-qty.

            FOR EACH quotechg
                WHERE quotechg.company EQ quoteqty.company
                AND quotechg.loc     EQ quoteqty.loc
                AND quotechg.q-no    EQ quoteqty.q-no
                AND ((quotechg.line  EQ quoteqty.line AND iFirstQty NE 0) OR
                quotechg.line   EQ 0)
                AND quotechg.qty     EQ iFirstQty:
                DELETE quotechg.
            END.
         
            FOR EACH bf-ef NO-LOCK
                WHERE bf-ef.company EQ quotehd.company
                AND bf-ef.est-no  EQ quotehd.est-no:

                li-prep-qty = 0.

                IF opbf-est.est-type GE 7 THEN
                    FOR EACH bf-eb FIELDS(bl-qty) NO-LOCK
                        WHERE bf-eb.company EQ bf-ef.company
                        AND bf-eb.est-no  EQ bf-ef.est-no
                        AND bf-eb.form-no EQ bf-ef.form-no
                        AND bf-eb.cust-no EQ w-probeit.cust-no:
                        li-prep-qty = li-prep-qty + bf-eb.bl-qty.
                    END.

                ELSE li-prep-qty = w-probeit.bl-qty.
                
                FOR EACH est-prep NO-LOCK
                    WHERE est-prep.company EQ bf-ef.company
                    AND est-prep.est-no  EQ bf-ef.est-no
                    AND est-prep.s-num   EQ bf-ef.form-no
                    AND est-prep.simon   EQ "S":

                    CREATE quotechg.
                    ASSIGN
                        quotechg.company     = quoteqty.company
                        quotechg.loc         = quoteqty.loc
                        quotechg.q-no        = quoteqty.q-no
                        quotechg.line        = IF iFirstQty EQ 0 THEN 0 ELSE quoteqty.line
                        quotechg.qty         = iFirstQty
                        quotechg.quote-date  = quoteqty.quote-date
                        quotechg.CODE        = est-prep.CODE
                        quotechg.charge      = est-prep.dscr
                        quotechg.bill        = IF est-prep.ml THEN "M" ELSE "L"
                        quotechg.amt         = IF gcCePrepPrice EQ "Profit" THEN
                                  est-prep.qty * est-prep.cost /
                                  (1 - (est-prep.mkup / 100)) *
                                  (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)
                               ELSE /*Cost Markup*/
                                  est-prep.qty * est-prep.cost *
                                  (1 + (est-prep.mkup / 100)) *
                                  (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)  
                        quotechg.mkup        = est-prep.mkup
                        quotechg.spare-dec-1 = est-prep.spare-dec-1
                        quotechg.cost        = est-prep.cost
                        quotechg.amtz        = est-prep.amtz
                        quotechg.prep-qty    = est-prep.qty
                        quotechg.s-num       = est-prep.s-num
                        quotechg.b-num       = est-prep.b-num
                        quotechg.simon       = est-prep.simon. 

                    IF gcCePrep EQ "Dollar" THEN 
                    DO:
                        {sys/inc/roundup.i quotechg.amt}
                    END.
                    ELSE IF gcCePrep EQ "FiveDollar" THEN 
                        DO:
                            {sys/inc/roundupfive.i quotechg.amt}
                        END.
                END.

                DO j = 1 TO 6:
                    IF bf-ef.mis-simon[j] EQ "S" AND bf-ef.mis-cost[j] NE "" THEN 
                    DO:
                        CREATE quotechg.

                        IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) AND
                            (bf-ef.mis-matf[j] EQ 0 AND bf-ef.mis-matm[j] EQ 0) THEN
                            quotechg.bill = "L".
                        ELSE
                            IF (bf-ef.mis-labf[j] EQ 0 AND bf-ef.mis-labm[j] EQ 0) AND
                                (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
                                quotechg.bill = "M".
                            ELSE
                                IF (bf-ef.mis-labf[j] NE 0 OR bf-ef.mis-labm[j] NE 0) OR
                                    (bf-ef.mis-matf[j] NE 0 OR bf-ef.mis-matm[j] NE 0) THEN
                                    quotechg.bill = "T".

                        ASSIGN
                            quotechg.company    = quoteqty.company
                            quotechg.loc        = quoteqty.loc
                            quotechg.q-no       = quoteqty.q-no
                            quotechg.line       = IF iFirstQty EQ 0 THEN 0 ELSE quoteqty.line
                            quotechg.qty        = iFirstQty
                            quotechg.quote-date = quoteqty.quote-date
                            quotechg.prep-qty   = li-prep-qty
                            quotechg.s-num      = bf-ef.mis-snum[j]
                            quotechg.b-num      = bf-ef.mis-bnum[j]
                            quotechg.charge     = bf-ef.mis-cost[j]
                            quotechg.labf       = bf-ef.mis-labf[j]
                            quotechg.matf       = bf-ef.mis-matf[j]
                            quotechg.labm       = bf-ef.mis-labm[j]
                            quotechg.matm       = bf-ef.mis-matm[j]
                            quotechg.mkup       = bf-ef.mis-mkup[j]
                            quotechg.simon      = bf-ef.mis-simon[j]
                            quotechg.amtz       = 100.
               
                        {est/qt-misc.i "MAT" j}
                        quotechg.matm = ld-cost.
                        {est/qt-misc.i "LAB" j}

                        ASSIGN
                            quotechg.labm = ld-cost.

                        IF gcCePrepPrice EQ "Profit" THEN
                            ASSIGN
                                v-tot-mat = (quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000))) /
                              (1 - (quotechg.mkup / 100))
                                v-tot-lab = (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000))) /
                              (1 - (quotechg.mkup / 100)).
                        ELSE
                            ASSIGN
                                v-tot-mat = (quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000))) *
                              (1 + (quotechg.mkup / 100))
                                v-tot-lab = (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000))) *
                              (1 + (quotechg.mkup / 100)).

                        quotechg.amt = v-tot-mat + v-tot-lab.
            
                        IF quotechg.prep-qty NE 0 THEN
                            quotechg.cost = ((quotechg.matf + (quotechg.matm * (quotechg.prep-qty / 1000)))
                                + (quotechg.labf + (quotechg.labm * (quotechg.prep-qty / 1000)))) / quotechg.prep-qty.
            
             
                        IF gcCePrep EQ "Dollar" THEN 
                        DO:
                            {sys/inc/roundup.i quotechg.amt}
                        END.
                        ELSE IF gcCePrep EQ "FiveDollar" THEN 
                            DO:
                                {sys/inc/roundupfive.i quotechg.amt}
                            END.
                    END.
                END.
            END.
        END.

        DELETE w-probeit.
    END.  /* each w-probeit */
  
  
    IF AVAILABLE quotechg THEN FIND CURRENT quotechg NO-LOCK NO-ERROR.
    IF AVAILABLE quoteqty THEN FIND CURRENT quoteqty NO-LOCK NO-ERROR.
    IF AVAILABLE quoteitm THEN FIND CURRENT quoteitm NO-LOCK NO-ERROR.
    IF AVAILABLE quotehd  THEN 
    DO:
        FIND CURRENT quotehd  NO-LOCK NO-ERROR.

        RELEASE quotechg.
        RELEASE quoteqty.
        RELEASE quoteitm.

        FOR EACH quoteitm OF quotehd NO-LOCK,
            FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ quotehd.company
            AND bf-eb.est-no  EQ quotehd.est-no
            AND bf-eb.part-no EQ quoteitm.part-no,
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ bf-eb.company
            AND itemfg.i-no    EQ bf-eb.stock-no,
            EACH quoteqty OF quoteitm NO-LOCK:

            RUN fg/makenote.p (BUFFER oe-ordl,
                BUFFER quoteqty,
                BUFFER ar-invl,
                NO,
                itemfg.rec_key).
        END.
    END.    

END PROCEDURE.




PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p  (INPUT ipcCompany,
        INPUT "QUOPRICE", 
        INPUT "C", 
        INPUT YES, 
        INPUT YES, 
        INPUT ipcCustomer,
        INPUT "", 
        OUTPUT gcQuoPriceChar, 
        OUTPUT lFound ).
    IF NOT lFound THEN
        gcQuoPriceChar = "M". 
    

    RUN sys/ref/nk1look.p (ipcCompany, "CEPREPPRICE", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN gcCePrepPrice = cReturn.

    RUN sys/ref/nk1look.p (ipcCompany, "CEPREP", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN gcCePrep = cReturn.
            
END PROCEDURE.

PROCEDURE pUpdateQuotePriceFromMatrix PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Procedure for updating quotes from matrix 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER oprwRowid AS ROWID NO-UNDO.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-quoteqty FOR quoteqty.
    DEFINE BUFFER bf-quotehd FOR quotehd.
      FIND FIRST bf-oe-prmtx NO-LOCK WHERE ROWID(bf-oe-prmtx) EQ oprwRowid NO-ERROR.
      IF bf-oe-prmtx.quoteID NE 0 THEN
      DO:      
         FIND FIRST bf-quotehd EXCLUSIVE-LOCK
              WHERE bf-quotehd.company EQ bf-oe-prmtx.company                
                AND bf-quotehd.q-no    EQ bf-oe-prmtx.quoteID
              NO-ERROR.
         IF AVAIL bf-quotehd THEN
         DO:
            ASSIGN
                 bf-quotehd.effectiveDate = bf-oe-prmtx.eff-date
                 bf-quotehd.expireDate    = bf-oe-prmtx.exp-date.
                
            FOR EACH quoteitm
                WHERE quoteitm.company EQ bf-quotehd.company
                AND quoteitm.loc     EQ bf-quotehd.loc
                AND quoteitm.q-no    EQ bf-quotehd.q-no
                AND quoteitm.i-no    EQ bf-oe-prmtx.i-no 
                EXCLUSIVE-LOCK :
                
                iCount = 0.
                
                FOR EACH bf-quoteqty WHERE bf-quoteqty.company = quoteitm.company 
                    AND bf-quoteqty.loc = quoteitm.loc 
                    AND bf-quoteqty.q-no = quoteitm.q-no 
                    AND bf-quoteqty.line = quoteitm.LINE EXCLUSIVE-LOCK BREAK BY bf-quoteqty.qty : 
                    
                    iCount = iCount + 1.
                     
                    IF FIRST-OF(bf-quoteqty.qty) AND bf-oe-prmtx.qty[iCount] NE 0 THEN
                    DO:
                       ASSIGN
                        quoteitm.qty   = bf-oe-prmtx.qty[iCount]
                        quoteitm.uom   = bf-oe-prmtx.uom[iCount]
                        quoteitm.price = bf-oe-prmtx.price[iCount]   .
                    END.
                    IF bf-oe-prmtx.qty[iCount] NE 0 THEN DO: 
                      ASSIGN
                       bf-quoteqty.qty   = bf-oe-prmtx.qty[iCount]
                       bf-quoteqty.uom   = bf-oe-prmtx.uom[iCount]
                       bf-quoteqty.price = bf-oe-prmtx.price[iCount].
                    END.
                END.     
            END. 
         END.  /* AVAIL bf-quotehd*/      
      END.   /* bf-oe-prmtx.quoteID NE 0*/

      RELEASE bf-quoteqty.
      RELEASE bf-quotehd. 
   
END PROCEDURE.

PROCEDURE pUnApprovedDuplicateQuote:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.       
    DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFgItem AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-quotehd FOR quotehd.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    
    FIND FIRST quotehd NO-LOCK
         WHERE ROWID(quotehd) EQ iprwRowid NO-ERROR .
           
    FOR EACH bf-quotehd EXCLUSIVE-LOCK
        WHERE bf-quotehd.company EQ quotehd.company
        AND bf-quotehd.cust-no EQ quotehd.cust-no
        AND bf-quotehd.ship-id EQ quotehd.ship-id
        AND bf-quotehd.pricingMethod EQ quotehd.pricingMethod
        AND bf-quotehd.effectiveDate LT TODAY
        AND rowid(bf-quotehd) NE iprwRowid ,
        EACH quoteitm NO-LOCK
             WHERE quoteitm.company EQ bf-quotehd.company
             AND quoteitm.loc     EQ bf-quotehd.loc
             AND quoteitm.q-no    EQ bf-quotehd.q-no
             AND quoteitm.i-no    EQ ipcFgItem
             AND quoteitm.part-no EQ ipcPartNo:                  
        IF bf-quotehd.approved THEN do:
          bf-quotehd.approved = NO.
          bf-quotehd.expireDate = TODAY - 1.          
          RUN ClearTagsByRecKey(bf-quotehd.rec_key).  /*Clear all hold tags - TagProcs.p*/
          FIND FIRST bf-oe-prmtx EXCLUSIVE-LOCK
              WHERE bf-oe-prmtx.company EQ bf-quotehd.company                
              AND bf-oe-prmtx.quoteID  EQ  bf-quotehd.q-no   
              NO-ERROR.
          IF avail bf-oe-prmtx THEN
           bf-oe-prmtx.exp-date = bf-quotehd.expireDate.           
                    
        END.  
    END.
    RELEASE bf-quotehd.
    RELEASE bf-oe-prmtx.
   
END PROCEDURE.

PROCEDURE quote_CreatePriceMatrixForQuote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 define input PARAMETER ipcCompany AS CHARACTER NO-UNDO.
 define input PARAMETER ipcEstimate AS CHARACTER NO-UNDO.
 define input PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
 define input PARAMETER ipcItemNo AS CHARACTER NO-UNDO.
 
  RUN pCreatePriceMatrixForQuote(INPUT ipcCompany, INPUT ipcEstimate, INPUT ipcPartNo, INPUT ipcItemNo).  

END PROCEDURE.

PROCEDURE UpdateQuotePriceFromMatrix:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER oprwRowid AS ROWID NO-UNDO.
                 
    RUN pUpdateQuotePriceFromMatrix(oprwRowid ).        
   
END PROCEDURE.

PROCEDURE unApprovedDuplicateQuote:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER ipcFgItem AS CHARACTER NO-UNDO.
                 
    RUN pUnApprovedDuplicateQuote(INPUT iprwRowid, INPUT ipcPartNo, INPUT ipcFgItem).        
   
END PROCEDURE.




/* ************************  Function Implementations ***************** */


