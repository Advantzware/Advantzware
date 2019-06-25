



/*------------------------------------------------------------------------
    File        : ViewQuote.p
    Purpose     : Quote Maintenance

    Syntax      :

    Description : Return a Dataset of Quote Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewQuote.i}
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     as INTEGER  NO-UNDO.

DEFINE INPUT PARAMETER PrmDate      AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRfq       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDel       AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER prmBill      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBill2     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBill3     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBill4     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShipid    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip2     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip3     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip4     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER PrmContact   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSoldid    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSold      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmSold2     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmSold3     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmSold4     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmSman      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmTerms     AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmCarr      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmZone      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmStat      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmPart      AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrdscr  AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmTermDscr  AS CHARACTER  NO-UNDO.  
DEFINE INPUT PARAMETER prmSName     AS CHARACTER  NO-UNDO.                                                             
DEFINE INPUT PARAMETER prmZondesc   AS CHARACTER  NO-UNDO.                                                             

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewQuote.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

/*{fg/d-invprc.i NEW}*/

DEFINE BUFFER bf-quotehd FOR quotehd.
DEFINE BUFFER bf-quoteitm FOR quoteitm.
DEFINE BUFFER bf-carrier FOR carrier.
DEFINE BUFFER bf-terms FOR terms.
DEFINE BUFFER bf-sman FOR sman.
    DEFINE BUFFER bf-carr-mtx FOR carr-mtx.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE prmComp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE q-noValue1  AS INT NO-UNDO.
DEFINE VARIABLE q-noValue2  AS INT NO-UNDO.

def var li-qte as int no-undo.
    def var  quotenum AS int no-undo.
     DEF VAR lv-ship-id LIKE quotehd.ship-id NO-UNDO.
  DEF VAR lv-sman LIKE quotehd.sman NO-UNDO.
   DEF BUFFER bf-eb FOR eb.
   DEF VAR v-prev-q-no AS INT NO-UNDO.
   def buffer bf-hd for quotehd.


IF prmUser = ? THEN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmAction = "" THEN ASSIGN prmAction = "Select".
IF prmQuote = ? THEN ASSIGN prmQuote = 0.
   
IF prmCust      = ? THEN ASSIGN prmCust     = "0".   
IF prmEst       = ? THEN ASSIGN prmEst      = "".
IF prmRfq       = ? THEN ASSIGN prmRfq      = "0".
IF prmBill      = ? THEN ASSIGN prmBill     = "".
IF prmBill2     = ? THEN ASSIGN prmBill2    = "".
IF prmBill3     = ? THEN ASSIGN prmBill3    = "".
IF prmBill4     = ? THEN ASSIGN prmBill4    = "".
IF prmShipid    = ? THEN ASSIGN prmShipid   = "".
IF prmShip      = ? THEN ASSIGN prmShip     = "".
IF prmShip2     = ? THEN ASSIGN prmShip2    = "".  
IF prmShip3     = ? THEN ASSIGN prmShip3    = "".  
IF prmShip4     = ? THEN ASSIGN prmShip4    = "".  
IF PrmContact   = ? THEN ASSIGN PrmContact  = "".  
IF prmSoldid    = ? THEN ASSIGN prmSoldid   = "".  
IF prmSold      = ? THEN ASSIGN prmSold     = "".  
IF prmSold2     = ? THEN ASSIGN prmSold2    = "".  
IF prmSold3     = ? THEN ASSIGN prmSold3    = "".  
IF prmSold4     = ? THEN ASSIGN prmSold4    = "".  
IF prmSman      = ? THEN ASSIGN prmSman     = "".  
IF prmTerms     = ? THEN ASSIGN prmTerms    = "".  
IF prmCarr      = ? THEN ASSIGN prmCarr     = "".  
IF prmZone      = ? THEN ASSIGN prmZone     = "".  
IF prmStat      = ? THEN ASSIGN prmStat     = "".  
IF prmPart      = ? THEN ASSIGN prmPart     = "".  
IF prmCarrdscr  = ? THEN ASSIGN prmCarrdscr = "".  
IF prmTermDscr  = ? THEN ASSIGN prmTermDscr = "".  
IF prmSName     = ? THEN ASSIGN prmSName    = "".  
IF prmZondesc   = ? THEN ASSIGN prmZondesc  = "".  


/* ********************  Preprocessor Definitions  ******************** */

DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc AS CHAR NO-UNDO.

DEFINE VAR prmLoc AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

/*prmComp = "001".*/
 prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 prmLoc   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN
    g_company = prmComp
    g_loc     = "MAIN"  .



IF prmAction = "FindQuote" THEN DO:
   
    FIND FIRST quotehd WHERE quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) NO-LOCK NO-ERROR.
    FIND FIRST quoteitm OF quotehd NO-LOCK NO-ERROR.
   IF NOT AVAIL quoteitm THEN DO:
       ASSIGN
           cError = "Sorry, no Quote exists for the estimate. You must create Quote via the print folder".
           RETURN .
    END.

END.

IF prmAction = "Update" THEN DO:

    IF prmCust <> "" THEN DO:
      FIND FIRST cust
          WHERE cust.company EQ prmComp
            AND cust.cust-no EQ prmCust NO-LOCK NO-ERROR.

      IF NOT AVAIL cust THEN DO:
          cError =  "Invalid Bill To, try help..." .
          RETURN .
      END.
    END.


  ASSIGN  prmShipid = CAPS(prmShipid).
  
   FIND FIRST shipto WHERE shipto.company EQ prmComp
                        AND shipto.cust-no EQ prmCust
                        AND shipto.ship-id EQ prmShipid NO-LOCK NO-ERROR.
     IF NOT AVAIL shipto THEN DO:
         cError = "Invalid Ship To, try help..." .
        RETURN .
      END.

  ASSIGN  prmSoldid = CAPS(prmSoldid).

    IF prmSoldid  <> "" THEN DO:
      FIND FIRST soldto
                      WHERE soldto.company EQ prmComp
                        AND soldto.cust-no EQ prmCust
                        AND soldto.sold-id EQ prmSoldid NO-LOCK NO-ERROR.
      IF NOT AVAIL soldto THEN DO:
          cError = "Invalid Sold To, try help..." .
        RETURN .
      END.
    END.

   IF prmSman NE '' THEN DO:
       FIND FIRST sman NO-LOCK WHERE sman.sman EQ prmSman NO-ERROR.
       IF NOT AVAILABLE sman THEN DO:
         cError =  'Invalid Sales Rep. Try Help.' .
        RETURN .
       END.
     END.
     
    
     IF prmTerms NE '' THEN DO:
       FIND FIRST terms NO-LOCK WHERE terms.t-code EQ prmTerms NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
         cError =  'Invalid Terms. Try Help.' .
        RETURN .
       END.     
     END.
     
   
     IF prmCarr EQ '' THEN DO:
       FIND FIRST carrier NO-LOCK WHERE carrier.company EQ prmComp
                                    AND carrier.loc EQ prmLoc
                                    AND carrier.carrier EQ prmCarr NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
         cError = 'Invalid Carrier Code. Try Help.' .
         RETURN .
       END.
     END.

    
     IF prmZone EQ '' THEN DO:
       FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ prmComp
                                     AND carr-mtx.loc EQ prmLoc
                                     AND carr-mtx.carrier EQ prmCarr
                                     AND carr-mtx.del-zone EQ prmZone NO-ERROR.
       IF NOT AVAILABLE carr-mtx THEN DO:
         cError =  'Invalid Delivey Zone. Try Help.' .
         RETURN .
       END.
     END.
  

END.  /* end of validation update*/


IF prmAction = "Update" THEN DO:
   FIND FIRST bf-quotehd WHERE bf-quotehd.company = prmComp AND bf-quotehd.q-no = prmQuote EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST bf-quoteitm WHERE bf-quoteitm.company = bf-quotehd.company AND bf-quoteitm.q-no= bf-quotehd.q-no  EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL bf-quotehd THEN DO:
           ASSIGN
               lv-ship-id = bf-quotehd.ship-id
               lv-sman = bf-quotehd.sman
               v-prev-q-no = bf-quotehd.q-no.

        assign
          bf-quotehd.quo-date        = PrmDate      
          bf-quotehd.est-no          = prmEst        
          bf-quotehd.rfq             = prmRfq        
          bf-quotehd.del-date        = prmDel        
          bf-quotehd.ship-id         = prmShipid       
          bf-quotehd.contact         = PrmContact   
          bf-quotehd.sold-id         = prmSoldid     
          bf-quotehd.sman            = prmSman    
          bf-quotehd.terms           = prmTerms    
          bf-quotehd.carrier         = prmCarr     
          bf-quotehd.del-zone        = prmZone   .         
       END.

       FIND FIRST shipto WHERE shipto.company EQ prmComp
           AND shipto.cust-no EQ prmCust
           AND shipto.ship-id EQ prmShipid 
           AND shipto.ship-id NE "TEMP"  NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       bf-quotehd.shipto[1] = shipto.ship-name
       bf-quotehd.shipto[2] = shipto.ship-addr[1]
       bf-quotehd.shipto[3] = shipto.ship-addr[2]
       bf-quotehd.shipto[4] = TRIM(shipto.ship-city) + ", " +
                                        TRIM(shipto.ship-state) + "  " +
                                        TRIM(shipto.ship-zip).

    FIND soldto WHERE soldto.company EQ prmComp
          AND soldto.cust-no EQ prmCust
          AND soldto.sold-id EQ prmSoldid
          AND soldto.sold-id NE "TEMP"
          AND soldto.sold-id NE "" 
        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN
       bf-quotehd.soldto[1] = soldto.sold-name
       bf-quotehd.soldto[2] = soldto.sold-addr[1]
       bf-quotehd.soldto[3] = soldto.sold-addr[2]
       bf-quotehd.soldto[4] = TRIM(soldto.sold-city) + ", " +
                                        TRIM(soldto.sold-state) + "  " +
                                        TRIM(soldto.sold-zip).

FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(quotehd.est-no))) + TRIM(quotehd.est-no).
    
    IF quotehd.est-no <> "" THEN DO:
        IF /*lv-ship-no <> 0 AND*/ lv-ship-id <> quotehd.ship-id AND
            NOT CAN-FIND(FIRST bf-eb
                         WHERE bf-eb.company  EQ quotehd.company
                         AND bf-eb.est-no  EQ quotehd.est-no 
                         AND bf-eb.cust-no EQ quotehd.cust-no
                         AND bf-eb.ship-id NE lv-ship-id)
            THEN DO:
            FOR each bf-eb WHERE bf-eb.company = quotehd.company
                AND bf-eb.est-no = quotehd.est-no 
                AND bf-eb.cust-no = quotehd.cust-no:
                IF bf-eb.ship-id = lv-ship-id THEN DO:
                    FIND FIRST shipto  WHERE shipto.company = quotehd.company
                        AND shipto.cust-no = quotehd.cust-no
                        AND shipto.ship-id = quotehd.ship-id 
                        NO-LOCK NO-ERROR.
                    IF AVAIL shipto THEN ASSIGN bf-eb.ship-no = shipto.ship-no
                                            bf-eb.ship-id = quotehd.ship-id
                                            bf-eb.ship-name = quotehd.shipto[1]
                                            bf-eb.ship-addr[1] = quotehd.shipto[2]
                                            bf-eb.ship-addr[2] = quotehd.shipto[3]
                                            bf-eb.ship-city = shipto.ship-city
                                            bf-eb.ship-state = shipto.ship-state
                                            bf-eb.ship-zip = shipto.ship-zip .
                    END.
             END.
     END.
     IF lv-sman <> quotehd.sman
     THEN FOR each bf-eb WHERE bf-eb.company = quotehd.company
                           AND bf-eb.est-no = quotehd.est-no :
          bf-eb.sman = quotehd.sman.        
     END.  
    END.
  /*IF NOT CAN-FIND(FIRST quoteitm OF quotehd) THEN RUN create-line-items.*/

RELEASE bf-quotehd.
RELEASE bf-quoteitm.

    ASSIGN prmAction = "Select".
END.    /*IF prmAction = "Update" THEN DO:*/

/**********************add***************/

IF prmAction = "AddValdate" THEN DO:
     IF prmCust <> "" THEN DO:
      FIND FIRST cust
          WHERE cust.company EQ prmComp
            AND cust.cust-no EQ prmCust NO-LOCK NO-ERROR.

      IF NOT AVAIL cust THEN DO:
          cError =  "Invalid Bill To, try help..." .
          RETURN .
      END.
    END.


  ASSIGN  prmShipid = CAPS(prmShipid).
  
   FIND FIRST shipto WHERE shipto.company EQ prmComp
                        AND shipto.cust-no EQ prmCust
                        AND shipto.ship-id EQ prmShipid NO-LOCK NO-ERROR.
     IF NOT AVAIL shipto THEN DO:
         cError = "Invalid Ship To, try help..." .
        RETURN .
      END.

  ASSIGN  prmSoldid = CAPS(prmSoldid).

    IF prmSoldid  <> "" THEN DO:
      FIND FIRST soldto
                      WHERE soldto.company EQ prmComp
                        AND soldto.cust-no EQ prmCust
                        AND soldto.sold-id EQ prmSoldid NO-LOCK NO-ERROR.
      IF NOT AVAIL soldto THEN DO:
          cError = "Invalid Sold To, try help..." .
        RETURN .
      END.
    END.

   IF prmSman NE '' THEN DO:
       FIND FIRST sman NO-LOCK WHERE sman.sman EQ prmSman NO-ERROR.
       IF NOT AVAILABLE sman THEN DO:
         cError =  'Invalid Sales Rep. Try Help.' .
        RETURN .
       END.
     END.
     
    
     IF prmTerms NE '' THEN DO:
       FIND FIRST terms NO-LOCK WHERE terms.t-code EQ prmTerms NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
         cError =  'Invalid Terms. Try Help.' .
        RETURN .
       END.     
     END.
     
   
     IF prmCarr EQ '' THEN DO:
       FIND FIRST carrier NO-LOCK WHERE carrier.company EQ prmComp
                                    AND carrier.loc EQ prmLoc
                                    AND carrier.carrier EQ prmCarr NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
         cError = 'Invalid Carrier Code. Try Help.' .
         RETURN .
       END.
     END.

    
     IF prmZone EQ '' THEN DO:
       FIND FIRST carr-mtx NO-LOCK WHERE carr-mtx.company EQ prmComp
                                     AND carr-mtx.loc EQ prmLoc
                                     AND carr-mtx.carrier EQ prmCarr
                                     AND carr-mtx.del-zone EQ prmZone NO-ERROR.
       IF NOT AVAILABLE carr-mtx THEN DO:
         cError =  'Invalid Delivey Zone. Try Help.' .
         RETURN .
       END.
     END.

END.


IF prmAction = "Add" THEN DO:
   
  
   find first bf-hd use-index q-no where bf-hd.company = prmComp and
                                        bf-hd.loc = prmLoc no-lock no-error.
  
     CREATE quotehd.
          assign 
              quotehd.quo-date = TODAY .


          ASSIGN
              
              quotehd.quo-date        = PrmDate     
              quotehd.cust-no         = prmCust
              quotehd.est-no          = prmEst        
              quotehd.rfq             = prmRfq        
              quotehd.del-date        = prmDel        
              quotehd.ship-id         = prmShipid       
              quotehd.contact         = PrmContact   
              quotehd.sold-id         = prmSoldid     
              quotehd.sman            = prmSman    
              quotehd.terms           = prmTerms    
              quotehd.carrier         = prmCarr     
              quotehd.del-zone        = prmZone   
              quotehd.sts             = prmStat      .
        


          if avail bf-hd then 
                    assign
                        quotehd.comment[1] = bf-hd.comment[1]
                        quotehd.comment[2] = bf-hd.comment[2]
                        quotehd.comment[3] = bf-hd.comment[3]
                        quotehd.comment[4] = bf-hd.comment[4]
                        quotehd.comment[5] = bf-hd.comment[5]
                             .  
          

          FIND cust WHERE cust.company EQ prmComp
              AND cust.cust-no EQ prmCust AND cust.cust-no NE "TEMP"
              NO-LOCK NO-ERROR.
          IF AVAIL cust THEN DO:
              ASSIGN
                  quotehd.billto[1] = cust.name
                  quotehd.billto[2] = cust.addr[1]
                  quotehd.billto[3] = cust.addr[2]
                  quotehd.billto[4] = cust.city + ", " +
                                        cust.state + "  " +
                                        cust.zip .
          END.

        FIND FIRST shipto WHERE shipto.company EQ prmComp
           AND shipto.cust-no EQ prmCust
           AND shipto.ship-id EQ prmShipid 
           AND shipto.ship-id NE "TEMP"  NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       quotehd.shipto[1] = shipto.ship-name
       quotehd.shipto[2] = shipto.ship-addr[1]
       quotehd.shipto[3] = shipto.ship-addr[2]
       quotehd.shipto[4] = TRIM(shipto.ship-city) + ", " +
                                        TRIM(shipto.ship-state) + "  " +
                                        TRIM(shipto.ship-zip).

    FIND soldto WHERE soldto.company EQ prmComp
          AND soldto.cust-no EQ prmCust
          AND soldto.sold-id EQ prmSoldid
          AND soldto.sold-id NE "TEMP"
          AND soldto.sold-id NE "" 
        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN
       quotehd.soldto[1] = soldto.sold-name
       quotehd.soldto[2] = soldto.sold-addr[1]
       quotehd.soldto[3] = soldto.sold-addr[2]
       quotehd.soldto[4] = TRIM(soldto.sold-city) + ", " +
                                        TRIM(soldto.sold-state) + "  " +
                                        TRIM(soldto.sold-zip).


    quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(quotehd.est-no))) + TRIM(quotehd.est-no).
    
    IF quotehd.est-no <> "" THEN DO:
        IF /*lv-ship-no <> 0 AND*/ lv-ship-id <> quotehd.ship-id AND
            NOT CAN-FIND(FIRST bf-eb
                         WHERE bf-eb.company  EQ quotehd.company
                         AND bf-eb.est-no  EQ quotehd.est-no 
                         AND bf-eb.cust-no EQ quotehd.cust-no
                         AND bf-eb.ship-id NE lv-ship-id)
            THEN DO:
            FOR each bf-eb WHERE bf-eb.company = quotehd.company
                AND bf-eb.est-no = quotehd.est-no 
                AND bf-eb.cust-no = quotehd.cust-no:
                IF bf-eb.ship-id = lv-ship-id THEN DO:
                    FIND FIRST shipto  WHERE shipto.company = quotehd.company
                        AND shipto.cust-no = quotehd.cust-no
                        AND shipto.ship-id = quotehd.ship-id 
                        NO-LOCK NO-ERROR.
                    IF AVAIL shipto THEN ASSIGN bf-eb.ship-no = shipto.ship-no
                                            bf-eb.ship-id = quotehd.ship-id
                                            bf-eb.ship-name = quotehd.shipto[1]
                                            bf-eb.ship-addr[1] = quotehd.shipto[2]
                                            bf-eb.ship-addr[2] = quotehd.shipto[3]
                                            bf-eb.ship-city = shipto.ship-city
                                            bf-eb.ship-state = shipto.ship-state
                                            bf-eb.ship-zip = shipto.ship-zip .
                    END.
             END.
     END.
     IF lv-sman <> quotehd.sman
     THEN FOR each bf-eb WHERE bf-eb.company = quotehd.company
                           AND bf-eb.est-no = quotehd.est-no :
          bf-eb.sman = quotehd.sman.        
     END.  
    END. 

    /* copy*/

    /*find first bf-quotehd use-index q-no where bf-quotehd.company = prmComp 
                             AND bf-quotehd.loc = prmLoc
                             AND bf-quotehd.q-no = v-prev-q-no no-lock no-error.
     ASSIGN quotehd.comment[1] = bf-quotehd.comment[1]
            quotehd.comment[2] = bf-quotehd.comment[2]
            quotehd.comment[3] = bf-quotehd.comment[3]
            quotehd.comment[4] = bf-quotehd.comment[4]
            quotehd.comment[5] = bf-quotehd.comment[5].

     FOR EACH bf-quoteitm OF bf-quotehd NO-LOCK :
         CREATE quoteitm.
         BUFFER-COPY bf-quoteitm EXCEPT bf-quoteitm.q-no TO quoteitm.
         ASSIGN quoteitm.q-no = quotehd.q-no.

         FOR EACH bf-quoteqty WHERE bf-quoteqty.company = bf-quoteitm.company
                                AND bf-quoteqty.loc = bf-quoteitm.loc
                                AND bf-quoteqty.q-no = bf-quoteitm.q-no
                                AND bf-quoteqty.line = bf-quoteitm.line NO-LOCK:
             CREATE quoteqty.
             BUFFER-COPY bf-quoteqty EXCEPT bf-quoteqty.q-no TO quoteqty.
             ASSIGN quoteqty.q-no = quotehd.q-no.

             FOR EACH bf-quotechg WHERE bf-quotechg.company eq bf-quoteqty.company
                                    AND bf-quotechg.loc eq bf-quoteqty.loc
                                    AND bf-quotechg.q-no eq bf-quoteqty.q-no
                                    AND ((bf-quotechg.line eq bf-quoteqty.line AND bf-quotechg.qty eq bf-quoteqty.qty) OR
                                        (bf-quotechg.LINE eq 0                 AND bf-quotechg.qty eq 0               )) NO-LOCK:
                 CREATE quotechg.
                 BUFFER-COPY bf-quotechg EXCEPT bf-quotechg.q-no TO quotechg.
                 ASSIGN quotechg.q-no = quotehd.q-no.
             END.   
         END.
     END.
/*end of copy */ */


  
       
/*RELEASE bf-quotehd.*/
    ASSIGN
        prmQuote = quotehd.q-no .
    ASSIGN prmAction = "Select".
END.    /*IF prmAction = "Update" THEN DO:*/

IF prmAction = "Delete" THEN DO:
    
 FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote EXCLUSIVE-LOCK NO-ERROR.
 IF AVAIL quotehd THEN
     DELETE quotehd .
FIND LAST quotehd WHERE quotehd.company = prmComp NO-LOCK NO-ERROR.
ASSIGN
  prmQuote = quotehd.q-no 
    prmAction = "Select" .

END.


IF prmAction = "Select" THEN DO:
    FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
    /*FIND FIRST quoteitm WHERE quoteitm.company = quotehd.company AND quoteitm.q-no= quotehd.q-no  NO-LOCK NO-ERROR.*/
    create ttViewQuote.
    assign
        ttViewQuote.vQuote        = quotehd.q-no
        ttViewQuote.vDate         = quotehd.quo-date
        ttViewQuote.vCust         = quotehd.cust-no
        ttViewQuote.vEstimate     = quotehd.est-no
        ttViewQuote.vRfq          = quotehd.rfq
        ttViewQuote.vDelDate      = quotehd.del-date
        ttViewQuote.vBill         = quotehd.billto[1]
        ttViewQuote.vBill2        = quotehd.billto[2]
        ttViewQuote.vBill3        = quotehd.billto[3]
        ttViewQuote.vBill4        = quotehd.billto[4]
        ttViewQuote.vShipid       = quotehd.ship-id
        ttViewQuote.vShip         = quotehd.shipto[1]
        ttViewQuote.vShip2        = quotehd.shipto[2] 
        ttViewQuote.vShip3        = quotehd.shipto[3]
        ttViewQuote.vShip4        = quotehd.shipto[4]
        ttViewQuote.vContact      = quotehd.contact 
        ttViewQuote.vSoldId       = quotehd.sold-id
        ttViewQuote.vSold         = quotehd.soldto[1]
        ttViewQuote.vSold2        = quotehd.soldto[2]
        ttViewQuote.vSold3        = quotehd.soldto[3]
        ttViewQuote.vSold4        = quotehd.soldto[4]
        ttViewQuote.vSman         = quotehd.sman
        ttViewQuote.vTerms        = quotehd.terms
        ttViewQuote.vCarrier      = quotehd.carrier 
        ttViewQuote.vDelZone      = quotehd.del-zone
        ttViewQuote.vStat         = quotehd.sts
        /*ttViewQuote.vPart         = quoteitm.part-no*/
        .  
        if quotehd.sts = "O" then ttViewQuote.vStat  = "Ordered".
        else ttViewQuote.vStat  = "Quote".

    FIND FIRST soldto WHERE soldto.sold-id =  ttViewQuote.vSoldId NO-LOCK NO-ERROR.
    IF NOT AVAIL soldto THEN DO:        
        ASSIGN
        ttViewQuote.vSoldId       = quotehd.cust-no
        ttViewQuote.vSold         = quotehd.billto[1]
        ttViewQuote.vSold2        = quotehd.billto[2]
        ttViewQuote.vSold3        = quotehd.billto[3]
        ttViewQuote.vSold4        = quotehd.billto[4].

    END.

    FIND FIRST carrier  WHERE carrier.company = quotehd.company AND carrier.loc = quotehd.loc
                          AND carrier.carrier EQ quotehd.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttViewQuote.vCarrdscr = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = quotehd.company and terms.t-code  eq quotehd.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttViewQuote.vTermDscr     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = quotehd.company AND sman.sman= quotehd.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttViewQuote.vSname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = quotehd.company and 
                               carr-mtx.loc = quotehd.loc and
                               carr-mtx.carrier = quotehd.carrier and
                               carr-mtx.del-zone = quotehd.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttViewQuote.vZondesc = carr-mtx.del-dscr.
     
END.   /*IF prmAction = "select" THEN DO:*/

/*********************************************************************************/
/*
PROCEDURE create-line-items :

  DEF VAR li AS INT NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN fg/d-invprc.w (quotehd.cust-no).

  FOR EACH tt-inv WHERE tt-inv.selekt,
      FIRST ar-invl WHERE ROWID(ar-invl) EQ tt-inv.row-id NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company EQ ar-invl.company
        AND itemfg.i-no    EQ ar-invl.i-no
      NO-LOCK:
  
    FIND LAST quoteitm
        WHERE quoteitm.company EQ quotehd.company
          AND quoteitm.loc     EQ quotehd.loc
          AND quoteitm.q-no    EQ quotehd.q-no
        USE-INDEX q-line NO-LOCK NO-ERROR.
    li = (IF AVAIL quoteitm THEN quoteitm.line else 0) + 1.

    ASSIGN
     lv-part-no = ""
     lv-rowid   = ROWID(itemfg).
    RUN custom/getcpart.p (cocode, quotehd.cust-no,
                           INPUT-OUTPUT lv-part-no, INPUT-OUTPUT lv-rowid).

    CREATE quoteitm.
    ASSIGN
     quoteitm.company    = quotehd.company
     quoteitm.loc        = quotehd.loc
     quoteitm.q-no       = quotehd.q-no
     quoteitm.line       = li
     quoteitm.upd-date   = TODAY
     quoteitm.upd-user   = USERID("nosweat")
     quoteitm.part-no    = IF lv-part-no EQ "" THEN itemfg.part-no
                                               ELSE lv-part-no
     quoteitm.part-dscr1 = itemfg.i-name
     quoteitm.style      = itemfg.style
     quoteitm.price      = ar-invl.unit-pr
     quoteitm.qty        = ar-invl.inv-qty
     quoteitm.uom        = ar-invl.pr-qty-uom
     quoteitm.size       = STRING(itemfg.l-score[50]) + " x " +
                           STRING(itemfg.w-score[50]) + " x " +
                           STRING(itemfg.d-score[50])
     /*RCO400*/
     quoteitm.i-no = itemfg.i-no.

    CREATE quoteqty.
    ASSIGN
     quoteqty.company    = quoteitm.company
     quoteqty.loc        = quoteitm.loc
     quoteqty.q-no       = quoteitm.q-no
     quoteqty.line       = quoteitm.line
     quoteqty.qty        = quoteitm.qty
     quoteqty.price      = quoteitm.price
     quoteqty.uom        = quoteitm.uom
     quoteqty.quote-date = TODAY
     quoteqty.quote-user = USERID("nosweat").

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-mat-cost,
                           OUTPUT quoteqty.mat-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-lab-cost,
                           OUTPUT quoteqty.lab-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-fix-cost,
                           OUTPUT quoteqty.fo-cost).

    RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                           itemfg.std-var-cost,
                           OUTPUT quoteqty.vo-cost).
  END.

END PROCEDURE.
*/
