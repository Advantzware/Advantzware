




/*------------------------------------------------------------------------
    File        : BrwsQuote.p
    Purpose     : Quote Maintenance

    Syntax      :

    Description : Return a Dataset of Quote Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBrwsRfqQuote NO-UNDO
        FIELD abc AS INT 
        FIELD vQuote          AS INTEGER FORMAT ">>>>>9"
        FIELD vDate           AS DATE    FORMAT "99/99/9999"
        FIELD vCust           AS CHAR    FORMAT "x(8)"
        FIELD vContact        AS CHAR    FORMAT "x(30)"
        FIELD vEstimate       AS CHAR    FORMAT "x(8)"
        FIELD vRfq            AS CHAR    FORMAT "x(10)"
        FIELD VPart           AS CHAR    FORMAT "x(20)"
        
        . 
DEFINE DATASET dsBrwsRfqQuote FOR ttBrwsRfqQuote .

DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     as INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate      AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmContact   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRfq       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPart      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBrwsRfqQuote.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE prmComp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE q-noValue1  AS INT NO-UNDO.
DEFINE VARIABLE q-noValue2  AS INT NO-UNDO.
DEFINE VARIABLE vEst   AS CHARACTER  NO-UNDO.
IF prmUser = ? THEN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmAction = "" THEN ASSIGN prmAction = "Select".
IF prmQuote = ? THEN ASSIGN prmQuote = 0.

IF prmCustomer = ? THEN ASSIGN prmCustomer = "".
IF prmContact = ? THEN ASSIGN prmContact = "".
IF prmEstimate = ? THEN ASSIGN prmEstimate = "".
IF prmRfq = ? THEN ASSIGN prmRfq = "".
IF prmPart = ? THEN ASSIGN prmPart = "".
/* ********************  Preprocessor Definitions  ******************** */

FOR EACH ttBrwsRfqQuote NO-LOCK:
    DELETE ttBrwsRfqQuote.
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "jyoti" prmAction prmUser prmRfq.
IF prmAction = "Select" THEN DO:
   ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) .
        FOR EACH quotehd WHERE quotehd.rfq  = prmRfq
            ,EACH quoteitm OF quotehd NO-LOCK :
            IF quotehd.rfq <> "" THEN DO:
                create ttBrwsRfqQuote.
                assign
                    ttBrwsRfqQuote.vQuote        = quotehd.q-no
                    ttBrwsRfqQuote.vDate         = quotehd.quo-date
                    ttBrwsRfqQuote.vCust         = quotehd.cust-no
                    ttBrwsRfqQuote.vContact      = quotehd.contact
                    ttBrwsRfqQuote.vEstimate     = quotehd.est-no
                    ttBrwsRfqQuote.vRfq          = quotehd.rfq
                    ttBrwsRfqQuote.VPart         = quoteitm.part-no.
            END.  /*if then do:*/
          END. /* For Each quotehd*/
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

    
IF prmAction = "Search" THEN DO:
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) .
    FOR EACH quotehd WHERE (quotehd.q-no = prmQuote or prmQuote = 0 )
                           AND (quotehd.cust-no BEGINS prmCustomer or prmCustomer = "" )
                           AND (quotehd.quo-date >= prmDate or prmDate = ? )
                           AND (quotehd.contact BEGINS prmContact OR prmContact = "" )
                           AND (quotehd.est-no  = vEst or vEst = "" )
                               AND (quotehd.rfq BEGINS prmRfq or prmRfq = "" ) NO-LOCK:
             FOR EACH quoteitm OF quotehd WHERE (quoteitm.part-no BEGINS prmPart OR  prmPart = "") NO-LOCK:
                          
            create ttBrwsRfqQuote.
            assign
                ttBrwsRfqQuote.vQuote        = quotehd.q-no
                ttBrwsRfqQuote.vDate         = quotehd.quo-date
                ttBrwsRfqQuote.vCust         = quotehd.cust-no
                ttBrwsRfqQuote.vContact      = quotehd.contact
                ttBrwsRfqQuote.vEstimate     = quotehd.est-no
                ttBrwsRfqQuote.vRfq          = quotehd.rfq
                ttBrwsRfqQuote.VPart         = quoteitm.part-no
               .
             END.
    END. /* For Each oe-ordl*/
END.   /*if prmAction = "Search"*/

/*************************************************/
                   


