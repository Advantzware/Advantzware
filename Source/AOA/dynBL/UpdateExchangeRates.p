/*------------------------------------------------------------------------
  File:         UpdateExchangeRates.p
  Description:  Business Logic
  Author:       Rajesh (Ron Stark)
  Date Created: 12.10.2021
------------------------------------------------------------------------*/

BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttExchangeRate
DEFINE TEMP-TABLE ttExchangeRate NO-UNDO
    FIELD company          AS CHARACTER FORMAT "x(3)"            LABEL "Company"
    FIELD baseCurrencyCode AS CHARACTER FORMAT "x(3)"            LABEL "Base Currency"
    FIELD rateCurrencyCode AS CHARACTER FORMAT "x(3)"            LABEL "Rate Currency"
    FIELD asOfDate         AS DATE      FORMAT "99/99/9999"      LABEL "As Of Date"
    FIELD exchangeRate     AS DECIMAL   FORMAT "->>,>>9.99<<<<<" LABEL "Exchange Rate"
    .
{api/ttAPIOutboundEvent.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 198
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cExchangeRateCurrencies AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cInputParameters AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lIsAPIActive     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcResponseData   AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bAPIOutboundEvent FOR APIOutboundEvent.
    DEFINE BUFFER bCurrency         FOR currency.
    DEFINE BUFFER bExchangeRate     FOR exchangeRate.
    
    IF cCurrency EQ "" THEN DO:
        FIND FIRST bCurrency NO-LOCK
             WHERE bCurrency.company EQ cCompany
               AND bCurrency.is-base EQ TRUE
             NO-ERROR.
        IF NOT AVAILABLE bCurrency THEN DO:
            ASSIGN
                lError   = TRUE
                cMessage = "Please Enter a Valid Base Currency or Select a Record in Currency Maintenance as Base Currency"
                .
            RETURN.
        END.        
        cCurrency = bCurrency.c-code.       
    END.
    
    IF cExchangeRateCurrencies EQ "" THEN DO:
        FOR EACH bCurrency NO-LOCK
            WHERE bCurrency.company EQ cCompany
              AND bCurrency.c-code  NE cCurrency
            :
            cExchangeRateCurrencies = cExchangeRateCurrencies + bCurrency.c-code + "|".
        END.        
        cExchangeRateCurrencies = TRIM(cExchangeRateCurrencies, "|").
    END.
    
    cInputParameters = IF dtAsOfDate EQ ? THEN ""
                       ELSE STRING(dtAsOfDate).
    
    cInputParameters = cInputParameters + "," + cCurrency + "," + cExchangeRateCurrencies.
                           
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    RUN Outbound_IsApiScopeActive IN hdOutboundProcs (
        cCompany,
        "", /* Location */
        "GetExchangeRate",
        "", /* Scope ID */
        "", /* Scope Type */
        "GetExchangeRate",
        OUTPUT lIsAPIActive
        ).
    IF NOT lIsAPIActive THEN DO:
        ASSIGN
            lError   = TRUE
            cMessage = "GetExchangeRate API is NOT Setup"
            .
        RETURN.
    END.
        
    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
        cCompany,                /* Company Code (Mandatory) */
        "",                      /* Location Code (Mandatory) */
        "GetExchangeRate",       /* API ID (Mandatory) */
        "",                      /* Scope ID */
        "",                      /* Scope Type */
        "GetExchangeRate",       /* Trigger ID (Mandatory) */
        "ExchangeRateAsOfDate,BaseCurrency,ExchangeRateSymbols", /* Comma separated list of table names for which data being sent (Mandatory) */
        cInputParameters,        /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
        "ASI",                   /* Primary ID for which API is called for (Mandatory) */   
        "Exchange rate request", /* Event's description (Optional) */
        OUTPUT lSuccess,         /* Success/Failure flag */
        OUTPUT cMessage          /* Status message */
        ) NO-ERROR.
        
    RUN Outbound_GetEvents IN hdOutboundProcs (
        OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
        ).
    
    IF NOT TEMP-TABLE ttAPIOutboundEvent:HAS-RECORDS THEN DO:
        ASSIGN
            lError   = TRUE
            cMessage = "Failure in Calling the API"
            .
        RETURN.
    END.
    
    FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
    
    FIND FIRST bAPIOutboundEvent NO-LOCK
         WHERE bAPIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
         NO-ERROR.
    IF AVAILABLE bAPIOutboundEvent THEN DO:
        IF NOT bAPIOutboundEvent.success THEN DO:
            ASSIGN
                lError   = TRUE
                cMessage = "API Failed to Fetch Exchange Rates"
                .
            RETURN.    
        END.
        lcResponseData = bAPIOutboundEvent.responseData.
    END.
    ELSE DO:
        ASSIGN
            lError   = TRUE
            cMessage = "Failure in Calling the API"
            .
        RETURN.
    END.
    
    RUN pReadResponse (lcResponseData, OUTPUT lError, OUTPUT cMessage).    
    
    FOR EACH ttExchangeRate:
        FIND FIRST bExchangeRate EXCLUSIVE-LOCK
             WHERE bExchangeRate.company          EQ ttExchangeRate.company
               AND bExchangeRate.baseCurrencyCode EQ ttExchangeRate.baseCurrencyCode
               AND bExchangeRate.rateCurrencyCode EQ ttExchangeRate.rateCurrencyCode
               AND bExchangeRate.asOfDate         EQ ttExchangeRate.asOfDate
             NO-ERROR.
        IF NOT AVAILABLE bExchangeRate THEN
            CREATE bExchangeRate.        
        BUFFER-COPY ttExchangeRate TO bExchangeRate.
    END.

    FINALLY:
        IF VALID-HANDLE (hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.    		
    END FINALLY.    

END PROCEDURE.

PROCEDURE pReadResponse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser   AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joError        AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joRates        AS JsonObject        NO-UNDO.
    
    DEFINE VARIABLE lcResponseData AS LONGCHAR  NO-UNDO.    
    DEFINE VARIABLE cSuccess       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBaseCurrency  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAsOfDate      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtAsOfDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE cRateCurrency  AS CHARACTER NO-UNDO EXTENT.
    DEFINE VARIABLE dExchangeRate  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER   NO-UNDO.
        
    oModelParser = NEW ObjectModelParser().

    FIX-CODEPAGE(lcResponseData) = 'utf-8'.
    
    IF iplcResponseData EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Could Not Get Any Response from API"
            .
        RETURN.
    END.
            
    ASSIGN
        lcResponseData = iplcResponseData
        oObject        = CAST(oModelParser:Parse(INPUT lcResponseData),JsonObject)
        NO-ERROR.
    
    cSuccess = oObject:GetJsonText("success") NO-ERROR.
    
    IF cSuccess NE "" THEN
        oplError = NOT LOGICAL(cSuccess).
    
    IF oplError THEN DO:
        joError = oObject:GetJsonObject("error") NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:
            opcMessage = joError:GetJsonText ("message") NO-ERROR.
            oplError = TRUE.
            RETURN.        
        END. 
    END.
    
    cBaseCurrency = oObject:GetJsonText("base") NO-ERROR.
    
    IF cBaseCurrency EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Could Not Get Base Currency from API"
            .
        RETURN.        
    END.
    
    cAsOfDate = oObject:GetJsonText("date") NO-ERROR.

    IF cAsOfDate EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Could not get date from API"
            .
        RETURN.        
    END.
    
    dtAsOfDate = DATE(INTEGER(ENTRY(2, cAsOfDate, "-")), INTEGER(ENTRY(3, cAsOfDate, "-")), INTEGER(ENTRY(1, cAsOfDate, "-"))) NO-ERROR.
    
    IF dtAsOfDate EQ ? THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Error Parsing Date from API"
            .
        RETURN.            
    END.
    
    joRates = oObject:GetJsonObject("rates") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        cRateCurrency = joRates:GetNames().
        DO iIndex = 1 TO EXTENT(cRateCurrency):
            CREATE ttExchangeRate.
            ASSIGN
                ttExchangeRate.company          = cCompany
                ttExchangeRate.baseCurrencyCode = cBaseCurrency
                ttExchangeRate.rateCurrencyCode = cRateCurrency[iIndex]
                ttExchangeRate.asOfDate         = dtAsOfDate
                .
            ttExchangeRate.exchangeRate = joRates:GetDecimal(cRateCurrency[iIndex]) NO-ERROR.        
        END.
    END.

    FINALLY:
        IF VALID-OBJECT (oModelParser) THEN
            DELETE OBJECT oModelParser.
           
        IF VALID-OBJECT (oObject) THEN
            DELETE OBJECT oObject.
    
        IF VALID-OBJECT (joError) THEN
            DELETE OBJECT joError.     

        IF VALID-OBJECT (joRates) THEN
            DELETE OBJECT joRates.                 		
    END FINALLY.

END PROCEDURE.

PROCEDURE pUpdateExchangeRates:
    DEFINE INPUT  PARAMETER ipcCompany                AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBaseCurrency           AS CHARACTER NO-UNDO.
    /* Pipe separated list of currencies for which we need to fetch exchange rate */
    DEFINE INPUT  PARAMETER ipcExchangeRateCurrencies AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtAsOfDate              AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError                  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.

    ASSIGN
        cCompany                = ipcCompany
        cCurrency               = ipcBaseCurrency
        cExchangeRateCurrencies = ipcExchangeRateCurrencies
        dtAsOfDate              = ipdtAsOfDate
        .
    RUN pBusinessLogic.
    ASSIGN
        oplError                = lError
        opcMessage              = cMessage
        .

END PROCEDURE.
