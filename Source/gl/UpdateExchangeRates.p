
/*------------------------------------------------------------------------
    File        : UpdateExchangeRates.p
    Purpose     : 

    Syntax      :

    Description : Procedure to update exchange rates for a given base currency

    Author(s)   : DEVA$!
    Created     : Thu Dec 02 11:33:16 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.

DEFINE INPUT  PARAMETER ipcCompany                AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcBaseCurrency           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcExchangeRateCurrencies AS CHARACTER NO-UNDO. /* Pipe separated list of currencies for which we need to fetch exchange rate */
DEFINE INPUT  PARAMETER ipdtAsOfDate              AS DATE      NO-UNDO.
DEFINE OUTPUT PARAMETER oplError                  AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                AS CHARACTER NO-UNDO.
/* ***************************  Definitions  ************************** */
{api/ttAPIOutboundEvent.i}

DEFINE TEMP-TABLE ttExchangeRate NO-UNDO
    FIELD company          AS CHARACTER
    FIELD baseCurrencyCode AS CHARACTER
    FIELD rateCurrencyCode AS CHARACTER 
    FIELD asOfDate         AS DATE
    FIELD exchangeRate     AS DECIMAL
    .
    
DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInputParameters AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponseData   AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lIsAPIActive     AS LOGICAL   NO-UNDO.

DEFINE BUFFER bf-currency         FOR currency.
DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
DEFINE BUFFER bf-exchangeRate     FOR exchangeRate.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

IF ipcBaseCurrency EQ "" THEN DO:
    FIND FIRST bf-currency NO-LOCK
         WHERE bf-currency.company EQ ipcCompany
           AND bf-currency.is-base EQ TRUE
         NO-ERROR.
    IF NOT AVAILABLE bf-currency THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Please enter a valid base currency or select a record in Currency Maintenance as base currency"
            .
        RETURN.
    END.    
    
    ipcBaseCurrency = bf-currency.c-code.       
END.

IF ipcExchangeRateCurrencies EQ "" THEN DO:
    FOR EACH bf-currency NO-LOCK
        WHERE bf-currency.company EQ ipcCompany
          AND bf-currency.c-code  NE ipcBaseCurrency:
        ipcExchangeRateCurrencies = ipcExchangeRateCurrencies + "|" + bf-currency.c-code.
    END.    
    
    ipcExchangeRateCurrencies = TRIM(ipcExchangeRateCurrencies, "|").
END.

cInputParameters = IF ipdtAsOfDate EQ ? THEN
                       ""
                   ELSE
                       STRING (ipdtAsOfDate).

cInputParameters = cInputParameters + "," + ipcBaseCurrency + "," + ipcExchangeRateCurrencies.
                       
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN Outbound_IsApiScopeActive IN hdOutboundProcs (
    INPUT  ipcCompany,
    INPUT  "",   /* Location */
    INPUT  "GetExchangeRate",
    INPUT  "",   /* Scope ID */
    INPUT  "",   /* Scope Type */
    INPUT  "GetExchangeRate",
    OUTPUT lIsAPIActive
    ).
IF NOT lIsAPIActive THEN DO:
    ASSIGN
        oplError   = TRUE
        opcMessage = "GetExchangeRate API is not setup"
        .
    RETURN.
END.
    
RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
    INPUT  ipcCompany,                             /* Company Code (Mandatory) */
    INPUT  "",                                     /* Location Code (Mandatory) */
    INPUT  "GetExchangeRate",                      /* API ID (Mandatory) */
    INPUT  "",                                     /* Scope ID */
    INPUT  "",                                     /* Scope Type */
    INPUT  "GetExchangeRate",                      /* Trigger ID (Mandatory) */
    INPUT  "ExchangeRateAsOfDate,BaseCurrency,ExchangeRateSymbols",                           /* Comma separated list of table names for which data being sent (Mandatory) */
    INPUT  cInputParameters,                       /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
    INPUT  "ASI",                                  /* Primary ID for which API is called for (Mandatory) */   
    INPUT  "Exchange rate request",                /* Event's description (Optional) */
    OUTPUT lSuccess,                               /* Success/Failure flag */
    OUTPUT cMessage                                /* Status message */
    ) NO-ERROR.
    
RUN Outbound_GetEvents IN hdOutboundProcs (
    OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
    ).

IF NOT TEMP-TABLE ttAPIOutboundEvent:HAS-RECORDS THEN DO:
    ASSIGN
        oplError   = TRUE
        opcMessage = "Failure in calling the api"
        .
    RETURN.
END.

FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.

FIND FIRST bf-APIOutboundEvent NO-LOCK
     WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
     NO-ERROR.
IF AVAILABLE bf-APIOutboundEvent THEN DO:
    IF NOT bf-APIOutboundEvent.success THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "API failed to fetch exchange rates"
            .
        RETURN.    
    END.
    
    lcResponseData = bf-APIOutboundEvent.responseData.
END.
ELSE DO:
    ASSIGN
        oplError   = TRUE
        opcMessage = "Failure in calling the api"
        .
    RETURN.
END.

RUN pReadResponse (lcResponseData, OUTPUT oplError, OUTPUT opcMessage).


FOR EACH ttExchangeRate:
    FIND FIRST bf-exchangeRate EXCLUSIVE-LOCK
         WHERE bf-exchangeRate.company EQ ttExchangeRate.company
           AND bf-exchangeRate.baseCurrencyCode EQ ttExchangeRate.baseCurrencyCode
           AND bf-exchangeRate.rateCurrencyCode EQ ttExchangeRate.rateCurrencyCode
           AND bf-exchangeRate.asOfDate EQ ttExchangeRate.asOfDate
         NO-ERROR.
    IF NOT AVAILABLE bf-exchangeRate THEN
        CREATE bf-exchangeRate.
    
    BUFFER-COPY ttExchangeRate TO bf-exchangeRate.
END.

FINALLY:
    IF VALID-HANDLE (hdOutboundProcs) THEN
        DELETE PROCEDURE hdOutboundProcs.    		
END FINALLY.    


/* **********************  Internal Procedures  *********************** */

PROCEDURE pReadResponse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oModelParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE oObject          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joError          AS JsonObject        NO-UNDO.
    DEFINE VARIABLE joRates          AS JsonObject        NO-UNDO.
    
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
            opcMessage = "Could not get any response from API"
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
            opcMessage = "Could not get base currency from API"
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
            opcMessage = "Error parsing date from API"
            .
        
        RETURN.            
    END.
    
    joRates = oObject:GetJsonObject("rates") NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN DO:
        cRateCurrency = joRates:GetNames().
        DO iIndex = 1 TO EXTENT (cRateCurrency):
            CREATE ttExchangeRate.
            ASSIGN
                ttExchangeRate.company = ipcCompany
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