&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoa/appServer/aoaAR.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Aged Receivables.rpa */
{aoa/tempTable/ttAgedReceivables.i}

/* Aged Receivables Totals.rpa */
{aoa/tempTable/ttAgedReceivablesTotals.i}

/* Cash Receipt By SalesRep Name.rpa */
{aoa/tempTable/ttCashReceiptBySalesRepName.i}

/* Commision Cash Receipt.rpa */
{aoa/tempTable/ttCommissionCashReceipt.i}

/* CRM Contacts.rpa */
{CRM/ttCRMContacts.i}

/* CRM Customers.rpa */
{CRM/ttCRMCustomers.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fAgedReceivables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAgedReceivables Procedure 
FUNCTION fAgedReceivables RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fAgedReceivablesTotals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAgedReceivablesTotals Procedure 
FUNCTION fAgedReceivablesTotals RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCashReceiptBySalesRepName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCashReceiptBySalesRepName Procedure 
FUNCTION fCashReceiptBySalesRepName RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCRMContacts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCRMContacts Procedure 
FUNCTION fCRMContacts RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCRMCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCRMCustomers Procedure 
FUNCTION fCRMCustomers RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 17.24
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fAgedReceivables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAgedReceivables Procedure 
FUNCTION fAgedReceivables RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
  /*------------------------------------------------------------------------------
    Purpose:  Aged Receivables.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAgedReceivables.
    EMPTY TEMP-TABLE ttAgedReceivablesTotals.
    
    /* subject business logic */
    RUN aoa/BL/r-araged.p (OUTPUT TABLE ttAgedReceivables, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttAgedReceivables:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fAgedReceivablesTotals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAgedReceivablesTotals Procedure 
FUNCTION fAgedReceivablesTotals RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
  /*------------------------------------------------------------------------------
    Purpose:  Aged Receivables Totals.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAgedReceivables.
    EMPTY TEMP-TABLE ttAgedReceivablesTotals.
    
    /* subject business logic */
    RUN aoa/BL/agedtot.p (OUTPUT TABLE ttAgedReceivablesTotals, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttAgedReceivablesTotals:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCashReceiptBySalesRepName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCashReceiptBySalesRepName Procedure 
FUNCTION fCashReceiptBySalesRepName RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
  /*------------------------------------------------------------------------------
    Purpose:  Cash Receipt By SalesRep Name.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCashReceiptBySalesRepName.
    
    /* subject business logic */
    RUN aoa/BL/r-cashs2.p (OUTPUT TABLE ttCashReceiptBySalesRepName, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttCashReceiptBySalesRepName:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Commission Cash Receipt.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCommissionCashReceipt.

    /* subject business logic */
    RUN aoa/BL/r-commcr.p (OUTPUT TABLE ttCommissionCashReceipt, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCRMContacts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCRMContacts Procedure 
FUNCTION fCRMContacts RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  CRM Contacts.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCRMContacts.

    /* subject business logic */
    RUN aoa/BL/crmContacts.p (OUTPUT TABLE ttCRMContacts, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttCRMContacts:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCRMCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCRMCustomers Procedure 
FUNCTION fCRMCustomers RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  CRM Customers.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCRMCustomers.

    /* subject business logic */
    RUN aoa/BL/crmCustomers.p (OUTPUT TABLE ttCRMCustomers, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttCRMCustomers:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Aged Receivables Totals.rpa */
        WHEN "agedtot." THEN
        RETURN TEMP-TABLE ttAgedReceivablesTotals:HANDLE.
        /* Aged Receivables.rpa */
        WHEN "r-araged." THEN
        RETURN TEMP-TABLE ttAgedReceivables:HANDLE.
        /* Cash Receipt By SalesRep Name.rpa */
        WHEN "r-cashs2." THEN
        RETURN TEMP-TABLE ttCashReceiptBySalesRepName:HANDLE.
        /* Commision Cash Receipt.rpa */
        WHEN "r-commcr." THEN
        RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE.
        /* CRM Contacts.rpa */
        WHEN "crmCont." THEN
        RETURN TEMP-TABLE ttCRMContacts:HANDLE.
        /* CRM Customers.rpa */
        WHEN "crmCust." THEN
        RETURN TEMP-TABLE ttCRMCustomers:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

