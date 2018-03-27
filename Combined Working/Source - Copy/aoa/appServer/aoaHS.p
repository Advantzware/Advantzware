&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoa/appServer/aoaHS.p
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

/* Customer Item By Month.rpa */
{aoa/tempTable/ttCustomerItemByMonth.i}
{aoa/tempTable/ttCustomerItemByMonthDetail.i}
{aoa/tempTable/ttCustomerItemByMonthSummary.i}

/* Invoice Highlights.edp */
{aoa/tempTable/ttInvoiceHighlights.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fCustomerItemByMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCustomerItemByMonth Procedure
FUNCTION fCustomerItemByMonth RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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

&IF DEFINED(EXCLUDE-fInvoiceHighlights) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInvoiceHighlights Procedure 
FUNCTION fInvoiceHighlights RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCustomerItemByMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCustomerItemByMonth Procedure
FUNCTION fCustomerItemByMonth RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer Item By Month.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCustomerItemByMonth.
    EMPTY TEMP-TABLE ttCustomerItemByMonthDetail.
    EMPTY TEMP-TABLE ttCustomerItemByMonthSummary.

    /* subject business logic */
    RUN aoa/BL/custitem.p (OUTPUT TABLE ttCustomerItemByMonth, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttCustomerItemByMonth:HANDLE .

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
        /* Customer Item By Month.rpa */
        WHEN "custitem." THEN 
        RETURN TEMP-TABLE ttCustomerItemByMonth:HANDLE.
        /* Invoice Highlights.edp */
        WHEN "dbinvmh." THEN
        RETURN TEMP-TABLE ttInvoiceHighlights:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fInvoiceHighlights) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInvoiceHighlights Procedure 
FUNCTION fInvoiceHighlights RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Invoice Hightlights.edp
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInvoiceHighlights.

    /* subject business logic */
    RUN aoa/BL/dbinvmh.p (OUTPUT TABLE ttInvoiceHighlights, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttInvoiceHighlights:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

