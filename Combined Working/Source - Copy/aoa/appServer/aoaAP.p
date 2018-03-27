&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoa/appServer/aoaAP.p
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

/* AP Invoice Posting.rpa */
{aoa/tempTable/ttAPInvoicePosting.i}
{aoa/tempTable/ttAPInvoicePostingGL.i}
{aoa/tempTable/ttAPInvoicePostingSummary.i}
{aoa/tempTable/ttAPInvoicePostingMsg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fAPInvoicePosting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAPInvoicePosting Procedure 
FUNCTION fAPInvoicePosting RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fAPInvoicePostingGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAPInvoicePostingGL Procedure
FUNCTION fAPInvoicePostingGL RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fAPInvoicePostingMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAPInvoicePostingMsg Procedure
FUNCTION fAPInvoicePostingMsg RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fAPInvoicePostingSummary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAPInvoicePostingSummary Procedure
FUNCTION fAPInvoicePostingSummary RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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

&IF DEFINED(EXCLUDE-fAPInvoicePosting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAPInvoicePosting Procedure 
FUNCTION fAPInvoicePosting RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  AP Invoice Posting.rpa.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAPInvoicePosting.
    EMPTY TEMP-TABLE ttAPInvoicePostingGL.
    EMPTY TEMP-TABLE ttAPInvoicePostingSummary.
    
    /* subject business logic */
    RUN aoa/BL/r-apve&p.p (OUTPUT TABLE ttAPInvoicePosting,
                           OUTPUT TABLE ttAPInvoicePostingGL,
                           OUTPUT TABLE ttAPInvoicePostingSummary,
                           ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttAPInvoicePosting:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fAPInvoicePostingGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAPInvoicePostingGL Procedure
FUNCTION fAPInvoicePostingGL RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  AP Invoice Posting.rpa.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAPInvoicePostingGL.
    
    /* subject business logic */
    RUN aoa/BL/r-apve&pGL.p (OUTPUT TABLE ttAPInvoicePostingGL, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttAPInvoicePostingGL:HANDLE.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fAPInvoicePostingMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAPInvoicePostingMsg Procedure
FUNCTION fAPInvoicePostingMsg RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
 Purpose:  AP Invoice Posting.rpa.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAPInvoicePostingMsg.
    
    /* subject business logic */
    RUN aoa/BL/r-apve&pMsg.p (OUTPUT TABLE ttAPInvoicePostingMsg, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttAPInvoicePostingMsg:HANDLE.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fAPInvoicePostingSummary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAPInvoicePostingSummary Procedure
FUNCTION fAPInvoicePostingSummary RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  AP Invoice Posting.rpa.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAPInvoicePostingSummary.
    
    /* subject business logic */
    RUN aoa/BL/r-apve&pSummary.p (OUTPUT TABLE ttAPInvoicePostingSummary, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttAPInvoicePostingSummary:HANDLE.

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
        /* AP Invoice Posting.rpa */
        WHEN "r-apve&p." THEN
        RETURN TEMP-TABLE ttAPInvoicePosting:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

