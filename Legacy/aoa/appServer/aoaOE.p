&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoa/appServer/aoaOE.p
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

/* BOL Packing List.rpa */
{aoa/tempTable/ttBOLPackingList.i}

/* Invoice Post Update GL.rpa */
{aoa/tempTable/ttInvoicePostUpdateGL.i}

/* Orders Booked.rpa */
{aoa/tempTable/ttOrdersBooked.i}

/* Order Acknowledgements.rpa */
{aoa/tempTable/ttOrderAcknowledgements.i}

/* Open Order Report.rpa */
{aoa/tempTable/ttOpenOrderReport.i}

/* Orders Booked By Order No.rpa */
{aoa/tempTable/ttOrdersBookedByOrderNo.i}

/* Post BOL Create Invoice.rpa */
{aoa/tempTable/ttPostBolCreateInvoice.i}

/* Recap Product Category.rpa */
{aoa/tempTable/ttRecapProductCategory.i}

{sys/ref/CustList.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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

&IF DEFINED(EXCLUDE-fInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInvoicePostUpdateGL Procedure 
FUNCTION fInvoicePostUpdateGL RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOpenOrderReport Procedure 
FUNCTION fOpenOrderReport RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrderAcknowledgements Procedure 
FUNCTION fOrderAcknowledgements RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrdersBookedByOrderNo Procedure 
FUNCTION fOrdersBookedByOrderNo RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPostBOLCreateInvoice Procedure 
FUNCTION fPostBOLCreateInvoice RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRecapProductCategory Procedure 
FUNCTION fRecapProductCategory RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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
         HEIGHT             = 23.57
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLPackingList Procedure 
PROCEDURE pBOLPackingList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/bolpcklst.p (OUTPUT TABLE ttBOLPackingList, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInvoicePostUpdateGL Procedure 
PROCEDURE pInvoicePostUpdateGL :
/*------------------------------------------------------------------------------
  Purpose:     Invoice Post Update GL.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/r-inve&pb.p (OUTPUT TABLE ttInvoicePostUpdateGL, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenOrderReport Procedure 
PROCEDURE pOpenOrderReport :
/*------------------------------------------------------------------------------
  Purpose:     Open Order Report.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/r-ordopn.p (OUTPUT TABLE ttOpenOrderReport, ipcCompany, ipiBatch, ipcUserID).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrderAcknowledgements Procedure 
PROCEDURE pOrderAcknowledgements :
/*------------------------------------------------------------------------------
  Purpose:     Order Acknowledgements.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/orderack.p (OUTPUT TABLE ttOrderAcknowledgements, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked Procedure 
PROCEDURE pOrdersBooked :
/*------------------------------------------------------------------------------
Purpose:     Orders Booked.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/r-booked.p (OUTPUT TABLE ttOrdersBooked, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBookedByOrderNo Procedure 
PROCEDURE pOrdersBookedByOrderNo :
/*------------------------------------------------------------------------------
Purpose:     Orders Booked by Order No.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/r-booko#.p (OUTPUT TABLE ttOrdersBookedByOrderNo, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostBOLCreateInvoice Procedure 
PROCEDURE pPostBOLCreateInvoice :
/*------------------------------------------------------------------------------
  Purpose:     Post BOL Create Invoice.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/r-bolpst.p (OUTPUT TABLE ttPostBOLCreateInvoice, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRecapProductCategory Procedure 
PROCEDURE pRecapProductCategory :
/*------------------------------------------------------------------------------
Purpose:     Recap Product Category.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoa/includes/aoaInputDefParams.i}
    
    /* subject business logic */
    RUN aoa/BL/recappc.p (OUTPUT TABLE ttRecapProductCategory, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  BOL Packing List.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttBOLPackingList.

    RUN pBOLPackingList (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttBOLPackingList:HANDLE .

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
        /* BOL Packing List.rpa */
        WHEN "bolpcklst." THEN
        RETURN TEMP-TABLE ttBOLPackingList:HANDLE.
        /* Print Order Acknowledgements.rpa */
        WHEN "r-acknow." THEN
        RETURN TEMP-TABLE ttOrderAcknowledgements:HANDLE.
        /* Order Booked.rpa */
        WHEN "r-booked." THEN 
        RETURN TEMP-TABLE ttOrdersBooked:HANDLE.
        /* Order Booked By Order No.rpa */
        WHEN "r-booko#." THEN
        RETURN TEMP-TABLE ttOrdersBookedByOrderNo:HANDLE.
        /* Open Order Repoer.rpa */
        WHEN "r-ordopn." THEN
        RETURN TEMP-TABLE ttOpenOrderReport:HANDLE.
        /* Post BOL Create Invoice.rpa */
        WHEN "r-bolpst." THEN
        RETURN TEMP-TABLE ttPostBOLCreateInvoice:HANDLE.
        /* Invoice Post Update GL.rpa */
        WHEN "r-inve&p." THEN
        RETURN TEMP-TABLE ttInvoicePostUpdateGL:HANDLE.
        /* Recap Product Category.rpa */
        WHEN "recappc." THEN
        RETURN TEMP-TABLE ttRecapProductCategory:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInvoicePostUpdateGL Procedure 
FUNCTION fInvoicePostUpdateGL RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Post BOL Create Invoice.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInvoicePostUpdateGL.
    
    RUN pInvoicePostUpdateGL (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttInvoicePostUpdateGL:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOpenOrderReport Procedure 
FUNCTION fOpenOrderReport RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Open Order  Report.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOpenOrderReport.
    
    RUN pOpenOrderReport (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOpenOrderReport:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrderAcknowledgements Procedure 
FUNCTION fOrderAcknowledgements RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose: OrderAcknowledgements.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrderAcknowledgements.
    
    RUN pOrderAcknowledgements (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrderAcknowledgements:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Order Booked.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBooked.
    EMPTY TEMP-TABLE ttRecapProductCategory.

    RUN pOrdersBooked (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrdersBooked:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrdersBookedByOrderNo Procedure 
FUNCTION fOrdersBookedByOrderNo RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Orders Booked By Order No.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBookedByOrderNo.
    
    RUN pOrdersBookedByOrderNo (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrdersBookedByOrderNo:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPostBOLCreateInvoice Procedure 
FUNCTION fPostBOLCreateInvoice RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Post BOL Create Invoice.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttPostBOLCreateInvoice.
    
    RUN pPostBOLCreateInvoice (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttPostBOLCreateInvoice:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRecapProductCategory Procedure 
FUNCTION fRecapProductCategory RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Recap Product Category.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBooked.
    EMPTY TEMP-TABLE ttRecapProductCategory.
    
    RUN pRecapProductCategory (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttRecapProductCategory:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

