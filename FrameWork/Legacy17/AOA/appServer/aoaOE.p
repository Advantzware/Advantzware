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
{aoa/tempTable/ttOpenOrderReportDetail.i}

/* Orders Booked By Order No.rpa */
{aoa/tempTable/ttOrdersBookedByOrderNo.i}

/* Post BOL Create Invoice.rpa */
{aoa/tempTable/ttPostBolCreateInvoice.i}

/* Recap Product Category.rpa */
{aoa/tempTable/ttRecapProductCategory.i}

/* Scheduled Releases.rpa */
{aoa/tempTable/ttScheduledReleases.i}
{aoa/tempTable/ttScheduledReleasesNotes.i}
{aoa/tempTable/ttScheduledReleasesStats.i}

/* Shipment Report.rpa */
{aoa/tempTable/ttShipmentReport.i}
{aoa/tempTable/ttShipmentReportDetail.i}
{aoa/tempTable/ttShipmentReportSummary.i}

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

&IF DEFINED(EXCLUDE-fOpenOrderReportDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOpenOrderReportDetail Procedure 
FUNCTION fOpenOrderReportDetail RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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

&IF DEFINED(EXCLUDE-fScheduledReleases) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fScheduledReleases Procedure 
FUNCTION fScheduledReleases RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fScheduledReleasesNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fScheduledReleasesNotes Procedure
FUNCTION fScheduledReleasesNotes RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fScheduledReleasesStats) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fScheduledReleasesStats Procedure
FUNCTION fScheduledReleasesStats RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fShipmentReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fShipmentReport Procedure 
FUNCTION fShipmentReport RETURNS HANDLE ( {aoa/includes/fInputVars.i} )  FORWARD.

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


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  BOL Packing List.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttBOLPackingList.

    /* subject business logic */
    RUN aoa/BL/bolpcklst.p (OUTPUT TABLE ttBOLPackingList, ipcCompany, ipiBatch, ipcUserID).

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
        /* Scheduled Releases.rpa */
        WHEN "r-sched." THEN
        RETURN TEMP-TABLE ttScheduledReleases:HANDLE.
        /* Shipment Report.rpa */
        WHEN "shiprpt." THEN
        RETURN TEMP-TABLE ttShipmentReport:HANDLE.
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
    
    /* subject business logic */
    RUN aoa/BL/r-inve&pb.p (OUTPUT TABLE ttInvoicePostUpdateGL, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttInvoicePostUpdateGL:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOpenOrderReport Procedure 
FUNCTION fOpenOrderReport RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Open Order Report.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOpenOrderReport.
    EMPTY TEMP-TABLE ttOpenOrderReportDetail.
    
    /* subject business logic */
    RUN aoa/BL/r-ordopn.p (OUTPUT TABLE ttOpenOrderReport,
                           OUTPUT TABLE ttOpenOrderReportDetail,
                           ipcCompany, ipiBatch, ipcUserID
                           ).
    
    RETURN TEMP-TABLE ttOpenOrderReport:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReportDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOpenOrderReportDetail Procedure 
FUNCTION fOpenOrderReportDetail RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
Purpose:  Open Order Report Detail.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOpenOrderReportDetail.
    
    /* subject business logic */
    RUN aoa/BL/r-ordopnDetail.p (OUTPUT TABLE ttOpenOrderReportDetail, ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttOpenOrderReportDetail:HANDLE.

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
    
    /* subject business logic */
    RUN aoa/BL/orderack.p (OUTPUT TABLE ttOrderAcknowledgements, ipcCompany, ipiBatch, ipcUserID).
    
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

    /* subject business logic */
    RUN aoa/BL/r-booked.p (OUTPUT TABLE ttOrdersBooked, ipcCompany, ipiBatch, ipcUserID).
    
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
    
    /* subject business logic */
    RUN aoa/BL/r-booko#.p (OUTPUT TABLE ttOrdersBookedByOrderNo, ipcCompany, ipiBatch, ipcUserID).
    
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
    
    /* subject business logic */
    RUN aoa/BL/r-bolpst.p (OUTPUT TABLE ttPostBOLCreateInvoice, ipcCompany, ipiBatch, ipcUserID).
    
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
    
    /* subject business logic */
    RUN aoa/BL/recappc.p (OUTPUT TABLE ttRecapProductCategory, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttRecapProductCategory:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fScheduledReleases) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fScheduledReleases Procedure 
FUNCTION fScheduledReleases RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Scheduled Releases.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttScheduledReleases.
    EMPTY TEMP-TABLE ttScheduledReleasesNotes.
    EMPTY TEMP-TABLE ttScheduledReleasesStats.
    
    /* subject business logic */
    RUN aoa/BL/r-sched.p (OUTPUT TABLE ttScheduledReleases,
                          OUTPUT TABLE ttScheduledReleasesNotes,
                          OUTPUT TABLE ttScheduledReleasesStats,
                          ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttScheduledReleases:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fScheduledReleasesNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fScheduledReleasesNotes Procedure
FUNCTION fScheduledReleasesNotes RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Scheduled Releases.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttScheduledReleasesNotes.
    
    /* subject business logic */
    RUN aoa/BL/r-schedNotes.p (OUTPUT TABLE ttScheduledReleasesNotes, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttScheduledReleasesNotes:HANDLE .

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fScheduledReleasesStats) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fScheduledReleasesStats Procedure
FUNCTION fScheduledReleasesStats RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Scheduled Releases.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttScheduledReleasesStats.
    
    /* subject business logic */
    RUN aoa/BL/r-schedStats.p (OUTPUT TABLE ttScheduledReleasesStats, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttScheduledReleasesStats:HANDLE .

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fShipmentReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fShipmentReport Procedure 
FUNCTION fShipmentReport RETURNS HANDLE ( {aoa/includes/fInputVars.i} ) :
/*------------------------------------------------------------------------------
  Purpose:  Shipment Report.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttShipmentReport.
    EMPTY TEMP-TABLE ttShipmentReportDetail.
    EMPTY TEMP-TABLE ttShipmentReportSummary.
    
    /* subject business logic */
    RUN aoa/BL/shiprpt.p (OUTPUT TABLE ttShipmentReport, ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttShipmentReport:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

