&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: api/APIOutboundTest.w

  Description: Will allow the user to test the API Outbound events

  Input Parameters:

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 06/22/2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{api/ttArgs.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO INITIAL "001".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 fiAPIId Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fiSSLEnabled fiClientID fiAPIIDLabel ~
fiAPIId fiSSLEnabledlb fiClientIDlb fiRequestVerb fiReqDataType fiAuthType ~
fiReqDataTypelb fiRequestVerb-2 fiRequestVerblb fiPrimaryKeyLabel ~
fiPrimaryKey fiEndPointLabel edEndpoint fiRequestDataLabel edRequestData ~
fiResponseDataLabel edResponseData fiErrorMessageLabel edErrorMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Cancel" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON btSubmit 
     IMAGE-UP FILE "Graphics/32x32/check.ico":U
     LABEL "Submit" 
     SIZE 20 BY 2.62
     BGCOLOR 8 .

DEFINE BUTTON btUpdateRequest 
     LABEL "Update Request" 
     SIZE 23 BY 1.33
     FONT 35.

DEFINE VARIABLE edEndpoint AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.62
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edErrorMessage AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 2.48
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edRequestData AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 5.71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE edResponseData AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 119 BY 5.95
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiAPIIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API ID:" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiAuthType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12.2 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiClientID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiClientIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Client ID:" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiEndPointLabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Point:" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiErrorMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Result:" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiPrimaryKey AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiPrimaryKeyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Key:" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiReqDataTypelb AS CHARACTER FORMAT "X(256)":U INITIAL "Req Datatype:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerb AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerb-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Auth Type:" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerblb AS CHARACTER FORMAT "X(256)":U INITIAL "Request Verb:" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiResponseDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Data:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiSSLEnabled AS LOGICAL FORMAT "YES/NO":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 12.2 BY 1.19
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiSSLEnabledlb AS CHARACTER FORMAT "X(256)":U INITIAL "SSL Enabled:" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 6.1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 126 BY 23.1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 120 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiSSLEnabled AT ROW 1.76 COL 76.8 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fiClientID AT ROW 1.76 COL 109.8 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiAPIIDLabel AT ROW 1.81 COL 9.4 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 1.81 COL 22.4 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     fiSSLEnabledlb AT ROW 1.86 COL 57.4 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fiClientIDlb AT ROW 1.86 COL 94.4 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiRequestVerb AT ROW 3.38 COL 115.4 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     fiReqDataType AT ROW 3.48 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiAuthType AT ROW 3.48 COL 76.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     fiReqDataTypelb AT ROW 3.52 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiRequestVerb-2 AT ROW 3.52 COL 57.4 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     fiRequestVerblb AT ROW 3.52 COL 94.4 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     btUpdateRequest AT ROW 5.33 COL 70.8 WIDGET-ID 108
     fiPrimaryKeyLabel AT ROW 5.52 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiPrimaryKey AT ROW 5.52 COL 31.2 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiEndPointLabel AT ROW 8 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     edEndpoint AT ROW 9.19 COL 11 NO-LABEL WIDGET-ID 14
     fiRequestDataLabel AT ROW 12.1 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     edRequestData AT ROW 13.29 COL 11 NO-LABEL WIDGET-ID 2
     fiResponseDataLabel AT ROW 19.24 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     edResponseData AT ROW 20.43 COL 11 NO-LABEL WIDGET-ID 10
     fiErrorMessageLabel AT ROW 26.76 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     edErrorMessage AT ROW 27.91 COL 11 NO-LABEL WIDGET-ID 94
     btSubmit AT ROW 31.05 COL 43.2
     Btn_Cancel AT ROW 31.1 COL 76.2
     RECT-1 AT ROW 1.33 COL 8 WIDGET-ID 84
     RECT-2 AT ROW 7.67 COL 8 WIDGET-ID 86
     RECT-3 AT ROW 5.05 COL 11 WIDGET-ID 110
     SPACE(8.79) SKIP(26.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 
         TITLE "Outbound API Tester"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btSubmit IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdateRequest IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edEndpoint IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edEndpoint:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR EDITOR edErrorMessage IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edErrorMessage:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR EDITOR edRequestData IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edResponseData IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edResponseData:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAuthType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiAuthType:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiClientID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiClientID:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiClientIDlb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiClientIDlb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiEndPointLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiEndPointLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiErrorMessageLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiErrorMessageLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiPrimaryKey IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrimaryKeyLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiReqDataType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiReqDataType:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiReqDataTypelb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiReqDataTypelb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestDataLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestDataLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerb-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerb-2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiRequestVerblb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestVerblb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseDataLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiResponseDataLabel:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiSSLEnabled IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiSSLEnabled:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiSSLEnabledlb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       fiSSLEnabledlb:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Outbound API Tester */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSubmit Dialog-Frame
ON CHOOSE OF btSubmit IN FRAME Dialog-Frame /* Submit */
DO:
    DEFINE VARIABLE cAPIID         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cParentProgram AS CHARACTER NO-UNDO.
    
    IF edRequestData:SCREEN-VALUE EQ "" AND
       fiRequestVerb:SCREEN-VALUE EQ "POST" THEN DO:
        MESSAGE "Request Data cannot be empty" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
              
    ASSIGN
        lcRequestData = edRequestData:SCREEN-VALUE
        cAPIID        = fiAPIId:SCREEN-VALUE
        .
    
    SESSION:SET-WAIT-STATE("GENERAL").
      
    RUN api/CallOutBoundAPI.p (
        cAPIId,
        lcRequestData,
        PROGRAM-NAME(1),
        OUTPUT lSuccess,
        OUTPUT cMessage
        ). 
    SESSION:SET-WAIT-STATE(""). 

    FIND LAST APIOutboundEvent NO-LOCK
         WHERE APIOutboundEvent.apiID EQ fiAPIId:SCREEN-VALUE
         NO-ERROR.
    IF AVAILABLE APIOutboundEvent THEN
        ASSIGN
            edResponseData:SCREEN-VALUE = STRING(APIOutboundEvent.responseData)
            edErrorMessage:SCREEN-VALUE = IF APIOutboundEvent.success THEN 
                                              "SUCCESS" + "~n" + APIOUtboundEvent.errorMessage
                                          ELSE
                                              "FAILURE" + "~n" + APIOUtboundEvent.errorMessage
            edErrorMessage:FGCOLOR      = IF APIOutboundEvent.success THEN
                                              2
                                          ELSE
                                              12
            .
    
    ASSIGN
        btSubmit:SENSITIVE         = FALSE
        edRequestData:SENSITIVE    = FALSE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdateRequest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateRequest Dialog-Frame
ON CHOOSE OF btUpdateRequest IN FRAME Dialog-Frame /* Update Request */
DO:
    DEFINE VARIABLE cAPIID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParentProgram     AS CHARACTER NO-UNDO.   
    
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
 
    EMPTY TEMP-TABLE ttArgs.
 
    cParentProgram = "api\APIOutboundTest.w".
    
    ASSIGN
        btSubmit:SENSITIVE          = FALSE
        edRequestData:SENSITIVE     = FALSE
        .
        
    CASE fiAPIId:SCREEN-VALUE:
        WHEN "SendCustomer" THEN DO:
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ cCompany
                   AND cust.cust-no EQ fiPrimaryKey:SCREEN-VALUE NO-ERROR.
            IF NOT AVAILABLE cust THEN DO:    
                MESSAGE "Invalid Customer record" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = "cust"
                ttArgs.argValue = STRING(ROWID(cust))
                .    
                    
            cAPIID = "SendCustomer".
        END.
        WHEN "SendVendor" THEN DO:
            FIND FIRST vend NO-LOCK
                 WHERE vend.company EQ cCompany
                   AND vend.vend-no EQ fiPrimaryKey:SCREEN-VALUE NO-ERROR.
            IF NOT AVAILABLE vend THEN DO:    
                MESSAGE "Invalid Vendor Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = "vend"
                ttArgs.argValue = STRING(ROWID(vend))
                .    
                    
            cAPIID = "SendVendor".
        END.
        WHEN "SendFinishedGood" THEN DO:
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no EQ fiPrimaryKey:SCREEN-VALUE NO-ERROR.
            IF NOT AVAILABLE itemfg THEN DO:    
                MESSAGE "Invalid Item Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = "itemfg"
                ttArgs.argValue = STRING(ROWID(itemfg))
                .    
                    
            cAPIID = "SendFinishedGood".
        END.
        WHEN "SendPurchaseOrder" THEN DO:
            FIND FIRST po-ord NO-LOCK
                 WHERE po-ord.company EQ cCompany
                   AND po-ord.po-no   EQ INTEGER(fiPrimaryKey:SCREEN-VALUE) NO-ERROR.
            IF NOT AVAILABLE po-ord THEN DO:    
                MESSAGE "Invalid Purchase Order Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            
            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = "po-ord"
                ttArgs.argValue = STRING(ROWID(po-ord))
                .    
                    
            cAPIID = "SendPurchaseOrder".
        END.
        WHEN "SendRelease" THEN DO:
            FIND FIRST oe-relh NO-LOCK
                WHERE oe-relh.company  EQ cCompany
                  AND oe-relh.release# EQ INT(fiPrimaryKey:SCREEN-VALUE) NO-ERROR.
            IF NOT AVAILABLE oe-relh THEN DO:
                MESSAGE "Invalid Release Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            CREATE ttArgs.
            ASSIGN
                ttArgs.argType  = "ROWID"
                ttArgs.argKey   = "oe-relh"
                ttArgs.argValue = STRING(ROWID(oe-relh))
                .    
            
            cAPIID = "SendRelease".
        END.
    END CASE.

    RUN api/PrepareOutboundRequest.p (
        INPUT TABLE ttArgs,
        cAPIID,    
        OUTPUT lcRequestData,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
        RETURN.
    END.

    ASSIGN
        edRequestData:SCREEN-VALUE = STRING(lcRequestData)
        btSubmit:SENSITIVE         = TRUE
        edRequestData:SENSITIVE    = TRUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAPIId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAPIId Dialog-Frame
ON HELP OF fiAPIId IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        cCompany, 
        "apiID", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN DO:
        fiAPIId:SCREEN-VALUE = lookupField.
        
        APPLY "LEAVE" TO SELF.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAPIId Dialog-Frame
ON LEAVE OF fiAPIId IN FRAME Dialog-Frame
DO:
    ASSIGN
        fiPrimaryKey:SENSITIVE      = FALSE
        btSubmit:SENSITIVE          = FALSE
        edRequestData:SENSITIVE     = FALSE
        btUpdateRequest:SENSITIVE   = FALSE
        fiPrimaryKey:SCREEN-VALUE   = ""
        edEndpoint:SCREEN-VALUE     = ""
        fiRequestVerb:SCREEN-VALUE  = ""
        fiReqDataType:SCREEN-VALUE  = ""
        fiSSLEnabled:SCREEN-VALUE   = "NO"
        fiClientID:SCREEN-VALUE     = ""
        fiAuthType:SCREEN-VALUE     = ""
        edRequestData:SCREEN-VALUE  = ""
        .
        
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.apiID EQ SELF:SCREEN-VALUE NO-ERROR.
    IF AVAILABLE APIOutbound THEN DO:
        ASSIGN
            edEndpoint:SCREEN-VALUE     = APIOutbound.endPoint
            fiRequestVerb:SCREEN-VALUE  = APIOutbound.requestVerb
            fiReqDataType:SCREEN-VALUE  = APIOutbound.requestDataType
            fiSSLEnabled:SCREEN-VALUE   = STRING(APIOutbound.isSSLEnabled)
            fiClientID:SCREEN-VALUE     = APIOutbound.clientID
            fiAuthType:SCREEN-VALUE     = APIOutbound.authType
            edRequestData:SCREEN-VALUE  = STRING(APIOutbound.requestData)
            fiPrimaryKey:SENSITIVE      = TRUE
            btUpdateRequest:SENSITIVE   = TRUE
            .  
    END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPrimaryKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPrimaryKey Dialog-Frame
ON HELP OF fiPrimaryKey IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnRECID AS RECID     NO-UNDO.
    
    CASE fiAPIID:SCREEN-VALUE:
        WHEN "SendCustomer" THEN DO:
            RUN windows/l-cust.w (
                cCompany,
                fiPrimaryKey:SCREEN-VALUE, 
                OUTPUT cReturnValue
                ).
        END.
        WHEN "SendVendor" THEN DO:
            RUN windows/l-vendno.w (
                cCompany,
                "",     /* vend.active */
                fiPrimaryKey:SCREEN-VALUE, 
                OUTPUT cReturnValue
                ).
        END.
        WHEN "SendFinishedGood" THEN DO:
            RUN windows/l-itemfg.w (
                cCompany,
                "",     /* cust-no */
                fiPrimaryKey:SCREEN-VALUE, 
                OUTPUT cReturnValue
                ).
        END.
        WHEN "SendPurchaseOrder" THEN DO:
            RUN windows/l-ponopo.w (
                cCompany,
                TRUE,     /* po-ord.active */
                fiPrimaryKey:SCREEN-VALUE, 
                OUTPUT cReturnValue
                ).
        END.
        WHEN "SendRelease" THEN DO:
            RUN windows/l-oerelh.w (
                cCompany,
                fiPrimaryKey:SCREEN-VALUE, 
                OUTPUT cReturnValue,
                OUTPUT cReturnRECID
                ).        
        END.        
    END CASE.
    
    IF cReturnValue NE "" THEN DO:
        fiPrimaryKey:SCREEN-VALUE = ENTRY(1,cReturnValue).
        APPLY "CHOOSE" TO btUpdateRequest.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  ASSIGN
      edRequestData:WORD-WRAP  = TRUE
      edResponseData:WORD-WRAP = TRUE
      .

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiSSLEnabled fiClientID fiAPIIDLabel fiAPIId fiSSLEnabledlb 
          fiClientIDlb fiRequestVerb fiReqDataType fiAuthType fiReqDataTypelb 
          fiRequestVerb-2 fiRequestVerblb fiPrimaryKeyLabel fiPrimaryKey 
          fiEndPointLabel edEndpoint fiRequestDataLabel edRequestData 
          fiResponseDataLabel edResponseData fiErrorMessageLabel edErrorMessage 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 RECT-2 RECT-3 fiAPIId Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

