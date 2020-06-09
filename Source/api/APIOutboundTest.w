&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cPrimaryID        AS CHARACTER NO-UNDO.

RUN api\OutboundProcs.p PERSISTENT SET hdOutboundProcs.

ASSIGN
    cCompany  = g_company
    cLocation = g_loc
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 Btn_Cancel ~
btAPIIDLookup fiAPIId btClientIDLookup fiClientID btTriggerIDLookup btGo ~
fiTriggerID 
&Scoped-Define DISPLAYED-OBJECTS fiAPIIDLabel fiAPIId fiClientIDlb ~
fiClientID fiAPITriggerIDLabel fiTriggerID fiPrimaryKeyLabel fiPrimaryKey ~
fiEventDescLabel edEventDesc fiMessage fiEndPointLabel edEndpoint ~
fiRequestDataLabel edRequestData fiResponseDataLabel edResponseData ~
fiErrorMessageLabel edErrorMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAPIIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btClientIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Cancel" 
     SIZE 20 BY 2.62 TOOLTIP "Exit"
     BGCOLOR 8 .

DEFINE BUTTON btPrimaryKeyLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btSubmit 
     IMAGE-UP FILE "Graphics/32x32/check.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_check_disabled.ico":U
     LABEL "Submit" 
     SIZE 20 BY 2.62 TOOLTIP "Submit Request"
     BGCOLOR 8 .

DEFINE BUTTON btTriggerIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btUpdateRequest 
     LABEL "Update Request" 
     SIZE 23 BY 1.33
     FONT 6.

DEFINE VARIABLE edEndpoint AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 2.24
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE edErrorMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 2.91
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE edEventDesc AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 200 SCROLLBAR-VERTICAL
     SIZE 95.6 BY 2.86
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE edRequestData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 6.76
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE edResponseData AS LONGCHAR 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 133 BY 5.14
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.6 BY 1
     FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPIIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API ID:" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPITriggerIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Trigger ID:" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiClientID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.6 BY 1
     FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE VARIABLE fiClientIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Client ID:" 
     VIEW-AS FILL-IN 
     SIZE 12.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndPointLabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Point:" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiErrorMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Result:" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEventDescLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Event Description:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 118.4 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fiPrimaryKey AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE VARIABLE fiPrimaryKeyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Key:" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiRequestDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data:" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiResponseDataLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Response Data:" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTriggerID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 132 BY 8.33.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 157 BY 19.52.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 124 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Cancel AT ROW 1.33 COL 140 WIDGET-ID 132
     btAPIIDLookup AT ROW 1.67 COL 60.8 WIDGET-ID 46
     fiAPIIDLabel AT ROW 1.71 COL 4.4 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 1.71 COL 14.8 COLON-ALIGNED NO-LABEL WIDGET-ID 106
     btClientIDLookup AT ROW 1.71 COL 121.8 WIDGET-ID 122
     fiClientIDlb AT ROW 1.76 COL 70.8 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fiClientID AT ROW 1.76 COL 83.6 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     btTriggerIDLookup AT ROW 3 COL 60.8 WIDGET-ID 120
     btGo AT ROW 3 COL 111.4 WIDGET-ID 118
     fiAPITriggerIDLabel AT ROW 3.05 COL 4.4 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fiTriggerID AT ROW 3.05 COL 18.4 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     btUpdateRequest AT ROW 4.62 COL 103.4 WIDGET-ID 108
     btPrimaryKeyLookup AT ROW 4.76 COL 62.8 WIDGET-ID 112
     fiPrimaryKeyLabel AT ROW 4.81 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     fiPrimaryKey AT ROW 4.81 COL 28.4 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fiEventDescLabel AT ROW 6.1 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 124
     edEventDesc AT ROW 6.1 COL 30.4 NO-LABEL WIDGET-ID 126
     btSubmit AT ROW 6.86 COL 139.8 WIDGET-ID 134
     fiMessage AT ROW 9.95 COL 9.6 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     fiEndPointLabel AT ROW 11.1 COL 9.6 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     edEndpoint AT ROW 11.1 COL 25 NO-LABEL WIDGET-ID 14
     fiRequestDataLabel AT ROW 13.62 COL 4.6 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     edRequestData AT ROW 13.62 COL 25 NO-LABEL WIDGET-ID 2
     fiResponseDataLabel AT ROW 20.62 COL 3.8 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     edResponseData AT ROW 20.62 COL 25 NO-LABEL WIDGET-ID 10
     fiErrorMessageLabel AT ROW 26.05 COL 1.8 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     edErrorMessage AT ROW 26.05 COL 25 NO-LABEL WIDGET-ID 94
     RECT-1 AT ROW 1.24 COL 3 WIDGET-ID 84
     RECT-2 AT ROW 9.81 COL 3 WIDGET-ID 86
     RECT-3 AT ROW 4.33 COL 6 WIDGET-ID 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Outbound API Tester"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btPrimaryKeyLookup IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSubmit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdateRequest IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edEndpoint IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       edEndpoint:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR edErrorMessage IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       edErrorMessage:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR edEventDesc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edRequestData IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edResponseData IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       edResponseData:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiAPITriggerIDLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiClientIDlb IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiClientIDlb:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiEndPointLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiEndPointLabel:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiErrorMessageLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiErrorMessageLabel:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiEventDescLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrimaryKey IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrimaryKeyLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRequestDataLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiRequestDataLabel:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fiResponseDataLabel IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiResponseDataLabel:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Outbound API Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Outbound API Tester */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAPIIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAPIIDLookup W-Win
ON CHOOSE OF btAPIIDLookup IN FRAME F-Main
DO:
    APPLY "HELP" TO fiAPIId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btClientIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClientIDLookup W-Win
ON CHOOSE OF btClientIDLookup IN FRAME F-Main
DO:
    APPLY "HELP" TO fiAPIId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo W-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO:
    DEFINE VARIABLE iAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lValid                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.

    ASSIGN
        fiPrimaryKey:SENSITIVE       = FALSE
        btSubmit:SENSITIVE           = FALSE
        edRequestData:SENSITIVE      = FALSE
        btUpdateRequest:SENSITIVE    = FALSE
        btPrimaryKeyLookup:SENSITIVE = FALSE
        fiPrimaryKey:SCREEN-VALUE    = ""
        fiMessage:SCREEN-VALUE       = ""
        edEndpoint:SCREEN-VALUE      = ""
        edRequestData:SCREEN-VALUE   = ""
        edResponseData:SCREEN-VALUE  = ""
        edErrorMessage:SCREEN-VALUE  = ""
        .

    IF fiAPIID:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "API ID cannot be empty, please a valid API ID"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF fiClientID:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Client ID cannot be empty, please a valid Client ID"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    IF fiTriggerID:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Trigger ID cannot be empty, please a valid Trigger ID"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    RUN pValidateOutboundAPI (
         OUTPUT iAPIOutboundID,
         OUTPUT iAPIOutboundTriggerID,
         OUTPUT lValid,
         OUTPUT cMessage
         ).
         
    IF NOT lValid THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
         
    FIND FIRST APIOutbound NO-LOCK
         WHERE APIOutbound.apiOutboundID EQ iAPIOutboundID
         NO-ERROR.
    IF AVAILABLE APIOutbound THEN DO:
        ASSIGN
            edEndpoint:SCREEN-VALUE      = APIOutbound.endPoint
            edRequestData:SCREEN-VALUE   = STRING(APIOutbound.requestData)
            fiPrimaryKey:SENSITIVE       = TRUE
            edEventDesc:SENSITIVE        = TRUE
            btUpdateRequest:SENSITIVE    = TRUE
            btPrimaryKeyLookup:SENSITIVE = TRUE
            .  
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel W-Win
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrimaryKeyLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrimaryKeyLookup W-Win
ON CHOOSE OF btPrimaryKeyLookup IN FRAME F-Main
DO:
    APPLY "HELP" TO fiPrimaryKey.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSubmit W-Win
ON CHOOSE OF btSubmit IN FRAME F-Main /* Submit */
DO:
    DEFINE VARIABLE cAPIID                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClientID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cEventDescription     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData         AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcResponseData        AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cParentProgram        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAPIOutboundEventID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cRequestType          AS CHARACTER NO-UNDO.
    
    RUN pValidateOutboundAPI (
         OUTPUT iAPIOutboundID,
         OUTPUT iAPIOutboundTriggerID,
         OUTPUT lSuccess,
         OUTPUT cMessage
         ).
         
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
    
    IF edRequestData:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Request Data cannot be empty" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
              
    ASSIGN
        lcRequestData     = edRequestData:SCREEN-VALUE
        cAPIID            = fiAPIId:SCREEN-VALUE
        cClientID         = fiClientID:SCREEN-VALUE
        cTriggerID        = fiTriggerID:SCREEN-VALUE
        cEventDescription = edEventDesc:SCREEN-VALUE
        cParentProgram    = IF NUM-ENTRIES(PROGRAM-NAME(1)," ") EQ 2 THEN 
                                ENTRY(2, PROGRAM-NAME(1), " ")
                            ELSE
                                PROGRAM-NAME(1)
        .
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    RUN Outbound_GetAPIRequestType IN hdOutboundProcs (
        INPUT  cCompany,
        INPUT  fiAPIID:SCREEN-VALUE,
        INPUT  fiClientID:SCREEN-VALUE,
        OUTPUT cRequestType,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF cRequestType EQ "API" THEN
        RUN api/CallOutBoundAPI.p (
            INPUT  iAPIOutboundID,
            INPUT  lcRequestData,
            INPUT  cParentProgram,
            OUTPUT lcResponseData,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ). 
    ELSE
        ASSIGN
            lcResponseData = "Success"
            lSuccess       = TRUE
            cMessage       = "Success"
            .

    SESSION:SET-WAIT-STATE(""). 

    RUN api/CreateAPIOutboundEvent.p (
        INPUT  FALSE,        /* Re-trigger flag */
        INPUT  ?,            /* API Outbound Event ID: Pass ? to create new Event*/
        INPUT  cCompany,
        INPUT  cLocation,
        INPUT  cAPIID,
        INPUT  cClientID,
        INPUT  cTriggerID,
        INPUT  cPrimaryID,
        INPUT  cEventDescription,
        INPUT  lcRequestData,
        INPUT  lcResponseData,
        INPUT  cParentProgram,
        INPUT  lSuccess,
        INPUT  cMessage,
        INPUT  NOW,
        OUTPUT iAPIOutboundEventID
        ).

    ASSIGN
        edResponseData:SCREEN-VALUE  = lcResponseData
        edErrorMessage:SCREEN-VALUE  = IF lSuccess THEN 
                                           "SUCCESS" + "~n" + cMessage
                                       ELSE
                                           "FAILURE" + "~n" + cMessage
        edErrorMessage:FGCOLOR       = IF lSuccess THEN
                                           2
                                       ELSE
                                           12
        btSubmit:SENSITIVE           = FALSE
        fiPrimaryKey:SENSITIVE       = FALSE
        edEventDesc:SENSITIVE        = FALSE
        edEventDesc:SCREEN-VALUE     = ""
        btUpdateRequest:SENSITIVE    = FALSE
        btPrimaryKeyLookup:SENSITIVE = FALSE        
        edRequestData:SENSITIVE      = FALSE
        fiMessage:SCREEN-VALUE       = "Outbound Event ID: "
                                     + STRING(iAPIOutboundEventID)
                                     + " created"
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTriggerIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTriggerIDLookup W-Win
ON CHOOSE OF btTriggerIDLookup IN FRAME F-Main
DO:
    APPLY "HELP" TO fiTriggerId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdateRequest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateRequest W-Win
ON CHOOSE OF btUpdateRequest IN FRAME F-Main /* Update Request */
DO:
    DEFINE VARIABLE cParentProgram        AS CHARACTER NO-UNDO.   
    DEFINE VARIABLE iAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcRequestData         AS LONGCHAR  NO-UNDO.
 
    EMPTY TEMP-TABLE ttArgs.
 
    cParentProgram = "api\APIOutboundTest.w".
    
    ASSIGN
        btSubmit:SENSITIVE          = FALSE
        edRequestData:SENSITIVE     = FALSE
        .    

    RUN pValidateOutboundAPI (
         OUTPUT iAPIOutboundID,
         OUTPUT iAPIOutboundTriggerID,
         OUTPUT lSuccess,
         OUTPUT cMessage
         ).
         
    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
            
    CASE fiAPIId:SCREEN-VALUE:
        WHEN "SendCustomer" THEN DO:
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ cCompany
                   AND cust.cust-no EQ fiPrimaryKey:SCREEN-VALUE
                 NO-ERROR.
            IF NOT AVAILABLE cust THEN DO:    
                MESSAGE "Invalid Customer record" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            
            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "cust",
                INPUT STRING(ROWID(cust))
                ).
        END.
        WHEN "SendVendor" THEN DO:
            FIND FIRST vend NO-LOCK
                 WHERE vend.company EQ cCompany
                   AND vend.vend-no EQ fiPrimaryKey:SCREEN-VALUE
                 NO-ERROR.
            IF NOT AVAILABLE vend THEN DO:    
                MESSAGE "Invalid Vendor Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "vend",
                INPUT STRING(ROWID(vend))
                ).
        END.
        WHEN "SendFinishedGood" THEN DO:
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no EQ fiPrimaryKey:SCREEN-VALUE 
                 NO-ERROR.
            IF NOT AVAILABLE itemfg THEN DO:    
                MESSAGE "Invalid Item Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "itemfg",
                INPUT STRING(ROWID(itemfg))
                ).
        END.
        WHEN "SendPurchaseOrder" OR
        WHEN "SendPurchaseOrderStatus" THEN DO:
            FIND FIRST po-ord NO-LOCK
                 WHERE po-ord.company EQ cCompany
                   AND po-ord.po-no   EQ INTEGER(fiPrimaryKey:SCREEN-VALUE)
                 NO-ERROR.
            IF NOT AVAILABLE po-ord THEN DO:    
                MESSAGE "Invalid Purchase Order Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "po-ord",
                INPUT STRING(ROWID(po-ord))
                ).
        END.
        WHEN "SendPurchaseOrderLineStatus" THEN DO:
            IF INDEX(fiPrimaryKey:SCREEN-VALUE,"-") GT 0 THEN DO:
                FIND FIRST po-ordl NO-LOCK
                     WHERE po-ordl.company EQ cCompany
                       AND po-ordl.po-no   EQ INTEGER(ENTRY(1,fiPrimaryKey:SCREEN-VALUE,"-"))
                       AND po-ordl.line    EQ INTEGER(ENTRY(2,fiPrimaryKey:SCREEN-VALUE,"-"))
                     NO-ERROR.
                IF NOT AVAILABLE po-ordl THEN DO:    
                    MESSAGE "Invalid Purchase Order Number and Line" VIEW-AS ALERT-BOX ERROR.
                    RETURN.
                END.

                RUN pCreateArgs (
                    INPUT "ROWID",
                    INPUT "po-ordl",
                    INPUT STRING(ROWID(po-ordl))
                    ).
            END.
            ELSE DO:
                MESSAGE "Invalid Purchase Order and Line" VIEW-AS ALERT-BOX ERROR.    
                RETURN.
            END.    
        END.
        WHEN "SendRelease" THEN DO:
            FIND FIRST oe-relh NO-LOCK
                WHERE oe-relh.company  EQ cCompany
                  AND oe-relh.release# EQ INT(fiPrimaryKey:SCREEN-VALUE)
                NO-ERROR.
            IF NOT AVAILABLE oe-relh THEN DO:
                MESSAGE "Invalid Release Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "oe-relh",
                INPUT STRING(ROWID(oe-relh))
                ).
        END.
        WHEN "SendAdvancedShipNotice" THEN DO:
            FIND FIRST oe-bolh NO-LOCK
                WHERE oe-bolh.company EQ cCompany
                  AND oe-bolh.bol-no  EQ INT(fiPrimaryKey:SCREEN-VALUE)
                NO-ERROR.
            IF NOT AVAILABLE oe-bolh THEN DO:
                MESSAGE "Invalid BOL Number" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "oe-bolh",
                INPUT STRING(ROWID(oe-bolh))
                ).
        END.
        WHEN "SendJob" THEN DO:
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ cCompany
                   AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(ENTRY(1,fiPrimaryKey:SCREEN-VALUE,"-")))) + TRIM(ENTRY(1,fiPrimaryKey:SCREEN-VALUE,"-"))
                   AND job.job-no2 EQ INTEGER(TRIM((ENTRY(2,fiPrimaryKey:SCREEN-VALUE,"-"))))
                 NO-ERROR.
            IF NOT AVAILABLE job THEN DO:
                MESSAGE "Invalid Job Number"
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.                
            END. 
            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "job",
                INPUT STRING(ROWID(job))
                ).                      
        END.
        WHEN "SendInvoice" THEN DO:
            FIND FIRST inv-head NO-LOCK
                 WHERE inv-head.company EQ cCompany
                   AND inv-head.inv-no   EQ INTEGER(fiPrimaryKey:SCREEN-VALUE)
                 NO-ERROR.
            IF NOT AVAIL inv-head THEN
              FIND FIRST ar-inv NO-LOCK
                WHERE ar-inv.company EQ cCompany
                  AND ar-inv.inv-no EQ INTEGER(fiPrimaryKey:SCREEN-VALUE)
                  NO-ERROR. 
            IF NOT AVAILABLE inv-head AND NOT AVAIL ar-inv THEN DO:    
                MESSAGE "Invalid invoice Number" ccompany fiPrimaryKey:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            IF AVAIL inv-head THEN 
                RUN pCreateArgs (
                    INPUT "ROWID",
                    INPUT "inv-head",
                    INPUT STRING(ROWID(inv-head))
                    ).
            ELSE IF AVAIL ar-inv THEN 
                RUN pCreateArgs (
                    INPUT "ROWID",
                    INPUT "ar-inv",
                    INPUT STRING(ROWID(ar-inv))
                    ).
        END.
        WHEN "SendOrderAck"  THEN DO:
            FIND FIRST oe-ord NO-LOCK
                 WHERE oe-ord.company EQ cCompany
                   AND oe-ord.ord-no   EQ INTEGER(fiPrimaryKey:SCREEN-VALUE)
                 NO-ERROR.
            IF NOT AVAILABLE oe-ord THEN DO:    
                MESSAGE "Invalid Sales Order" VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.

            RUN pCreateArgs (
                INPUT "ROWID",
                INPUT "oe-ord",
                INPUT STRING(ROWID(oe-ord))
                ).
        END.
    END CASE.
    RUN api/PrepareOutboundRequest.p (
        INPUT TABLE ttArgs,
        INPUT iAPIOutboundID,
        INPUT iAPIOutboundTriggerID,        
        OUTPUT lcRequestData,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).

    IF NOT lSuccess THEN DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
        RETURN.
    END.

    ASSIGN
        edRequestData:SCREEN-VALUE  = lcRequestData
        btSubmit:SENSITIVE          = TRUE
        edRequestData:SENSITIVE     = TRUE
        edResponseData:SCREEN-VALUE = ""
        edErrorMessage:SCREEN-VALUE = ""
        cPrimaryID                  = fiPrimaryKey:SCREEN-VALUE
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAPIId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAPIId W-Win
ON HELP OF fiAPIId IN FRAME F-Main
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
        ASSIGN
            fiAPIId:SCREEN-VALUE    =  IF NUM-ENTRIES(returnFields,"|") GE 2 THEN
                                           ENTRY(2, returnFields, "|")
                                       ELSE
                                           ""
            fiClientId:SCREEN-VALUE  = IF NUM-ENTRIES(returnFields, "|") GE 4 THEN
                                           ENTRY(4, returnFields, "|")
                                       ELSE
                                          ""           
            fiTriggerID:SCREEN-VALUE = ""
            .
        
        APPLY "LEAVE" TO SELF.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPrimaryKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPrimaryKey W-Win
ON HELP OF fiPrimaryKey IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
          
    CASE fiAPIID:SCREEN-VALUE:
        WHEN "SendCustomer" THEN DO:
            /* Customer lookup */
            RUN system/openlookup.p (
                cCompany, 
                "cust-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "SendVendor" THEN DO:
            /* Vendor lookup */
            RUN system/openlookup.p (
                cCompany, 
                "vend-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "SendFinishedGood" THEN DO:
            /* Finished Good lookup */
            RUN system/openlookup.p (
                cCompany, 
                "i-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "SendPurchaseOrder" OR
        WHEN "SendPurchaseOrderStatus" THEN DO:        
            /* Purchase Order lookup */
            RUN system/openlookup.p (
                cCompany, 
                "po-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "SendPurchaseOrderLineStatus" THEN DO:        
            /* Purchase Order lookup */
            RUN system/openlookup.p (
                cCompany, 
                "po-line", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "SendRelease" THEN DO:
            /* Release lookup */
            RUN system/openlookup.p (
                cCompany, 
                "release#", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ).      
        END.
        WHEN "SendJob" THEN DO:  
            RUN system/openlookup.p (
                cCompany, 
                "Job-no",            /* lookup field */
                0,                   /* Subject ID */
                "",                  /* User ID */
                0,                   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ).       
        END.    
    END CASE.

    IF lookupField NE "" THEN DO:
        IF fiAPIID:SCREEN-VALUE EQ "SendPurchaseOrderLineStatus" THEN
            fiPrimaryKey:SCREEN-VALUE = ENTRY(2,returnFields,"|") 
                                      + "-" + ENTRY(4,returnFields,"|").
        ELSE IF fiAPIID:SCREEN-VALUE EQ "SendJob" THEN
            fiPrimaryKey:SCREEN-VALUE = ENTRY(4,returnFields,"|")
                                     + "-" + ENTRY(6,returnFields,"|").                            
        ELSE
            fiPrimaryKey:SCREEN-VALUE = ENTRY(1,lookupField).
        
        APPLY "CHOOSE" TO btUpdateRequest.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTriggerID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTriggerID W-Win
ON HELP OF fiTriggerID IN FRAME F-Main
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        cCompany, 
        "triggerID", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            fiTriggerID:SCREEN-VALUE = lookupField
            .
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiAPIIDLabel fiAPIId fiClientIDlb fiClientID fiAPITriggerIDLabel 
          fiTriggerID fiPrimaryKeyLabel fiPrimaryKey fiEventDescLabel 
          edEventDesc fiMessage fiEndPointLabel edEndpoint fiRequestDataLabel 
          edRequestData fiResponseDataLabel edResponseData fiErrorMessageLabel 
          edErrorMessage 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 RECT-2 RECT-3 Btn_Cancel btAPIIDLookup fiAPIId btClientIDLookup 
         fiClientID btTriggerIDLookup btGo fiTriggerID 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    ASSIGN
        edRequestData:WORD-WRAP  IN FRAME {&FRAME-NAME} = TRUE
        edResponseData:WORD-WRAP IN FRAME {&FRAME-NAME} = TRUE
        .

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + cLocation.
  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateArgs W-Win 
PROCEDURE pCreateArgs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO.
    
    CREATE ttArgs.
    ASSIGN
        ttArgs.argType  = ipcType
        ttArgs.argKey   = ipcKey
        ttArgs.argValue = ipcValue
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateOutboundAPI W-Win 
PROCEDURE pValidateOutboundAPI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid                AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.

    RUN Outbound_GetAPIID IN hdOutboundProcs (
        INPUT  cCompany,
        INPUT  fiAPIID:SCREEN-VALUE,
        INPUT  fiClientID:SCREEN-VALUE,
        OUTPUT opiAPIOutboundID,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ).
    
    IF NOT oplValid THEN
        RETURN.
    
    RUN Outbound_GetAPITriggerID IN hdOutboundProcs (
        INPUT  cCompany,
        INPUT  fiAPIID:SCREEN-VALUE,
        INPUT  fiClientID:SCREEN-VALUE,
        INPUT  fiTriggerID:SCREEN-VALUE,
        OUTPUT opiAPIOutboundTriggerID,
        OUTPUT oplValid,
        OUTPUT opcMessage
        ).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

