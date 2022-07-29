&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: api/d-dataViewer.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{system/ttVariable.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER ioplcData AS LONGCHAR  NO-UNDO.
DEFINE INPUT        PARAMETER ipcAction AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER iplSave   AS LOGICAL   NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lCanEditVariable AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAction          AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttVariable

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttVariable.sequenceID ttVariable.varName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttVariable WHERE ttVariable.varName MATCHES "*" + fiSearch:SCREEN-VALUE + "*"
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttVariable WHERE ttVariable.varName MATCHES "*" + fiSearch:SCREEN-VALUE + "*".
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttVariable
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttVariable


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edData btCancel btOk RECT-1 RECT-2 RECT-3 ~
fiSearch fiTestValue btTest BROWSE-2 fiIfTrue fiIfFalse 
&Scoped-Define DISPLAYED-OBJECTS edData fiSearch fiTestValue fiName ~
cbDataType fiFormat cbAlignment fiIfValue fiIfTrue fiIfFalse fiReplaceValue ~
fiReplaceWith tgTrim tgCaps 

/* Custom List Definitions                                              */
/* VARIABLE-FIELDS,List-2,List-3,List-4,List-5,List-6                   */
&Scoped-define VARIABLE-FIELDS fiName cbDataType fiFormat cbAlignment ~
fiIfValue fiReplaceValue fiReplaceWith tgTrim tgCaps 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btEditorCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "CANCEL" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btEditorUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "UPDATE" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btOk AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON btTest 
     LABEL "TEST" 
     SIZE 15 BY 1.19
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btVarAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "ADD" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btVarCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "CANCEL" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btVarDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "DELETE" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE BUTTON btVarUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "UPDATE" 
     SIZE 8 BY 1.91
     BGCOLOR 15 FONT 35.

DEFINE VARIABLE cbAlignment AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "None","Left","Right" 
     DROP-DOWN-LIST
     SIZE 45 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE cbDataType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Character","Integer","Decimal","Logical","Date","Time","MTime","Decimal-Integer" 
     DROP-DOWN-LIST
     SIZE 45 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE edData AS LONGCHAR 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 101 BY 26.91
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE edMessage AS CHARACTER INITIAL "Place the cursor in editor where the new variable need to be placed" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 66.4 BY 2.14
     BGCOLOR 30 FONT 6 NO-UNDO.

DEFINE VARIABLE fiIfFalse AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 45 BY 2.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiIfTrue AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 45 BY 2.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiFormat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiIfValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiReplaceValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiReplaceWith AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE fiTestValue AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50.8 BY 1.19
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 14.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 7.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 3.33.

DEFINE VARIABLE tgCaps AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tgTrim AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttVariable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttVariable.sequenceID WIDTH 5
ttVariable.varName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 25.52
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btVarCancel AT ROW 25 COL 224.6 WIDGET-ID 90
     edData AT ROW 1 COL 1 NO-LABEL WIDGET-ID 2
     btEditorCancel AT ROW 27.95 COL 93.8 WIDGET-ID 88
     btEditorUpdate AT ROW 27.95 COL 83.6 WIDGET-ID 86
     btVarAdd AT ROW 25 COL 194.4 WIDGET-ID 94
     btVarDelete AT ROW 25 COL 214.6 WIDGET-ID 92
     btVarUpdate AT ROW 25 COL 204.6 WIDGET-ID 96
     btCancel AT ROW 27.91 COL 238
     btOk AT ROW 27.91 COL 229
     fiSearch AT ROW 1.05 COL 114 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiTestValue AT ROW 1.05 COL 178.6 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     btTest AT ROW 1.05 COL 231.8 WIDGET-ID 82
     edMessage AT ROW 2.19 COL 180.6 NO-LABEL WIDGET-ID 100
     BROWSE-2 AT ROW 2.38 COL 103 WIDGET-ID 200
     fiName AT ROW 4.33 COL 197 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     cbDataType AT ROW 5.86 COL 197 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiFormat AT ROW 7.24 COL 197 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbAlignment AT ROW 8.76 COL 197 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fiIfValue AT ROW 11.86 COL 196 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiIfTrue AT ROW 13.29 COL 198 NO-LABEL WIDGET-ID 78
     fiIfFalse AT ROW 15.91 COL 198 NO-LABEL WIDGET-ID 80
     fiReplaceValue AT ROW 20.1 COL 196.2 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     fiReplaceWith AT ROW 21.38 COL 196.2 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     tgTrim AT ROW 23.52 COL 191 WIDGET-ID 36
     tgCaps AT ROW 23.52 COL 209 WIDGET-ID 42
     "IF FUNCTION" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 11.14 COL 187 WIDGET-ID 50
          FGCOLOR 15 FONT 6
     "CAPS:" VIEW-AS TEXT
          SIZE 9 BY .95 AT ROW 23.43 COL 200 WIDGET-ID 40
          FGCOLOR 15 FONT 35
     "SEARCH:" VIEW-AS TEXT
          SIZE 12 BY .95 AT ROW 1.24 COL 104 WIDGET-ID 26
          FGCOLOR 15 FONT 35
     "WITH:" VIEW-AS TEXT
          SIZE 8.8 BY .95 AT ROW 21.43 COL 189.4 WIDGET-ID 76
          FGCOLOR 15 FONT 35
     "FUNCTIONS" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 10.05 COL 184 WIDGET-ID 32
          FGCOLOR 15 FONT 6
     "ELSE" VIEW-AS TEXT
          SIZE 6.8 BY .95 AT ROW 15.76 COL 191 WIDGET-ID 64
          FGCOLOR 15 FONT 35
     "IF VALUE:" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 11.95 COL 185 WIDGET-ID 60
          FGCOLOR 15 FONT 35
     "VALUE:" VIEW-AS TEXT
          SIZE 9.8 BY .95 AT ROW 20.24 COL 188.2 WIDGET-ID 74
          FGCOLOR 15 FONT 35
     "FORMAT:" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 7.29 COL 186 WIDGET-ID 22
          FGCOLOR 15 FONT 35
     "ALIGNMENT:" VIEW-AS TEXT
          SIZE 18 BY .95 AT ROW 8.76 COL 181 WIDGET-ID 24
          FGCOLOR 15 FONT 35
     "NAME:" VIEW-AS TEXT
          SIZE 10 BY .95 AT ROW 4.48 COL 189 WIDGET-ID 18
          FGCOLOR 15 FONT 35
     "DATA TYPE:" VIEW-AS TEXT
          SIZE 16 BY .95 AT ROW 5.91 COL 183 WIDGET-ID 20
          FGCOLOR 15 FONT 35
     "TRIM:" VIEW-AS TEXT
          SIZE 9 BY .95 AT ROW 23.43 COL 182 WIDGET-ID 38
          FGCOLOR 15 FONT 35
     "REPLACE FUNCTION" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 19.24 COL 187.2 WIDGET-ID 68
          FGCOLOR 15 FONT 6
     "THEN" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 13.19 COL 189.8 WIDGET-ID 62
          FGCOLOR 15 FONT 35
     RECT-1 AT ROW 10.29 COL 180 WIDGET-ID 30
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 
         CANCEL-BUTTON btCancel WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     RECT-2 AT ROW 11.38 COL 182.6 WIDGET-ID 48
     RECT-3 AT ROW 19.52 COL 182.8 WIDGET-ID 66
     SPACE(2.59) SKIP(7.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 
         TITLE "View/Update Data"
         CANCEL-BUTTON btCancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 edMessage D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btEditorCancel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btEditorUpdate IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btVarAdd IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btVarCancel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btVarDelete IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btVarUpdate IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbAlignment IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX cbDataType IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
ASSIGN 
       edData:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR EDITOR edMessage IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       edMessage:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN fiFormat IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
ASSIGN 
       fiIfFalse:READ-ONLY IN FRAME D-Dialog        = TRUE.

ASSIGN 
       fiIfTrue:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN fiIfValue IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiName IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiReplaceValue IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fiReplaceWith IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tgCaps IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tgTrim IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttVariable WHERE ttVariable.varName MATCHES "*" + fiSearch:SCREEN-VALUE + "*".
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* View/Update Data */
DO:  
    /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON VALUE-CHANGED OF BROWSE-2 IN FRAME D-Dialog
DO:
    IF AVAILABLE ttVariable AND ttVariable.varPosition NE 0 AND ttVariable.varLength NE 0 THEN
        edData:SET-SELECTION(ttVariable.varPosition, ttVariable.varLength + ttVariable.varPosition).
    
    RUN pUpdateFields.
    
    IF lCanEditVariable AND ipcAction EQ "Update" THEN DO:
        ASSIGN
            btVarAdd:SENSITIVE    = TRUE
            btVarDelete:SENSITIVE = TRUE
            btVarUpdate:SENSITIVE = TRUE
            btVarCancel:SENSITIVE = FALSE
            .
    END.
    
    IF NOT AVAILABLE ttVariable THEN DO:
        ASSIGN
            fiName:SCREEN-VALUE         = ""
            fiFormat:SCREEN-VALUE       = ""
            fiIfFalse:SCREEN-VALUE      = ""
            fiIfTrue:SCREEN-VALUE       = ""
            fiIfValue:SCREEN-VALUE      = ""
            fiReplaceValue:SCREEN-VALUE = ""
            fiReplaceWith:SCREEN-VALUE  = ""
            cbAlignment:SCREEN-VALUE    = "None"
            cbDataType:SCREEN-VALUE     = "Character"
            btVarDelete:SENSITIVE       = FALSE
            . 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEditorCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEditorCancel D-Dialog
ON CHOOSE OF btEditorCancel IN FRAME D-Dialog /* CANCEL */
DO:
    ASSIGN
        edData:READ-ONLY         = TRUE
        btEditorUpdate:LABEL     = "UPDATE"
        btEditorCancel:SENSITIVE = FALSE
        .
    
    ENABLE {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.
    
    btEditorUpdate:LOAD-IMAGE("Graphics\32x32\pencil.png").
    
    edData:SCREEN-VALUE = ioplcData.
    
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEditorUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEditorUpdate D-Dialog
ON CHOOSE OF btEditorUpdate IN FRAME D-Dialog /* UPDATE */
DO:
    IF SELF:LABEL EQ "UPDATE" THEN DO:
        ASSIGN
            edData:READ-ONLY         = FALSE
            btEditorUpdate:LABEL     = "SAVE"
            btEditorCancel:SENSITIVE = TRUE
            .

        btEditorUpdate:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
        
        DISABLE {&BROWSE-NAME} btVarAdd btVarCancel btVarDelete btVarUpdate WITH FRAME {&FRAME-NAME}.
    END.
    ELSE IF SELF:LABEL EQ "SAVE" THEN DO:
        ASSIGN
            edData:READ-ONLY         = TRUE
            btEditorUpdate:LABEL     = "UPDATE"
            btEditorCancel:SENSITIVE = FALSE
            edData
            .
        
        ENABLE {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.

        btEditorUpdate:LOAD-IMAGE("Graphics\32x32\pencil.png").
        
        ioplcData = edData.
        
        RUN pInit.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk D-Dialog
ON CHOOSE OF btOk IN FRAME D-Dialog /* OK */
DO:
    ASSIGN
        edData
        iplSave   = TRUE
        ioplcData = edData
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTest D-Dialog
ON CHOOSE OF btTest IN FRAME D-Dialog /* TEST */
DO:
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cVariableText AS CHARACTER NO-UNDO.
    
    RUN pValidateVariable(OUTPUT cMessage).
    IF cMessage NE "" THEN DO:
        MESSAGE cMessage
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    RUN pBuildVariable (OUTPUT cVariableText).
    
    RUN Format_UpdateRequestData (
        INPUT-OUTPUT cVariableText,
        fiName:SCREEN-VALUE,
        fiTestValue:SCREEN-VALUE,
        ""
        ).
    
    MESSAGE "Result:" SKIP cVariableText VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVarAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVarAdd D-Dialog
ON CHOOSE OF btVarAdd IN FRAME D-Dialog /* ADD */
DO:
    ASSIGN
        fiName:SCREEN-VALUE         = ""
        fiFormat:SCREEN-VALUE       = ""
        fiIfFalse:SCREEN-VALUE      = ""
        fiIfTrue:SCREEN-VALUE       = ""
        fiIfValue:SCREEN-VALUE      = ""
        fiReplaceValue:SCREEN-VALUE = ""
        fiReplaceWith:SCREEN-VALUE  = ""
        cbAlignment:SCREEN-VALUE    = "None"
        cbDataType:SCREEN-VALUE     = "Character"
        cAction                     = "ADD"
        btVarUpdate:LABEL           = "SAVE"
        btEditorUpdate:SENSITIVE    = FALSE
        btEditorCancel:SENSITIVE    = FALSE
        btVarCancel:SENSITIVE       = TRUE
        btVarAdd:SENSITIVE          = FALSE
        btVarDelete:SENSITIVE       = FALSE
        fiIfTrue:READ-ONLY          = FALSE
        fiIfFalse:READ-ONLY         = FALSE
        edMessage:VISIBLE           = TRUE
        edMessage:SCREEN-VALUE      = "Place the cursor in the editor where the new variable need to be placed"
        .
    
    DISABLE {&BROWSE-NAME} btEditorUpdate btEditorCancel WITH FRAME {&FRAME-NAME}.
    
    ENABLE {&VARIABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
    
    btVarUpdate:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVarCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVarCancel D-Dialog
ON CHOOSE OF btVarCancel IN FRAME D-Dialog /* CANCEL */
DO:
    ASSIGN
        cAction                  = ""
        btEditorUpdate:SENSITIVE = TRUE
        btVarUpdate:LABEL        = "UPDATE"
        btVarCancel:SENSITIVE    = FALSE
        btVarAdd:SENSITIVE       = TRUE
        fiIfTrue:READ-ONLY       = TRUE
        fiIfFalse:READ-ONLY      = TRUE
        btVarDelete:SENSITIVE    = TRUE
        edMessage:VISIBLE        = FALSE
        .
    
    ENABLE {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.
    
    DISABLE {&VARIABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
    
    btVarUpdate:LOAD-IMAGE("Graphics\32x32\pencil.png").
    
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVarDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVarDelete D-Dialog
ON CHOOSE OF btVarDelete IN FRAME D-Dialog /* DELETE */
DO:
    IF AVAILABLE ttVariable AND ttVariable.varPosition GT 0 AND ttVariable.varLength GT 0 THEN DO:
        MESSAGE "Delete Currently Selected Record?" VIEW-AS ALERT-BOX QUESTION
            BUTTON YES-NO UPDATE lChoice AS LOGICAL.
        IF NOT lChoice THEN
            RETURN NO-APPLY.
        
        ASSIGN 
            edData
            ioplcData = edData
            .
            
        ioplcData = SUBSTRING(ioplcData, 1, ttVariable.varPosition - 1) 
                  + SUBSTRING(ioplcData, ttVariable.varPosition + ttVariable.varLength).     
        
        RUN pInit.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVarUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVarUpdate D-Dialog
ON CHOOSE OF btVarUpdate IN FRAME D-Dialog /* UPDATE */
DO:
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVariableText AS CHARACTER NO-UNDO.
    
    IF NOT AVAILABLE ttVariable THEN
        RETURN NO-APPLY.
        
    IF SELF:LABEL EQ "UPDATE" THEN DO:
        ASSIGN
            cAction                  = "UPDATE"
            btVarUpdate:LABEL        = "SAVE"
            btEditorUpdate:SENSITIVE = FALSE
            btEditorCancel:SENSITIVE = FALSE
            btVarCancel:SENSITIVE    = TRUE
            btVarAdd:SENSITIVE       = FALSE
            btVarDelete:SENSITIVE    = FALSE
            fiIfTrue:READ-ONLY       = FALSE
            fiIfFalse:READ-ONLY      = FALSE
            .
        
        btVarUpdate:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
        
        DISABLE {&BROWSE-NAME} btEditorUpdate btEditorCancel WITH FRAME {&FRAME-NAME}.
        
        ENABLE {&VARIABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
    END.
    ELSE IF SELF:LABEL EQ "SAVE" THEN DO:
        RUN pValidateVariable(OUTPUT cMessage).

        IF cMessage NE "" THEN DO:
            MESSAGE cMessage
            VIEW-AS ALERT-BOX ERROR.
            
            RETURN NO-APPLY.
        END.
        
        RUN pBuildVariable(OUTPUT cVariableText).
        
        IF cAction EQ "UPDATE" THEN DO: 
            IF ttVariable.varPosition EQ 1 THEN DO:
                ioplcData = cVariableText 
                          + SUBSTRING(ioplcData, ttVariable.varPosition + ttVariable.varLength).        
            END.
            ELSE DO:
                ioplcData = SUBSTRING(ioplcData, 1, ttVariable.varPosition - 1)
                          + cVariableText 
                          + SUBSTRING(ioplcData, ttVariable.varPosition + ttVariable.varLength).     
            END.
        END.
        ELSE IF cAction EQ "ADD" THEN DO:
            IF edData:CURSOR-OFFSET EQ 1 THEN DO:
                ioplcData = cVariableText + ioplcData.        
            END.
            ELSE DO:
                ioplcData = SUBSTRING(ioplcData, 1, edData:CURSOR-OFFSET - 1)
                          + cVariableText 
                          + SUBSTRING(ioplcData, edData:CURSOR-OFFSET).     
            END.
        END.
        
        ASSIGN
            cAction                  = ""
            btVarUpdate:LABEL        = "UPDATE"
            btEditorUpdate:SENSITIVE = TRUE
            btVarCancel:SENSITIVE    = FALSE
            btVarAdd:SENSITIVE       = TRUE
            fiIfTrue:READ-ONLY       = TRUE
            fiIfFalse:READ-ONLY      = TRUE
            btVarDelete:SENSITIVE    = TRUE
            edMessage:VISIBLE        = FALSE
            .
        
        ENABLE {&BROWSE-NAME} WITH FRAME {&FRAME-NAME}.
        
        btVarUpdate:LOAD-IMAGE("Graphics\32x32\pencil.png").
        
        DISABLE {&VARIABLE-FIELDS} WITH FRAME {&FRAME-NAME}.
        
        RUN pInit.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edData D-Dialog
ON LEFT-MOUSE-CLICK OF edData IN FRAME D-Dialog
DO:
    DEFINE BUFFER bf-ttVariable FOR ttVariable.
    
    IF cAction NE "" THEN
        RETURN.
        
    IF fiSearch:SCREEN-VALUE NE "" THEN DO:
        fiSearch:SCREEN-VALUE = "".
        
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END.
    
    FIND LAST bf-ttVariable
         WHERE bf-ttVariable.varPosition LE edData:CURSOR-OFFSET
         NO-ERROR.
    IF AVAILABLE bf-ttVariable THEN DO:
        REPOSITION {&BROWSE-NAME} TO ROWID ROWID(bf-ttVariable) NO-ERROR.
        
        RUN pUpdateFields.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch D-Dialog
ON VALUE-CHANGED OF fiSearch IN FRAME D-Dialog
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}
    
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY edData fiSearch fiTestValue fiName cbDataType fiFormat cbAlignment 
          fiIfValue fiIfTrue fiIfFalse fiReplaceValue fiReplaceWith tgTrim 
          tgCaps 
      WITH FRAME D-Dialog.
  ENABLE edData btCancel btOk RECT-1 RECT-2 RECT-3 fiSearch fiTestValue btTest 
         BROWSE-2 fiIfTrue fiIfFalse 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildVariable D-Dialog 
PROCEDURE pBuildVariable PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcVariableText AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN fiName fiFormat cbDataType cbAlignment fiIfValue fiIfTrue fiIfFalse fiReplaceValue fiReplaceWith tgCaps tgTrim.
    
    IF fiName NE "" THEN DO:
        opcVariableText = "$" + fiName + "|".
        
        IF fiFormat NE "" THEN DO:
            IF cbDataType EQ "Character" THEN
                opcVariableText = opcVariableText + "X(" + fiFormat + ")|".
            ELSE
                opcVariableText = opcVariableText + fiFormat + "|".
        END.
        ELSE
            opcVariableText = opcVariableText + "|".
        
        IF cbDataType NE "" AND cbDataType NE ? THEN DO:
            IF cbDataType EQ "Character" THEN
                opcVariableText = opcVariableText + "|".
            ELSE IF cbDataType EQ "Integer" THEN
                opcVariableText = opcVariableText + "INT" + "|".
            ELSE IF cbDataType EQ "Decimal" THEN
                opcVariableText = opcVariableText + "DEC" + "|".
            ELSE IF cbDataType EQ "Decimal-Integer" THEN
                opcVariableText = opcVariableText + "DEC-INT" + "|".
            ELSE IF cbDataType EQ "Logical" THEN
                opcVariableText = opcVariableText + "LOG" + "|".
            ELSE
                opcVariableText = opcVariableText + cbDataType + "|".
        END.
        ELSE
            opcVariableText = opcVariableText + "|".
        
        IF TRIM(cbAlignment) NE "" AND cbAlignment NE ? AND cbAlignment NE "None" AND (cbDataType EQ "Integer" OR cbDataType EQ "Decimal") THEN
            opcVariableText = opcVariableText + (IF cbAlignment EQ "Left" THEN "L" ELSE IF cbAlignment EQ "Right" THEN "R" ELSE "") + "|". 
        ELSE
            opcVariableText = opcVariableText + "|".
        
        IF TRIM(fiIfValue) NE "" THEN DO:
            opcVariableText = opcVariableText + 'IF[' + '"' 
                            + fiIfValue + '"' + ',' + '"'
                            + fiIfTrue + '"' + ',' + '"'
                            + fiIfFalse + '"' + ']' + "|".
                            
        END.
        
        IF TRIM(fiReplaceValue) NE "" THEN DO:
            opcVariableText = opcVariableText + 'REPLACE[' + '"'
                            + fiReplaceValue + '"' + ',' + '"'
                            + fiReplaceWith + '"' + ']' + "|".
        END.
        
        IF tgTrim THEN
            opcVariableText = opcVariableText + "TRIM" + "|".
        
        IF tgCaps THEN
            opcVariableText = opcVariableText + "CAPS" + "|".
        
        opcVariableText = TRIM(opcVariableText, "|").
        
        IF opcVariableText NE "$" + fiName THEN
            opcVariableText = opcVariableText + "|".
        
        opcVariableText = opcVariableText + "$".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildVariables D-Dialog 
PROCEDURE pBuildVariables PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplVariablesLoaded AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iChar  AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DO iChar = 1 TO LENGTH(ioplcData):
        IF SUBSTRING(ioplcData, iChar, 1) EQ "$" THEN
            iCount = iCount + 1.             
    END.               
    
    IF iCount MOD 2 EQ 0 THEN DO:
        RUN system/BuildVariableData.p (ioplcData, OUTPUT TABLE ttVariable).
        
        {&OPEN-QUERY-{&BROWSE-NAME}}
        
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        
        oplVariablesLoaded = TRUE. 
    END.
    ELSE DO:
        MESSAGE "Mismatched '$'s in request data. Manually edit the request data in editor"  
            VIEW-AS ALERT-BOX ERROR.
        
        EMPTY TEMP-TABLE ttVariable.
                    
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit D-Dialog 
PROCEDURE pInit PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lVariablesLoaded AS LOGICAL NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    edData:SCREEN-VALUE = ioplcData.
    
    RUN pBuildVariables(OUTPUT lVariablesLoaded).
    
    IF ipcAction EQ "Update" THEN DO:
        ASSIGN
            btEditorUpdate:SENSITIVE = TRUE
            btVarAdd:VISIBLE         = lVariablesLoaded
            btVarCancel:VISIBLE      = lVariablesLoaded
            btVarDelete:VISIBLE      = lVariablesLoaded
            btVarUpdate:VISIBLE      = lVariablesLoaded
            .
    END.
    ELSE IF ipcAction EQ "View" THEN DO:
        ASSIGN
            btOk:VISIBLE           = FALSE
            btEditorCancel:VISIBLE = FALSE
            btEditorUpdate:VISIBLE = FALSE
            btVarAdd:VISIBLE       = FALSE
            btVarCancel:VISIBLE    = FALSE
            btVarDelete:VISIBLE    = FALSE
            btVarUpdate:VISIBLE    = FALSE
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateFields D-Dialog 
PROCEDURE pUpdateFields PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iChar         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFunctionText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReplaceText  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIfValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIfTrueValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIfFalseValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReplaceValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReplaceWith  AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF AVAILABLE ttVariable THEN DO:
        ASSIGN
            lCanEditVariable        = TRUE
            fiName:SCREEN-VALUE     = ttVariable.varName
            cbDataType:SCREEN-VALUE = ttVariable.varDataType
            .
        
        IF cbDataType:SCREEN-VALUE EQ "Character" THEN
            fiFormat:SCREEN-VALUE = REPLACE(REPLACE(ttVariable.varFormat, "X(",""), ")", "").
        ELSE
            fiFormat:SCREEN-VALUE = ttVariable.varFormat.
                    
        IF ttVariable.varAlign NE "" AND (cbDataType:SCREEN-VALUE EQ "Integer" OR cbDataType:SCREEN-VALUE EQ "Decimal") THEN DO:
            IF ttVariable.varAlign EQ "" THEN
                cbAlignment:SCREEN-VALUE = "None".
            ELSE IF ttVariable.varAlign EQ "L" THEN
                cbAlignment:SCREEN-VALUE = "Left".
            ELSE IF ttVariable.varAlign EQ "R" THEN
                cbAlignment:SCREEN-VALUE = "Right".        
        END.
        ELSE
            cbAlignment:SCREEN-VALUE = "None".
        
        ASSIGN
            tgTrim:CHECKED              = FALSE
            tgCaps:CHECKED              = FALSE
            fiIfValue:SCREEN-VALUE      = ""
            fiIfFalse:SCREEN-VALUE      = ""
            fiIfTrue:SCREEN-VALUE       = ""
            fiReplaceValue:SCREEN-VALUE = ""
            fiReplaceWith:SCREEN-VALUE  = ""
            .
            
        IF ttVariable.varFunction1 EQ "TRIM" OR ttVariable.varFunction2 EQ "TRIM" OR ttVariable.varFunction3 EQ "TRIM" OR ttVariable.varFunction4 EQ "TRIM" OR ttVariable.varFunction5 EQ "TRIM" THEN
            tgTrim:CHECKED = TRUE.

        IF ttVariable.varFunction1 EQ "CAPS" OR ttVariable.varFunction2 EQ "CAPS" OR ttVariable.varFunction3 EQ "CAPS" OR ttVariable.varFunction4 EQ "CAPS" OR ttVariable.varFunction5 EQ "CAPS" THEN
            tgCaps:CHECKED = TRUE.
        
        IF ttVariable.varFunction1 NE "" THEN DO:
            IF ENTRY(1, ttVariable.varFunction1, "[") EQ "IF" THEN DO:
                DO iChar = 1 TO LENGTH(ttVariable.varFunction1):
                    IF SUBSTRING(ttVariable.varFunction1, iChar, 1) EQ "," THEN
                        iCount = iCount + 1.
                END.
                
                IF iCount GT 2 THEN DO:
                    MESSAGE "IF VALUE, THEN and ELSE values cannot have commas. Manually fix the data in editor"
                        VIEW-AS ALERT-BOX WARNING.
                    lCanEditVariable = FALSE.
                END.
                ELSE DO:
                    ASSIGN
                        cFunctionText = REPLACE(SUBSTRING(ttVariable.varFunction1, INDEX(ttVariable.varFunction1, '[') + 1), ']', '')
                        cIfValue      = ENTRY(1, cFunctionText)
                        cIfTrueValue  = ENTRY(2, cFunctionText)
                        cIfFalseValue = ENTRY(3, cFunctionText)
                        cIfValue      = REPLACE(cIfValue, '"', '')
                        cIfValue      = REPLACE(cIfValue, "'", "")
                        cIfTrueValue  = REPLACE(cIfTrueValue, '"', '')
                        cIfTrueValue  = REPLACE(cIfTrueValue, "'", "")
                        cIfFalseValue = REPLACE(cIfFalseValue, '"', '')
                        cIfFalseValue = REPLACE(cIfFalseValue, "'", "")
                        NO-ERROR.
                    
                    ASSIGN
                        fiIfValue:SCREEN-VALUE = cIfValue
                        fiIfTrue:SCREEN-VALUE  = cIfTrueValue
                        fiIfFalse:SCREEN-VALUE = cIfFalseValue
                        .
                END.
            END.
        END.
        
        IF ttVariable.varFunction5 BEGINS "REPLACE" THEN
            cReplaceText = ttVariable.varFunction5.

        IF ttVariable.varFunction4 BEGINS "REPLACE" THEN
            cReplaceText = ttVariable.varFunction4.

        IF ttVariable.varFunction3 BEGINS "REPLACE" THEN
            cReplaceText = ttVariable.varFunction3.

        IF ttVariable.varFunction2 BEGINS "REPLACE" THEN
            cReplaceText = ttVariable.varFunction2.

        IF ttVariable.varFunction1 BEGINS "REPLACE" THEN
            cReplaceText = ttVariable.varFunction1.
        
        IF cReplaceText NE "" THEN DO:
            IF ENTRY(1, cReplaceText, "[") EQ "REPLACE" THEN DO:
                iCount = 0.
                DO iChar = 1 TO LENGTH(ttVariable.varFunction1):
                    IF SUBSTRING(ttVariable.varFunction1, iChar, 1) EQ "," THEN
                        iCount = iCount + 1.
                END.
                
                IF iCount GT 2 THEN DO:
                    MESSAGE "Replace VALUE and WITH cannot have commas. Manually fix the data in editor"
                        VIEW-AS ALERT-BOX WARNING.
                    lCanEditVariable = FALSE.
                END.
                ELSE DO:
                    ASSIGN
                        cReplaceText  = REPLACE(SUBSTRING(cReplaceText, INDEX(cReplaceText, '[') + 1), ']', '')
                        cReplaceValue = ENTRY(1, cReplaceText)
                        cReplaceWith  = ENTRY(2, cReplaceText)
                        cReplaceValue = REPLACE(cReplaceValue, '"', '')
                        cReplaceValue = REPLACE(cReplaceValue, "'", "")
                        cReplaceWith  = REPLACE(cReplaceWith, '"', '')
                        cReplaceWith  = REPLACE(cReplaceWith, "'", "")
                        NO-ERROR.
                    
                    ASSIGN
                        fiReplaceValue:SCREEN-VALUE = cReplaceValue
                        fiReplaceWith:SCREEN-VALUE  = cReplaceWith
                        .
                END.
            END.        
        END. 
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidateVariable D-Dialog 
PROCEDURE pValidateVariable PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN fiName fiFormat cbDataType cbAlignment fiIfValue fiIfTrue fiIfFalse fiReplaceValue fiReplaceWith tgCaps tgTrim.
    
    IF fiName EQ "" THEN DO:
        opcMessage = "Name cannot be empty".
        RETURN.
    END.

    IF fiFormat NE "" THEN DO:
        IF cbDataType EQ "Character" THEN
            INTEGER(fiFormat) NO-ERROR.
        ELSE IF cbDataType EQ "Integer" OR cbDataType EQ "Decimal" OR cbDataType EQ "Decimal-Integer" THEN
            STRING(1, fiFormat) NO-ERROR.
        ELSE IF cbDataType EQ "Logical" THEN
            STRING(TRUE, fiFormat).

        IF ERROR-STATUS:ERROR THEN DO:
            opcMessage = "Invalid format for " + cbDataType + ". Enter any positive value".
            RETURN.
        END.
    END.
        
    IF TRIM(fiIfValue) NE "" THEN DO:
        IF INDEX(fiIfValue, ",") GT 0 OR INDEX(fiIfTrue, ",") GT 0 OR INDEX(fiIfFalse, ",") GT 0 OR 
           INDEX(fiIfValue, '"') GT 0 OR INDEX(fiIfTrue, '"') GT 0 OR INDEX(fiIfFalse, '"') GT 0 THEN DO:
            opcMessage = 'Cannot have commas (,) or quotes (") in IF function IF VALUE, THEN or ELSE values'.
            RETURN.
        END.
    END.
    
    IF TRIM(fiReplaceValue) NE "" THEN DO:
        IF INDEX(fiReplaceValue, ",") GT 0 OR INDEX(fiReplaceWith, ",") GT 0 OR
           INDEX(fiReplaceValue, '"') GT 0 OR INDEX(fiReplaceWith, '"') GT 0 THEN DO:
            opcMessage = 'Cannot have commas (,) or quotes (") in REPLACE functions VALUE or WITH values'.
            RETURN.
        END.    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttVariable"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

