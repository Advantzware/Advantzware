&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: dynForm.w

  Description: Dynamic Forms

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.9.2021

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define dynFormFolder Resources/dynForms
&Scoped-define displayFields dynForm.formID ~
dynForm.clientID ~
dynForm.isDefault ~
dynForm.formWidth ~
dynForm.formHeight ~
~{&FIELDS-IN-QUERY-viewFrame}
&Scoped-define enabledFields ~{&FIELDS-IN-QUERY-viewFrame}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE cMode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetIDName AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBuilder      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSuperAdmin   AS LOGICAL   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME formBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynForm dynFormTarget

/* Definitions for BROWSE formBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-formBrowse dynForm.formID dynForm.clientID ~
dynForm.formName dynForm.isActive dynForm.isDefault dynForm.formType ~
dynForm.company dynForm.targetType dynForm.subjectID dynForm.formWidth ~
dynForm.formHeight 
&Scoped-define ENABLED-FIELDS-IN-QUERY-formBrowse 
&Scoped-define QUERY-STRING-formBrowse FOR EACH dynForm NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-formBrowse OPEN QUERY formBrowse FOR EACH dynForm NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-formBrowse dynForm
&Scoped-define FIRST-TABLE-IN-QUERY-formBrowse dynForm


/* Definitions for BROWSE targetBrowse                                  */
&Scoped-define FIELDS-IN-QUERY-targetBrowse fTargetIDName() @ cTargetIDName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-targetBrowse 
&Scoped-define QUERY-STRING-targetBrowse FOR EACH dynFormTarget ~
      WHERE dynFormTarget.formID EQ dynForm.formID AND ~
dynFormTarget.clientID EQ dynForm.clientID NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-targetBrowse OPEN QUERY targetBrowse FOR EACH dynFormTarget ~
      WHERE dynFormTarget.formID EQ dynForm.formID AND ~
dynFormTarget.clientID EQ dynForm.clientID NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-targetBrowse dynFormTarget
&Scoped-define FIRST-TABLE-IN-QUERY-targetBrowse dynFormTarget


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-formBrowse}~
    ~{&OPEN-QUERY-targetBrowse}

/* Definitions for FRAME viewFrame                                      */
&Scoped-define FIELDS-IN-QUERY-viewFrame dynForm.isActive dynForm.formName ~
dynForm.formType dynForm.company dynForm.targetType dynForm.subjectID ~
dynForm.externalForm 
&Scoped-define QUERY-STRING-viewFrame FOR EACH dynForm SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH dynForm SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFrame dynForm
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame dynForm


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS formBrowse btnAddTarget targetBrowse ~
btnDeleteTarget 

/* Custom List Definitions                                              */
/* transPanel,transInit,transUpdate,List-4,List-5,List-6                */
&Scoped-define transPanel btnAdd btnCancel btnCopy btnDelete btnReset ~
btnUpdate 
&Scoped-define transInit btnAdd btnCopy btnDelete btnUpdate 
&Scoped-define transUpdate btnCancel btnReset btnUpdate 
&Scoped-define List-6 btnFormBuilder 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNextClientID C-Win 
FUNCTION fNextClientID RETURNS INTEGER
  ( ipiFormID AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNextFormID C-Win 
FUNCTION fNextFormID RETURNS INTEGER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTargetIDName C-Win 
FUNCTION fTargetIDName RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddTarget 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Target" 
     SIZE 8 BY 1.91 TOOLTIP "Add Target".

DEFINE BUTTON btnDeleteTarget 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete Target" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Target".

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/element_copy_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnFormBuilder 
     IMAGE-UP FILE "Graphics/32x32/compasses.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/compasses_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Form Builder" 
     SIZE 8 BY 1.91 TOOLTIP "Form Builder".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 58 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY formBrowse FOR 
      dynForm SCROLLING.

DEFINE QUERY targetBrowse FOR 
      dynFormTarget SCROLLING.

DEFINE QUERY viewFrame FOR 
      dynForm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE formBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS formBrowse C-Win _STRUCTURED
  QUERY formBrowse NO-LOCK DISPLAY
      dynForm.formID FORMAT ">>>9":U LABEL-BGCOLOR 14
      dynForm.clientID FORMAT ">>>9":U
      dynForm.formName FORMAT "x(30)":U LABEL-BGCOLOR 14
      dynForm.isActive FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      dynForm.isDefault FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      dynForm.formType FORMAT "x(16)":U LABEL-BGCOLOR 14
      dynForm.company FORMAT "x(3)":U LABEL-BGCOLOR 14
      dynForm.targetType FORMAT "x(16)":U LABEL-BGCOLOR 14
      dynForm.subjectID FORMAT ">,>>>,>>9":U LABEL-BGCOLOR 14
      dynForm.formWidth FORMAT ">>>9":U
      dynForm.formHeight FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 28.57
         TITLE "Dynamic Forms".

DEFINE BROWSE targetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS targetBrowse C-Win _STRUCTURED
  QUERY targetBrowse NO-LOCK DISPLAY
      fTargetIDName() @ cTargetIDName COLUMN-LABEL "Target ID and Name" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 13.33
         TITLE "Target Type Records" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     formBrowse AT ROW 1 COL 1 WIDGET-ID 1600
     btnAddTarget AT ROW 27.67 COL 145 HELP
          "Add Target" WIDGET-ID 118
     targetBrowse AT ROW 14.33 COL 101 WIDGET-ID 1700
     btnDeleteTarget AT ROW 27.67 COL 153 HELP
          "Delete Target" WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57 WIDGET-ID 100.

DEFINE FRAME viewFrame
     btnAdd AT ROW 11.05 COL 11 HELP
          "Add" WIDGET-ID 118
     dynForm.formID AT ROW 1.24 COL 13 COLON-ALIGNED WIDGET-ID 680
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     dynForm.clientID AT ROW 1.24 COL 37 COLON-ALIGNED WIDGET-ID 678
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     dynForm.isActive AT ROW 1.24 COL 49 WIDGET-ID 690
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY 1
     dynForm.formName AT ROW 2.43 COL 13 COLON-ALIGNED WIDGET-ID 682
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     dynForm.isDefault AT ROW 2.43 COL 49 WIDGET-ID 692
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     dynForm.formType AT ROW 3.62 COL 13 COLON-ALIGNED WIDGET-ID 694
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Acknowledgement","BOL","Invoice","Job Ticket","Quote" 
          DROP-DOWN-LIST
          SIZE 23 BY 1
          BGCOLOR 15 
     dynForm.company AT ROW 4.81 COL 13 COLON-ALIGNED WIDGET-ID 708
          VIEW-AS COMBO-BOX INNER-LINES 100
          LIST-ITEM-PAIRS "999 - Company 999","999"
          DROP-DOWN-LIST
          SIZE 45 BY 1
     dynForm.targetType AT ROW 6 COL 13 COLON-ALIGNED WIDGET-ID 710
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Customer","Vendor" 
          DROP-DOWN-LIST
          SIZE 23 BY 1
     dynForm.subjectID AT ROW 7.19 COL 13 COLON-ALIGNED WIDGET-ID 706
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     dynForm.formWidth AT ROW 8.38 COL 13 COLON-ALIGNED WIDGET-ID 704
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
          BGCOLOR 15 
     dynForm.formHeight AT ROW 8.38 COL 37 COLON-ALIGNED WIDGET-ID 702
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     dynForm.externalForm AT ROW 9.57 COL 13 COLON-ALIGNED WIDGET-ID 712
          LABEL "External" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
          BGCOLOR 15 
     btnCancel AT ROW 11.05 COL 43 HELP
          "Cancel" WIDGET-ID 120
     btnCopy AT ROW 11.05 COL 19 HELP
          "Copy" WIDGET-ID 122
     btnDelete AT ROW 11.05 COL 27 HELP
          "Delete" WIDGET-ID 124
     btnReset AT ROW 11.05 COL 35 HELP
          "Reset" WIDGET-ID 126
     btnUpdate AT ROW 11.05 COL 3 HELP
          "Update/Save" WIDGET-ID 128
     btnFormBuilder AT ROW 11.05 COL 51 HELP
          "Form Builder" WIDGET-ID 296
     RECT-PANEL AT ROW 10.76 COL 2 WIDGET-ID 130
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 101 ROW 1
         SIZE 60 BY 13.33
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 1500.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic Form"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME viewFrame:MOVE-AFTER-TAB-ITEM (formBrowse:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME viewFrame:MOVE-BEFORE-TAB-ITEM (targetBrowse:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB formBrowse 1 DEFAULT-FRAME */
/* BROWSE-TAB targetBrowse viewFrame DEFAULT-FRAME */
ASSIGN 
       formBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAdd IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnCopy IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnDelete IN FRAME viewFrame
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnFormBuilder IN FRAME viewFrame
   6                                                                    */
/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR BUTTON btnUpdate IN FRAME viewFrame
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN dynForm.clientID IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR COMBO-BOX dynForm.company IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynForm.externalForm IN FRAME viewFrame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN dynForm.formHeight IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN dynForm.formID IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN dynForm.formName IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX dynForm.formType IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynForm.formWidth IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR TOGGLE-BOX dynForm.isActive IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dynForm.isDefault IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynForm.subjectID IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX dynForm.targetType IN FRAME viewFrame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE formBrowse
/* Query rebuild information for BROWSE formBrowse
     _TblList          = "ASI.dynForm"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > ASI.dynForm.formID
"dynForm.formID" ? ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ASI.dynForm.clientID
     _FldNameList[3]   > ASI.dynForm.formName
"dynForm.formName" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.dynForm.isActive
"dynForm.isActive" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[5]   > ASI.dynForm.isDefault
"dynForm.isDefault" ? ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[6]   > ASI.dynForm.formType
"dynForm.formType" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.dynForm.company
"dynForm.company" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.dynForm.targetType
"dynForm.targetType" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.dynForm.subjectID
"dynForm.subjectID" ? ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.dynForm.formWidth
     _FldNameList[11]   = ASI.dynForm.formHeight
     _Query            is OPENED
*/  /* BROWSE formBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE targetBrowse
/* Query rebuild information for BROWSE targetBrowse
     _TblList          = "ASI.dynFormTarget"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "dynFormTarget.formID EQ dynForm.formID AND
dynFormTarget.clientID EQ dynForm.clientID"
     _FldNameList[1]   > "_<CALC>"
"fTargetIDName() @ cTargetIDName" "Target ID and Name" "x(40)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE targetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "ASI.dynForm"
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Form */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Form */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF VALID-HANDLE(hBuilder) THEN
  RUN pCloseBuilder IN hBuilder.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnAddTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddTarget C-Win
ON CHOOSE OF btnAddTarget IN FRAME DEFAULT-FRAME /* Add Target */
DO:
    RUN pAddTarget.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnDeleteTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTarget C-Win
ON CHOOSE OF btnDeleteTarget IN FRAME DEFAULT-FRAME /* Delete Target */
DO:
    RUN pDeleteTarget.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnFormBuilder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFormBuilder C-Win
ON CHOOSE OF btnFormBuilder IN FRAME viewFrame /* Form Builder */
DO:
    IF NOT VALID-HANDLE(hBuilder) THEN
    RUN AOA/dynFormBldr.w PERSISTENT SET hBuilder.
    RUN pInitBuilder IN hBuilder (ROWID(dynForm)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dynForm.externalForm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynForm.externalForm C-Win
ON HELP OF dynForm.externalForm IN FRAME viewFrame /* External */
DO:
    DEFINE VARIABLE cJrxmlFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

    cInitDir = ".\".
    SYSTEM-DIALOG GET-FILE cJrxmlFile
        TITLE "Select Image File"
        FILTERS "Jasper Report Files (*.jrxml)" "*.jrxml",
                "All Files (*.*) " "*.*"
        INITIAL-DIR cInitDir
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN
    SELF:SCREEN-VALUE = cJrxmlFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME formBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME formBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formBrowse C-Win
ON DEFAULT-ACTION OF formBrowse IN FRAME DEFAULT-FRAME /* Dynamic Forms */
DO:
    APPLY "CHOOSE":U TO btnFormBuilder IN FRAME viewFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formBrowse C-Win
ON START-SEARCH OF formBrowse IN FRAME DEFAULT-FRAME /* Dynamic Forms */
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formBrowse C-Win
ON VALUE-CHANGED OF formBrowse IN FRAME DEFAULT-FRAME /* Dynamic Forms */
DO:
    RUN pDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME dynForm.subjectID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynForm.subjectID C-Win
ON LEAVE OF dynForm.subjectID IN FRAME viewFrame /* Subject ID */
DO:
    FIND FIRST dynSubject
         WHERE dynSubject.subjectID EQ INTEGER({&SELF-NAME}:SCREEN-VALUE)
         NO-ERROR.
    IF AVAILABLE dynSubject THEN DO:
    END. /* if avail */
    ELSE DO:
        MESSAGE 
            "Invalid Subject ID, Please Try Again."
        VIEW-AS ALERT-BOX ERROR.
        {&SELF-NAME}:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

&Scoped-define sdBrowseName formBrowse
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName targetBrowse
{methods/template/brwcustom2.i 2}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  lSuperAdmin = DYNAMIC-FUNCTION("sfIsUserSuperAdmin").
  lSuperAdmin = NO.
  RUN pGetCompany.
  RUN enable_UI.
  RUN pDisplay.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pByFormID" "dynForm.formID"}
{methods/sortByProc.i "pByFormName" "dynForm.formName"}
{methods/sortByProc.i "pByFormType" "dynForm.formType"}
{methods/sortByProc.i "pByTargetType" "dynForm.targetType"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE formBrowse btnAddTarget targetBrowse btnDeleteTarget 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  IF AVAILABLE dynForm THEN 
    DISPLAY dynForm.isActive dynForm.formName dynForm.formType dynForm.company 
          dynForm.targetType dynForm.subjectID dynForm.externalForm 
      WITH FRAME viewFrame IN WINDOW C-Win.
  ENABLE btnAdd btnCopy btnDelete btnUpdate btnFormBuilder 
      WITH FRAME viewFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddTarget C-Win 
PROCEDURE pAddTarget :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF dynForm.targetType EQ "" THEN
    MESSAGE 
        "Please Set a Specific Target Type for this Form."
    VIEW-AS ALERT-BOX.
    ELSE IF dynForm.company EQ "" THEN
    MESSAGE
        "Please Set a Specific Company for this Form."
    VIEW-AS ALERT-BOX ERROR.
    ELSE IF CAN-FIND(FIRST company
                     WHERE company.company EQ dynForm.company) THEN DO:
        RUN AOA/dynFormTarget.w (
            dynForm.formID,
            dynForm.clientID,
            dynForm.company,
            dynForm.targetType
            ).
        {&OPEN-QUERY-targetBrowse}
    END. /* if can-find */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign C-Win 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO TRANSACTION WITH FRAME viewFrame:
        FIND CURRENT dynForm EXCLUSIVE-LOCK.
        ASSIGN
            {&enabledFields}
            dynForm.formID
            dynForm.clientID
            dynForm.isDefault
            dynForm.formWidth
            dynForm.formHeight
            .
        FIND CURRENT dynForm NO-LOCK.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearView C-Win 
PROCEDURE pClearView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    DO WITH FRAME viewFrame:
        ASSIGN
            hWidget = FRAME viewFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "BUTTON" AND
               hWidget:SELECTABLE EQ NO AND 
               hWidget:SENSITIVE THEN
            hWidget:SCREEN-VALUE = IF hWidget:TYPE EQ "TOGGLE-BOX" THEN "NO" ELSE "".
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            dynForm.clientID:SCREEN-VALUE   = ""
            dynForm.company:SCREEN-VALUE    = ""
            dynForm.formHeight:SCREEN-VALUE = ""
            dynForm.formID:SCREEN-VALUE     = ""
            dynForm.formName:SCREEN-VALUE   = ""
            dynForm.formType:SCREEN-VALUE   = ""
            dynForm.formWidth:SCREEN-VALUE  = ""
            dynForm.targetType:SCREEN-VALUE = ""
            dynForm.subjectID:SCREEN-VALUE  = ""
            .
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rRowID    AS ROWID   NO-UNDO.

    DO WITH FRAME viewFrame:
        IF iphMode:LABEL EQ "Add" AND NOT lSuperAdmin THEN
        RETURN.
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                BROWSE {&BROWSE-NAME}:SENSITIVE = NO.
                ENABLE {&transUpdate} {&enabledFields}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
                IF iphMode:LABEL EQ "Add" THEN DO:
                    RUN pClearView.
                    DISABLE btnReset.
                END. /* add */
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL = "Save"
                    .
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        CREATE dynForm.
                        CASE cMode:
                            WHEN "Add" THEN
                            ASSIGN
                                dynForm.formID:SCREEN-VALUE    = STRING(fNextFormID())
                                dynForm.clientID:SCREEN-VALUE  = ""
                                dynForm.isDefault:SCREEN-VALUE = "YES"
                                .
                            WHEN "Copy" THEN
                            ASSIGN
                                dynForm.clientID:SCREEN-VALUE  = STRING(fNextClientID(INTEGER(dynForm.formID:SCREEN-VALUE)))
                                dynForm.isDefault:SCREEN-VALUE = "NO"
                                .
                        END CASE.
                        ASSIGN
                            dynForm.isActive:SCREEN-VALUE = "YES"
                            cColumnLabel = "formID"
                            rRowID = ROWID(dynForm)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pReopenBrowse.
                        REPOSITION formBrowse TO ROWID rRowID.
                    END. /* if add/copy */
                    ELSE
                    BROWSE {&BROWSE-NAME}:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields}.
                ENABLE {&transInit}.
                BROWSE {&BROWSE-NAME}:SENSITIVE = YES.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                ASSIGN
                    FRAME viewFrame:TITLE = "View"
                    btnUpdate:LABEL       = "Update"
                    BROWSE {&BROWSE-NAME}:SENSITIVE = YES
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE dynForm THEN DO:
                    MESSAGE
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        FIND CURRENT dynForm EXCLUSIVE-LOCK.
                        DELETE dynForm.
                        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE dynForm THEN
                    BROWSE {&BROWSE-NAME}:REFRESH().
                    RUN pDisplay.
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                DISABLE {&transPanel}.
                ENABLE {&transUpdate}.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF dynForm.formName:SENSITIVE THEN
        APPLY "ENTRY":U TO dynForm.formName.
        ELSE
        APPLY "ENTRY":U TO BROWSE {&BROWSE-NAME}.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteTarget C-Win 
PROCEDURE pDeleteTarget :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF NOT AVAILABLE dynFormTarget THEN RETURN.
    DO TRANSACTION:
        FIND CURRENT dynFormTarget EXCLUSIVE-LOCK.
        DELETE dynFormTarget.
    END.    
    {&OPEN-QUERY-targetBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay C-Win 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME viewFrame:
        IF AVAILABLE dynForm THEN DO:
            ASSIGN
                dynForm.company:SCREEN-VALUE    = " "
                dynForm.targetType:SCREEN-VALUE = " "
                .
            DISPLAY {&displayFields}.
            IF lSuperAdmin THEN
            ENABLE {&transInit}.
            ELSE IF dynForm.clientID GT 0 THEN DO:
                ENABLE {&transInit}.
                DISABLE btnAdd.
            END. /* clientid gt 0 */ 
            ELSE DO:
                DISABLE {&transPanel}.
                ENABLE btnCopy.
            END.
        END. /* if avail */
        ELSE DO:
            RUN pClearView.
            DISABLE {&transPanel}.
            ENABLE btnAdd.
        END. /* else */
        {&OPEN-QUERY-targetBrowse}
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCompany C-Win 
PROCEDURE pGetCompany :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.

    dynForm.company:LIST-ITEM-PAIRS IN FRAME viewFrame = "All Companies, ".
    FOR EACH company NO-LOCK:
        cLabel = REPLACE(company.company + " - " + company.name,",","").
        dynForm.company:ADD-LAST(cLabel,company.company).
    END. /* each company */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "formID" THEN
        RUN pByFormID.
        WHEN "formName" THEN
        RUN pByFormName.
        WHEN "formType" THEN
        RUN pByFormType.
        WHEN "targetType" THEN
        RUN pByTargetType.
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNextClientID C-Win 
FUNCTION fNextClientID RETURNS INTEGER
  ( ipiFormID AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DO WHILE TRUE:
        idx = idx + 1.
        IF CAN-FIND(FIRST dynForm
                    WHERE dynForm.formID   EQ ipiFormID
                      AND dynForm.clientID EQ idx) THEN
        NEXT.
        LEAVE.
    END. /* each bdynform */
    RETURN idx.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNextFormID C-Win 
FUNCTION fNextFormID RETURNS INTEGER
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DO WHILE TRUE:
        idx = idx + 1.
        IF CAN-FIND(FIRST dynForm
                    WHERE dynForm.formID EQ idx) THEN
        NEXT.
        LEAVE.
    END. /* each bdynform */
    RETURN idx.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTargetIDName C-Win 
FUNCTION fTargetIDName RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTargetIDName AS CHARACTER NO-UNDO.

    CASE dynForm.targetType:
        WHEN "Customer" THEN DO:
            FIND FIRST cust NO-LOCK
                 WHERE cust.rec_key EQ dynFormTarget.tableRecKey
                 NO-ERROR.
            cTargetIDName = IF AVAILABLE cust THEN cust.cust-no + " - " + cust.name
                            ELSE "".
        END. /* customer */
        WHEN "Vendor" THEN DO:
            FIND FIRST vend NO-LOCK
                 WHERE vend.rec_key EQ dynFormTarget.tableRecKey
                 NO-ERROR.
            cTargetIDName = IF AVAILABLE vend THEN vend.vend-no + " - " + vend.name
                            ELSE "".
        END. /* customer */
    END CASE.
    RETURN cTargetIDName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

