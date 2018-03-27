&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:              lookups.w

  Description:       Lookup Browser User Interface

  Input Parameters:  Lookup Browser Program Name

  Output Parameters: Saved Indicator

  Author:            Ron Stark

  Created:           12/01/96

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER m_lookup_prgm AS CHARACTER.
&ELSE
DEFINE VARIABLE m_lookup_prgm AS CHARACTER INITIAL "prgrms.".
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE shandle AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE m-show-fields AS CHARACTER NO-UNDO.
DEFINE VARIABLE m-show-fields-yellow AS CHARACTER NO-UNDO.
DEFINE VARIABLE m-order-fields AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE data-type AS CHARACTER NO-UNDO.
DEFINE VARIABLE data-format AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-1 RECT-2 Btn_Get_Field ~
m-where-statement m-order-values auto_set selected-set Btn_UP show-fields ~
order-fields select-list Btn_Add Btn_Remove Btn_Down m-frame-title ~
m-top-include m-def-include m-end-include m-ui-prgmname m-font ~
m-height-size m-width-size Btn_Reset Btn_Save Btn_Delete Btn_Cancel Btn_OK ~
Btn_Description 
&Scoped-Define DISPLAYED-OBJECTS m-lookup-prgm m-lookup-db m-lookup-file ~
m-return-field m-where-statement m-order-values auto_set selected-set ~
show-fields order-fields select-list m-frame-title m-top-include ~
m-def-include m-end-include m-ui-prgmname m_status m-font m-height-size ~
m-width-size F1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 F1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     IMAGE-UP FILE "adeicon/left":U
     LABEL "&Add" 
     SIZE 8.2 BY 2
     FONT 4.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Cancel" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Description 
     LABEL "Descriptio&n" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Down 
     IMAGE-UP FILE "adeicon/vcrfwd":U
     LABEL "Move Do&wn" 
     SIZE 8.2 BY 2
     FONT 4.

DEFINE BUTTON Btn_Get_Field 
     IMAGE-UP FILE "adeicon/fields-u":U
     LABEL "Get Field" 
     SIZE 8.2 BY 2.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Remove 
     IMAGE-UP FILE "adeicon/right":U
     LABEL "Re&move" 
     SIZE 8.2 BY 2
     FONT 4.

DEFINE BUTTON Btn_Reset 
     LABEL "&Reset" 
     SIZE 14 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Save 
     LABEL "&Save" 
     SIZE 14 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_UP 
     IMAGE-UP FILE "adeicon/vcrrew":U
     LABEL "Move &Up" 
     SIZE 8.2 BY 2
     FONT 4.

DEFINE VARIABLE m-font AS INTEGER FORMAT "9":U INITIAL 4 
     LABEL "Font" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "0","1","2","3","4","5","6","7" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE m-where-statement AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 128.8 BY 4.67 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE m-def-include AS CHARACTER FORMAT "X(256)":U 
     LABEL "Def. Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE m-end-include AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE m-frame-title AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frame Title" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE m-height-size AS CHARACTER FORMAT "X(02)":U INITIAL "19" 
     LABEL "Height" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE m-lookup-db AS CHARACTER FORMAT "X(256)":U 
     LABEL "DB" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE m-lookup-file AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE m-lookup-prgm AS CHARACTER FORMAT "X(12)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 NO-UNDO.

DEFINE VARIABLE m-order-values AS CHARACTER FORMAT "X(256)":U 
     LABEL "Order Values" 
     VIEW-AS FILL-IN 
     SIZE 102.8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE m-return-field AS CHARACTER FORMAT "X(30)":U 
     LABEL "Return Field" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

DEFINE VARIABLE m-top-include AS CHARACTER FORMAT "X(256)":U 
     LABEL "Top Include" 
     VIEW-AS FILL-IN 
     SIZE 95.2 BY 1 NO-UNDO.

DEFINE VARIABLE m-ui-prgmname AS CHARACTER FORMAT "X(12)":U 
     LABEL "UI Program" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1 NO-UNDO.

DEFINE VARIABLE m-width-size AS CHARACTER FORMAT "X(03)":U INITIAL "46" 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE m_status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE selected-set AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Show Database Fields", 1,
"Browser Order Fields", 2
     SIZE 64.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.6 BY 2.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 32.2 BY 2.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16 BY 2.

DEFINE VARIABLE order-fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 5.67 NO-UNDO.

DEFINE VARIABLE select-list AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 14.95 NO-UNDO.

DEFINE VARIABLE show-fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 32.2 BY 5.67 NO-UNDO.

DEFINE VARIABLE auto_set AS LOGICAL INITIAL yes 
     LABEL "Auto Set Order Values" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Get_Field AT ROW 1 COL 34.2 HELP
          "Get Database, Table, & Return Field"
     m-lookup-prgm AT ROW 1.38 COL 12.6 COLON-ALIGNED
     m-lookup-db AT ROW 1.38 COL 47.6 COLON-ALIGNED
     m-lookup-file AT ROW 1.38 COL 77 COLON-ALIGNED
     m-return-field AT ROW 1.38 COL 114.8 COLON-ALIGNED HELP
          "Enter Field Name for Lookup Browser to Return to Calling Frame"
     m-where-statement AT ROW 3 COL 14.6 HELP
          "Enter Lookup Table's Key Phrase" NO-LABEL
     m-order-values AT ROW 7.91 COL 12.6 COLON-ALIGNED HELP
          "Enter List of Browse Order Values (Comma Delimited)"
     auto_set AT ROW 7.91 COL 118.4 HELP
          "Auto Set Order Values Indicator"
     selected-set AT ROW 9.24 COL 14.6 HELP
          "Make Selection List Active" NO-LABEL
     Btn_UP AT ROW 10.29 COL 91.6 HELP
          "Move Selected Item Up"
     show-fields AT ROW 10.57 COL 14.6 HELP
          "Select Show Field" NO-LABEL
     order-fields AT ROW 10.57 COL 46.8 HELP
          "Select Browser Order Field" NO-LABEL
     select-list AT ROW 10.57 COL 111.2 HELP
          "Select Item to Add to Select List" NO-LABEL
     Btn_Add AT ROW 12.29 COL 87.4 HELP
          "Add Selected Item"
     Btn_Remove AT ROW 12.29 COL 95.8 HELP
          "Remove Selected Item"
     Btn_Down AT ROW 14.29 COL 91.6 HELP
          "Move Selected Item Down"
     m-frame-title AT ROW 16.57 COL 12.6 COLON-ALIGNED HELP
          "Enter Lookup Browser's Title (will also appear in prgrms)"
     m-top-include AT ROW 18.24 COL 12.6 COLON-ALIGNED HELP
          "Enter Include File Name to Execute at Beginning of Lookup"
     m-def-include AT ROW 19.48 COL 12.6 COLON-ALIGNED HELP
          "Enter Definition Include File Name to Declare Local Variables"
     m-end-include AT ROW 20.71 COL 12.6 COLON-ALIGNED HELP
          "Enter Include File Name to Execute at End of Lookup"
     m-ui-prgmname AT ROW 22.19 COL 12.4 COLON-ALIGNED HELP
          "Enter UI Program to Run from within Lookup Browser"
     m_status AT ROW 22.19 COL 30 NO-LABEL
     m-font AT ROW 22.19 COL 71 COLON-ALIGNED HELP
          "Select Font"
     m-height-size AT ROW 22.19 COL 86.4 COLON-ALIGNED HELP
          "Enter Height of Lookup Browser"
     m-width-size AT ROW 22.19 COL 99.4 COLON-ALIGNED HELP
          "Enter Width of Lookup Browser"
     Btn_Reset AT ROW 24.05 COL 31.8 HELP
          "Use this function to RESET this Lookup Browser"
     Btn_Save AT ROW 24.05 COL 47.2 HELP
          "Use this function to SAVE this Lookup Browser"
     Btn_Delete AT ROW 24.05 COL 62.6 HELP
          "Use this function to DELETE this Lookup Browser"
     Btn_Cancel AT ROW 24.05 COL 79.4 HELP
          "Use this function to CANCEL Update/Create Lookup Browser"
     Btn_OK AT ROW 24.05 COL 94.8
     Btn_Description AT ROW 24.1 COL 15.4 HELP
          "Use this function to CREATE DESCRIPTION Lookup Browser"
     F1 AT ROW 22.19 COL 27 NO-LABEL
     "Key Phrase:" VIEW-AS TEXT
          SIZE 11.6 BY 1 AT ROW 2.91 COL 3
     "Available Selections" VIEW-AS TEXT
          SIZE 19.4 BY 1 AT ROW 9.33 COL 118
     RECT-3 AT ROW 23.62 COL 14.4
     RECT-1 AT ROW 23.62 COL 30.4
     RECT-2 AT ROW 23.62 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143 BY 24.76.


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
         TITLE              = "Lookups Create/Update Interface"
         HEIGHT             = 24.76
         WIDTH              = 143
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 143
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 143
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
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN F1 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN m-lookup-db IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m-lookup-file IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m-lookup-prgm IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m-return-field IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m_status IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lookups Create/Update Interface */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lookups Create/Update Interface */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME auto_set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL auto_set C-Win
ON VALUE-CHANGED OF auto_set IN FRAME DEFAULT-FRAME /* Auto Set Order Values */
DO:
  ASSIGN
    {&SELF-NAME}
    m-order-values:SENSITIVE = NOT {&SELF-NAME}.
  RUN Auto_Set_Order_Values.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME DEFAULT-FRAME /* Add */
DO:
  DO i = 1 TO select-list:NUM-ITEMS:
    IF select-list:IS-SELECTED(i) THEN
      IF shandle:LOOKUP(select-list:ENTRY(i)) = 0 THEN
      ldummy = shandle:ADD-LAST(select-list:ENTRY(i)).
  END.
  DO i = 1 TO select-list:NUM-ITEMS:
    IF select-list:IS-SELECTED(i) THEN
    select-list:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  m_status:SCREEN-VALUE = "DELETE Lookup?".
  MESSAGE "Delete This Lookup Browser?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE m_response AS LOGICAL.
  CASE m_response:
    WHEN yes THEN
    DO:
      FIND prgrms WHERE prgrms.prgmname = m_lookup_prgm NO-ERROR.
      IF AVAILABLE prgrms THEN
      DELETE prgrms.
      IF SEARCH("lookups/" + m_lookup_prgm + "p") NE ? THEN
      OS-DELETE VALUE("lookups/" + m_lookup_prgm + "p").
      IF SEARCH("lookups/" + m_lookup_prgm + "r") NE ? THEN
      OS-DELETE VALUE("lookups/" + m_lookup_prgm + "r").
      APPLY "GO" TO FRAME {&FRAME-NAME}.
    END.
    WHEN no THEN
    m_status:SCREEN-VALUE = "".
    OTHERWISE
    m_status:SCREEN-VALUE = "DELETE Cancelled".
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Description
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Description C-Win
ON CHOOSE OF Btn_Description IN FRAME DEFAULT-FRAME /* Description */
DO:
  DEFINE VARIABLE descrip_lookup AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.

  descrip_lookup = TRIM(REPLACE(m_lookup_prgm,".","_.")).
  IF LENGTH(descrip_lookup) GE 10 THEN
  SUBSTR(descrip_lookup,8,1) = "".
  IF SEARCH("lookups/" + descrip_lookup + "p") NE ? THEN
  DO:
    MESSAGE "Lookup" descrip_lookup "already Exists, Open it?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ldummy.
    IF NOT ldummy THEN
    RETURN NO-APPLY.
    m_lookup_prgm = descrip_lookup.
    APPLY "CHOOSE" TO Btn_Reset.
    RETURN NO-APPLY.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      m_lookup_prgm = descrip_lookup
      m-lookup-prgm:SCREEN-VALUE = descrip_lookup
      m-frame-title:SCREEN-VALUE =
          REPLACE(m-frame-title:SCREEN-VALUE,"Lookup","Description Lookup")
      m-return-field:SCREEN-VALUE = order-fields:ENTRY(2)
      cdummy = show-fields:ENTRY(2)
      ldummy = show-fields:DELETE(2)
      ldummy = show-fields:ADD-FIRST(cdummy)
      cdummy = order-fields:ENTRY(2)
      ldummy = order-fields:DELETE(2)
      ldummy = order-fields:ADD-FIRST(cdummy).
    /*RUN Enable_UI.*/
    APPLY "VALUE-CHANGED" TO selected-set.
    RUN Auto_Set_Order_Values.
    ASSIGN
      m-order-values:SENSITIVE = NOT auto_set
      m_status:SCREEN-VALUE = "DESCRIPTION Lookup Created".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Down C-Win
ON CHOOSE OF Btn_Down IN FRAME DEFAULT-FRAME /* Move Down */
DO:
  IF shandle:SCREEN-VALUE NE "" THEN
  DO i = 1 TO shandle:NUM-ITEMS - 1:
    IF NOT shandle:IS-SELECTED(i) THEN
    NEXT.
    ASSIGN
      ldummy = shandle:INSERT(shandle:SCREEN-VALUE,i + 2)
      ldummy = shandle:DELETE(i)
      shandle:SCREEN-VALUE = shandle:ENTRY(i + 1).
    LEAVE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Get_Field
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Get_Field C-Win
ON CHOOSE OF Btn_Get_Field IN FRAME DEFAULT-FRAME /* Get Field */
DO:
  DEFINE VARIABLE get_field AS CHARACTER NO-UNDO.

  IF m-lookup-db = "" THEN
  m-lookup-db = "NOSWEAT".
  CREATE ALIAS dictdb FOR DATABASE VALUE(m-lookup-db).
  get_field = m-lookup-db:SCREEN-VALUE + "." +
        m-lookup-file:SCREEN-VALUE + "." + 
        m-return-field:SCREEN-VALUE.
  RUN Get_Procedure IN Persistent-Handle ("getfield.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (INPUT-OUTPUT get_field).
  ASSIGN
    m-lookup-db:SCREEN-VALUE = SUBSTR(get_field,1,INDEX(get_field,".") - 1)
    m-lookup-file:SCREEN-VALUE = SUBSTR(get_field,INDEX(get_field,".") + 1,
                                 R-INDEX(get_field,".") - INDEX(get_field,".") - 1)
    m-return-field:SCREEN-VALUE = SUBSTR(get_field,R-INDEX(get_field,".") + 1).
  FIND prgmxref
      WHERE prgmxref.table_name = m-lookup-file:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE prgmxref THEN
  DO:
    m-ui-prgmname:SCREEN-VALUE = prgmxref.prgmname.
    FIND prgrms OF prgmxref NO-LOCK NO-ERROR.
    IF AVAILABLE prgrms THEN
    m-frame-title:SCREEN-VALUE = prgrms.prgtitle + " Lookup".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "CHOOSE" TO Btn_Save.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME DEFAULT-FRAME /* Remove */
DO:
  IF shandle:SCREEN-VALUE NE "" THEN
  APPLY "DEFAULT-ACTION" TO shandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Reset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Reset C-Win
ON CHOOSE OF Btn_Reset IN FRAME DEFAULT-FRAME /* Reset */
DO:
  RUN Get_Lookup.
  RUN Enable_UI.
  APPLY "VALUE-CHANGED" TO selected-set.
  ASSIGN
    m-order-values:SENSITIVE = NOT auto_set
    m_status:SCREEN-VALUE = "Lookup Record RESET".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save C-Win
ON CHOOSE OF Btn_Save IN FRAME DEFAULT-FRAME /* Save */
DO:
  DEFINE VARIABLE cnt AS INTEGER NO-UNDO.

  ASSIGN
    m-lookup-db
    m-lookup-file
    m-where-statement
    m-order-values
    m-font
    m-height-size
    m-width-size
    m-frame-title
    m-return-field
    m-ui-prgmname
    m-def-include
    m-top-include
    m-end-include
    m-where-statement = TRIM(m-where-statement)
    auto_set.

  RUN Auto_Set_Order_Values.

  IF INT(m-height-size) GT 19 THEN
  ASSIGN
    m-height-size:SCREEN-VALUE = "19"
    m-height-size = "19".

  IF INT(m-width-size) LT 32 THEN
  ASSIGN
    m-width-size:SCREEN-VALUE = "32"
    m-width-size = "32".

  IF INT(m-width-size) GT 150 THEN
  ASSIGN
    m-width-size:SCREEN-VALUE = "150"
    m-width-size = "150".

  OUTPUT TO VALUE("lookups/" + m_lookup_prgm + "p").

  IF INDEX(m-where-statement,'"') NE 0 THEN
  ASSIGN m-where-statement = REPLACE(m-where-statement,'"','""').

  IF INDEX(m-def-include,'"') NE 0 THEN
  ASSIGN m-def-include = REPLACE(m-def-include,'"','""').

  IF INDEX(m-top-include,'"') NE 0 THEN
  ASSIGN m-top-include = REPLACE(m-top-include,'"','""').

  IF INDEX(m-end-include,'"') NE 0 THEN
  ASSIGN m-end-include = REPLACE(m-end-include,'"','""').

  PUT UNFORMATTED "/* " m_lookup_prgm "p - Generated " TODAY FORMAT "99/99/9999"
    " - " STRING(TIME,"HH:MM am") " by " USERID("NOSWEAT") SKIP
    "~"" m_lookup_prgm " ~" ~~" SKIP
    "~"" m-lookup-db " ~" ~~" SKIP
    "~"" m-lookup-file " ~" ~~" SKIP         
    "~"" m-where-statement " ~" ~~" SKIP
    "~"" m-return-field " ~" ~~" SKIP
    "~"" m-font " ~" ~~" SKIP
    "~"" m-height-size " ~" ~~" SKIP
    "~"" m-width-size " ~" ~~" SKIP
    "~"" show-fields:LIST-ITEMS " ~" ~~" SKIP
    "~"" m-order-values " ~" ~~" SKIP
    "~"" auto_set " ~" ~~" SKIP
    "~"" order-fields:LIST-ITEMS " ~" ~~" SKIP
    "~"" m-frame-title " ~" ~~" SKIP
    "~"" m-top-include " ~" ~~" SKIP
    "~"" m-def-include " ~" ~~" SKIP
    "~"" m-end-include " ~" ~~" SKIP
    "~"" m-ui-prgmname " ~" ~~" SKIP
    "*/" SKIP(1).

  IF m-lookup-db = "dictdb" OR m-lookup-db = "" THEN
  PUT UNFORMATTED
    "DEFINE INPUT-OUTPUT PARAMETER m-lookup-var AS CHARACTER." SKIP(1).

  ASSIGN m-lookup-db = IF m-lookup-db NE "" THEN m-lookup-db + "." ELSE " ".

  IF show-fields:NUM-ITEMS NE 0 THEN
  ASSIGN m-show-fields = "," + show-fields:LIST-ITEMS
    m-show-fields = REPLACE(m-show-fields,","," " + m-lookup-file + ".")
    m-show-fields = REPLACE(m-show-fields,"  "," ")
    m-show-fields = LEFT-TRIM(m-show-fields)
    m-show-fields-yellow = REPLACE(m-show-fields," "," LABEL-BGCOLOR 14 ") +
                           " LABEL-BGCOLOR 14".

  PUT UNFORMATTED
    "~&Scoped-define lookup-db " m-lookup-db SKIP
    "~&Scoped-define lookup-file " m-lookup-file SKIP
    "~&Scoped-define where-statement ".

  IF m-where-statement NE "" THEN
  PUT UNFORMATTED m-where-statement.
  ELSE
  PUT UNFORMATTED "TRUE".

  PUT UNFORMATTED SKIP
    "~&Scoped-define return-field " m-return-field SKIP
    "~&Scoped-define font " m-font SKIP
    "~&Scoped-define height-size " m-height-size SKIP
    "~&Scoped-define width-size " m-width-size SKIP
    "~&Scoped-define show-fields " m-show-fields SKIP
    "~&Scoped-define show-fields-yellow " m-show-fields-yellow SKIP
    "~&Scoped-define frame-title " m-frame-title SKIP
    "~&Scoped-define top-include ".
  IF m-top-include NE "" THEN
  PUT UNFORMATTED "~~" m-top-include.
  PUT UNFORMATTED SKIP "~&Scoped-define def-include ".
  IF m-def-include NE "" THEN
  PUT UNFORMATTED "~~" m-def-include.
  PUT UNFORMATTED SKIP "~&Scoped-define end-include ".
  IF m-end-include NE "" THEN
  PUT UNFORMATTED "~~" m-end-include.
  PUT UNFORMATTED SKIP
    "~&Scoped-define ui-prgmname " m-ui-prgmname SKIP
    "~&Scoped-define window-size " INTEGER(m-height-size) + 4 SKIP
    "~&Scoped-define window-col " (150 - INTEGER(m-width-size)) / 2 SKIP
    "~&Scoped-define rect-1-row " INTEGER(m-height-size) + 1.15 SKIP
    "~&Scoped-define by-row " INTEGER(m-height-size) + 1.42 SKIP
    "~&Scoped-define browse-order-width " INTEGER(m-width-size) - 6 SKIP
    "~&Scoped-define browse-order-row " INTEGER(m-height-size) + 1.42 SKIP
    "~&Scoped-define btn-row " INTEGER(m-height-size) + 2.77 SKIP
    "~&Scoped-define btn-ok-col " INTEGER(m-width-size) - 9 SKIP
    "~&Scoped-define btn-cancel-col " INTEGER(m-width-size) - 20 SKIP
    "~&Scoped-define auto-find-row " INTEGER(m-height-size) + 3.85 SKIP(1).

  DO i = 1 TO NUM-ENTRIES(m-order-values):
    data-type = "STRING".
    IF m-lookup-db NE "" THEN
    DO:
      CREATE ALIAS dictdb FOR DATABASE VALUE(m-lookup-db:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle ("get_type.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (m-lookup-file,order-fields:ENTRY(i),OUTPUT data-type).
      IF data-type NE "STRING" THEN
      PUT UNFORMATTED "~&Global-define DATATYP" i " " CAPS(data-type) SKIP.
      RUN Get_Procedure IN Persistent-Handle ("get_frmt.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (m-lookup-file,order-fields:ENTRY(i),OUTPUT data-format).
      PUT UNFORMATTED "~&Global-define FORMAT-" i " " data-format SKIP.
    END.
    PUT UNFORMATTED
      "~&Scoped-define FLDNAME" i " " m-lookup-file "." order-fields:ENTRY(i) SKIP
      "~&Scoped-define SORTBY-" i " BY ~{&FLDNAME" i "}".
    IF i GT 1 THEN
    PUT UNFORMATTED " ~{&SORTBY-" i - 1 "}".
    PUT UNFORMATTED SKIP
      "~&Scoped-define DESCRIP" i " " ENTRY(i,m-order-values) SKIP.
  END.

  PUT UNFORMATTED SKIP(1)
    "~{methods/lookup.i}" SKIP.

  OUTPUT CLOSE.

  MESSAGE "Compile Lookup?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE compile-lookup AS LOGICAL.
  IF compile-lookup THEN
  COMPILE VALUE("lookups\" + m_lookup_prgm + "p") SAVE.

  ASSIGN
    Btn_Cancel:LABEL = "&Close"
    m_status:SCREEN-VALUE = "Lookup SAVEd".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_UP C-Win
ON CHOOSE OF Btn_UP IN FRAME DEFAULT-FRAME /* Move Up */
DO:
  IF shandle:SCREEN-VALUE NE "" THEN
  DO i = 2 TO shandle:NUM-ITEMS:
    IF NOT shandle:IS-SELECTED(i) THEN
    NEXT.
    ASSIGN
      ldummy = shandle:INSERT(shandle:SCREEN-VALUE,i - 1)
      ldummy = shandle:DELETE(i + 1)
      shandle:SCREEN-VALUE = shandle:ENTRY(i - 1).
    LEAVE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m-ui-prgmname
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m-ui-prgmname C-Win
ON HELP OF m-ui-prgmname IN FRAME DEFAULT-FRAME /* UI Program */
DO:
  CREATE ALIAS dictdb FOR DATABASE NOSWEAT.
  RUN "lookups/ui_lkup.p".
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = g_lookup-var
    {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME order-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL order-fields C-Win
ON DEFAULT-ACTION OF order-fields IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL order-fields C-Win
ON ENTRY OF order-fields IN FRAME DEFAULT-FRAME
DO:
  selected-set:SCREEN-VALUE = "2".
  APPLY "VALUE-CHANGED" TO selected-set.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-list C-Win
ON DEFAULT-ACTION OF select-list IN FRAME DEFAULT-FRAME
DO:
  IF shandle:LOOKUP(select-list:SCREEN-VALUE) = 0 THEN
  ldummy = shandle:ADD-LAST(select-list:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selected-set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selected-set C-Win
ON VALUE-CHANGED OF selected-set IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.

  IF m-lookup-db:SCREEN-VALUE = "" THEN
  RETURN NO-APPLY.
  CASE selected-set:SCREEN-VALUE:
    WHEN "1" THEN
    DO:
      shandle = show-fields:HANDLE.
      CREATE ALIAS dictdb FOR DATABASE VALUE(m-lookup-db:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (m-lookup-file:SCREEN-VALUE,OUTPUT item_list).
      select-list:LIST-ITEMS = item_list.
    END.
    WHEN "2" THEN
    DO:
      shandle = order-fields:HANDLE.
      CREATE ALIAS dictdb FOR DATABASE VALUE(m-lookup-db:SCREEN-VALUE).
      RUN Get_Procedure IN Persistent-Handle (INPUT "fld_list.",OUTPUT run-proc,no).
      IF run-proc NE "" THEN
      RUN VALUE(run-proc) (m-lookup-file:SCREEN-VALUE,OUTPUT item_list).
      select-list:LIST-ITEMS = item_list.
      RUN Auto_Set_Order_Values.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME show-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL show-fields C-Win
ON DEFAULT-ACTION OF show-fields IN FRAME DEFAULT-FRAME
DO:
  IF {&SELF-NAME}:SCREEN-VALUE NE "" THEN
  ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL show-fields C-Win
ON ENTRY OF show-fields IN FRAME DEFAULT-FRAME
DO:
  selected-set:SCREEN-VALUE = "1".
  APPLY "VALUE-CHANGED" TO selected-set.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

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
  RUN Get_Lookup.
  RUN enable_UI.
  m-order-values:SENSITIVE = NOT auto_set.
  APPLY "VALUE-CHANGED" TO selected-set.
  {methods/enhance.i}
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Auto_Set_Order_Values C-Win 
PROCEDURE Auto_Set_Order_Values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE item_list AS CHARACTER NO-UNDO.

  IF NOT auto_set THEN
  RETURN.
  RUN Get_Procedure IN Persistent-Handle (INPUT "fld_lbls.",OUTPUT run-proc,no).
  IF run-proc NE "" THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN VALUE(run-proc)
        (m-lookup-file:SCREEN-VALUE,order-fields:LIST-ITEMS,OUTPUT item_list).
    ASSIGN
      m-order-values:SCREEN-VALUE = item_list
      m-order-values.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY m-lookup-prgm m-lookup-db m-lookup-file m-return-field 
          m-where-statement m-order-values auto_set selected-set show-fields 
          order-fields select-list m-frame-title m-top-include m-def-include 
          m-end-include m-ui-prgmname m_status m-font m-height-size m-width-size 
          F1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 RECT-1 RECT-2 Btn_Get_Field m-where-statement m-order-values 
         auto_set selected-set Btn_UP show-fields order-fields select-list 
         Btn_Add Btn_Remove Btn_Down m-frame-title m-top-include m-def-include 
         m-end-include m-ui-prgmname m-font m-height-size m-width-size 
         Btn_Reset Btn_Save Btn_Delete Btn_Cancel Btn_OK Btn_Description 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Lookup C-Win 
PROCEDURE Get_Lookup :
/* -----------------------------------------------------------
  Purpose:Find and Read Lookup Browser
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  IF SEARCH("lookups/" + m_lookup_prgm + "p") NE ? THEN
  DO WITH FRAME {&FRAME-NAME}:
    INPUT FROM VALUE("lookups/" + m_lookup_prgm + "p") NO-ECHO.
    IMPORT ^.
    IMPORT m-lookup-prgm
      m-lookup-db
      m-lookup-file
      m-where-statement
      m-return-field
      m-font
      m-height-size
      m-width-size
      m-show-fields
      m-order-values
      auto_set
      m-order-fields
      m-frame-title
      m-top-include
      m-def-include
      m-end-include
      m-ui-prgmname.
    INPUT CLOSE.
    ASSIGN
      show-fields:LIST-ITEMS = TRIM(m-show-fields)
      order-fields:LIST-ITEMS = TRIM(m-order-fields).
  END.
  ELSE
  m-lookup-prgm = m_lookup_prgm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

