&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:              mfdialog.w

  Description:       Dialog editor for Misc. Fields

  Input Parameters:  attr-settings,value-settings
  
  Output Parameters: attr-settings,value-settings

  Author:            Ron Stark

  Created:           03/01/98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER attrb_rowid AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/miscflds.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS attr_id attr_label attr_values attr_default ~
x-coord y-coord data_type pixel-height pixel-width data_format min-value ~
max-value horz-bar vert-bar font-setting Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS attr_id attr_type attr_label attr_values ~
attr_default x-coord y-coord data_type pixel-height pixel-width data_format ~
min-value max-value horz-bar vert-bar font-setting 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 16.8 BY 1.38
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 16.8 BY 1.38
     BGCOLOR 8 FONT 4.

DEFINE VARIABLE data_type AS CHARACTER FORMAT "X(256)":U INITIAL "Character" 
     LABEL "Data Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Character","Date","Decimal","Integer","Logical" 
     SIZE 22.4 BY 1 NO-UNDO.

DEFINE VARIABLE font-setting AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Font" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "0","1","2","3","4","5","6","7","8" 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE attr_default AS CHARACTER FORMAT "X(256)" 
     LABEL "Attribute Default" 
     VIEW-AS FILL-IN 
     SIZE 89.6 BY 1.

DEFINE VARIABLE attr_id AS CHARACTER FORMAT "X(20)":U 
     LABEL "Attribute ID" 
     VIEW-AS FILL-IN 
     SIZE 29.8 BY 1 NO-UNDO.

DEFINE VARIABLE attr_label AS CHARACTER FORMAT "X(256)" 
     LABEL "Attribute Label" 
     VIEW-AS FILL-IN 
     SIZE 89.6 BY 1.

DEFINE VARIABLE attr_type AS CHARACTER FORMAT "X(14)" 
     LABEL "Attribute Type" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE VARIABLE attr_values AS CHARACTER FORMAT "X(256)" 
     LABEL "Attribute Values" 
     VIEW-AS FILL-IN 
     SIZE 89.8 BY 1.

DEFINE VARIABLE data_format AS CHARACTER FORMAT "X(256)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

DEFINE VARIABLE max-value AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Maximum Values" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 NO-UNDO.

DEFINE VARIABLE min-value AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Minimum Value" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 NO-UNDO.

DEFINE VARIABLE pixel-height AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Pixel Height" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1 NO-UNDO.

DEFINE VARIABLE pixel-width AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Pixel Width" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1 NO-UNDO.

DEFINE VARIABLE x-coord AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "X Coordinate" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1 NO-UNDO.

DEFINE VARIABLE y-coord AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Y Coordinate" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1 NO-UNDO.

DEFINE VARIABLE horz-bar AS LOGICAL INITIAL no 
     LABEL "Horizontal Scrollbar" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE vert-bar AS LOGICAL INITIAL no 
     LABEL "Vertical Scrollbar" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     attr_id AT ROW 1.24 COL 16.2 COLON-ALIGNED HELP
          "Enter Attribute Identifier"
     attr_type AT ROW 1.24 COL 83 COLON-ALIGNED
     attr_label AT ROW 2.43 COL 16.2 COLON-ALIGNED HELP
          "Enter Attribute Label"
     attr_values AT ROW 3.62 COL 2 HELP
          "Enter Attribute Values"
     attr_default AT ROW 4.81 COL 16 COLON-ALIGNED HELP
          "Enter Attribute Default Value"
     x-coord AT ROW 6 COL 16 COLON-ALIGNED HELP
          "Enter X Coordinate"
     y-coord AT ROW 6 COL 59.4 COLON-ALIGNED HELP
          "Enter Y Coordinate"
     data_type AT ROW 6 COL 83.2 COLON-ALIGNED HELP
          "Select Data Type"
     pixel-height AT ROW 7.19 COL 16 COLON-ALIGNED HELP
          "Enter Pixel Height"
     pixel-width AT ROW 7.19 COL 59.4 COLON-ALIGNED HELP
          "Enter Pixel Width"
     data_format AT ROW 7.19 COL 79 COLON-ALIGNED HELP
          "Enter Data Format"
     min-value AT ROW 8.38 COL 16 COLON-ALIGNED HELP
          "Enter Slider's Minimum Value"
     max-value AT ROW 8.38 COL 59.4 COLON-ALIGNED HELP
          "Enter Slider's Maximum Value"
     horz-bar AT ROW 9.57 COL 18 HELP
          "Set Horizontal Scrollbar"
     vert-bar AT ROW 9.57 COL 61.4 HELP
          "Set Vertical Scrollbar"
     font-setting AT ROW 10.76 COL 16 COLON-ALIGNED HELP
          "Select Font Size"
     Btn_Cancel AT ROW 11 COL 72 HELP
          "CANCEL Edit Function"
     Btn_OK AT ROW 11 COL 90.2 HELP
          "OK to Save Edit Function Settings"
     SPACE(0.99) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Attribute Editor"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN attr_type IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN attr_values IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Attribute Editor */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  FIND attrb WHERE ROWID(attrb) = TO-ROWID(attrb_rowid) EXCLUSIVE-LOCK.
  ASSIGN 
    attrb.attr_label = attr_label:SCREEN-VALUE
    attrb.attr_values = attr_values:SCREEN-VALUE
    attrb.attr_default = attr_default:SCREEN-VALUE
    attrb.attr_x = INTEGER(x-coord:SCREEN-VALUE)
    attrb.attr_y = INTEGER(y-coord:SCREEN-VALUE)
    attrb.attr_height = INTEGER(pixel-height:SCREEN-VALUE)
    attrb.attr_width = INTEGER(pixel-width:SCREEN-VALUE).
  CASE attrb.attr_type:
    WHEN "FILL-IN" THEN
    DO:
      IF data_format:SCREEN-VALUE = "" THEN
      CASE data_type:SCREEN-VALUE:
        WHEN "Character" THEN
        data_format:SCREEN-VALUE = "X(8)".
        WHEN "Date" THEN
        data_format:SCREEN-VALUE = "99/99/99".
        WHEN "Decimal" THEN
        data_format:SCREEN-VALUE = "->>,>>9.99".
        WHEN "Integer" THEN
        data_format:SCREEN-VALUE = "->,>>>,>>9".
        WHEN "Logical" THEN
        data_format:SCREEN-VALUE = "yes/no".
      END CASE.
      ASSIGN
        attrb.attr_datatype = data_type:SCREEN-VALUE
        attrb.attr_settings = data_format:SCREEN-VALUE.
    END.
    WHEN "COMBO-BOX" THEN
    attrb.attr_settings = data_format:SCREEN-VALUE.
    WHEN "EDITOR" THEN
    attrb.attr_settings = horz-bar:SCREEN-VALUE + "," + vert-bar:SCREEN-VALUE.
    WHEN "RADIO-SET" THEN
    attrb.attr_settings = horz-bar:SCREEN-VALUE.
    WHEN "SLIDER" THEN
    ASSIGN
      attr_values = min-value:SCREEN-VALUE + "," + max-value:SCREEN-VALUE
      attr_settings = horz-bar:SCREEN-VALUE.
    WHEN "TEXT" THEN
    attrb.attr_settings = font-setting:SCREEN-VALUE.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME data_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data_type Dialog-Frame
ON VALUE-CHANGED OF data_type IN FRAME Dialog-Frame /* Data Type */
DO:
  ASSIGN {&SELF-NAME}.
  CASE {&SELF-NAME}:
    WHEN "Character" THEN
    data_format:SCREEN-VALUE = "X(8)".
    WHEN "Date" THEN
    data_format:SCREEN-VALUE = "99/99/99".
    WHEN "Decimal" THEN
    data_format:SCREEN-VALUE = "->>,>>9.99".
    WHEN "Integer" THEN
    data_format:SCREEN-VALUE = "->,>>>,>>9".
    WHEN "Logical" THEN
    data_format:SCREEN-VALUE = "yes/no".
  END CASE.
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
  FIND attrb WHERE ROWID(attrb) = TO-ROWID(attrb_rowid) NO-LOCK.
  RUN enable_UI.
  ASSIGN
    vert-bar:HIDDEN = YES
    horz-bar:HIDDEN = YES
    min-value:HIDDEN = YES
    max-value:HIDDEN = YES
    font-setting:HIDDEN = YES
    data_type:HIDDEN = YES
    data_format:HIDDEN = YES
    attr_id:SCREEN-VALUE = STRING(attrb.attr_id)
    attr_type:SCREEN-VALUE = attrb.attr_type
    attr_label:SCREEN-VALUE = attrb.attr_label
    attr_values:SCREEN-VALUE = attrb.attr_values
    attr_default:SCREEN-VALUE = attrb.attr_default
    x-coord:SCREEN-VALUE = STRING(attrb.attr_x)
    y-coord:SCREEN-VALUE = STRING(attrb.attr_y)
    pixel-height:SCREEN-VALUE = STRING(attrb.attr_height)
    pixel-width:SCREEN-VALUE = STRING(attrb.attr_width).
  CASE attrb.attr_type:
    WHEN "COMBO-BOX" THEN
    ASSIGN
      data_format:HIDDEN = NO
      data_format:SCREEN-VALUE = attrb.attr_settings.
    WHEN "EDITOR" THEN
    ASSIGN
      vert-bar:HIDDEN = NO
      horz-bar:HIDDEN = NO
      horz-bar:SCREEN-VALUE = ENTRY(1,attrb.attr_settings)
      vert-bar:SCREEN-VALUE = ENTRY(2,attrb.attr_settings).
    WHEN "FILL-IN" THEN
    ASSIGN
      data_type:HIDDEN = NO
      data_format:HIDDEN = NO
      data_type:SCREEN-VALUE = attrb.attr_datatype
      data_format:SCREEN-VALUE = attrb.attr_settings.
    WHEN "RADIO-SET" THEN
    ASSIGN
      horz-bar:HIDDEN = NO
      horz-bar:SCREEN-VALUE = attrb.attr_settings.
    WHEN "SLIDER" THEN
    DO:
      ASSIGN
        min-value:HIDDEN = NO
        max-value:HIDDEN = NO
        horz-bar:HIDDEN = NO
        min-value:SCREEN-VALUE = ENTRY(1,attrb.attr_values)
        max-value:SCREEN-VALUE = ENTRY(2,attrb.attr_values)
        horz-bar:SCREEN-VALUE = ENTRY(1,attrb.attr_settings)
        attr_values:HIDDEN = YES.
    END.
    WHEN "TEXT" THEN
    ASSIGN
      font-setting:HIDDEN = NO
      font-setting:SCREEN-VALUE = attrb.attr_settings.
  END CASE.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY attr_id attr_type attr_label attr_values attr_default x-coord y-coord 
          data_type pixel-height pixel-width data_format min-value max-value 
          horz-bar vert-bar font-setting 
      WITH FRAME Dialog-Frame.
  ENABLE attr_id attr_label attr_values attr_default x-coord y-coord data_type 
         pixel-height pixel-width data_format min-value max-value horz-bar 
         vert-bar font-setting Btn_Cancel Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


