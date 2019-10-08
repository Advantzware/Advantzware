&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ip-qty LIKE pc-prdd.qty NO-UNDO.
DEF INPUT PARAM ip-employee_code AS cha NO-UNDO.
DEF INPUT PARAM ip-company_code AS cha NO-UNDO.

DEF VAR h-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR lv-bgcolor-enabled AS INT INIT 14 NO-UNDO.
DEF VAR lv-bgcolor-disabled AS INT INIT ? NO-UNDO.

/* Local Variable Definitions ---                                       */
FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid.
FIND itemfg OF fg-bin.


{touch/touchdef.i}

 ASSIGN company_code = ip-company_code
        employee_code = ip-employee_code.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-bin itemfg

/* Definitions for DIALOG-BOX gDialog                                   */
&Scoped-define FIELDS-IN-QUERY-gDialog itemfg.i-no fg-bin.loc ~
fg-bin.loc-bin itemfg.case-count fg-bin.cases-unit fg-bin.unit-count 
&Scoped-define ENABLED-FIELDS-IN-QUERY-gDialog itemfg.case-count ~
fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES-IN-QUERY-gDialog itemfg fg-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-gDialog itemfg
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-gDialog fg-bin
&Scoped-define QUERY-STRING-gDialog FOR EACH fg-bin ~
      WHERE rowid(fg-bin) eq ip-rowid SHARE-LOCK, ~
      EACH itemfg OF fg-bin SHARE-LOCK
&Scoped-define OPEN-QUERY-gDialog OPEN QUERY gDialog FOR EACH fg-bin ~
      WHERE rowid(fg-bin) eq ip-rowid SHARE-LOCK, ~
      EACH itemfg OF fg-bin SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-gDialog fg-bin itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-gDialog fg-bin
&Scoped-define SECOND-TABLE-IN-QUERY-gDialog itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.case-count fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES itemfg fg-bin
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-define SECOND-ENABLED-TABLE fg-bin
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-9 btn-1 btn-2 btn-3 btn-4 btn-5 ~
btn-6 btn-7 btn-8 btn-9 btn-10 btn-del Btn_OK 
&Scoped-Define DISPLAYED-FIELDS itemfg.i-no fg-bin.loc fg-bin.loc-bin ~
itemfg.case-count fg-bin.cases-unit fg-bin.unit-count 
&Scoped-define DISPLAYED-TABLES itemfg fg-bin
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-define SECOND-DISPLAYED-TABLE fg-bin
&Scoped-Define DISPLAYED-OBJECTS fi_units 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-1 
     LABEL "1" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-10 
     LABEL "0" 
     SIZE 9 BY 2.

DEFINE BUTTON btn-2 
     LABEL "2" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-3 
     LABEL "3" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-4 
     LABEL "4" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-5 
     LABEL "5" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-6 
     LABEL "6" 
     SIZE 9 BY 2
     FONT 6.

DEFINE BUTTON btn-7 
     LABEL "7" 
     SIZE 9 BY 2.

DEFINE BUTTON btn-8 
     LABEL "8" 
     SIZE 9 BY 2.

DEFINE BUTTON btn-9 
     LABEL "9" 
     SIZE 9 BY 2.

DEFINE BUTTON btn-del 
     LABEL "BACKSPACE" 
     SIZE 18 BY 2.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 37 BY 1.14.

DEFINE VARIABLE fi_units AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "(Total Cases, Bundles, or Pallets)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31 BY 9.76.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 102 BY 10.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY gDialog FOR 
      fg-bin, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     btn-1 AT ROW 1.71 COL 107
     btn-2 AT ROW 1.71 COL 116
     btn-3 AT ROW 1.71 COL 125
     itemfg.i-no AT ROW 1.95 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     fg-bin.loc AT ROW 2.91 COL 85 COLON-ALIGNED
          LABEL "Warehouse Location"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     fg-bin.loc-bin AT ROW 3.86 COL 85 COLON-ALIGNED
          LABEL "Bin Location"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     btn-4 AT ROW 3.86 COL 107
     btn-5 AT ROW 3.86 COL 116
     btn-6 AT ROW 3.86 COL 125
     fi_units AT ROW 5.52 COL 85 COLON-ALIGNED
     btn-7 AT ROW 6 COL 107
     btn-8 AT ROW 6 COL 116
     btn-9 AT ROW 6 COL 125
     itemfg.case-count AT ROW 6.48 COL 85 COLON-ALIGNED
          LABEL "(Case Count, Bundle Count, or Pallet Count)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     fg-bin.cases-unit AT ROW 7.43 COL 85 COLON-ALIGNED
          LABEL "(Enter 1 if Unit is a Pallet)" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     btn-10 AT ROW 8.14 COL 107
     btn-del AT ROW 8.14 COL 116
     fg-bin.unit-count AT ROW 9.33 COL 85 COLON-ALIGNED
          LABEL "(Calculated as Qty per Unit * Cases/Bundles per Pallet)"
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     Btn_OK AT ROW 11.48 COL 49
     "Cases/Bundles per Pallet" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 7.43 COL 11
          FGCOLOR 9 
     "Total Number of Units" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 5.52 COL 11
          FGCOLOR 9 
     "Qty per Unit" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 6.48 COL 11
          FGCOLOR 9 
     "Pallet Count" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 9.33 COL 11
          FGCOLOR 9 
     RECT-10 AT ROW 1.24 COL 105
     RECT-9 AT ROW 1 COL 1
     SPACE(33.39) SKIP(2.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Packing Quantities"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.case-count IN FRAME gDialog
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fg-bin.cases-unit IN FRAME gDialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_units IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-bin.loc IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.loc-bin IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.unit-count IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _TblList          = "ASI.fg-bin,ASI.itemfg OF ASI.fg-bin"
     _Options          = "SHARE-LOCK"
     _Where[1]         = "rowid(fg-bin) eq ip-rowid"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Update Packing Quantities */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "choose" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-1 gDialog
ON CHOOSE OF btn-1 IN FRAME gDialog /* 1 */
DO:
   RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-10 gDialog
ON CHOOSE OF btn-10 IN FRAME gDialog /* 0 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-2 gDialog
ON CHOOSE OF btn-2 IN FRAME gDialog /* 2 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-3 gDialog
ON CHOOSE OF btn-3 IN FRAME gDialog /* 3 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-4 gDialog
ON CHOOSE OF btn-4 IN FRAME gDialog /* 4 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-5 gDialog
ON CHOOSE OF btn-5 IN FRAME gDialog /* 5 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-6 gDialog
ON CHOOSE OF btn-6 IN FRAME gDialog /* 6 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-7 gDialog
ON CHOOSE OF btn-7 IN FRAME gDialog /* 7 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-8 gDialog
ON CHOOSE OF btn-8 IN FRAME gDialog /* 8 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-9 gDialog
ON CHOOSE OF btn-9 IN FRAME gDialog /* 9 */
DO:
  RUN APPLY-key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-del gDialog
ON CHOOSE OF btn-del IN FRAME gDialog /* BACKSPACE */
DO:
  RUN APPLY-key (SELF:LABEL). 
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
   DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     itemfg.case-count
     fg-bin.cases-unit
     fg-bin.unit-count.
  END.

  RUN valid-case-count NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cases-unit NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.case-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count gDialog
ON ENTRY OF itemfg.case-count IN FRAME gDialog /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  h-focus:bgcolor = lv-bgcolor-disabled.
  h-focus = SELF.
  h-focus:bgcolor = lv-bgcolor-enabled.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count gDialog
ON LEAVE OF itemfg.case-count IN FRAME gDialog /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-case-count NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count gDialog
ON VALUE-CHANGED OF itemfg.case-count IN FRAME gDialog /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-bin.cases-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.cases-unit gDialog
ON ENTRY OF fg-bin.cases-unit IN FRAME gDialog /* (Enter 1 if Unit is a Pallet) */
DO:
   h-focus:bgcolor = lv-bgcolor-disabled.
   h-focus = SELF.
   h-focus:bgcolor = lv-bgcolor-enabled.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.cases-unit gDialog
ON LEAVE OF fg-bin.cases-unit IN FRAME gDialog /* (Enter 1 if Unit is a Pallet) */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cases-unit NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.cases-unit gDialog
ON VALUE-CHANGED OF fg-bin.cases-unit IN FRAME gDialog /* (Enter 1 if Unit is a Pallet) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_units
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_units gDialog
ON ENTRY OF fi_units IN FRAME gDialog /* (Total Cases, Bundles, or Pallets) */
DO:
   h-focus = SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-bin.unit-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.unit-count gDialog
ON ENTRY OF fg-bin.unit-count IN FRAME gDialog /* (Calculated as Qty per Unit * Cases/Bundles per Pallet) */
DO:
  h-focus:bgcolor = lv-bgcolor-disabled.
  h-focus = SELF.
  h-focus:bgcolor = lv-bgcolor-enabled.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
fi_units = TRUNC(ip-qty / itemfg.case-count,0).

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-key gDialog 
PROCEDURE apply-key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-keylabel AS cha NO-UNDO.


  IF index("1234567890",ip-keylabel) > 0  THEN
     h-focus:SCREEN-VALUE = IF length(h-focus:screen-value) < length(h-focus:FORMAT)
         THEN h-focus:SCREEN-VALUE + ip-keylabel ELSE h-focus:SCREEN-VALUE.
  ELSE IF ip-keylabel = "BACKSPACE" THEN DO:
    /*  APPLY KEYCODE(ip-keylabel) TO h-focus. not working */
      h-focus:SCREEN-VALUE = SUBSTR(h-focus:screen-value,1,LENGTH(h-focus:SCREEN-VALUE) - 1).
  END.

  APPLY "value-changed" TO h-focus.
  APPLY "entry" TO h-focus.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display_keyboard gDialog 
PROCEDURE display_keyboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER Keyboard_Name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER Frame_Handle AS WIDGET-HANDLE NO-UNDO.


  CASE Keyboard_Name:
    WHEN 'numeric.' THEN
    DO:
      IF VALID-HANDLE(h_numeric) THEN DELETE PROCEDURE h_numeric.
      Keyboard_Name = 'touch/' + Keyboard_Name + 'w'.
      RUN VALUE(Keyboard_Name) PERSISTENT SET h_numeric (Frame_Handle).
      MESSAGE "valid? keyboard: " keyboard_name VALID-HANDLE(h_numeric) VIEW-AS ALERT-BOX.
    END.
    OTHERWISE
    DO:
      IF VALID-HANDLE(h_keyboard) THEN
      DELETE PROCEDURE h_keyboard.
      IF employee_code NE '' THEN
      DO:
        FIND employee WHERE employee.company = company_code
                        AND employee.employee = employee_code
                      EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE employee THEN
           employee.keyboard_type = Keyboard_Name.
        RELEASE employee.
      END.
      Keyboard_Name = 'touch/' + Keyboard_Name + 'w'.      
      RUN VALUE(Keyboard_Name) PERSISTENT SET h_keyboard (Frame_Handle, FRAME {&frame-name}:ROW).
    END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fi_units 
      WITH FRAME gDialog.
  IF AVAILABLE fg-bin THEN 
    DISPLAY fg-bin.loc fg-bin.loc-bin fg-bin.cases-unit fg-bin.unit-count 
      WITH FRAME gDialog.
  IF AVAILABLE itemfg THEN 
    DISPLAY itemfg.i-no itemfg.case-count 
      WITH FRAME gDialog.
  ENABLE RECT-10 RECT-9 btn-1 btn-2 btn-3 btn-4 btn-5 btn-6 btn-7 btn-8 btn-9 
         itemfg.case-count fg-bin.cases-unit btn-10 btn-del Btn_OK 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pal-cnt.
  h-focus = itemfg.case-count:HANDLE IN FRAME {&FRAME-NAME}.
  h-focus:bgcolor = lv-bgcolor-enabled.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE key_stroke gDialog 
PROCEDURE key_stroke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* {touch/keystrok.i}*/
  /* keystrok.i */

DEFINE INPUT PARAMETER keystroke AS CHARACTER NO-UNDO.

IF keystroke = 'CAPS LOCK (ON)' THEN
caps_lock = YES.
ELSE
IF keystroke = 'CAPS LOCK (OFF)' THEN
caps_lock = NO.
ELSE
DO WITH FRAME {&FRAME-NAME}:
  IF NOT caps_lock THEN
  keystroke = LC(keystroke).
  CASE keystroke:
    WHEN 'ALPHA' THEN
     RUN Display_Keyboard ('alphabet.',THIS-PROCEDURE:HANDLE).
    WHEN 'BACKSPACE' THEN
    IF field_value NE '' THEN
    field_value = SUBSTR(field_value,1,LENGTH(field_value) - 1).
    WHEN 'CLEAR' THEN
    field_value = ''.
    WHEN 'DQ' THEN /* Double Quote */
    field_value = field_value + '"'.
    WHEN 'QWERTY' THEN
       RUN Display_Keyboard ('keyboard.',THIS-PROCEDURE:HANDLE).
    WHEN 'SPACE' THEN
    field_value = field_value + '`'.
    WHEN 'SORT' THEN
      RUN Display_Keyboard ('sortpad.',THIS-PROCEDURE:HANDLE).
    OTHERWISE
    field_value = field_value + keystroke.
  END CASE.
  IF VALID-HANDLE(h_field) THEN
  h_field:SCREEN-VALUE = REPLACE(field_value,'`',' ').
 
END.

&IF '{&KEYSTROKE}' = 'ALLMACHS' OR
    '{&KEYSTROKE}' = 'ALLJOBS'  OR
    '{&KEYSTROKE}' = 'EMPLOYEE' &THEN
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DO i = 1 TO SL:NUM-ITEMS:
    IF SL:ENTRY(i) BEGINS h_field:SCREEN-VALUE THEN
    LEAVE.
  END.

 IF i GT SL:NUM-ITEMS THEN
  DO:
    MESSAGE 'NO MATCH FOUND BEGINNING' SKIP(1) h_field:SCREEN-VALUE
        VIEW-AS ALERT-BOX.
    RUN Key_Stroke ('BACKSPACE').
    RETURN.
  END.
  ASSIGN
    SL:SCREEN-VALUE = SL:ENTRY(i)
    item = i.
END.
&ELSEIF '{&KEYSTROKE}' = 'PASSWORD' &THEN
h_field:SCREEN-VALUE = FILL('*',LENGTH(field_value)).
&ENDIF


  
MESSAGE "keystroke" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pal-cnt gDialog 
PROCEDURE pal-cnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fi_units:SCREEN-VALUE = STRING(TRUNC(ip-qty / DEC(itemfg.case-count:SCREEN-VALUE),0),
                                    fi_units:FORMAT)
     fg-bin.unit-count:SCREEN-VALUE = STRING(DEC(itemfg.case-count:SCREEN-VALUE) *
                                             DEC(fg-bin.cases-unit:SCREEN-VALUE),
                                             fg-bin.unit-count:FORMAT).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-case-count gDialog 
PROCEDURE valid-case-count :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    IF itemfg.case-count LE 0 THEN DO:
      MESSAGE "Qty per Case/Bundle/Unit must be greater than zero..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.case-count.
      RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cases-unit gDialog 
PROCEDURE valid-cases-unit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF fg-bin.cases-unit LE 0 THEN DO:
      MESSAGE "Cases/Bundles per Pallet must be greater than zero..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-bin.cases-unit.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

