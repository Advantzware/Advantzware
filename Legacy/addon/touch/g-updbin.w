&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ip-qty LIKE pc-prdd.qty NO-UNDO.
DEF INPUT PARAM ip-employee_code AS cha NO-UNDO.
DEF INPUT PARAM ip-company_code AS cha NO-UNDO.

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

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg fg-bin

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain itemfg.i-no fg-bin.loc fg-bin.loc-bin ~
itemfg.case-count fg-bin.cases-unit fg-bin.unit-count 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain itemfg.case-count ~
fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain itemfg fg-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain itemfg
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-fMain fg-bin
&Scoped-define QUERY-STRING-fMain FOR EACH itemfg SHARE-LOCK, ~
      EACH fg-bin OF itemfg SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH itemfg SHARE-LOCK, ~
      EACH fg-bin OF itemfg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain itemfg fg-bin
&Scoped-define FIRST-TABLE-IN-QUERY-fMain itemfg
&Scoped-define SECOND-TABLE-IN-QUERY-fMain fg-bin


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.case-count fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES itemfg fg-bin
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-define SECOND-ENABLED-TABLE fg-bin
&Scoped-Define ENABLED-OBJECTS Btn_OK RECT-9 
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

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_units AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "(Total Cases, Bundles, or Pallets)" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 107 BY 10.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      itemfg, 
      fg-bin SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     itemfg.i-no AT ROW 1.95 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fg-bin.loc AT ROW 2.91 COL 79.6 COLON-ALIGNED
          LABEL "Warehouse Location"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     fg-bin.loc-bin AT ROW 3.86 COL 76.6 COLON-ALIGNED
          LABEL "Bin Location"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     fi_units AT ROW 5.52 COL 79.2 COLON-ALIGNED
     itemfg.case-count AT ROW 6.48 COL 79.2 COLON-ALIGNED
          LABEL "(Case Count, Bundle Count, or Pallet Count)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     fg-bin.cases-unit AT ROW 7.43 COL 79.2 COLON-ALIGNED
          LABEL "(Enter 1 if Unit is a Pallet)" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     fg-bin.unit-count AT ROW 9.33 COL 79.4 COLON-ALIGNED
          LABEL "(Calculated as Qty per Unit * Cases/Bundles per Pallet)"
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     Btn_OK AT ROW 11.48 COL 46
     RECT-9 AT ROW 1 COL 1
     "Total Number of Units" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 5.52 COL 11
          FGCOLOR 9 
     "Qty per Unit" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 6.48 COL 11
          FGCOLOR 9 
     "Cases/Bundles per Pallet" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 7.43 COL 11
          FGCOLOR 9 
     "Pallet Count" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 9.33 COL 11
          FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.2 BY 12.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Update Packing Quantities"
         HEIGHT             = 12.48
         WIDTH              = 107.2
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* SETTINGS FOR FILL-IN itemfg.case-count IN FRAME fMain
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fg-bin.cases-unit IN FRAME fMain
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_units IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.i-no IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-bin.loc IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.loc-bin IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.unit-count IN FRAME fMain
   NO-ENABLE EXP-LABEL                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "ASI.itemfg,ASI.fg-bin OF ASI.itemfg"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Update Packing Quantities */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Update Packing Quantities */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* OK */
DO:
  RUN valid-case-count NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cases-unit NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     itemfg.case-count
     fg-bin.cases-unit
     fg-bin.unit-count.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.case-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count wWin
ON LEAVE OF itemfg.case-count IN FRAME fMain /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-case-count NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.case-count wWin
ON VALUE-CHANGED OF itemfg.case-count IN FRAME fMain /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-bin.cases-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.cases-unit wWin
ON LEAVE OF fg-bin.cases-unit IN FRAME fMain /* (Enter 1 if Unit is a Pallet) */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cases-unit NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.cases-unit wWin
ON VALUE-CHANGED OF fg-bin.cases-unit IN FRAME fMain /* (Enter 1 if Unit is a Pallet) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */
fi_units = TRUNC(ip-qty / itemfg.case-count,0).
/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display_keyboard wWin 
PROCEDURE display_keyboard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER Keyboard_Name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER Frame_Handle AS WIDGET-HANDLE NO-UNDO.

  MESSAGE "keyboard: " keyboard_name VALID-HANDLE(h_numeric) VIEW-AS ALERT-BOX.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  DISPLAY fi_units 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE fg-bin THEN 
    DISPLAY fg-bin.loc fg-bin.loc-bin fg-bin.cases-unit fg-bin.unit-count 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE itemfg THEN 
    DISPLAY itemfg.i-no itemfg.case-count 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE itemfg.case-count fg-bin.cases-unit Btn_OK RECT-9 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE key_stroke wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pal-cnt wWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-case-count wWin 
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
      RETURN.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cases-unit wWin 
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
      RETURN.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

