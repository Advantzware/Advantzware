&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

def var k_frac as dec init 6.25 no-undo.
&global-define style-formular Corr

ASSIGN cocode = g_company
       locode = g_loc.
/*{sys/inc/f16to32.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES style flute
&Scoped-define FIRST-EXTERNAL-TABLE style


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR style, flute.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS style.formula[1] style.formula[2] ~
style.formula[3] style.formula[4] style.formula[5] style.formula[6] ~
style.formula[7] style.formula[8] style.kdf-width style.kdf-length ~
style.balecount style.formula[12] style.use-w[2] style.use-w[3] ~
style.use-w[4] style.use-w[5] style.use-w[6] style.use-w[7] style.use-w[8] ~
style.use-w[9] style.use-w[10] style.use-w[11] style.use-w[12] ~
style.use-w[13] style.use-l[2] style.use-l[3] style.use-l[4] style.use-l[5] ~
style.use-l[6] style.use-l[7] style.use-l[8] style.use-l[9] style.use-l[10] ~
style.use-l[11] style.use-l[12] style.use-l[13] style.sqft-len-trim ~
style.sqft-wid-trim 
&Scoped-define ENABLED-TABLES style
&Scoped-define FIRST-ENABLED-TABLE style
&Scoped-Define ENABLED-OBJECTS RECT-16 
&Scoped-Define DISPLAYED-FIELDS style.formula[1] style.formula[2] ~
style.formula[3] style.formula[4] style.formula[5] style.formula[6] ~
style.formula[7] style.formula[8] style.kdf-width style.kdf-length ~
style.balecount style.formula[12] style.use-w[2] style.use-w[3] ~
style.use-w[4] style.use-w[5] style.use-w[6] style.use-w[7] style.use-w[8] ~
style.use-w[9] style.use-w[10] style.use-w[11] style.use-w[12] ~
style.use-w[13] style.use-l[2] style.use-l[3] style.use-l[4] style.use-l[5] ~
style.use-l[6] style.use-l[7] style.use-l[8] style.use-l[9] style.use-l[10] ~
style.use-l[11] style.use-l[12] style.use-l[13] style.sqft-len-trim ~
style.sqft-wid-trim 
&Scoped-define DISPLAYED-TABLES style
&Scoped-define FIRST-DISPLAYED-TABLE style
&Scoped-Define DISPLAYED-OBJECTS ld-box-fit 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS style.sqft-len-trim style.sqft-wid-trim 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE ld-box-fit AS DECIMAL FORMAT ".99":U INITIAL 0 
     LABEL "Sq Box Fit" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     style.formula[1] AT ROW 2.67 COL 18 COLON-ALIGNED
          LABEL "Formula 1 W" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[2] AT ROW 3.62 COL 18 COLON-ALIGNED
          LABEL " L" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[3] AT ROW 4.57 COL 18 COLON-ALIGNED
          LABEL "Formula 2 W" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[4] AT ROW 5.52 COL 18 COLON-ALIGNED
          LABEL "L" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[5] AT ROW 6.48 COL 18 COLON-ALIGNED
          LABEL "Formula 3 W" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[6] AT ROW 7.43 COL 18 COLON-ALIGNED
          LABEL "L" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 118 BY 1
          BGCOLOR 15 FONT 4
     style.formula[7] AT ROW 8.62 COL 18 COLON-ALIGNED
          LABEL "Square Feet W"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
          BGCOLOR 15 FONT 4
     style.formula[8] AT ROW 9.57 COL 18 COLON-ALIGNED
          LABEL "L"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
          BGCOLOR 15 FONT 4
     style.kdf-width AT ROW 10.76 COL 18 COLON-ALIGNED
          LABEL "KDF W"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     style.kdf-length AT ROW 11.71 COL 18 COLON-ALIGNED
          LABEL "L"
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     ld-box-fit AT ROW 12.91 COL 18 COLON-ALIGNED
     style.balecount AT ROW 12.91 COL 51 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     style.formula[12] AT ROW 9.57 COL 77 COLON-ALIGNED
          LABEL "Die Rule" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     style.use-w[2] AT ROW 11.48 COL 78 COLON-ALIGNED
          LABEL "Formula"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[3] AT ROW 11.48 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[4] AT ROW 11.48 COL 86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[5] AT ROW 11.48 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[6] AT ROW 11.48 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[7] AT ROW 11.48 COL 98 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[8] AT ROW 11.48 COL 102 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[9] AT ROW 11.48 COL 106 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[10] AT ROW 11.48 COL 110 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.use-w[11] AT ROW 11.48 COL 114 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[12] AT ROW 11.48 COL 118 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-w[13] AT ROW 11.48 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[2] AT ROW 13.38 COL 78 COLON-ALIGNED
          LABEL "Formula"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[3] AT ROW 13.38 COL 82 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[4] AT ROW 13.38 COL 86 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[5] AT ROW 13.38 COL 90 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[6] AT ROW 13.38 COL 94 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[7] AT ROW 13.38 COL 98 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[8] AT ROW 13.38 COL 102 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[9] AT ROW 13.38 COL 106 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[10] AT ROW 13.38 COL 110 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[11] AT ROW 13.38 COL 114 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[12] AT ROW 13.38 COL 118 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.use-l[13] AT ROW 13.38 COL 122 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     style.sqft-len-trim AT ROW 14.1 COL 18 COLON-ALIGNED HELP
          "Enter inches added to length for sell price based on PerMSF" WIDGET-ID 2 FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     style.sqft-wid-trim AT ROW 14.1 COL 45 COLON-ALIGNED HELP
          "Enter inches added to width for sell price based on PerMSF" WIDGET-ID 4 FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     "5" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 93
     "3" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 85
     "7" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 101
     "13" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 124
     "3" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 85
     "6" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 97
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "10" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 112
     "2" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 81
     "12" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 120
     "5" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 93
     "11" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 116
     "2" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 81
     "4" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 89
     "# On Wid" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 12.67 COL 67
     "13" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 124
     "9" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 109
     "# On Len" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 10.76 COL 67
     "8" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 105
     "LAYOUT FORMULAS" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.48 COL 5
     "4" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 89
     "9" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 109
     "6" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 97
     "10" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 112
     "12" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 120
     "8" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 105
     "7" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 12.67 COL 101
     "Nesting Formula" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 8.62 COL 72
     "11" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 10.76 COL 116
     RECT-16 AT ROW 8.86 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.style,ASI.flute
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN style.formula[12] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[4] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[5] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[6] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN style.formula[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.formula[8] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.kdf-length IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.kdf-width IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ld-box-fit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style.sqft-len-trim IN FRAME F-Main
   2 EXP-FORMAT EXP-HELP                                                */
/* SETTINGS FOR FILL-IN style.sqft-wid-trim IN FRAME F-Main
   2 EXP-FORMAT EXP-HELP                                                */
/* SETTINGS FOR FILL-IN style.use-l[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.use-w[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME style.balecount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.balecount V-table-Win
ON LEAVE OF style.balecount IN FRAME F-Main /* Bale Count */
DO:
   {&methods/lValidateError.i YES}
   if lastkey <> -1 and index("12",self:screen-value) <= 0 then do:
      message "Invalid Balecount. Enter 1 or 2." view-as alert-box error.
      return no-apply.
   end.
   {&methods/lValidateError.i NO} 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-box-fit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-box-fit V-table-Win
ON LEAVE OF ld-box-fit IN FRAME F-Main /* Sq Box Fit */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.sqft-len-trim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.sqft-len-trim V-table-Win
ON LEAVE OF style.sqft-len-trim IN FRAME F-Main /* Add to Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.sqft-wid-trim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.sqft-wid-trim V-table-Win
ON LEAVE OF style.sqft-wid-trim IN FRAME F-Main /* Add to Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.



  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "style"}
  {src/adm/template/row-list.i "flute"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "style"}
  {src/adm/template/row-find.i "flute"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation V-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation V-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-style-formular V-table-Win 
PROCEDURE enable-style-formular :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* callled in methods/viewers/enable/style.i */

  enable ld-box-fit with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = style.style
                        and reftable.loc = flute.code
                        and reftable.code = "DIM-FIT"
                        no-error.
  if not avail reftable then do:
     create reftable.
     assign reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "DIM-FIT".
  end.
  reftable.val[1] = dec( ld-box-fit:screen-value in frame {&frame-name}) * k_frac. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable ld-box-fit with frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail style then return.

  find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = style.style
                        and reftable.loc = flute.code
                        and reftable.code = "DIM-FIT"
                        no-lock no-error.
  ld-box-fit = (if avail reftable then reftable.val[1]
                            else style.dim-fit) / k_frac.        
  disable ld-box-fit with frame {&frame-name}.
  display ld-box-fit with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF INDEX("12",style.balecount:SCREEN-VALUE) LE 0 THEN DO:
      MESSAGE "Invalid Balecount, Please enter 1 or 2..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO style.balecount.
      RETURN NO-APPLY.
    END.

    RUN valid-dim (ld-box-fit:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (style.sqft-len-trim:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-dim (style.sqft-wid-trim:HANDLE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable ld-box-fit with frame {&frame-name}.
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "style"}
  {src/adm/template/snd-list.i "flute"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dim V-table-Win 
PROCEDURE valid-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ip-focus:SCREEN-VALUE) - TRUNC(DEC(ip-focus:SCREEN-VALUE),0) GT .15
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) +
              " Decimal must be less than (.16)..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

