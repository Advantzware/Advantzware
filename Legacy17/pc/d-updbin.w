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
DEF INPUT PARAM  ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM  ip-qty LIKE pc-prdd.qty NO-UNDO.
DEF INPUT-OUTPUT PARAM io-units AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */

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
&Scoped-define INTERNAL-TABLES fg-bin

/* Definitions for DIALOG-BOX gDialog                                   */
&Scoped-define FIELDS-IN-QUERY-gDialog fg-bin.i-no fg-bin.loc ~
fg-bin.loc-bin fg-bin.case-count fg-bin.cases-unit fg-bin.unit-count 
&Scoped-define ENABLED-FIELDS-IN-QUERY-gDialog fg-bin.case-count ~
fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES-IN-QUERY-gDialog fg-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-gDialog fg-bin
&Scoped-define QUERY-STRING-gDialog FOR EACH fg-bin ~
      WHERE rowid(fg-bin) eq ip-rowid SHARE-LOCK
&Scoped-define OPEN-QUERY-gDialog OPEN QUERY gDialog FOR EACH fg-bin ~
      WHERE rowid(fg-bin) eq ip-rowid SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-gDialog fg-bin
&Scoped-define FIRST-TABLE-IN-QUERY-gDialog fg-bin


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS fg-bin.case-count fg-bin.cases-unit 
&Scoped-define ENABLED-TABLES fg-bin
&Scoped-define FIRST-ENABLED-TABLE fg-bin
&Scoped-Define ENABLED-OBJECTS fi_units Btn_OK RECT-9 
&Scoped-Define DISPLAYED-FIELDS fg-bin.i-no fg-bin.loc fg-bin.loc-bin ~
fg-bin.case-count fg-bin.cases-unit fg-bin.unit-count 
&Scoped-define DISPLAYED-TABLES fg-bin
&Scoped-define FIRST-DISPLAYED-TABLE fg-bin
&Scoped-Define DISPLAYED-OBJECTS fi_qty fi_units fi_partial 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_partial AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "(Partial Count)" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_qty AS INTEGER FORMAT "->,>>>,>>>":U INITIAL 0 
     LABEL "Qty" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_units AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "(Total Cases, Bundles, or Pallets)" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 109 BY 10.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY gDialog FOR 
      fg-bin SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fg-bin.i-no AT ROW 1.95 COL 19 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     fi_qty AT ROW 1.95 COL 88 COLON-ALIGNED
     fg-bin.loc AT ROW 3.38 COL 88 COLON-ALIGNED
          LABEL "Warehouse Location"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     fg-bin.loc-bin AT ROW 4.33 COL 88 COLON-ALIGNED
          LABEL "Bin Location"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     fi_units AT ROW 5.52 COL 88 COLON-ALIGNED
     fg-bin.case-count AT ROW 6.48 COL 88 COLON-ALIGNED
          LABEL "(Case Count, Bundle Count, or Pallet Count)" FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     fi_partial AT ROW 7.43 COL 88 COLON-ALIGNED
     fg-bin.cases-unit AT ROW 8.38 COL 88 COLON-ALIGNED
          LABEL "(Enter 1 if Unit is a Pallet)" FORMAT ">,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     fg-bin.unit-count AT ROW 9.33 COL 88 COLON-ALIGNED
          LABEL "(Calculated as Qty per Unit * Cases/Bundles per Pallet)" FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     Btn_OK AT ROW 11.48 COL 49
     RECT-9 AT ROW 1 COL 1
     "Total Number of Units" VIEW-AS TEXT
          SIZE 23 BY 1 AT ROW 5.52 COL 11
          FGCOLOR 9 
     "Qty per Unit" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 6.48 COL 11
          FGCOLOR 9 
     "Cases/Bundles per Pallet" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 8.38 COL 11
          FGCOLOR 9 
     "Pallet Count" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 9.33 COL 11
          FGCOLOR 9 
     "Partial" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 7.43 COL 11
          FGCOLOR 9 
     SPACE(91.59) SKIP(4.75)
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

/* SETTINGS FOR FILL-IN fg-bin.case-count IN FRAME gDialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-bin.cases-unit IN FRAME gDialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_partial IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_qty IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-bin.i-no IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.loc IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.loc-bin IN FRAME gDialog
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN fg-bin.unit-count IN FRAME gDialog
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _TblList          = "ASI.fg-bin"
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
  RUN valid-case-count NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cases-unit NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  FIND CURRENT fg-bin.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     io-units = INT(fi_units:SCREEN-VALUE)
     fg-bin.case-count
     fg-bin.cases-unit
     fg-bin.unit-count.
  END.

  FIND CURRENT fg-bin NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-bin.case-count
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-bin.case-count gDialog
ON VALUE-CHANGED OF fg-bin.case-count IN FRAME gDialog /* (Case Count, Bundle Count, or Pallet Count) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-bin.cases-unit
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
ON VALUE-CHANGED OF fi_units IN FRAME gDialog /* (Total Cases, Bundles, or Pallets) */
DO:
  RUN pal-cnt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL fg-bin THEN DO:
  IF io-units EQ 0 THEN io-units = TRUNC(ip-qty / fg-bin.case-count,0).

  ASSIGN
   fi_qty     = ip-qty
   fi_units   = io-units
   fi_partial = ip-qty - (fi_units * fg-bin.case-count).

  {src/adm2/dialogmn.i}
END.

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
  DISPLAY fi_qty fi_units fi_partial 
      WITH FRAME gDialog.
  IF AVAILABLE fg-bin THEN 
    DISPLAY fg-bin.i-no fg-bin.loc fg-bin.loc-bin fg-bin.case-count 
          fg-bin.cases-unit fg-bin.unit-count 
      WITH FRAME gDialog.
  ENABLE fi_units fg-bin.case-count fg-bin.cases-unit Btn_OK RECT-9 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeVisualContainer gDialog 
PROCEDURE initializeVisualContainer :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pal-cnt.

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
     fi_partial:SCREEN-VALUE = STRING(ip-qty -
                                      (DEC(fi_units:SCREEN-VALUE) *
                                       DEC(fg-bin.case-count:SCREEN-VALUE)),
                                      fi_partial:FORMAT)
     fg-bin.unit-count:SCREEN-VALUE = STRING(DEC(fg-bin.case-count:SCREEN-VALUE) *
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
    IF fg-bin.case-count LE 0 THEN DO:
      MESSAGE "Qty per Case/Bundle/Unit must be greater than zero..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fg-bin.case-count.
      RETURN.
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
      RETURN.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

