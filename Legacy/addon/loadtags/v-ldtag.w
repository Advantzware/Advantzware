&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: addon\loadtags\v-ldtag.w

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

&SCOPED-DEFINE post-enable post-enable

/* gdm - 09210908 */
DEF VAR v-ldpalwt LIKE loadtag.misc-dec[2] NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES loadtag rfidtag
&Scoped-define FIRST-EXTERNAL-TABLE loadtag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR loadtag, rfidtag.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS loadtag.sts loadtag.i-no loadtag.i-name ~
loadtag.ord-no loadtag.po-no loadtag.loc loadtag.loc-bin loadtag.tag-date ~
loadtag.shift loadtag.job-no loadtag.job-no2 loadtag.qty-case ~
loadtag.case-bundle loadtag.pallet-count loadtag.misc-dec[1] ~
loadtag.misc-char[2] loadtag.partial loadtag.misc-dec[2] ~
loadtag.misc-char[1] loadtag.tot-cases loadtag.misc-dec[3] loadtag.crew ~
loadtag.completed 
&Scoped-define ENABLED-TABLES loadtag
&Scoped-define FIRST-ENABLED-TABLE loadtag
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 
&Scoped-Define DISPLAYED-FIELDS loadtag.tag-no loadtag.sts loadtag.i-no ~
loadtag.i-name rfidtag.rfidtag loadtag.ord-no loadtag.po-no ~
loadtag.spare-char-1 loadtag.loc loadtag.loc-bin loadtag.tag-date ~
loadtag.shift loadtag.job-no loadtag.job-no2 loadtag.qty-case ~
loadtag.case-bundle loadtag.pallet-count loadtag.misc-dec[1] ~
loadtag.misc-char[2] loadtag.partial loadtag.misc-dec[2] ~
loadtag.misc-char[1] loadtag.tot-cases loadtag.misc-dec[3] loadtag.crew ~
loadtag.completed 
&Scoped-define DISPLAYED-TABLES loadtag rfidtag
&Scoped-define FIRST-DISPLAYED-TABLE loadtag
&Scoped-define SECOND-DISPLAYED-TABLE rfidtag
&Scoped-Define DISPLAYED-OBJECTS v-tagtime textField-1 textField-2 ~
textField-3 textField-4 textField-5 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,textFields,F1 */
&Scoped-define ADM-CREATE-FIELDS loadtag.tag-no rfidtag.rfidtag 
&Scoped-define textFields textField-1 textField-2 textField-3 textField-4 ~
textField-5 

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
DEFINE VARIABLE textField-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Folding Cartons = Case Count" 
      VIEW-AS TEXT 
     SIZE 35 BY .62 NO-UNDO.

DEFINE VARIABLE textField-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Corrugated Box = Pallet Count" 
      VIEW-AS TEXT 
     SIZE 35 BY .62 NO-UNDO.

DEFINE VARIABLE textField-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Folding Cartons = Cases Stacked Per Pallet" 
      VIEW-AS TEXT 
     SIZE 50 BY .62 NO-UNDO.

DEFINE VARIABLE textField-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Corrugated Box = 1 if Unit Count = Pallet Count" 
      VIEW-AS TEXT 
     SIZE 55 BY .62 NO-UNDO.

DEFINE VARIABLE textField-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Corrugated Box = Bundles Stacked Per Pallet if Unit Count is Bundle Count)" 
      VIEW-AS TEXT 
     SIZE 86 BY .62 NO-UNDO.

DEFINE VARIABLE v-tagtime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 15.24.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     loadtag.tag-no AT ROW 1.24 COL 16 COLON-ALIGNED FORMAT "X(23)"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     loadtag.sts AT ROW 1.24 COL 93 COLON-ALIGNED
          LABEL "Status"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Printed","Received","On Hand","Bill of Lading","Invoiced","Completed","Deleted","Issued","Transferred" 
          DROP-DOWN
          SIZE 23 BY 1
     loadtag.i-no AT ROW 2.43 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     loadtag.i-name AT ROW 2.43 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     rfidtag.rfidtag AT ROW 2.43 COL 93 COLON-ALIGNED WIDGET-ID 14
          LABEL "RFID Tag" FORMAT "x(24)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     loadtag.ord-no AT ROW 3.62 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     loadtag.po-no AT ROW 3.62 COL 42 COLON-ALIGNED
          LABEL "PO #"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     loadtag.spare-char-1 AT ROW 3.62 COL 93 COLON-ALIGNED HELP
          "" WIDGET-ID 16
          LABEL "SSCC" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     loadtag.loc AT ROW 4.81 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     loadtag.loc-bin AT ROW 4.81 COL 42 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     loadtag.tag-date AT ROW 4.81 COL 93 COLON-ALIGNED
          LABEL "Tag Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     loadtag.shift AT ROW 4.81 COL 129 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     loadtag.job-no AT ROW 6 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     loadtag.job-no2 AT ROW 6 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     v-tagtime AT ROW 6 COL 93 COLON-ALIGNED
     loadtag.qty AT ROW 6 COL 129 COLON-ALIGNED
          LABEL "Qty(Pallet Cnt)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.qty-case AT ROW 8.14 COL 28 COLON-ALIGNED
          LABEL "Unit Count"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.std-cost AT ROW 8.14 COL 117 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.cost-uom AT ROW 9.1 COL 125 COLON-ALIGNED
          LABEL "UOM"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     loadtag.case-bundle AT ROW 10.05 COL 28 COLON-ALIGNED
          LABEL "Units Per Pallet"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.pallet-count AT ROW 12.19 COL 28 COLON-ALIGNED
          LABEL "Qty Per Pallet/Tag#"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.misc-dec[1] AT ROW 12.24 COL 68.2 COLON-ALIGNED WIDGET-ID 2
          LABEL "Unit Wt" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     loadtag.misc-char[2] AT ROW 12.24 COL 97.2 COLON-ALIGNED HELP
          "" WIDGET-ID 12
          LABEL "FG Lot#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     loadtag.partial AT ROW 13.38 COL 28 COLON-ALIGNED
          LABEL "Partial Qty"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     loadtag.misc-dec[2] AT ROW 13.48 COL 68 COLON-ALIGNED WIDGET-ID 4
          LABEL "Loaded Pallet Wt" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     loadtag.misc-char[1] AT ROW 13.48 COL 97.2 COLON-ALIGNED HELP
          "" WIDGET-ID 10
          LABEL "Vendor Tag#" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 27.8 BY 1
     loadtag.tot-cases AT ROW 14.57 COL 28 COLON-ALIGNED
          LABEL "Total Units O/H"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loadtag.misc-dec[3] AT ROW 14.57 COL 68 COLON-ALIGNED WIDGET-ID 6
          LABEL "Wt of Wood Pallet" FORMAT "->>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     loadtag.crew AT ROW 14.57 COL 97.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     loadtag.completed AT ROW 14.57 COL 127.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     textField-1 AT ROW 8.14 COL 44 COLON-ALIGNED NO-LABEL
     textField-2 AT ROW 8.86 COL 44 COLON-ALIGNED NO-LABEL
     textField-3 AT ROW 10.05 COL 44 COLON-ALIGNED NO-LABEL
     textField-4 AT ROW 10.76 COL 44 COLON-ALIGNED NO-LABEL
     textField-5 AT ROW 11.48 COL 44 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 7.43 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.loadtag,ASI.rfidtag
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 15.24
         WIDTH              = 145.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN loadtag.case-bundle IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.cost-uom IN FRAME F-Main
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       loadtag.cost-uom:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN loadtag.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.misc-char[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN loadtag.misc-char[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN loadtag.misc-dec[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loadtag.misc-dec[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loadtag.misc-dec[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loadtag.pallet-count IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.partial IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.qty IN FRAME F-Main
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       loadtag.qty:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN loadtag.qty-case IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfidtag.rfidtag IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN loadtag.spare-char-1 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN loadtag.std-cost IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       loadtag.std-cost:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX loadtag.sts IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loadtag.tag-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loadtag.tag-no IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN textField-1 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN textField-2 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN textField-3 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN textField-4 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN textField-5 IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN loadtag.tot-cases IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN v-tagtime IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME loadtag.misc-dec[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtag.misc-dec[1] V-table-Win
ON LEAVE OF loadtag.misc-dec[1] IN FRAME F-Main /* Unit Wt */
DO:
   /* gdm - 09210908 */
   DO WITH FRAME {&FRAME-NAME}: /* gdm - 09210908 */
     ASSIGN v-ldpalwt = 0.
     RUN calc-ldweight(OUTPUT v-ldpalwt). 
     ASSIGN loadtag.misc-dec[2]:SCREEN-VALUE = STRING(v-ldpalwt).
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadtag.misc-dec[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtag.misc-dec[2] V-table-Win
ON LEAVE OF loadtag.misc-dec[2] IN FRAME F-Main /* Loaded Pallet Wt */
DO:
    DEF VAR v-ansflg AS LOG NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}: /* gdm - 09210908 */
      ASSIGN v-ldpalwt = 0.
      RUN calc-ldweight(OUTPUT v-ldpalwt). 

      IF DEC(loadtag.misc-dec[2]:SCREEN-VALUE) NE 0 AND
          loadtag.misc-dec[2]:SCREEN-VALUE NE "" AND
          DEC(loadtag.misc-dec[2]:SCREEN-VALUE) NE DEC(INT(v-ldpalwt)) THEN DO:
       MESSAGE 
         "The weight entered is not the same as the calculated weight." SKIP
         " Recalculate Unit Weight?"
         VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
           UPDATE v-ansflg.
       IF v-ansflg 
         THEN
          ASSIGN
            loadtag.misc-dec[1]:SCREEN-VALUE = 
               STRING(DEC(loadtag.misc-dec[2]:SCREEN-VALUE) / 
                DEC(loadtag.tot-case)).
      END.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadtag.misc-dec[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtag.misc-dec[3] V-table-Win
ON LEAVE OF loadtag.misc-dec[3] IN FRAME F-Main /* Wt of Wood Pallet */
DO:
  DO WITH FRAME {&FRAME-NAME}: /* gdm - 09210908 */
     ASSIGN v-ldpalwt = 0.
     RUN calc-ldweight(OUTPUT v-ldpalwt). 
     ASSIGN loadtag.misc-dec[2]:SCREEN-VALUE = STRING(v-ldpalwt).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loadtag.tot-cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loadtag.tot-cases V-table-Win
ON LEAVE OF loadtag.tot-cases IN FRAME F-Main /* Total Units O/H */
DO:
   
   DO WITH FRAME {&FRAME-NAME}: /* gdm - 09210908 */
     ASSIGN v-ldpalwt = 0.
     RUN calc-ldweight(OUTPUT v-ldpalwt). 
     ASSIGN loadtag.misc-dec[2]:SCREEN-VALUE = STRING(v-ldpalwt).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "loadtag"}
  {src/adm/template/row-list.i "rfidtag"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "loadtag"}
  {src/adm/template/row-find.i "rfidtag"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-ldweight V-table-Win 
PROCEDURE calc-ldweight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM op-ldpalwt LIKE loadtag.misc-dec[2] NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN 
   op-ldpalwt = (INT(loadtag.tot-cases:SCREEN-VALUE) *
                DEC(loadtag.misc-dec[1]:SCREEN-VALUE)) + 
                DEC(loadtag.misc-dec[3]:SCREEN-VALUE).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bfRfid FOR rfidtag.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF USERID('nosweat') = "asi" AND AVAIL rfidtag AND
     rfidtag.rfidtag <> rfidtag.rfidtag:SCREEN-VALUE IN FRAME {&FRAME-NAME}
     THEN DO:
     FIND FIRST bfRfid WHERE bfRfid.company = rfidtag.company
                         AND bfRfid.rfidtag = rfidtag.rfidtag EXCLUSIVE-LOCK.
     IF AVAIL bfRfid THEN DO:
       ASSIGN bfRfid.rfidtag = rfidtag.rfidtag:SCREEN-VALUE.
       RELEASE bfRfid.
     END.     
  END.
  ASSIGN rfidtag.rfidtag:SENSITIVE = NO.    

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
  ASSIGN rfidtag.rfidtag:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  loadtag.company = g_company.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
    RUN get-item-type IN WIDGET-HANDLE(char-hdl) (OUTPUT loadtag.item-type).
    RUN get-is-case-tag IN WIDGET-HANDLE(char-hdl) (OUTPUT loadtag.is-case-tag).
  END.

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

  IF AVAIL loadtag THEN
     ASSIGN
        loadtag.sts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = loadtag.sts
        v-tagtime:SCREEN-VALUE = STRING(loadtag.tag-time,"hh:mm:ss").

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"loadtag-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN enable-all IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-enable V-table-Win 
PROCEDURE post-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"loadtag-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN disable-all IN WIDGET-HANDLE(char-hdl).
  IF USERID('nosweat') = "asi" AND AVAIL rfidtag THEN
     ASSIGN rfidtag.rfidtag:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-loadtag V-table-Win 
PROCEDURE print-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN addon/loadtags/d-tagprt.w (RECID(loadtag)).
   find CURRENT loadtag NO-LOCK NO-ERROR.
   IF AVAIL loadtag THEN 
      loadtag.sts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = loadtag.sts.
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
  {src/adm/template/snd-list.i "loadtag"}
  {src/adm/template/snd-list.i "rfidtag"}

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

