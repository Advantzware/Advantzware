&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: jcrep\v-wiptag.w

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

{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}

assign cocode = g_company
       locode = g_loc.

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
&Scoped-define EXTERNAL-TABLES wiptag
&Scoped-define FIRST-EXTERNAL-TABLE wiptag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR wiptag.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS wiptag.sts wiptag.completed ~
wiptag.wip-warehouse wiptag.wip-rm-bin wiptag.shift wiptag.fg-i-no ~
wiptag.fg-i-name wiptag.cust-no wiptag.job-no wiptag.job-no2 wiptag.form-no ~
wiptag.blank-no wiptag.n-out wiptag.num-up wiptag.rm-i-no wiptag.rm-i-name ~
wiptag.rm-tag-no wiptag.rm-whs wiptag.rm-bin wiptag.pallet-count ~
wiptag.tag-date wiptag.spare-char-1 
&Scoped-define ENABLED-TABLES wiptag
&Scoped-define FIRST-ENABLED-TABLE wiptag
&Scoped-Define ENABLED-OBJECTS RECT-42 RECT-43 RECT-44 RECT-45 RECT-46 
&Scoped-Define DISPLAYED-FIELDS wiptag.tag-no wiptag.sts wiptag.completed ~
wiptag.wip-warehouse wiptag.wip-rm-bin wiptag.shift wiptag.fg-i-no ~
wiptag.fg-i-name wiptag.cust-no wiptag.job-no wiptag.job-no2 wiptag.form-no ~
wiptag.blank-no wiptag.n-out wiptag.num-up wiptag.rm-i-no wiptag.rm-i-name ~
wiptag.crt-userid wiptag.crt-date wiptag.rm-tag-no wiptag.rm-whs ~
wiptag.rm-bin wiptag.pallet-count wiptag.upd-userid wiptag.upd-date ~
wiptag.tag-date wiptag.spare-char-1 
&Scoped-define DISPLAYED-TABLES wiptag
&Scoped-define FIRST-DISPLAYED-TABLE wiptag
&Scoped-Define DISPLAYED-OBJECTS fi_sht-wid fi_sht-len v-tagtime ~
v-tagtime-upd v-tagtime-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,textFields,F1 */
&Scoped-define ADM-CREATE-FIELDS wiptag.tag-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_sht-wid fi_sht-len 
&Scoped-define DISPLAY-FIELD fi_sht-wid fi_sht-len 

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
DEFINE VARIABLE fi_sht-len AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Length" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sht-wid AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Width" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE v-tagtime AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-tagtime-2 AS CHARACTER FORMAT "99:99:99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE v-tagtime-upd AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133 BY 2.86.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57.7 BY 7.38.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 3.1.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 3.1.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.2 BY 7.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     wiptag.tag-no AT ROW 1.48 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     wiptag.sts AT ROW 1.48 COL 85.2 COLON-ALIGNED HELP
          "" WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Printed","Received","On Hand","Bill of Lading","Invoiced","Completed","Deleted","Issued","Transferred" 
          DROP-DOWN
          SIZE 23 BY 1
     wiptag.completed AT ROW 1.48 COL 125.4 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     wiptag.wip-warehouse AT ROW 2.76 COL 24.8 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "WIP Whs./Bin"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     wiptag.wip-rm-bin AT ROW 2.76 COL 41 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     wiptag.shift AT ROW 2.86 COL 125.4 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.fg-i-no AT ROW 4.62 COL 15 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.fg-i-name AT ROW 4.62 COL 41.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     wiptag.cust-no AT ROW 5.86 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     wiptag.job-no AT ROW 5.86 COL 54.6 COLON-ALIGNED HELP
          "Job Number." WIDGET-ID 6
          LABEL "Job Number"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     wiptag.job-no2 AT ROW 5.86 COL 67 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.form-no AT ROW 5.86 COL 81.2 COLON-ALIGNED WIDGET-ID 14
          LABEL "Form #"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     wiptag.blank-no AT ROW 5.86 COL 98.6 COLON-ALIGNED WIDGET-ID 8
          LABEL "Blank #"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.n-out AT ROW 5.86 COL 112.8 COLON-ALIGNED WIDGET-ID 24
          LABEL "# Out"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.num-up AT ROW 5.86 COL 125.8 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     wiptag.rm-i-no AT ROW 8 COL 15 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.rm-i-name AT ROW 8 COL 41.4 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     fi_sht-wid AT ROW 9.43 COL 14.8 COLON-ALIGNED WIDGET-ID 82
     fi_sht-len AT ROW 9.43 COL 42.6 COLON-ALIGNED WIDGET-ID 80
     wiptag.crt-userid AT ROW 10.43 COL 87.8 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     wiptag.crt-date AT ROW 10.43 COL 102.6 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     v-tagtime AT ROW 10.43 COL 120.2 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     wiptag.rm-tag-no AT ROW 11.67 COL 3 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.rm-whs AT ROW 11.67 COL 29.2 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     wiptag.rm-bin AT ROW 11.67 COL 41.8 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     wiptag.pallet-count AT ROW 11.67 COL 58.2 COLON-ALIGNED NO-LABEL WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     wiptag.upd-userid AT ROW 11.67 COL 87.8 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     wiptag.upd-date AT ROW 11.67 COL 102.6 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     v-tagtime-upd AT ROW 11.67 COL 120.2 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     wiptag.tag-date AT ROW 12.86 COL 102.4 COLON-ALIGNED NO-LABEL WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     v-tagtime-2 AT ROW 12.86 COL 120.2 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     wiptag.spare-char-1 AT ROW 12.91 COL 3 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 84 FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1 TOOLTIP "Second RM Tag used for WIP Tag quantity."
     /*"User:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 9.57 COL 90.2 WIDGET-ID 96*/
     /*"Time:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 9.57 COL 122 WIDGET-ID 98*/
     "Created" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 10.62 COL 79 WIDGET-ID 90
     "Updated" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 11.81 COL 78.8 WIDGET-ID 92
     "RM Whs./Bin:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 31.8 WIDGET-ID 104
     "Tag" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 13 COL 83.2 WIDGET-ID 94
     "RM Tag#:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 5.6 WIDGET-ID 86
     "Pallet Count:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.76 COL 60.4 WIDGET-ID 88
     /*"Date:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 9.57 COL 104.6 WIDGET-ID 100*/
     RECT-42 AT ROW 4.33 COL 2.97 WIDGET-ID 102
     RECT-43 AT ROW 7.43 COL 78.4 WIDGET-ID 106
     RECT-44 AT ROW 1.24 COL 3 WIDGET-ID 108
     RECT-45 AT ROW 1.24 COL 71 WIDGET-ID 110
     RECT-46 AT ROW 7.43 COL 3 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.wiptag
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
         WIDTH              = 139.
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

/* SETTINGS FOR FILL-IN wiptag.blank-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.crt-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.crt-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.cust-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.fg-i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_sht-len IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_sht-wid IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN wiptag.form-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.job-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.n-out IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.pallet-count IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.rm-bin IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.rm-i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.rm-tag-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.rm-whs IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX wiptag.sts IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN wiptag.tag-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.tag-no IN FRAME F-Main
   NO-ENABLE 1 EXP-HELP                                                 */
/* SETTINGS FOR FILL-IN wiptag.upd-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.upd-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN v-tagtime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-tagtime-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-tagtime-upd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wiptag.wip-rm-bin IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.wip-warehouse IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "wiptag"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "wiptag"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  DO WITH FRAME {&FRAME-NAME}:

     wiptag.tag-time = (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,1,2)) * 3600)
                     + (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,4,2)) * 60)
                     + (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,7,2))).

     RUN reftable-values(INPUT NO).
  END.

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
  DISABLE v-tagtime-2 fi_sht-wid fi_sht-len WITH FRAME {&FRAME-NAME}.
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
  DO WITH FRAME {&FRAME-NAME}:

  ASSIGN
     wiptag.company = g_company
     fi_sht-wid:SCREEN-VALUE = "0"
     fi_sht-len:SCREEN-VALUE = "0".
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

  IF AVAIL wiptag THEN
  DO:
     ASSIGN
        wiptag.sts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wiptag.sts
        v-tagtime:SCREEN-VALUE = STRING(wiptag.crt-time,"hh:mm:ss")
        v-tagtime-upd:SCREEN-VALUE = STRING(wiptag.upd-time,"hh:mm:ss")
        v-tagtime-2:SCREEN-VALUE = STRING(wiptag.tag-time,"hh:mm:ss").

     IF NOT adm-new-record THEN
        RUN reftable-values (YES).
  END.

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

  /* Dispatch standard ADM method.                             */
  {&methods/lValidateError.i YES} 
   IF USERID("NOSWEAT") NE "ASI" THEN do:
       MESSAGE "User not ASI update not allow... " VIEW-AS ALERT-BOX ERROR .
       RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i NO}

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN local-display-fields.
  DISABLE v-tagtime-2 fi_sht-wid fi_sht-len WITH FRAME {&FRAME-NAME}.
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-display AS LOG NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

       IF AVAIL wiptag THEN DO:
          FIND FIRST reftable WHERE
               reftable.reftable = "WIPLEN" AND
               reftable.company = wiptag.company AND
               reftable.CODE = wiptag.tag-no
               USE-INDEX CODE
               NO-ERROR.

          IF NOT AVAIL reftable THEN
          DO:
             CREATE reftable.
             ASSIGN
                reftable.reftable = "WIPLEN"
                reftable.company = wiptag.company
                reftable.CODE = wiptag.tag-no.
          END.

          IF ip-display THEN
             ASSIGN
                fi_sht-wid:SCREEN-VALUE = STRING(reftable.val[1])
                fi_sht-len:SCREEN-VALUE = STRING(reftable.val[2]).
          ELSE
             ASSIGN
                reftable.val[1] = DEC(fi_sht-wid:SCREEN-VALUE)
                reftable.val[2] = DEC(fi_sht-len:SCREEN-VALUE).

          FIND CURRENT reftable NO-LOCK.
       END.
    END.
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
  {src/adm/template/snd-list.i "wiptag"}

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

