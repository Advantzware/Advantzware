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

DEF VAR newRecord AS LOG NO-UNDO.
{custom/resizdef.i}

DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR IsfirstTime AS LOG INIT YES NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES wiptag wiptag-mch mach
&Scoped-define FIRST-EXTERNAL-TABLE wiptag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR wiptag, wiptag-mch, mach.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS wiptag-mch.spare-char-1 ~
wiptag-mch.produced-qty 
&Scoped-define ENABLED-TABLES wiptag-mch
&Scoped-define FIRST-ENABLED-TABLE wiptag-mch
&Scoped-Define ENABLED-OBJECTS RECT-39 RECT-40 RECT-47 RECT-48 RECT-49 
&Scoped-Define DISPLAYED-FIELDS wiptag.tag-no wiptag-mch.spare-char-1 ~
wiptag-mch.m-code wiptag.wip-rm-bin wiptag.wip-warehouse wiptag.fg-i-no ~
wiptag.fg-i-name wiptag-mch.produced-qty wiptag.job-no wiptag.job-no2 ~
wiptag.form-no wiptag.blank-no wiptag.n-out wiptag.num-up wiptag.rm-i-no ~
wiptag.rm-i-name wiptag.crt-userid wiptag.crt-date wiptag.upd-userid ~
wiptag.upd-date wiptag.rm-tag-no wiptag.pallet-count wiptag.tag-date ~
wiptag.spare-char-1 
&Scoped-define DISPLAYED-TABLES wiptag wiptag-mch
&Scoped-define FIRST-DISPLAYED-TABLE wiptag
&Scoped-define SECOND-DISPLAYED-TABLE wiptag-mch
&Scoped-Define DISPLAYED-OBJECTS fi_mach-completed fi_m-dscr fi_sht-width ~
fi_sht-length fi_created-time fi_updated-time fi_tag-time 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,textFields,F1 */

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
DEFINE VARIABLE fi_created-time AS CHARACTER FORMAT "99:99:99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi_m-dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Machine description".

DEFINE VARIABLE fi_mach-completed AS CHARACTER FORMAT "X(8)":U INITIAL "NO" 
     LABEL "Completed" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sht-length AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Length" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sht-width AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Width" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi_tag-time AS CHARACTER FORMAT "99:99:99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi_updated-time AS CHARACTER FORMAT "99:99:99":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 3.33.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 140 BY 3.1.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 3.33.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 8.33.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_mach-completed AT ROW 1.48 COL 122 COLON-ALIGNED WIDGET-ID 188 NO-TAB-STOP 
     wiptag.tag-no AT ROW 1.67 COL 15.6 COLON-ALIGNED WIDGET-ID 148
          LABEL "WIP Tag#"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1 TOOLTIP "WIP Tag Number" NO-TAB-STOP 
     wiptag-mch.spare-char-1 AT ROW 3 COL 124.2 COLON-ALIGNED HELP
          "" WIDGET-ID 202
          LABEL "Shift" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1 TOOLTIP "Machine production shift"
     wiptag-mch.m-code AT ROW 3.05 COL 20 COLON-ALIGNED WIDGET-ID 164
          LABEL "Machine Code"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     fi_m-dscr AT ROW 3.05 COL 32.6 COLON-ALIGNED NO-LABEL WIDGET-ID 166 NO-TAB-STOP 
     wiptag.wip-rm-bin AT ROW 3.05 COL 100.6 COLON-ALIGNED NO-LABEL WIDGET-ID 158
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1 NO-TAB-STOP 
     wiptag.wip-warehouse AT ROW 3.1 COL 86.4 COLON-ALIGNED NO-LABEL WIDGET-ID 160
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1 NO-TAB-STOP 
     wiptag.fg-i-no AT ROW 4.91 COL 16.8 COLON-ALIGNED WIDGET-ID 126
          VIEW-AS FILL-IN 
          SIZE 25 BY 1 NO-TAB-STOP 
     wiptag.fg-i-name AT ROW 4.91 COL 42.6 COLON-ALIGNED NO-LABEL WIDGET-ID 124
          VIEW-AS FILL-IN 
          SIZE 34 BY 1 NO-TAB-STOP 
     wiptag-mch.produced-qty AT ROW 4.91 COL 100.2 COLON-ALIGNED WIDGET-ID 200
          VIEW-AS FILL-IN 
          SIZE 28 BY 1 TOOLTIP "The qty produced at the machine."
     wiptag.job-no AT ROW 6.29 COL 16.8 COLON-ALIGNED WIDGET-ID 130
          LABEL "Job #"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1 NO-TAB-STOP 
     wiptag.job-no2 AT ROW 6.29 COL 31.6 COLON-ALIGNED NO-LABEL WIDGET-ID 132
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1 NO-TAB-STOP 
     wiptag.form-no AT ROW 6.29 COL 49.6 COLON-ALIGNED WIDGET-ID 128
          LABEL "Form #"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1 NO-TAB-STOP 
     wiptag.blank-no AT ROW 6.29 COL 68 COLON-ALIGNED WIDGET-ID 120
          LABEL "Blank"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1 NO-TAB-STOP 
     wiptag.n-out AT ROW 6.29 COL 85.2 COLON-ALIGNED WIDGET-ID 134
          LABEL "# Out"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1 NO-TAB-STOP 
     wiptag.num-up AT ROW 6.29 COL 101.2 COLON-ALIGNED WIDGET-ID 136
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1 NO-TAB-STOP 
     wiptag.rm-i-no AT ROW 7.95 COL 16.8 COLON-ALIGNED WIDGET-ID 142
          VIEW-AS FILL-IN 
          SIZE 25 BY 1 NO-TAB-STOP 
     wiptag.rm-i-name AT ROW 7.95 COL 42.6 COLON-ALIGNED NO-LABEL WIDGET-ID 140
          VIEW-AS FILL-IN 
          SIZE 34 BY 1 NO-TAB-STOP 
     fi_sht-width AT ROW 9.19 COL 16.8 COLON-ALIGNED WIDGET-ID 190 NO-TAB-STOP 
     fi_sht-length AT ROW 9.19 COL 49 COLON-ALIGNED WIDGET-ID 192 NO-TAB-STOP 
     wiptag.crt-userid AT ROW 10.1 COL 93.2 COLON-ALIGNED NO-LABEL WIDGET-ID 122
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1 NO-TAB-STOP 
     wiptag.crt-date AT ROW 10.1 COL 106.6 COLON-ALIGNED NO-LABEL WIDGET-ID 172
          VIEW-AS FILL-IN 
          SIZE 15 BY 1 NO-TAB-STOP 
     fi_created-time AT ROW 10.1 COL 123 COLON-ALIGNED NO-LABEL WIDGET-ID 194 NO-TAB-STOP 
     wiptag.rm-whs AT ROW 11.38 COL 51.6 COLON-ALIGNED NO-LABEL WIDGET-ID 144
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     wiptag.rm-bin AT ROW 11.38 COL 65.2 COLON-ALIGNED NO-LABEL WIDGET-ID 138
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1 NO-TAB-STOP 
     wiptag.upd-userid AT ROW 11.38 COL 93.2 COLON-ALIGNED NO-LABEL WIDGET-ID 156
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1 NO-TAB-STOP 
     wiptag.upd-date AT ROW 11.38 COL 106.6 COLON-ALIGNED NO-LABEL WIDGET-ID 152
          VIEW-AS FILL-IN 
          SIZE 15 BY 1 NO-TAB-STOP 
     fi_updated-time AT ROW 11.38 COL 123 COLON-ALIGNED NO-LABEL WIDGET-ID 198 NO-TAB-STOP 
     wiptag.rm-tag-no AT ROW 11.43 COL 3.4 COLON-ALIGNED NO-LABEL WIDGET-ID 170
          VIEW-AS FILL-IN 
          SIZE 31 BY 1 NO-TAB-STOP 
     wiptag.pallet-count AT ROW 11.43 COL 35.4 COLON-ALIGNED NO-LABEL WIDGET-ID 168
          VIEW-AS FILL-IN 
          SIZE 14 BY 1 NO-TAB-STOP 
     wiptag.tag-date AT ROW 12.67 COL 106.6 COLON-ALIGNED NO-LABEL WIDGET-ID 146
          VIEW-AS FILL-IN 
          SIZE 15 BY 1 NO-TAB-STOP 
     fi_tag-time AT ROW 12.67 COL 123 COLON-ALIGNED NO-LABEL WIDGET-ID 196 NO-TAB-STOP 
     wiptag.spare-char-1 AT ROW 12.86 COL 3.4 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 174 FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 31 BY 1 TOOLTIP "2nd RM Tag used."
     "Pallet Count:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 10.57 COL 37.2 WIDGET-ID 220
     /*"Time:" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 9.24 COL 125.2 WIDGET-ID 216
     "Date:" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 9.24 COL 109 WIDGET-ID 214
     "User:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 9.29 COL 95.4 WIDGET-ID 212*/
     "WIP Whs./Bin:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 3.33 COL 70.4 WIDGET-ID 224
     "Tag" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 12.86 COL 89.2 WIDGET-ID 210
     "Updated" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 11.57 COL 84 WIDGET-ID 208
     "RM Tag#:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 10.57 COL 5.8 WIDGET-ID 204
     "Created" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 10.29 COL 84.2 WIDGET-ID 206
     RECT-39 AT ROW 1.24 COL 2 WIDGET-ID 62
     RECT-40 AT ROW 4.57 COL 2 WIDGET-ID 76
     RECT-47 AT ROW 1.24 COL 68 WIDGET-ID 226
     RECT-48 AT ROW 7.67 COL 2 WIDGET-ID 228
     RECT-49 AT ROW 7.67 COL 83 WIDGET-ID 230
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.wiptag,asi.wiptag-mch,asi.mach
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
         HEIGHT             = 15.33
         WIDTH              = 141.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.blank-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.crt-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.crt-date:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.crt-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.crt-userid:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.fg-i-name IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.fg-i-name:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.fg-i-no IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       wiptag.fg-i-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_created-time IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_created-time:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_m-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_m-dscr:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_mach-completed IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_mach-completed:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_sht-length IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sht-length:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_sht-width IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sht-width:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_tag-time IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_tag-time:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi_updated-time IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_updated-time:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.form-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.form-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.job-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.job-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.job-no2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.job-no2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag-mch.m-code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN wiptag.n-out IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.n-out:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.num-up IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       wiptag.num-up:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.pallet-count IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.pallet-count:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.rm-bin IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       wiptag.rm-bin:HIDDEN IN FRAME F-Main           = TRUE
       wiptag.rm-bin:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.rm-i-name IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.rm-i-name:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.rm-i-no IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       wiptag.rm-i-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.rm-tag-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.rm-tag-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.rm-whs IN FRAME F-Main
   NO-DISPLAY NO-ENABLE EXP-LABEL                                       */
ASSIGN 
       wiptag.rm-whs:HIDDEN IN FRAME F-Main           = TRUE
       wiptag.rm-whs:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag-mch.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN wiptag.spare-char-1 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       wiptag.spare-char-1:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.tag-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN wiptag.tag-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.tag-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.upd-date IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       wiptag.upd-date:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.upd-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.upd-userid:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.wip-rm-bin IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.wip-rm-bin:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN wiptag.wip-warehouse IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       wiptag.wip-warehouse:READ-ONLY IN FRAME F-Main        = TRUE.

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

&Scoped-define SELF-NAME wiptag-mch.m-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.m-code V-table-Win
ON HELP OF wiptag-mch.m-code IN FRAME F-Main /* Machine Code */
DO:
   DEF VAR char-val AS cha NO-UNDO.

   RUN windows/l-mach.w (g_company, g_loc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
   IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                 fi_m-dscr:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag-mch.m-code V-table-Win
ON VALUE-CHANGED OF wiptag-mch.m-code IN FRAME F-Main /* Machine Code */
DO:
  FIND FIRST mach NO-LOCK WHERE mach.company = g_company AND
                                mach.loc = g_loc AND
                                mach.m-code = wiptag-mch.m-code:SCREEN-VALUE
      NO-ERROR.

  IF AVAIL mach THEN ASSIGN fi_m-dscr:SCREEN-VALUE = mach.m-dscr.
  ELSE ASSIGN fi_m-dscr:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/resizmn.i}

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
  {src/adm/template/row-list.i "wiptag-mch"}
  {src/adm/template/row-list.i "mach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "wiptag"}
  {src/adm/template/row-find.i "wiptag-mch"}
  {src/adm/template/row-find.i "mach"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .*/
  /* overwrite all adm for no database tables*/
  newRecord = YES.
  RUN dispatch ('add-record').

/*   RUN dispatch ('enable-fields').                                             */
/*                                                                               */
/*   RUN notify ('add-record, GROUP-ASSIGN-TARGET':U).                           */
/*   RUN new-state('update':U). /* Signal that we're in a record update now. */  */
/*   RUN dispatch IN THIS-PROCEDURE ('apply-entry':U).                           */

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi_m-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h_browse AS HANDLE NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF adm-new-record THEN DO:
      ASSIGN wiptag-mch.company = wiptag.company
             wiptag-mch.tag-no = wiptag.tag-no
             wiptag-mch.m-code = wiptag-mch.m-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
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
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  RUN notify ('cancel-record, GROUP-ASSIGN-TARGET':U).

  RUN new-state('update-complete':U).

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ('initial').

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
   IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lMachineCode AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   IF NOT adm-new-record THEN DO:                                          */
/*      RUN get-link-handle IN adm-broker-hdl                                */
/*        (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).                */
/*      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:                    */
/*         RUN getMachine IN WIDGET-HANDLE(char-hdl) (OUTPUT lMachineCode).  */
/*         IF isFirstTime THEN do:                                           */
/*            RUN applyValueChanged IN WIDGET-HANDLE(char-hdl).              */
/*            isFirsttime = NO.                                              */
/*         END.                                                              */
/*      END.                                                                 */
/*                                                                           */
/*   END.                                                                    */
  FIND FIRST job-mat NO-LOCK WHERE job-mat.company = wiptag.company AND
                                   job-mat.rm-i-no = wiptag.rm-i-no AND
                                   job-mat.job-no = wiptag.job-no AND
                                   job-mat.job-no2 = wiptag.job-no2 AND
                                   job-mat.frm = wiptag.form-no AND
                                   job-mat.blank-no = wiptag.blank-no
  NO-ERROR.
  IF AVAIL job-mat THEN 
    ASSIGN 
       fi_sht-width:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(job-mat.wid)
       fi_sht-length:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(job-mat.len).

  IF AVAIL wiptag THEN
     ASSIGN fi_created-time:SCREEN-VALUE = STRING(wiptag.crt-time,"HH:MM:SS")
            fi_updated-time:SCREEN-VALUE = STRING(wiptag.upd-time,"HH:MM:SS")
            fi_tag-time:SCREEN-VALUE = STRING(wiptag.tag-time,"HH:MM:SS").

  FIND FIRST mach NO-LOCK WHERE mach.company = g_company AND
                                mach.loc = g_loc AND
                                mach.m-code = wiptag-mch.m-code:SCREEN-VALUE
      NO-ERROR.

  IF AVAIL mach THEN ASSIGN fi_m-dscr:SCREEN-VALUE = mach.m-dscr.
  ELSE ASSIGN fi_m-dscr:SCREEN-VALUE = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT adm-new-record THEN
    DISABLE wiptag-mch.m-code.
  ELSE IF adm-new-record THEN ENABLE wiptag-mch.m-code WITH FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
     RUN set-buttons IN WIDGET-HANDLE(char-hdl) ('initial').

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
  IF USERID("NOSWEAT") NE "ASI" THEN do:
       MESSAGE "User not ASI update not allow... " VIEW-AS ALERT-BOX ERROR .
       RETURN NO-APPLY.
   END.

  RUN valid-machine-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN wiptag-mch.m-code:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE max-widget V-table-Win 
PROCEDURE max-widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{custom/resizmx2.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restore-widget V-table-Win 
PROCEDURE restore-widget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{custom/resizrs2.i}

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
  {src/adm/template/snd-list.i "wiptag-mch"}
  {src/adm/template/snd-list.i "mach"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-machine-code V-table-Win 
PROCEDURE valid-machine-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
 IF NOT CAN-FIND(FIRST mach where mach.company = g_company AND
          mach.loc = g_loc AND 
          mach.m-code = wiptag-mch.m-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    ) THEN DO:
     MESSAGE "Invalid Machine. Try help. "
         VIEW-AS ALERT-BOX ERROR.
     APPLY 'entry' TO wiptag-mch.m-code.
     RETURN ERROR.
 END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

