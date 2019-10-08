&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          ptdb1            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"fa/sdofamast.i"}.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "fa/sdofamast.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.asset-code RowObject.asset-desc ~
RowObject.fa-entity RowObject.serial# RowObject.gl-code RowObject.new-used ~
RowObject.location RowObject.Cust-no RowObject.asset-status ~
RowObject.group-code RowObject.dep-basis-bk RowObject.acc-dep-book ~
RowObject.Entity-code RowObject.dep-basis-t1 RowObject.acc-dep-tax1 ~
RowObject.sort-code1 RowObject.dep-basis-t2 RowObject.acc-dep-tax2 ~
RowObject.sort-code2 RowObject.cost-book RowObject.Depr-alt ~
RowObject.life-book RowObject.salvage RowObject.Exch-rate ~
RowObject.life-tax-1 RowObject.method-tax-1 RowObject.itc-amt ~
RowObject.proceeds RowObject.life-tax-2 RowObject.method-tax-2 ~
RowObject.date-aquired RowObject.purch-order# RowObject.Document ~
RowObject.Job-no RowObject.date-service RowObject.par-asset RowObject.memo ~
RowObject.last-autodepr-date RowObject.date-retired RowObject.child-par ~
RowObject.business-% RowObject.yr-of-depr RowObject.method-book ~
RowObject.mth-year RowObject.auto RowObject.multiple 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define DISPLAYED-FIELDS RowObject.asset-code RowObject.asset-desc ~
RowObject.fa-entity RowObject.serial# RowObject.gl-code RowObject.new-used ~
RowObject.location RowObject.Cust-no RowObject.asset-status ~
RowObject.group-code RowObject.dep-basis-bk RowObject.acc-dep-book ~
RowObject.Entity-code RowObject.dep-basis-t1 RowObject.acc-dep-tax1 ~
RowObject.sort-code1 RowObject.dep-basis-t2 RowObject.acc-dep-tax2 ~
RowObject.sort-code2 RowObject.cost-book RowObject.Depr-alt ~
RowObject.life-book RowObject.salvage RowObject.Exch-rate ~
RowObject.life-tax-1 RowObject.method-tax-1 RowObject.itc-amt ~
RowObject.proceeds RowObject.life-tax-2 RowObject.method-tax-2 ~
RowObject.date-aquired RowObject.purch-order# RowObject.Document ~
RowObject.Job-no RowObject.date-service RowObject.par-asset RowObject.memo ~
RowObject.last-autodepr-date RowObject.date-retired RowObject.child-par ~
RowObject.business-% RowObject.yr-of-depr RowObject.method-book ~
RowObject.mth-year RowObject.auto RowObject.multiple 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.asset-code AT ROW 1 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.asset-desc AT ROW 1 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31.14 BY 1
     RowObject.fa-entity AT ROW 1 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.serial# AT ROW 2.25 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.gl-code AT ROW 2.25 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.new-used AT ROW 2.25 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
     RowObject.location AT ROW 3.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.Cust-no AT ROW 3.5 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.asset-status AT ROW 3.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
     RowObject.group-code AT ROW 4.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.14 BY 1
     RowObject.dep-basis-bk AT ROW 4.5 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.acc-dep-book AT ROW 4.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.Entity-code AT ROW 5.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.dep-basis-t1 AT ROW 5.5 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.acc-dep-tax1 AT ROW 5.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.sort-code1 AT ROW 6.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.dep-basis-t2 AT ROW 6.5 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.acc-dep-tax2 AT ROW 6.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.sort-code2 AT ROW 7.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.cost-book AT ROW 7.5 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.Depr-alt AT ROW 7.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.14 BY 1
     RowObject.life-book AT ROW 8.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY 1
     RowObject.salvage AT ROW 8.75 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.Exch-rate AT ROW 8.75 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     RowObject.life-tax-1 AT ROW 9.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
     RowObject.method-tax-1 AT ROW 9.75 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.itc-amt AT ROW 9.75 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.proceeds AT ROW 9.75 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.14 BY 1
     RowObject.life-tax-2 AT ROW 10.75 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.method-tax-2 AT ROW 10.75 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.date-aquired AT ROW 12 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.14 BY 1
     RowObject.purch-order# AT ROW 12 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.14 BY 1
     RowObject.Document AT ROW 12 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.Job-no AT ROW 12 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.date-service AT ROW 13 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.14 BY 1
     RowObject.par-asset AT ROW 13 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY 1
     RowObject.memo AT ROW 13.25 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35 BY 1
     RowObject.last-autodepr-date AT ROW 14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.14 BY 1
     RowObject.date-retired AT ROW 15 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.child-par AT ROW 15 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY 1
     RowObject.business-% AT ROW 15 COL 70 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.14 BY 1
     RowObject.yr-of-depr AT ROW 15 COL 92 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.14 BY 1
     RowObject.method-book AT ROW 16.25 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.mth-year AT ROW 16.25 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY 1
     RowObject.auto AT ROW 16.25 COL 64 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY 1
     RowObject.multiple AT ROW 16.5 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.14 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "fa/sdofamast.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {fa/sdofamast.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 16.5
         WIDTH              = 114.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

