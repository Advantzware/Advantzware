&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rfq-ctrl
&Scoped-define FIRST-EXTERNAL-TABLE rfq-ctrl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfq-ctrl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfq-ctrl.rfq-num rfq-ctrl.rfq-range[1] ~
rfq-ctrl.rfq-range[2] rfq-ctrl.q-num rfq-ctrl.q-range[1] ~
rfq-ctrl.q-range[2] rfq-ctrl.ls-length rfq-ctrl.ls-triml rfq-ctrl.ls-width ~
rfq-ctrl.ls-trimw rfq-ctrl.mat-cost[1] rfq-ctrl.mat-pct[1] rfq-ctrl.def-ink ~
rfq-ctrl.mat-cost[2] rfq-ctrl.mat-pct[2] rfq-ctrl.def-inkcov ~
rfq-ctrl.hd-net rfq-ctrl.mat-cost[3] rfq-ctrl.mat-pct[3] rfq-ctrl.def-case ~
rfq-ctrl.hd-gross rfq-ctrl.mat-cost[4] rfq-ctrl.mat-pct[4] ~
rfq-ctrl.def-coat rfq-ctrl.sell-by rfq-ctrl.mat-cost[5] rfq-ctrl.mat-pct[5] ~
rfq-ctrl.def-pal rfq-ctrl.shp-add rfq-ctrl.mat-cost[6] rfq-ctrl.mat-pct[6] ~
rfq-ctrl.r-cost rfq-ctrl.comm-add rfq-ctrl.lab-cost[1] rfq-ctrl.lab-pct[1] ~
rfq-ctrl.fg-rate rfq-ctrl.sho-labor rfq-ctrl.lab-cost[2] ~
rfq-ctrl.lab-pct[2] rfq-ctrl.rm-rate rfq-ctrl.comm-mrkup ~
rfq-ctrl.lab-cost[3] rfq-ctrl.lab-pct[3] rfq-ctrl.hand-pct ~
rfq-ctrl.lab-cost[4] rfq-ctrl.lab-pct[4] rfq-ctrl.whse-mrkup ~
rfq-ctrl.lab-cost[5] rfq-ctrl.lab-pct[5] rfq-ctrl.spec-lab ~
rfq-ctrl.spec-pct rfq-ctrl.lab-cost[6] rfq-ctrl.lab-pct[6] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}rfq-num ~{&FP2}rfq-num ~{&FP3}~
 ~{&FP1}rfq-range[1] ~{&FP2}rfq-range[1] ~{&FP3}~
 ~{&FP1}rfq-range[2] ~{&FP2}rfq-range[2] ~{&FP3}~
 ~{&FP1}q-num ~{&FP2}q-num ~{&FP3}~
 ~{&FP1}q-range[1] ~{&FP2}q-range[1] ~{&FP3}~
 ~{&FP1}q-range[2] ~{&FP2}q-range[2] ~{&FP3}~
 ~{&FP1}ls-length ~{&FP2}ls-length ~{&FP3}~
 ~{&FP1}ls-triml ~{&FP2}ls-triml ~{&FP3}~
 ~{&FP1}ls-width ~{&FP2}ls-width ~{&FP3}~
 ~{&FP1}ls-trimw ~{&FP2}ls-trimw ~{&FP3}~
 ~{&FP1}mat-cost[1] ~{&FP2}mat-cost[1] ~{&FP3}~
 ~{&FP1}mat-pct[1] ~{&FP2}mat-pct[1] ~{&FP3}~
 ~{&FP1}def-ink ~{&FP2}def-ink ~{&FP3}~
 ~{&FP1}mat-cost[2] ~{&FP2}mat-cost[2] ~{&FP3}~
 ~{&FP1}mat-pct[2] ~{&FP2}mat-pct[2] ~{&FP3}~
 ~{&FP1}def-inkcov ~{&FP2}def-inkcov ~{&FP3}~
 ~{&FP1}hd-net ~{&FP2}hd-net ~{&FP3}~
 ~{&FP1}mat-cost[3] ~{&FP2}mat-cost[3] ~{&FP3}~
 ~{&FP1}mat-pct[3] ~{&FP2}mat-pct[3] ~{&FP3}~
 ~{&FP1}def-case ~{&FP2}def-case ~{&FP3}~
 ~{&FP1}hd-gross ~{&FP2}hd-gross ~{&FP3}~
 ~{&FP1}mat-cost[4] ~{&FP2}mat-cost[4] ~{&FP3}~
 ~{&FP1}mat-pct[4] ~{&FP2}mat-pct[4] ~{&FP3}~
 ~{&FP1}def-coat ~{&FP2}def-coat ~{&FP3}~
 ~{&FP1}sell-by ~{&FP2}sell-by ~{&FP3}~
 ~{&FP1}mat-cost[5] ~{&FP2}mat-cost[5] ~{&FP3}~
 ~{&FP1}mat-pct[5] ~{&FP2}mat-pct[5] ~{&FP3}~
 ~{&FP1}def-pal ~{&FP2}def-pal ~{&FP3}~
 ~{&FP1}shp-add ~{&FP2}shp-add ~{&FP3}~
 ~{&FP1}mat-cost[6] ~{&FP2}mat-cost[6] ~{&FP3}~
 ~{&FP1}mat-pct[6] ~{&FP2}mat-pct[6] ~{&FP3}~
 ~{&FP1}r-cost ~{&FP2}r-cost ~{&FP3}~
 ~{&FP1}comm-add ~{&FP2}comm-add ~{&FP3}~
 ~{&FP1}lab-cost[1] ~{&FP2}lab-cost[1] ~{&FP3}~
 ~{&FP1}lab-pct[1] ~{&FP2}lab-pct[1] ~{&FP3}~
 ~{&FP1}fg-rate ~{&FP2}fg-rate ~{&FP3}~
 ~{&FP1}sho-labor ~{&FP2}sho-labor ~{&FP3}~
 ~{&FP1}lab-cost[2] ~{&FP2}lab-cost[2] ~{&FP3}~
 ~{&FP1}lab-pct[2] ~{&FP2}lab-pct[2] ~{&FP3}~
 ~{&FP1}rm-rate ~{&FP2}rm-rate ~{&FP3}~
 ~{&FP1}comm-mrkup ~{&FP2}comm-mrkup ~{&FP3}~
 ~{&FP1}lab-cost[3] ~{&FP2}lab-cost[3] ~{&FP3}~
 ~{&FP1}lab-pct[3] ~{&FP2}lab-pct[3] ~{&FP3}~
 ~{&FP1}hand-pct ~{&FP2}hand-pct ~{&FP3}~
 ~{&FP1}lab-cost[4] ~{&FP2}lab-cost[4] ~{&FP3}~
 ~{&FP1}lab-pct[4] ~{&FP2}lab-pct[4] ~{&FP3}~
 ~{&FP1}whse-mrkup ~{&FP2}whse-mrkup ~{&FP3}~
 ~{&FP1}lab-cost[5] ~{&FP2}lab-cost[5] ~{&FP3}~
 ~{&FP1}lab-pct[5] ~{&FP2}lab-pct[5] ~{&FP3}~
 ~{&FP1}spec-lab ~{&FP2}spec-lab ~{&FP3}~
 ~{&FP1}spec-pct ~{&FP2}spec-pct ~{&FP3}~
 ~{&FP1}lab-cost[6] ~{&FP2}lab-cost[6] ~{&FP3}~
 ~{&FP1}lab-pct[6] ~{&FP2}lab-pct[6] ~{&FP3}
&Scoped-define ENABLED-TABLES rfq-ctrl
&Scoped-define FIRST-ENABLED-TABLE rfq-ctrl
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-6 
&Scoped-Define DISPLAYED-FIELDS rfq-ctrl.rfq-num rfq-ctrl.rfq-range[1] ~
rfq-ctrl.rfq-range[2] rfq-ctrl.q-num rfq-ctrl.q-range[1] ~
rfq-ctrl.q-range[2] rfq-ctrl.ls-length rfq-ctrl.ls-triml rfq-ctrl.ls-width ~
rfq-ctrl.ls-trimw rfq-ctrl.mat-cost[1] rfq-ctrl.mat-pct[1] rfq-ctrl.def-ink ~
rfq-ctrl.mat-cost[2] rfq-ctrl.mat-pct[2] rfq-ctrl.def-inkcov ~
rfq-ctrl.hd-net rfq-ctrl.mat-cost[3] rfq-ctrl.mat-pct[3] rfq-ctrl.def-case ~
rfq-ctrl.hd-gross rfq-ctrl.mat-cost[4] rfq-ctrl.mat-pct[4] ~
rfq-ctrl.def-coat rfq-ctrl.sell-by rfq-ctrl.mat-cost[5] rfq-ctrl.mat-pct[5] ~
rfq-ctrl.def-pal rfq-ctrl.shp-add rfq-ctrl.mat-cost[6] rfq-ctrl.mat-pct[6] ~
rfq-ctrl.r-cost rfq-ctrl.comm-add rfq-ctrl.lab-cost[1] rfq-ctrl.lab-pct[1] ~
rfq-ctrl.fg-rate rfq-ctrl.sho-labor rfq-ctrl.lab-cost[2] ~
rfq-ctrl.lab-pct[2] rfq-ctrl.rm-rate rfq-ctrl.comm-mrkup ~
rfq-ctrl.lab-cost[3] rfq-ctrl.lab-pct[3] rfq-ctrl.hand-pct ~
rfq-ctrl.lab-cost[4] rfq-ctrl.lab-pct[4] rfq-ctrl.whse-mrkup ~
rfq-ctrl.lab-cost[5] rfq-ctrl.lab-pct[5] rfq-ctrl.spec-lab ~
rfq-ctrl.spec-pct rfq-ctrl.lab-cost[6] rfq-ctrl.lab-pct[6] 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 113 BY 2.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfq-ctrl.rfq-num AT ROW 1.24 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.rfq-range[1] AT ROW 1.24 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.rfq-range[2] AT ROW 1.24 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.q-num AT ROW 2.19 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.q-range[1] AT ROW 2.19 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.q-range[2] AT ROW 2.19 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.ls-length AT ROW 3.86 COL 24.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfq-ctrl.ls-triml AT ROW 3.86 COL 69 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.ls-width AT ROW 4.81 COL 24.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfq-ctrl.ls-trimw AT ROW 4.81 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.mat-cost[1] AT ROW 5.29 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[1] AT ROW 5.29 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-ink AT ROW 5.76 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.mat-cost[2] AT ROW 6.24 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[2] AT ROW 6.24 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-inkcov AT ROW 6.71 COL 31.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     rfq-ctrl.hd-net AT ROW 6.71 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     rfq-ctrl.mat-cost[3] AT ROW 7.19 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[3] AT ROW 7.19 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-case AT ROW 7.67 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.hd-gross AT ROW 7.91 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     rfq-ctrl.mat-cost[4] AT ROW 8.14 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[4] AT ROW 8.14 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-coat AT ROW 8.62 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.sell-by AT ROW 8.86 COL 75.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.mat-cost[5] AT ROW 9.1 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[5] AT ROW 9.1 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.def-pal AT ROW 9.57 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     rfq-ctrl.shp-add AT ROW 9.81 COL 75.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rfq-ctrl.mat-cost[6] AT ROW 10.05 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.mat-pct[6] AT ROW 10.05 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.r-cost AT ROW 10.52 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     rfq-ctrl.comm-add AT ROW 10.76 COL 75.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     rfq-ctrl.lab-cost[1] AT ROW 11 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[1] AT ROW 11 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.fg-rate AT ROW 11.71 COL 27.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.sho-labor AT ROW 11.71 COL 71 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     rfq-ctrl.lab-cost[2] AT ROW 11.95 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[2] AT ROW 11.95 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.rm-rate AT ROW 12.67 COL 27.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.comm-mrkup AT ROW 12.91 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.lab-cost[3] AT ROW 12.91 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[3] AT ROW 12.91 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.hand-pct AT ROW 13.62 COL 27.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.lab-cost[4] AT ROW 13.86 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[4] AT ROW 13.86 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.whse-mrkup AT ROW 14.57 COL 27.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.lab-cost[5] AT ROW 14.81 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[5] AT ROW 14.81 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.spec-lab AT ROW 15.52 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfq-ctrl.spec-pct AT ROW 15.52 COL 69.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     rfq-ctrl.lab-cost[6] AT ROW 15.76 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     rfq-ctrl.lab-pct[6] AT ROW 15.76 COL 116 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RECT-7 AT ROW 4.33 COL 102
     "MSF" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 4.57 COL 108
     "Markup%" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 4.57 COL 118
     RECT-6 AT ROW 1 COL 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfq-ctrl
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 16.86
         WIDTH              = 140.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "rfq-ctrl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfq-ctrl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "rfq-ctrl"}

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


