&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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


DEF TEMP-TABLE tt-mach FIELD m-code AS cha .
                            
DEF TEMP-TABLE tt-date FIELD m-code AS cha
                       FIELDS s-date AS DATE.
                            
DEF TEMP-TABLE tt-sch-tmp FIELDS m-code AS cha
                      FIELDS s-date AS DATE FORM "99/99/9999"
                      FIELDS job-no LIKE job-hdr.job-no
                      FIELDS job-no2 LIKE job-hdr.job-no2
                      FIELDS i-no LIKE job-hdr.i-no
                      FIELDS job-list AS cha FORM "x(50)"
                      FIELDS seq AS INT
                      FIELDS p-width AS INT
                      FIELD job-color AS cha FORM "x(30)".
/*
DEF TEMP-TABLE tt-sch FIELDS m-code AS cha
                      FIELDS s-date AS DATE FORM "99/99/9999"
                      FIELDS job-no LIKE job-hdr.job-no
                      FIELDS job-no2 LIKE job-hdr.job-no2
                      FIELDS i-no LIKE job-hdr.i-no
                      FIELDS job-list AS cha FORM "x(50)"
                      FIELDS seq AS INT
                      FIELDS p-width AS INT
                      FIELD job-color AS cha FORM "x(30)".

  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-sch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-sch.m-code tt-sch.s-date tt-sch.job-list tt-sch.p-width tt-sch.i-no tt-sch.job-color   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-sch.m-code tt-sch.s-date ~
  tt-sch.i-no   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-sch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-sch
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH tt-sch
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH tt-sch.
&Scoped-define TABLES-IN-QUERY-br_table tt-sch
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-sch


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-sch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-sch.m-code tt-sch.s-date
      tt-sch.job-list 
        tt-sch.p-width
         tt-sch.i-no
      tt-sch.job-color
      ENABLE
       tt-sch.m-code tt-sch.s-date 
         tt-sch.i-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 114.2 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 11.43
         WIDTH              = 114.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-sch.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
/* ASSIGN  /* tt-sch.job-color:format IN BROWSE {&browse-name}
                    = "x(" + string(LENGTH(tt-sch.job-color)) + ")"  */
     tt-sch.job-color:BGCOLOR IN BROWSE {&browse-name}  = LENGTH(tt-sch.job-color)
     tt-sch.job-color:FGCOLOR IN BROWSE {&browse-name}  = LENGTH(tt-sch.job-color).    .

  */  
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table B-table-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-col-width AS INT NO-UNDO.
  DEF VAR lv-colhd1 AS WIDGET-HANDLE NO-UNDO.

  FOR EACH job-mch WHERE job-mch.m-code >= "408"
                     AND job-mch.m-code <= "b"
                     AND job-mch.start-date >= 01/18/04 
                     AND job-mch.start-date <= 02/28/04
      NO-LOCK
                     BREAK BY job-mch.m-code BY start-date BY start-time BY job-no:
      IF FIRST-OF(job-mch.m-code) THEN DO:
         CREATE tt-mach.
         ASSIGN tt-mach.m-code = job-mch.m-code.
      END.
      
      IF FIRST-OF(job-mch.start-date) THEN DO:
         CREATE tt-date .
         ASSIGN tt-date.m-code = job-mch.m-code
                tt-date.s-date = job-mch.start-date.
      END.

      IF FIRST-OF(job-no) THEN lv-col-width = 1.

      FIND job-hdr WHERE job-hdr.company = job-mch.company
                     AND job-hdr.job = job-mch.job
                     AND job-hdr.job-no = job-mch.job-no
                     AND job-hdr.job-no2 = job-mch.job-no2 NO-LOCK NO-ERROR.
      /*
      FIND FIRST tt-sch-tmp WHERE tt-sch-tmp.m-code = tt-mach.m-code 
                           and tt-sch-tmp.s-date = job-mch.start-date NO-LOCK NO-ERROR.
      IF not AVAIL tt-sch-tmp THEN
      */
      CREATE tt-sch-tmp.
      /*
      FIND FIRST tt-sch-tmp WHERE tt-sch-tmp.m-code = tt-mach.m-code 
                          and tt-sch-tmp.s-date = job-mch.start-date  NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-sch-tmp THEN BROWSE {&browse-name}:ADD-LIKE-COLUMN(tt-sch-tmp.s-date,
      */
      ASSIGN tt-sch-tmp.m-code = tt-mach.m-code
             tt-sch-tmp.s-date = tt-date.s-date
             tt-sch-tmp.job-list = tt-sch-tmp.job-list + 
                               job-mch.job-no + "-" + STRING(job-mch.job-no2,">9")
             tt-sch-tmp.p-width = tt-sch-tmp.p-width + 1
             tt-sch-tmp.job-no = job-mch.job-no
             tt-sch-tmp.job-no2 = job-mch.job-no2
             tt-sch-tmp.i-no = IF AVAIL job-hdr THEN job-hdr.i-no ELSE ""
             tt-sch-tmp.job-color = tt-sch-tmp.job-color + "X".      

  END.

  FOR EACH tt-sch-tmp BREAK BY tt-sch-tmp.m-code BY tt-sch-tmp.s-date:
      IF FIRST-OF(tt-sch-tmp.m-code) THEN DO:
         CREATE tt-sch.
         ASSIGN tt-sch.m-code = tt-sch-tmp.m-code.
      END.
      IF FIRST-OF(tt-sch-tmp.s-date) THEN DO:
         lv-colhd1 = BROWSE {&browse-name}:ADD-LIKE-COLUMN("tt-sch.s-date").
         lv-colhd1:LABEL  = string(job-mch.start-date,"99/99/9999").
         lv-colhd1:WIDTH-CHARS = 12    .      
         /*lv-colhd1:FORMAT = "x(10)" */.
      END.
      /*
      ASSIGN tt-sch.job-list = tt-sch-tmp.job-list
             tt-sch.p-width = tt-sch-tmp.p-width
             tt-sch.job-color = tt-sch-tmp.job-color.
      */
      lv-colhd1 = lv-colhd1 +
                             tt-sch-tmp.job-no + "-" + STRING(tt-sch-tmp.job-no2,">9").
      IF LAST-OF(tt-sch-tmp.s-date) THEN
          ASSIGN /*tt-sch-tmp.job-color:WIDTH-CHARS IN BROWSE {&browse-name}
                 =  LENGTH(tt-sch-tmp.job-color) + 10 */
                 tt-sch.job-color:LABEL IN BROWSE {&browse-name}
                         = string(tt-sch-tmp.s-date,"99/99/9999").
        
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN build-table.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-sch"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

