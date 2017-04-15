&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

&glob DATA-LOGIC-PROCEDURE .p

{custom/globdefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES job-hdr job

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  job-no job-no2 i-no est-no ord-no cust-no company start-date
&Scoped-define ENABLED-FIELDS-IN-job-hdr job-no job-no2 i-no est-no ord-no ~
cust-no company start-date 
&Scoped-Define DATA-FIELDS  job-no job-no2 i-no est-no ord-no cust-no company start-date
&Scoped-define DATA-FIELDS-IN-job-hdr job-no job-no2 i-no est-no ord-no ~
cust-no company start-date 
&Scoped-Define MANDATORY-FIELDS  i-no est-no cust-no company
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "jcinq/do-jobno.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH job-hdr WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST job OF job-hdr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH job-hdr WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST job OF job-hdr NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main job-hdr job
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main job-hdr
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main job


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      job-hdr, 
      job SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.43
         WIDTH              = 36.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "asi.job-hdr,asi.job OF asi.job-hdr"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _FldNameList[1]   > asi.job-hdr.job-no
"job-no" "job-no" ? ? "character" ? ? ? ? ? ? yes ? no 11.4 yes
     _FldNameList[2]   > asi.job-hdr.job-no2
"job-no2" "job-no2" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[3]   > asi.job-hdr.i-no
"i-no" "i-no" ? ? "character" ? ? ? ? ? ? yes ? yes 15 yes
     _FldNameList[4]   > asi.job-hdr.est-no
"est-no" "est-no" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes
     _FldNameList[5]   > asi.job-hdr.ord-no
"ord-no" "ord-no" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[6]   > asi.job-hdr.cust-no
"cust-no" "cust-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes
     _FldNameList[7]   > asi.job-hdr.company
"company" "company" ? ? "character" ? ? ? ? ? ? yes ? yes 8.8 yes
     _FldNameList[8]   > asi.job-hdr.start-date
"start-date" "start-date" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareDataForFetch dTables  _DB-REQUIRED
PROCEDURE prepareDataForFetch :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT        PARAMETER phTopContainer AS HANDLE NO-UNDO.
  DEFINE INPUT        PARAMETER pcAppService   AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER pcSourceName   AS CHARACTER NO-UNDO.
  DEFINE INPUT        PARAMETER pcOptions      AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcHandles      AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcRunNames     AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcQualNames    AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcQueryFields  AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcQueries      AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcTempTables   AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT phTopContainer, INPUT pcAppService, INPUT pcSourceName, INPUT pcOptions, INPUT-OUTPUT pcHandles, INPUT-OUTPUT pcRunNames, INPUT-OUTPUT pcQualNames, INPUT-OUTPUT pcQueryFields, INPUT-OUTPUT pcQueries, INPUT-OUTPUT pcTempTables).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

