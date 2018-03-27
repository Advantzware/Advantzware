&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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
&Scoped-define INTERNAL-TABLES fa-jrnl

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Asset-code Credit-amt Currency-cod Debit-amt Entity-code Entry-no~
 Explanation fa-entity Gl-code hm-credit-amt hm-debit-amt Job-no Line-no~
 method not-used Prd rev Trans-date Yr
&Scoped-define ENABLED-FIELDS-IN-fa-jrnl Asset-code Credit-amt Currency-cod ~
Debit-amt Entity-code Entry-no Explanation fa-entity Gl-code hm-credit-amt ~
hm-debit-amt Job-no Line-no method not-used Prd rev Trans-date Yr 
&Scoped-Define DATA-FIELDS  Asset-code Credit-amt Currency-cod Debit-amt Entity-code Entry-no~
 Explanation fa-entity Gl-code hm-credit-amt hm-debit-amt Job-no Line-no~
 method not-used Prd rev Trans-date Yr
&Scoped-define DATA-FIELDS-IN-fa-jrnl Asset-code Credit-amt Currency-cod ~
Debit-amt Entity-code Entry-no Explanation fa-entity Gl-code hm-credit-amt ~
hm-debit-amt Job-no Line-no method not-used Prd rev Trans-date Yr 
&Scoped-Define MANDATORY-FIELDS  Currency-cod Entity-code Job-no
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "fa/sdoFaJrnl.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH fa-jrnl NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH fa-jrnl NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main fa-jrnl
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main fa-jrnl


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      fa-jrnl SCROLLING.
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
         HEIGHT             = 1.62
         WIDTH              = 46.6.
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
     _TblList          = "ASI.fa-jrnl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ASI.fa-jrnl.Asset-code
"Asset-code" "Asset-code" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[2]   > ASI.fa-jrnl.Credit-amt
"Credit-amt" "Credit-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.2 yes ?
     _FldNameList[3]   > ASI.fa-jrnl.Currency-cod
"Currency-cod" "Currency-cod" ? ? "character" ? ? ? ? ? ? yes ? yes 12 yes ?
     _FldNameList[4]   > ASI.fa-jrnl.Debit-amt
"Debit-amt" "Debit-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.2 yes ?
     _FldNameList[5]   > ASI.fa-jrnl.Entity-code
"Entity-code" "Entity-code" ? ? "character" ? ? ? ? ? ? yes ? yes 8.8 yes ?
     _FldNameList[6]   > ASI.fa-jrnl.Entry-no
"Entry-no" "Entry-no" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ?
     _FldNameList[7]   > ASI.fa-jrnl.Explanation
"Explanation" "Explanation" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[8]   > ASI.fa-jrnl.fa-entity
"fa-entity" "fa-entity" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[9]   > ASI.fa-jrnl.Gl-code
"Gl-code" "Gl-code" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[10]   > ASI.fa-jrnl.hm-credit-amt
"hm-credit-amt" "hm-credit-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.8 yes ?
     _FldNameList[11]   > ASI.fa-jrnl.hm-debit-amt
"hm-debit-amt" "hm-debit-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[12]   > ASI.fa-jrnl.Job-no
"Job-no" "Job-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes ?
     _FldNameList[13]   > ASI.fa-jrnl.Line-no
"Line-no" "Line-no" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ?
     _FldNameList[14]   > ASI.fa-jrnl.method
"method" "method" ? ? "character" ? ? ? ? ? ? yes ? no 7.2 yes ?
     _FldNameList[15]   > ASI.fa-jrnl.not-used
"not-used" "not-used" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[16]   > ASI.fa-jrnl.Prd
"Prd" "Prd" ? ? "integer" ? ? ? ? ? ? yes ? no 3.2 yes ?
     _FldNameList[17]   > ASI.fa-jrnl.rev
"rev" "rev" ? ? "logical" ? ? ? ? ? ? yes ? no 3.8 yes ?
     _FldNameList[18]   > ASI.fa-jrnl.Trans-date
"Trans-date" "Trans-date" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[19]   > ASI.fa-jrnl.Yr
"Yr" "Yr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ?
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

