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
&Scoped-define INTERNAL-TABLES est est-qty ef eb

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  est-no company est-date est-type eqty board brd-dscr cal cust-no i-coat~
 i-col len part-dscr1 part-no procat ship-id style stock-no wid dep
&Scoped-define ENABLED-FIELDS-IN-est est-no company est-date est-type 
&Scoped-define ENABLED-FIELDS-IN-est-qty eqty 
&Scoped-define ENABLED-FIELDS-IN-ef board brd-dscr cal 
&Scoped-define ENABLED-FIELDS-IN-eb cust-no i-coat i-col len part-dscr1 ~
part-no procat ship-id style stock-no wid dep 
&Scoped-Define DATA-FIELDS  est-no company est-date est-type eqty board brd-dscr cal cust-no i-coat~
 i-col len part-dscr1 part-no procat ship-id style stock-no wid dep
&Scoped-define DATA-FIELDS-IN-est est-no company est-date est-type 
&Scoped-define DATA-FIELDS-IN-est-qty eqty 
&Scoped-define DATA-FIELDS-IN-ef board brd-dscr cal 
&Scoped-define DATA-FIELDS-IN-eb cust-no i-coat i-col len part-dscr1 ~
part-no procat ship-id style stock-no wid dep 
&Scoped-Define MANDATORY-FIELDS  est-no company cust-no
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "est/dt-est.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH est ~
      WHERE est.company = "001" and ~
ASI.est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      EACH est-qty WHERE est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      EACH ef WHERE ef.company = est.company ~
  AND ef.est-no = est.est-no NO-LOCK, ~
      EACH eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no ~
      AND eb.form-no > 0 and eb.blank-no > 0 NO-LOCK ~
    BY est.est-no DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH est ~
      WHERE est.company = "001" and ~
ASI.est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      EACH est-qty WHERE est.est-type >= 1 and est.est-type <= 4 NO-LOCK, ~
      EACH ef WHERE ef.company = est.company ~
  AND ef.est-no = est.est-no NO-LOCK, ~
      EACH eb WHERE eb.company = est.company ~
  AND eb.est-no = est.est-no ~
      AND eb.form-no > 0 and eb.blank-no > 0 NO-LOCK ~
    BY est.est-no DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main est est-qty ef eb
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main est
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main est-qty
&Scoped-define THIRD-TABLE-IN-QUERY-Query-Main ef
&Scoped-define FOURTH-TABLE-IN-QUERY-Query-Main eb


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      est, 
      est-qty, 
      ef, 
      eb SCROLLING.
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
     _TblList          = "ASI.est,ASI.est-qty WHERE ASI.est ...,ASI.ef WHERE ASI.est ...,ASI.eb WHERE ASI.est ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.est.est-no|no"
     _Where[1]         = "est.company = ""001"" and
ASI.est.est-type >= 1 and est.est-type <= 4"
     _JoinCode[2]      = "ASI.est.est-type >= 1 and est.est-type <= 4"
     _JoinCode[3]      = "ASI.ef.company = ASI.est.company
  AND ASI.ef.est-no = ASI.est.est-no"
     _JoinCode[4]      = "ASI.eb.company = ASI.est.company
  AND ASI.eb.est-no = ASI.est.est-no"
     _Where[4]         = "eb.form-no > 0 and eb.blank-no > 0"
     _FldNameList[1]   > ASI.est.est-no
"est-no" "est-no" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes
     _FldNameList[2]   > ASI.est.company
"company" "company" ? ? "character" ? ? ? ? ? ? yes ? yes 8.8 yes
     _FldNameList[3]   > ASI.est.est-date
"est-date" "est-date" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[4]   > ASI.est.est-type
"est-type" "est-type" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes
     _FldNameList[5]   > ASI.est-qty.eqty
"eqty" "eqty" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[6]   > ASI.ef.board
"board" "board" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[7]   > ASI.ef.brd-dscr
"brd-dscr" "brd-dscr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes
     _FldNameList[8]   > ASI.ef.cal
"cal" "cal" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.8 yes
     _FldNameList[9]   > ASI.eb.cust-no
"cust-no" "cust-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes
     _FldNameList[10]   > ASI.eb.i-coat
"i-coat" "i-coat" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes
     _FldNameList[11]   > ASI.eb.i-col
"i-col" "i-col" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes
     _FldNameList[12]   > ASI.eb.len
"len" "len" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[13]   > ASI.eb.part-dscr1
"part-dscr1" "part-dscr1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[14]   > ASI.eb.part-no
"part-no" "part-no" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[15]   > ASI.eb.procat
"procat" "procat" ? ? "character" ? ? ? ? ? ? yes ? no 8.4 yes
     _FldNameList[16]   > ASI.eb.ship-id
"ship-id" "ship-id" ? ? "character" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[17]   > ASI.eb.style
"style" "style" ? ? "character" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[18]   > ASI.eb.stock-no
"stock-no" "stock-no" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes
     _FldNameList[19]   > ASI.eb.wid
"wid" "wid" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
     _FldNameList[20]   > ASI.eb.dep
"dep" "dep" ? ? "decimal" ? ? ? ? ? ? yes ? no 9 yes
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

