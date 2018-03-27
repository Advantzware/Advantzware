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
&Scoped-define INTERNAL-TABLES fa-control fa-control-d

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  aqui-service calc-mode Currency-cod Entity-name FA-entity fa-gl-clear~
 fa-gl-disp gl-installed Job-no last-ad last-prd not-used11 not-used12~
 not-used13 not-used14 not-used15 not-used16 not-used17 not-used18~
 not-used19 not-used110 not-used111 not-used112 not-used113 not-used21~
 not-used22 not-used23 not-used24 not-used25 not-used26 not-used27~
 not-used28 not-used29 not-used210 not-used211 not-used212 not-used213~
 Number-prd Prd state-fed tag-update View-login Yr dep-a-no fa-a-no~
 FA-entity-2 gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no~
 Next-seq-no sort1-no sort2-no tag-no tax-a-no
&Scoped-define ENABLED-FIELDS-IN-fa-control aqui-service calc-mode ~
Currency-cod Entity-name FA-entity fa-gl-clear fa-gl-disp gl-installed ~
Job-no last-ad last-prd not-used11 not-used12 not-used13 not-used14 ~
not-used15 not-used16 not-used17 not-used18 not-used19 not-used110 ~
not-used111 not-used112 not-used113 not-used21 not-used22 not-used23 ~
not-used24 not-used25 not-used26 not-used27 not-used28 not-used29 ~
not-used210 not-used211 not-used212 not-used213 Number-prd Prd state-fed ~
tag-update View-login Yr 
&Scoped-define ENABLED-FIELDS-IN-fa-control-d dep-a-no fa-a-no FA-entity-2 ~
gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no Next-seq-no ~
sort1-no sort2-no tag-no tax-a-no 
&Scoped-Define DATA-FIELDS  aqui-service calc-mode Currency-cod Entity-name FA-entity fa-gl-clear~
 fa-gl-disp gl-installed Job-no last-ad last-prd not-used11 not-used12~
 not-used13 not-used14 not-used15 not-used16 not-used17 not-used18~
 not-used19 not-used110 not-used111 not-used112 not-used113 not-used21~
 not-used22 not-used23 not-used24 not-used25 not-used26 not-used27~
 not-used28 not-used29 not-used210 not-used211 not-used212 not-used213~
 Number-prd Prd state-fed tag-update View-login Yr dep-a-no fa-a-no~
 FA-entity-2 gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no~
 Next-seq-no sort1-no sort2-no tag-no tax-a-no
&Scoped-define DATA-FIELDS-IN-fa-control aqui-service calc-mode ~
Currency-cod Entity-name FA-entity fa-gl-clear fa-gl-disp gl-installed ~
Job-no last-ad last-prd not-used11 not-used12 not-used13 not-used14 ~
not-used15 not-used16 not-used17 not-used18 not-used19 not-used110 ~
not-used111 not-used112 not-used113 not-used21 not-used22 not-used23 ~
not-used24 not-used25 not-used26 not-used27 not-used28 not-used29 ~
not-used210 not-used211 not-used212 not-used213 Number-prd Prd state-fed ~
tag-update View-login Yr 
&Scoped-define DATA-FIELDS-IN-fa-control-d dep-a-no fa-a-no FA-entity-2 ~
gl-dist-gno gl-dist-no gl-mast-no glcode-a-no jrnl-no loc-a-no Next-seq-no ~
sort1-no sort2-no tag-no tax-a-no 
&Scoped-Define MANDATORY-FIELDS  Currency-cod Job-no state-fed
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.not-used11 = fa-control.not-used1[1]~
  rowObject.not-used12 = fa-control.not-used1[2]~
  rowObject.not-used13 = fa-control.not-used1[3]~
  rowObject.not-used14 = fa-control.not-used1[4]~
  rowObject.not-used15 = fa-control.not-used1[5]~
  rowObject.not-used16 = fa-control.not-used1[6]~
  rowObject.not-used17 = fa-control.not-used1[7]~
  rowObject.not-used18 = fa-control.not-used1[8]~
  rowObject.not-used19 = fa-control.not-used1[9]~
  rowObject.not-used110 = fa-control.not-used1[10]~
  rowObject.not-used111 = fa-control.not-used1[11]~
  rowObject.not-used112 = fa-control.not-used1[12]~
  rowObject.not-used113 = fa-control.not-used1[13]~
  rowObject.not-used21 = fa-control.not-used2[1]~
  rowObject.not-used22 = fa-control.not-used2[2]~
  rowObject.not-used23 = fa-control.not-used2[3]~
  rowObject.not-used24 = fa-control.not-used2[4]~
  rowObject.not-used25 = fa-control.not-used2[5]~
  rowObject.not-used26 = fa-control.not-used2[6]~
  rowObject.not-used27 = fa-control.not-used2[7]~
  rowObject.not-used28 = fa-control.not-used2[8]~
  rowObject.not-used29 = fa-control.not-used2[9]~
  rowObject.not-used210 = fa-control.not-used2[10]~
  rowObject.not-used211 = fa-control.not-used2[11]~
  rowObject.not-used212 = fa-control.not-used2[12]~
  rowObject.not-used213 = fa-control.not-used2[13]~
  rowObject.FA-entity-2 = fa-control-d.FA-entity
&Scoped-Define DATA-FIELD-DEFS "fa/sdoFaControl.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH fa-control NO-LOCK, ~
      EACH fa-control-d OF fa-control NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH fa-control NO-LOCK, ~
      EACH fa-control-d OF fa-control NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main fa-control fa-control-d
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main fa-control
&Scoped-define SECOND-TABLE-IN-QUERY-Query-Main fa-control-d


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      fa-control, 
      fa-control-d SCROLLING.
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
     _TblList          = "ASI.fa-control,ASI.fa-control-d OF ASI.fa-control"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ASI.fa-control.aqui-service
"aqui-service" "aqui-service" ? ? "logical" ? ? ? ? ? ? yes ? no 16.2 yes ?
     _FldNameList[2]   > ASI.fa-control.calc-mode
"calc-mode" "calc-mode" ? ? "character" ? ? ? ? ? ? yes ? no 16.2 yes ?
     _FldNameList[3]   > ASI.fa-control.Currency-cod
"Currency-cod" "Currency-cod" ? ? "character" ? ? ? ? ? ? yes ? yes 12 yes ?
     _FldNameList[4]   > ASI.fa-control.Entity-name
"Entity-name" "Entity-name" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ?
     _FldNameList[5]   > ASI.fa-control.FA-entity
"FA-entity" "FA-entity" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[6]   > ASI.fa-control.fa-gl-clear
"fa-gl-clear" "fa-gl-clear" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[7]   > ASI.fa-control.fa-gl-disp
"fa-gl-disp" "fa-gl-disp" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[8]   > ASI.fa-control.gl-installed
"gl-installed" "gl-installed" ? ? "logical" ? ? ? ? ? ? yes ? no 14.4 yes ?
     _FldNameList[9]   > ASI.fa-control.Job-no
"Job-no" "Job-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes ?
     _FldNameList[10]   > ASI.fa-control.last-ad
"last-ad" "last-ad" ? ? "integer" ? ? ? ? ? ? yes ? no 20.6 yes ?
     _FldNameList[11]   > ASI.fa-control.last-prd
"last-prd" "last-prd" ? ? "integer" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[12]   > ASI.fa-control.not-used1[1]
"not-used1[1]" "not-used11" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[13]   > ASI.fa-control.not-used1[2]
"not-used1[2]" "not-used12" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[14]   > ASI.fa-control.not-used1[3]
"not-used1[3]" "not-used13" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[15]   > ASI.fa-control.not-used1[4]
"not-used1[4]" "not-used14" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[16]   > ASI.fa-control.not-used1[5]
"not-used1[5]" "not-used15" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[17]   > ASI.fa-control.not-used1[6]
"not-used1[6]" "not-used16" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[18]   > ASI.fa-control.not-used1[7]
"not-used1[7]" "not-used17" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[19]   > ASI.fa-control.not-used1[8]
"not-used1[8]" "not-used18" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[20]   > ASI.fa-control.not-used1[9]
"not-used1[9]" "not-used19" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[21]   > ASI.fa-control.not-used1[10]
"not-used1[10]" "not-used110" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[22]   > ASI.fa-control.not-used1[11]
"not-used1[11]" "not-used111" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[23]   > ASI.fa-control.not-used1[12]
"not-used1[12]" "not-used112" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[24]   > ASI.fa-control.not-used1[13]
"not-used1[13]" "not-used113" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[25]   > ASI.fa-control.not-used2[1]
"not-used2[1]" "not-used21" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[26]   > ASI.fa-control.not-used2[2]
"not-used2[2]" "not-used22" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[27]   > ASI.fa-control.not-used2[3]
"not-used2[3]" "not-used23" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[28]   > ASI.fa-control.not-used2[4]
"not-used2[4]" "not-used24" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[29]   > ASI.fa-control.not-used2[5]
"not-used2[5]" "not-used25" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[30]   > ASI.fa-control.not-used2[6]
"not-used2[6]" "not-used26" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[31]   > ASI.fa-control.not-used2[7]
"not-used2[7]" "not-used27" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[32]   > ASI.fa-control.not-used2[8]
"not-used2[8]" "not-used28" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[33]   > ASI.fa-control.not-used2[9]
"not-used2[9]" "not-used29" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[34]   > ASI.fa-control.not-used2[10]
"not-used2[10]" "not-used210" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[35]   > ASI.fa-control.not-used2[11]
"not-used2[11]" "not-used211" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[36]   > ASI.fa-control.not-used2[12]
"not-used2[12]" "not-used212" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[37]   > ASI.fa-control.not-used2[13]
"not-used2[13]" "not-used213" ? ? "date" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[38]   > ASI.fa-control.Number-prd
"Number-prd" "Number-prd" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ?
     _FldNameList[39]   > ASI.fa-control.Prd
"Prd" "Prd" ? ? "integer" ? ? ? ? ? ? yes ? no 3.2 yes ?
     _FldNameList[40]   > ASI.fa-control.state-fed
"state-fed" "state-fed" ? ? "character" ? ? ? ? ? ? yes ? yes 31 yes ?
     _FldNameList[41]   > ASI.fa-control.tag-update
"tag-update" "tag-update" ? ? "logical" ? ? ? ? ? ? yes ? no 17.6 yes ?
     _FldNameList[42]   > ASI.fa-control.View-login
"View-login" "View-login" ? ? "logical" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[43]   > ASI.fa-control.Yr
"Yr" "Yr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ?
     _FldNameList[44]   > ASI.fa-control-d.dep-a-no
"dep-a-no" "dep-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[45]   > ASI.fa-control-d.fa-a-no
"fa-a-no" "fa-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[46]   > ASI.fa-control-d.FA-entity
"FA-entity" "FA-entity-2" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[47]   > ASI.fa-control-d.gl-dist-gno
"gl-dist-gno" "gl-dist-gno" ? ? "integer" ? ? ? ? ? ? yes ? no 11.8 yes ?
     _FldNameList[48]   > ASI.fa-control-d.gl-dist-no
"gl-dist-no" "gl-dist-no" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[49]   > ASI.fa-control-d.gl-mast-no
"gl-mast-no" "gl-mast-no" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[50]   > ASI.fa-control-d.glcode-a-no
"glcode-a-no" "glcode-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 13.2 yes ?
     _FldNameList[51]   > ASI.fa-control-d.jrnl-no
"jrnl-no" "jrnl-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[52]   > ASI.fa-control-d.loc-a-no
"loc-a-no" "loc-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[53]   > ASI.fa-control-d.Next-seq-no
"Next-seq-no" "Next-seq-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
     _FldNameList[54]   > ASI.fa-control-d.sort1-no
"sort1-no" "sort1-no" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[55]   > ASI.fa-control-d.sort2-no
"sort2-no" "sort2-no" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[56]   > ASI.fa-control-d.tag-no
"tag-no" "tag-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[57]   > ASI.fa-control-d.tax-a-no
"tax-a-no" "tax-a-no" ? ? "integer" ? ? ? ? ? ? yes ? no 12.4 yes ?
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

