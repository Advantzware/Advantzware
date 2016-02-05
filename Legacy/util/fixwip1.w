&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

&SCOPED-DEFINE KEY-PHRASE TRUE
&SCOPED-DEFINE SORTBY-PHRASE BY mch-act.op-date BY mch-act.frm BY mch-act.blank-no BY mch-act.start BY mch-act.stopp

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
&Scoped-define INTERNAL-TABLES job mch-act job-mch

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table mch-act.op-date mch-act.frm ~
mch-act.blank-no mch-act.m-code mch-act.dept mch-act.code mch-act.shift ~
mch-act.pass mch-act.hours mch-act.crew mch-act.qty mch-act.waste ~
job-mch.n-on job-mch.n-out 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table mch-act.hours mch-act.crew ~
mch-act.qty mch-act.waste job-mch.n-on job-mch.n-out 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table mch-act job-mch
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table mch-act
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table job-mch
&Scoped-define QUERY-STRING-br_table FOR EACH job WHERE ~{&KEY-PHRASE} ~
      AND job.company = cocode ~
 AND job.job-no = begin_job-no ~
 AND job.job-no2 = int(begin_job-no2) NO-LOCK, ~
      EACH mch-act WHERE mch-act.company = job.company ~
  AND mch-act.job = job.job ~
  AND mch-act.job-no = job.job-no ~
  AND mch-act.job-no2 = job.job-no2 NO-LOCK, ~
      FIRST job-mch WHERE job-mch.company = mch-act.company ~
  AND job-mch.job = mch-act.job ~
  AND job-mch.job-no = mch-act.job-no ~
  AND job-mch.job-no2 = mch-act.job-no2 ~
  AND job-mch.frm = mch-act.frm ~
  AND job-mch.blank-no = mch-act.blank-no ~
  AND job-mch.m-code = mch-act.m-code ~
  AND job-mch.pass = mch-act.pass NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH job WHERE ~{&KEY-PHRASE} ~
      AND job.company = cocode ~
 AND job.job-no = begin_job-no ~
 AND job.job-no2 = int(begin_job-no2) NO-LOCK, ~
      EACH mch-act WHERE mch-act.company = job.company ~
  AND mch-act.job = job.job ~
  AND mch-act.job-no = job.job-no ~
  AND mch-act.job-no2 = job.job-no2 NO-LOCK, ~
      FIRST job-mch WHERE job-mch.company = mch-act.company ~
  AND job-mch.job = mch-act.job ~
  AND job-mch.job-no = mch-act.job-no ~
  AND job-mch.job-no2 = mch-act.job-no2 ~
  AND job-mch.frm = mch-act.frm ~
  AND job-mch.blank-no = mch-act.blank-no ~
  AND job-mch.m-code = mch-act.m-code ~
  AND job-mch.pass = mch-act.pass NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table job mch-act job-mch
&Scoped-define FIRST-TABLE-IN-QUERY-br_table job
&Scoped-define SECOND-TABLE-IN-QUERY-br_table mch-act
&Scoped-define THIRD-TABLE-IN-QUERY-br_table job-mch


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_job-no begin_job-no2 br_table 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 

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
company||y|asi.eb.company
Carrier||y|asi.eb.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
DEFINE VARIABLE begin_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2 AS CHARACTER FORMAT "-99":U INITIAL "00" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      job, 
      mch-act, 
      job-mch SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      mch-act.op-date COLUMN-LABEL "Date" FORMAT "99/99/99":U WIDTH 13
      mch-act.frm COLUMN-LABEL "Form#" FORMAT ">>9":U WIDTH 7
      mch-act.blank-no COLUMN-LABEL "Blk#" FORMAT ">>9":U WIDTH 6
      mch-act.m-code COLUMN-LABEL "Machine" FORMAT "x(6)":U WIDTH 10
      mch-act.dept FORMAT "x(2)":U WIDTH 6
      mch-act.code FORMAT "x(5)":U WIDTH 7
      mch-act.shift FORMAT ">9":U
      mch-act.pass FORMAT ">>9":U WIDTH 6
      mch-act.hours FORMAT ">>9.99-":U WIDTH 13
      mch-act.crew COLUMN-LABEL "Crew" FORMAT ">>9":U WIDTH 7
      mch-act.qty FORMAT "->>>,>>>,>>9":U WIDTH 18
      mch-act.waste FORMAT "->>>,>>>,>>9":U WIDTH 18
      job-mch.n-on COLUMN-LABEL "#On" FORMAT ">>9":U WIDTH 5
      job-mch.n-out COLUMN-LABEL "#Out" FORMAT ">>9":U
  ENABLE
      mch-act.hours
      mch-act.crew
      mch-act.qty
      mch-act.waste HELP "Waste"
      job-mch.n-on
      job-mch.n-out HELP "Enter number of net sheets on the board fed into this machine"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-COLUMN-SCROLLING SEPARATORS SIZE 149 BY 16.67
         BGCOLOR 8 FONT 6
         TITLE BGCOLOR 8?.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     begin_job-no AT ROW 1 COL 15 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job-no2 AT ROW 1 COL 29 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     br_table AT ROW 2.19 COL 1
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
         HEIGHT             = 18.91
         WIDTH              = 149.4.
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
/* BROWSE-TAB br_table begin_job-no2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       begin_job-no:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
       begin_job-no2:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "asi.job,asi.mch-act WHERE asi.job ...,asi.job-mch WHERE asi.mch-act ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",, FIRST"
     _Where[1]         = "asi.job.company = cocode
 AND asi.job.job-no = begin_job-no
 AND asi.job.job-no2 = int(begin_job-no2)"
     _JoinCode[2]      = "asi.mch-act.company = asi.job.company
  AND asi.mch-act.job = asi.job.job
  AND asi.mch-act.job-no = asi.job.job-no
  AND asi.mch-act.job-no2 = asi.job.job-no2"
     _JoinCode[3]      = "asi.job-mch.company = asi.mch-act.company
  AND asi.job-mch.job = asi.mch-act.job
  AND asi.job-mch.job-no = asi.mch-act.job-no
  AND asi.job-mch.job-no2 = asi.mch-act.job-no2
  AND asi.job-mch.frm = asi.mch-act.frm
  AND asi.job-mch.blank-no = asi.mch-act.blank-no
  AND asi.job-mch.m-code = asi.mch-act.m-code
  AND asi.job-mch.pass = asi.mch-act.pass"
     _FldNameList[1]   > asi.mch-act.op-date
"mch-act.op-date" "Date" "99/99/99" "date" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[2]   > asi.mch-act.frm
"mch-act.frm" "Form#" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > asi.mch-act.blank-no
"mch-act.blank-no" "Blk#" ">>9" "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[4]   > asi.mch-act.m-code
"mch-act.m-code" "Machine" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[5]   > asi.mch-act.dept
"mch-act.dept" ? ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[6]   > asi.mch-act.code
"mch-act.code" ? ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[7]   = asi.mch-act.shift
     _FldNameList[8]   > asi.mch-act.pass
"mch-act.pass" ? ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[9]   > asi.mch-act.hours
"mch-act.hours" ? ? "decimal" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[10]   > asi.mch-act.crew
"mch-act.crew" "Crew" ">>9" "decimal" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[11]   > asi.mch-act.qty
"mch-act.qty" ? "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? yes ? no no "18" yes no no "U" "" ""
     _FldNameList[12]   > asi.mch-act.waste
"mch-act.waste" ? "->>>,>>>,>>9" "integer" ? ? ? ? ? ? yes "Waste" no no "18" yes no no "U" "" ""
     _FldNameList[13]   > asi.job-mch.n-on
"job-mch.n-on" "#On" ">>9" "integer" ? ? ? ? ? ? yes ? no no "5" yes no no "U" "" ""
     _FldNameList[14]   > asi.job-mch.n-out
"job-mch.n-out" "#Out" ">>9" "integer" ? ? ? ? ? ? yes "Enter number of net sheets on the board fed into this machine" no no ? yes no no "U" "" ""
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

&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no B-table-Win
ON LEAVE OF begin_job-no IN FRAME F-Main /* Job# */
DO:
  IF LASTKEY NE -1 THEN RUN get-job.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 B-table-Win
ON LEAVE OF begin_job-no2 IN FRAME F-Main
DO:
  IF LASTKEY NE -1 THEN RUN get-job.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
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


&Scoped-define SELF-NAME job-mch.n-on
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job B-table-Win 
PROCEDURE get-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    begin_job-no:SCREEN-VALUE = 
        FILL(" ",6 - LENGTH(begin_job-no:SCREEN-VALUE)) + begin_job-no:SCREEN-VALUE.

    ASSIGN
     begin_job-no
     begin_job-no2.

    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
          AND job.job-no  EQ begin_job-no
          AND job.job-no2 EQ INT(begin_job-no2)
          AND CAN-FIND(FIRST mch-act
                       WHERE mch-act.company EQ job.company
                         AND mch-act.job     EQ job.job
                         AND mch-act.job-no  EQ job.job-no
                         AND mch-act.job-no2 EQ job.job-no2)
        NO-ERROR.

    IF AVAIL job THEN RUN dispatch ("open-query").

    ELSE
    IF FOCUS:NAME EQ "begin_job-no2" THEN DO:
      MESSAGE "Job WIP does not exist..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO begin_job-no.
      RETURN NO-APPLY.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF CAN-FIND(FIRST mach
              WHERE mach.company EQ job-mch.company
                AND mach.m-code  EQ job-mch.m-code
                AND CAN-DO("A,P,B",mach.p-type)) THEN
    ASSIGN
     job-mch.n-on  = 1
     job-mch.n-out = 1.

  ELSE DO:
    ld = job-mch.n-on / job-mch.n-out.

    {sys/inc/roundup.i ld}

    job-mch.n-on = ld * job-mch.n-out.
  END.

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
  FOR EACH job NO-LOCK
      WHERE job.company EQ cocode
        AND job.job-no  EQ begin_job-no
        AND job.job-no2 EQ INT(begin_job-no2),

      EACH mch-act NO-LOCK
      WHERE mch-act.company EQ job.company
        AND mch-act.job     EQ job.job
        AND mch-act.job-no  EQ job.job-no
        AND mch-act.job-no2 EQ job.job-no2
      
      TRANSACTION:

    FIND FIRST job-mch
        WHERE job-mch.company  EQ mch-act.company
          AND job-mch.job      EQ mch-act.job
          AND job-mch.job-no   EQ mch-act.job-no
          AND job-mch.job-no2  EQ mch-act.job-no2
          AND job-mch.frm      EQ mch-act.frm
          AND job-mch.blank-no EQ mch-act.blank-no
          AND job-mch.m-code   EQ mch-act.m-code
          AND job-mch.pass     EQ mch-act.pass
        NO-ERROR.

    IF NOT AVAIL job-mch THEN DO:
      CREATE job-mch.
      BUFFER-COPY mch-act EXCEPT rec_key TO job-mch.
    END.

    IF job-mch.n-out EQ 0 THEN job-mch.n-out = 1.
    IF job-mch.n-on  EQ 0 THEN job-mch.n-on  = 1.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch ("display-fields").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "eb" "company"}
  {src/adm/template/sndkycas.i "Carrier" "eb" "Carrier"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "job"}
  {src/adm/template/snd-list.i "mch-act"}
  {src/adm/template/snd-list.i "job-mch"}

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

