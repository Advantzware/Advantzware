&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mcash NO-UNDO LIKE ar-mcash
       field check-no as char.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  ar/b-mcash-.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}
  
DEF BUFFER b-ar-mcash FOR ar-mcash.
DEF BUFFER bf-reftable FOR reftable.
DEF BUFFER bf2-reftable FOR reftable.
DEF VAR v-posted AS LOG NO-UNDO.
DEF VAR v-check-no AS CHAR NO-UNDO.

&SCOPED-DEFINE browse2 ar/j-mcash.i

DEF VAR v-checkno AS CHAR FORMAT "9999999999" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mcash ar-mcash reftable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-mcash.check-no ~
tt-mcash.payer tt-mcash.check-amt tt-mcash.check-date tt-mcash.bank-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-mcash WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST ar-mcash WHERE ar-mcash.m-no       = tt-mcash.m-no NO-LOCK, ~
      FIRST reftable WHERE TRUE /* Join to ar-mcash incomplete */ ~
      AND reftable.reftable = "AR-MCASH" and       ~
reftable.company  = tt-mcash.company and ~
reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") and ~
reftable.code     = tt-mcash.rec_key and ~
reftable.code2    = tt-mcash.check-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH tt-mcash WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST ar-mcash WHERE ar-mcash.m-no       = tt-mcash.m-no NO-LOCK, ~
      FIRST reftable WHERE TRUE /* Join to ar-mcash incomplete */ ~
      AND reftable.reftable = "AR-MCASH" and       ~
reftable.company  = tt-mcash.company and ~
reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") and ~
reftable.code     = tt-mcash.rec_key and ~
reftable.code2    = tt-mcash.check-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-mcash ar-mcash reftable
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-mcash
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table ar-mcash
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table reftable


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get_checkno B-table-Win 
FUNCTION get_checkno RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-mcash
    FIELDS(tt-mcash.check-no
      tt-mcash.payer
      tt-mcash.check-amt
      tt-mcash.check-date
      tt-mcash.bank-code), 
      ar-mcash, 
      reftable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-mcash.check-no COLUMN-LABEL "Check No" FORMAT "x(10)":U
            WIDTH 14.2
      tt-mcash.payer FORMAT "x(30)":U
      tt-mcash.check-amt COLUMN-LABEL "Check Amount" FORMAT "->>,>>>,>>9.99":U
            WIDTH 19.4
      tt-mcash.check-date COLUMN-LABEL "Check Date" FORMAT "99/99/9999":U
            WIDTH 17.2
      tt-mcash.bank-code COLUMN-LABEL "Bank Code" FORMAT "x(8)":U
            WIDTH 15.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 17.86
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     RECT-4 AT ROW 19.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mcash T "?" NO-UNDO asi ar-mcash
      ADDITIONAL-FIELDS:
          field check-no as char
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:PRIVATE-DATA IN FRAME F-Main           = 
                "2".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "Temp-Tables.tt-mcash,ASI.ar-mcash WHERE Temp-Tables.tt-mcash ... ... ... ...,asi.reftable WHERE ASI.ar-mcash ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED, FIRST, FIRST"
     _JoinCode[2]      = "ASI.ar-mcash.m-no       = Temp-Tables.tt-mcash.m-no"
     _Where[3]         = "reftable.reftable = ""AR-MCASH"" and      
reftable.company  = tt-mcash.company and
reftable.loc      = STRING(tt-mcash.m-no,"">>>>>>9"") and
reftable.code     = tt-mcash.rec_key and
reftable.code2    = tt-mcash.check-no"
     _FldNameList[1]   > "_<CALC>"
"tt-mcash.check-no" "Check No" "x(10)" ? ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.tt-mcash.payer
     _FldNameList[3]   > Temp-Tables.tt-mcash.check-amt
"tt-mcash.check-amt" "Check Amount" ? "decimal" ? ? ? ? ? ? no ? no no "19.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt-mcash.check-date
"tt-mcash.check-date" "Check Date" ? "date" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt-mcash.bank-code
"tt-mcash.bank-code" "Bank Code" ? "character" ? ? ? ? ? ? no ? no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt B-table-Win 
PROCEDURE create-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN browse-order auto_find.

     IF browse-order = 1 THEN
        v-posted = NO.
     ELSE
        v-posted = YES.
   
     FOR EACH b-ar-mcash WHERE 
         b-ar-mcash.company = g_company AND
         b-ar-mcash.posted EQ v-posted AND
         b-ar-mcash.payer BEGINS auto_find
         NO-LOCK
         BY b-ar-mcash.payer:
    
         FIND FIRST bf-reftable WHERE
              bf-reftable.reftable = "AR-MCASH" AND
              bf-reftable.company  = b-ar-mcash.company AND
              bf-reftable.loc      = STRING(b-ar-mcash.m-no,">>>>>>9") AND
              bf-reftable.code     = b-ar-mcash.rec_key
              NO-LOCK NO-ERROR.

         IF AVAIL bf-reftable THEN
            v-check-no = bf-reftable.code2.
         ELSE
            v-check-no = FILL("0",10).

         FIND FIRST tt-mcash WHERE
              tt-mcash.company = b-ar-mcash.company AND
              tt-mcash.posted = b-ar-mcash.posted AND
              tt-mcash.payer = b-ar-mcash.payer AND
              tt-mcash.check-date = b-ar-mcash.check-date AND
              tt-mcash.bank-code = b-ar-mcash.bank-code AND
              tt-mcash.curr-code[1] = b-ar-mcash.curr-code[1] AND
              tt-mcash.check-no = v-check-no
              NO-ERROR.
         
         IF NOT AVAIL tt-mcash THEN
         DO:
            CREATE tt-mcash.
            BUFFER-COPY b-ar-mcash EXCEPT check-amt TO tt-mcash
               ASSIGN tt-mcash.check-no = v-check-no.
         END.
    
         tt-mcash.check-amt = tt-mcash.check-amt + b-ar-mcash.check-amt.
    
         RELEASE tt-mcash.
     END.
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
  EMPTY TEMP-TABLE tt-mcash.

  RUN create-tt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repos-to-new B-table-Win 
PROCEDURE repos-to-new :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-old-posted AS LOG NO-UNDO.
   DEFINE INPUT PARAMETER ip-old-payer AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-old-check-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER ip-old-bank-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-old-curr-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-old-check-no AS CHAR NO-UNDO.

   DEFINE INPUT PARAMETER ip-new-posted AS LOG NO-UNDO.
   DEFINE INPUT PARAMETER ip-new-payer AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-new-check-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER ip-new-bank-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-new-curr-code AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-new-check-no AS CHAR NO-UNDO.

   FOR EACH tt-mcash WHERE
        tt-mcash.company EQ cocode AND
        tt-mcash.posted EQ ip-old-posted AND
        tt-mcash.payer EQ ip-old-payer AND
        tt-mcash.check-date EQ ip-old-check-date AND
        tt-mcash.bank-code EQ ip-old-bank-code AND
        tt-mcash.curr-code[1] EQ ip-old-curr-code,
        FIRST reftable WHERE
              reftable.reftable = "AR-MCASH" AND
              reftable.company  = tt-mcash.company AND
              reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") AND
              reftable.code     = tt-mcash.rec_key AND
              reftable.code2    = ip-old-check-no
              NO-LOCK:
        LEAVE.
   END.

   IF AVAIL tt-mcash THEN
   DO:
      RUN local-open-query.

      FOR EACH tt-mcash WHERE
          tt-mcash.company EQ cocode AND
          tt-mcash.posted EQ ip-new-posted AND
          tt-mcash.payer EQ ip-new-payer AND
          tt-mcash.check-date EQ ip-new-check-date AND
          tt-mcash.bank-code EQ ip-new-bank-code AND
          tt-mcash.curr-code[1] EQ ip-new-curr-code,
          FIRST ar-mcash WHERE
                ar-mcash.company = tt-mcash.company AND
                ar-mcash.posted = tt-mcash.posted AND
                ar-mcash.payer = tt-mcash.payer AND
                ar-mcash.bank-code = tt-mcash.bank-code AND
                ar-mcash.curr-code[1] = tt-mcash.curr-code[1] AND
                ar-mcash.check-date = tt-mcash.check-date AND
                ar-mcash.m-no = tt-mcash.m-no
                NO-LOCK,
          FIRST reftable WHERE
                reftable.reftable = "AR-MCASH" AND
                reftable.company  = tt-mcash.company AND
                reftable.loc      = STRING(tt-mcash.m-no,">>>>>>9") AND
                reftable.code     = tt-mcash.rec_key AND
                reftable.code2    = ip-new-check-no
                NO-LOCK:

         REPOSITION {&browse-name} TO ROWID ROWID(tt-mcash).
         APPLY "VALUE-CHANGED" TO {&browse-name} IN FRAME {&FRAME-NAME}.
         
         LEAVE.
      END.
   END.
   ELSE
   DO:
      FIND FIRST tt-mcash NO-ERROR.

      IF AVAIL tt-mcash THEN
      DO:
         RUN local-open-query.
         FIND FIRST tt-mcash NO-ERROR.

         IF AVAIL tt-mcash THEN
         DO:
            REPOSITION {&browse-name} TO ROWID ROWID(tt-mcash).
            APPLY "VALUE-CHANGED" TO {&browse-name} IN FRAME {&FRAME-NAME}.
         END.
      END.
   END.
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
  {src/adm/template/snd-list.i "tt-mcash"}
  {src/adm/template/snd-list.i "ar-mcash"}
  {src/adm/template/snd-list.i "reftable"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get_checkno B-table-Win 
FUNCTION get_checkno RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR op-checkno AS CHAR FORMAT "9999999999" NO-UNDO.

  FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "AR-MCASH"       
        AND reftable.company  = ar-mcash.company
        AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
        AND reftable.code     = ar-mcash.rec_key NO-ERROR.
  IF AVAIL reftable 
    THEN ASSIGN op-checkno = STRING(reftable.code2,"9999999999").

  RETURN op-checkno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

