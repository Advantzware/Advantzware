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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR lv-size-recid AS RECID NO-UNDO.
DEF VAR li-next-line# LIKE stack-size.line# NO-UNDO.
DEF TEMP-TABLE tmp-stack-size NO-UNDO LIKE stack-size.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES stack-size

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table stack-size.line# stack-size.vals 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table stack-size.line# ~
stack-size.vals 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table stack-size
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table stack-size
&Scoped-define QUERY-STRING-br_table FOR EACH stack-size WHERE stack-size.company = item.company ~
  AND stack-size.loc = item.loc ~
  AND stack-size.pallet = item.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH stack-size WHERE stack-size.company = item.company ~
  AND stack-size.loc = item.loc ~
  AND stack-size.pallet = item.i-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table stack-size
&Scoped-define FIRST-TABLE-IN-QUERY-br_table stack-size


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
company||y|ASI.stack-size.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).

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
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      stack-size SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      stack-size.line# FORMAT ">>>>9":U
      stack-size.vals COLUMN-LABEL "00000000011111111112222222222333333333344444444445555555555666666666677777!12345678901234567890123456789012345678901234567890123456789012345678901234" FORMAT "x(74)":U
  ENABLE
      stack-size.line#
      stack-size.vals
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 116 BY 14.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.71 COL 1
     "Width" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 2
          FONT 6
     "L e n g t h" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1 COL 45
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ASI.item
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 15.43
         WIDTH              = 141.2.
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
/* BROWSE-TAB br_table TEXT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.stack-size WHERE ASI.item <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "ASI.stack-size.company = ASI.item.company
  AND ASI.stack-size.loc = ASI.item.loc
  AND ASI.stack-size.pallet = ASI.item.i-no"
     _FldNameList[1]   > ASI.stack-size.line#
"stack-size.line#" ? ">>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.stack-size.vals
"stack-size.vals" "00000000011111111112222222222333333333344444444445555555555666666666677777!12345678901234567890123456789012345678901234567890123456789012345678901234" "x(74)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
   def var phandle as widget-handle no-undo.
   def var char-hdl as cha no-undo.   
   RUN get-link-handle IN adm-broker-hdl
      (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
   phandle = WIDGET-HANDLE(char-hdl).
   
   RUN new-state in phandle ('update-begin':U).

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
/*   {src/adm/template/brsleave.i}  same but update like add 
                                    when user press return key                
                                    info is not display until re-open
*/
    {brsleave.i} 
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


&Scoped-define SELF-NAME stack-size.line#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL stack-size.line# br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF stack-size.line# IN BROWSE br_table /* Line# */
DO:
  IF adm-new-record AND li-next-line# NE 0 THEN DO:
    {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(li-next-line#).
    li-next-line# = 0.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-line# B-table-Win 
PROCEDURE get-next-line# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-stack-size FOR stack-size.
  DEF BUFFER b-b-stack-size FOR stack-size.
      
      
  li-next-line# = 0.
  FOR EACH b-stack-size NO-LOCK
      WHERE b-stack-size.company EQ item.company
        AND b-stack-size.loc     EQ item.loc
        AND b-stack-size.pallet  EQ item.i-no
        AND (NOT AVAIL stack-size  OR
             NOT adm-adding-record OR
             ROWID(b-stack-size) NE ROWID(stack-size))
        AND NOT CAN-FIND(FIRST b-b-stack-size
                         WHERE b-b-stack-size.company EQ b-stack-size.company
                           AND b-b-stack-size.loc     EQ b-stack-size.loc
                           AND b-b-stack-size.pallet  EQ b-stack-size.pallet
                           AND b-b-stack-size.line#   EQ b-stack-size.line# + 1)
      BY b-stack-size.line#:

    li-next-line# = b-stack-size.line#.
    LEAVE.
  END.
  li-next-line# = li-next-line# + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel B-table-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-answer AS LOG NO-UNDO.
   def var chFile as cha no-undo.
   def var ll-ok as log no-undo.
   DEF VAR chExcelApplication   AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorkBook           AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorksheet          AS COM-HANDLE   NO-UNDO.
   DEF VAR viDownCount AS INT NO-UNDO.
   DEF VAR viAcrossCount AS INT NO-UNDO.
   DEF VAR viCount AS INT NO-UNDO.
   DEF VAR viCount2 AS INT NO-UNDO.
   DEF VAR viRowCount AS INT INIT 6 NO-UNDO.
   DEF VAR viColCount AS INT NO-UNDO.
   DEF VAR viColLetter AS CHAR NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
   
      MESSAGE "Override Pattern Codes?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lv-answer.
     
      IF lv-answer THEN
      DO:
         system-dialog get-file chFile 
                       title "Select File to Import"
                       filters "Excel File (*.xls) " "*.xls"
                       initial-dir "c:\"
                       MUST-EXIST
                       USE-FILENAME
                       UPDATE ll-ok.
     
         IF ll-ok THEN
         DO:
            IF LENGTH(chFile) LT 4 OR
               SUBSTR(chFile,LENGTH(chFile) - 3) NE ".xls" THEN
            DO:
               MESSAGE "Invalid File.  Must Choose Excel (.xls) File."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                LEAVE.
            END.
     
            SESSION:SET-WAIT-STATE ("general").
     
            EMPTY TEMP-TABLE tmp-stack-size.
     
            /* Initialize Excel. */
            CREATE "Excel.Application" chExcelApplication NO-ERROR.
     
            /* Check if Excel got initialized. */
            IF not (valid-handle (chExcelApplication)) THEN
               DO:
                  MESSAGE "Unable to Start Excel." VIEW-AS ALERT-BOX ERROR.
                  RETURN ERROR.
               END.
     
            /* Open our Excel File. */  
            chExcelApplication:Visible = FALSE.
            chWorkbook = chExcelApplication:Workbooks:OPEN(chfile) no-error.
     
            /* Do not display Excel error messages. */
            chExcelApplication:DisplayAlerts = false  no-error.
     
            /* Go to the Active Sheet. */
            chWorkbook:WorkSheets(1):Activate no-error.

            ASSIGN
               chWorkSheet = chExcelApplication:Sheets:item(1)
               viDownCount = chWorkSheet:Range("C3"):value
               viAcrossCount = chWorkSheet:Range("D3"):value .
     
           ASSIGN viDownCount = viDownCount + 1 .

            DO viCount = viDownCount TO 1 BY -1:
     
               CREATE tmp-stack-size.
               assign tmp-stack-size.company = item.company
                      tmp-stack-size.loc =  item.loc
                      tmp-stack-size.pallet = item.i-no
                      tmp-stack-size.line# = INT(chWorkSheet:Range("C" + STRING(viRowCount)):value)
                      viColCount = 67. /*col C*/
               
               DO viCount2 = 1 TO /*viAcrossCount*/  74:
     
                  RUN incr-col-letter(INPUT viColCount, OUTPUT viColLetter).
                  
                  ASSIGN
                     tmp-stack-size.vals = tmp-stack-size.vals +
                                           IF STRING(chWorkSheet:Range(viColLetter + STRING(viRowCount)):value) EQ ? THEN ' '
                                           ELSE STRING(chWorkSheet:Range(viColLetter + STRING(viRowCount)):value)
                     viColCount = viColCount + 1.
               END.
     
               viRowCount = viRowCount + 1.
     
               RELEASE tmp-stack-size.
            END.
     
            FOR EACH stack-size WHERE
                stack-size.company = item.company AND
                stack-size.loc =  item.loc AND
                stack-size.pallet = item.i-no
                EXCLUSIVE-LOCK:
     
                DELETE stack-size.
            END.
            
            FOR EACH tmp-stack-size:
                CREATE stack-size.
                BUFFER-COPY tmp-stack-size TO stack-size.
                RELEASE stack-size.
            END.
     
            /*Free memory*/
            chWorkbook = chExcelApplication:Workbooks:CLOSE() no-error.
            
            /* Release created objects. */
            RELEASE OBJECT chWorkbook NO-ERROR.
            RELEASE OBJECT chWorkSheet NO-ERROR.
            RELEASE OBJECT chExcelApplication NO-ERROR.
     
            CLOSE QUERY br_table.
            {&OPEN-QUERY-{&BROWSE-NAME}}

            MESSAGE "Excel File Imported."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incr-col-letter B-table-Win 
PROCEDURE incr-col-letter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-int AS INT NO-UNDO.
   DEFINE OUTPUT PARAMETER op-letter AS CHAR NO-UNDO.

   IF ip-int LE 89 THEN
      op-letter = CHR(ip-int + 1).
   ELSE IF ip-int LE 115 THEN
      op-letter = "A" + CHR(ip-int - 25).
   ELSE IF ip-int LE 141 THEN
      op-letter = "B" + CHR(ip-int - 51).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-copied AS LOG NO-UNDO.
  

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN est/copystsze.w (ROWID(stack-size), OUTPUT ll-copied).

  IF NOT ll-copied THEN DO:
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN get-next-line#.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   lv-size-recid      = RECID(stack-size)
   stack-size.company = item.company
   stack-size.loc     = item.loc
   stack-size.pallet  = item.i-no.

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-adding-record THEN DO:
      RUN get-next-line#.
      stack-size.line# = li-next-line#.
      DISPLAY stack-size.line# WITH BROWSE {&browse-name}.
      li-next-line# = 0.
    END.

    ELSE stack-size.line# = INT(stack-size.line#:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
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
  {src/adm/template/sndkycas.i "company" "stack-size" "company"}

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
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "stack-size"}

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

