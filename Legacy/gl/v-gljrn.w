&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: gl\v-gljrn.w

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
{sys/inc/var.i new shared }

assign
 cocode = g_company
 locode = g_loc.

DEF TEMP-TABLE tt-gl-jrn NO-UNDO
    FIELD DATE AS DATE
    FIELD reverse AS LOG
    FIELD id AS CHAR
    FIELD j-no AS INT
    FIELD period AS INT.

DEF TEMP-TABLE tt-gl-jrnl NO-UNDO
    FIELD LINE AS INT
    FIELD actnum AS CHAR
    FIELD dscr AS CHAR
    FIELD tr-amt AS DEC
    FIELD id AS CHAR.

DEF VAR ll-recur AS LOG NO-UNDO.

DEF BUFFER bf-jrn FOR gl-jrn.

&SCOPED-DEFINE enable-jrn proc-enable

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES gl-jrn
&Scoped-define FIRST-EXTERNAL-TABLE gl-jrn


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gl-jrn.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS gl-jrn.tr-date 
&Scoped-define ENABLED-TABLES gl-jrn
&Scoped-define FIRST-ENABLED-TABLE gl-jrn
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS gl-jrn.journal gl-jrn.tcred gl-jrn.tr-date ~
gl-jrn.tdeb gl-jrn.period gl-jrn.tr-amt 
&Scoped-define DISPLAYED-TABLES gl-jrn
&Scoped-define FIRST-DISPLAYED-TABLE gl-jrn
&Scoped-Define DISPLAYED-OBJECTS tb_reverse tb_from-reverse cb_freq 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS gl-jrn.period tb_reverse cb_freq 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
j-no|y|y|ASI.gl-jrn.j-no
company||y|ASI.gl-jrn.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "j-no",
     Keys-Supplied = "j-no,company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb_freq AS CHARACTER FORMAT "X(256)":U 
     LABEL "Frequency" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","Daily","Weekly","Monthly","Annually","Bi-weekly","Bi-monthly","Bi-annually","Intermittently" 
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 5.71.

DEFINE VARIABLE tb_from-reverse AS LOGICAL INITIAL no 
     LABEL "Entry From Previous Reversing Entry?" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reverse AS LOGICAL INITIAL no 
     LABEL "Reverse This Entry?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     gl-jrn.journal AT ROW 1.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     gl-jrn.tcred AT ROW 1.24 COL 59 COLON-ALIGNED
          LABEL "Total Credits" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     gl-jrn.tr-date AT ROW 2.24 COL 19 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     gl-jrn.tdeb AT ROW 2.24 COL 59 COLON-ALIGNED
          LABEL "Total Debits" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     gl-jrn.period AT ROW 3.24 COL 19 COLON-ALIGNED
          LABEL "Period"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     gl-jrn.tr-amt AT ROW 3.24 COL 59 COLON-ALIGNED
          LABEL "Balance" FORMAT "->>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     tb_reverse AT ROW 4.33 COL 9
     tb_from-reverse AT ROW 5.29 COL 9
     cb_freq AT ROW 4.24 COL 59 COLON-ALIGNED HELP
          "Please enter how often this journal entry will be applied"
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.gl-jrn
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb_freq IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN gl-jrn.journal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gl-jrn.period IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX tb_from-reverse IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_reverse IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN gl-jrn.tcred IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gl-jrn.tdeb IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gl-jrn.tr-amt IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN gl-jrn.tr-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gl-jrn.tr-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gl-jrn.tr-date V-table-Win
ON LEAVE OF gl-jrn.tr-date IN FRAME F-Main /* Date */
DO:
  DEF VAR char-hdl AS cha NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
  {&methods/lValidateError.i YES}
    RUN check-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    READKEY PAUSE 0. 
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
    RUN finish-new-record IN WIDGET-HANDLE (char-hdl).
  {&methods/lValidateError.i NO}
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'j-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = gl-jrn
           &WHERE = "WHERE gl-jrn.j-no eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "gl-jrn"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gl-jrn"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-balanced V-table-Win 
PROCEDURE check-balanced :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAM op-balanced AS LOG NO-UNDO.

 IF NOT AVAIL gl-jrn THEN DO:
     op-balanced = YES.
     RETURN.
 END.

 op-balanced = gl-jrn.tr-amt = 0.

 IF NOT op-balanced THEN DO:
    MESSAGE "Debit and Credit do not balance. Do you want to continue?"
        VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
    op-balanced = IF ll-ans THEN ll-ans ELSE NO.         
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date V-table-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR choice AS LOG NO-UNDO.
  {&methods/lValidateError.i YES}
  IF NOT ll-recur THEN
  DO WITH FRAME {&FRAME-NAME}:
         find first period where period.company  =  g_company  and
                                period.pst  <= date(gl-jrn.tr-date:SCREEN-VALUE) and
                                period.pend >= date(gl-jrn.tr-date:SCREEN-VALUE)
                                no-lock no-error.
         IF AVAIL period AND period.pstat then do:
            assign gl-jrn.period:SCREEN-VALUE = string(period.pnum).          
         end.
         else do:
            message "Period is not OPEN !" VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
         end.
         choice = not (FOCUS:NAME NE "tr-date" AND avail gl-jrn and period.pnum ne gl-jrn.period).
         if not choice then DO:            
            message "Reset Journal Entries' period to new Period ?"
                   VIEW-AS ALERT-BOX BUTTON YES-NO update choice.
         end.
         if not choice then do:
           ASSIGN
            gl-jrn.tr-date:SCREEN-VALUE = STRING(gl-jrn.tr-date)
            gl-jrn.period:SCREEN-VALUE  = STRING(gl-jrn.period).
           RETURN ERROR.
         END.
  END.
  {&methods/lValidateError.i NO}

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date-excel V-table-Win 
PROCEDURE check-date-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER op-period AS INT NO-UNDO.

   DEF VAR choice AS LOG NO-UNDO.
   {&methods/lValidateError.i YES}
   IF NOT ll-recur THEN
   DO WITH FRAME {&FRAME-NAME}:
      find first period where period.company  =  g_company  and
           period.pst  <= ip-date and
           period.pend >= ip-date
           no-lock no-error.

      IF AVAIL period AND period.pstat then do:
         assign op-period = period.pnum.          
      end.
      else do:
         message "Period is not OPEN !" VIEW-AS ALERT-BOX ERROR.
         RETURN ERROR.
      end.
   END.
   {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE cb_freq tb_reverse.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-excel V-table-Win 
PROCEDURE import-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-answer AS LOG NO-UNDO.
   def var chFile as cha no-undo.
   def var ll-ok as log no-undo.
   DEF VAR chExcelApplication AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorkBook AS COM-HANDLE   NO-UNDO.
   DEF VAR chWorksheet AS COM-HANDLE   NO-UNDO.
   DEF VAR viRowCount AS INT INIT 1 NO-UNDO.
   DEF VAR v-line AS INT INIT 1 NO-UNDO.
   DEF VAR valid-flag AS LOG INIT YES NO-UNDO.
   def var char-hdl as cha no-undo.
   DEF VAR v-rowid AS ROWID NO-UNDO.
   DEF VAR v-id AS CHAR NO-UNDO.
   DEF VAR i-actnum AS INTE NO-UNDO.
   {&methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:

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

         EMPTY TEMP-TABLE tt-gl-jrn.
         EMPTY TEMP-TABLE tt-gl-jrnl.

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
            chWorkSheet = chExcelApplication:Sheets:item(1).

         REPEAT:
            IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE EQ ? OR
               NOT valid-flag THEN LEAVE.

            IF chWorkSheet:Range("B" + STRING(viRowCount)):VALUE = "H" THEN /*Header*/
            DO:
               CREATE tt-gl-jrn.
               tt-gl-jrn.DATE = chWorkSheet:Range("C" + STRING(viRowCount)):value NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
               DO:
                  MESSAGE "Invalid Header Date, in row " + STRING(viRowCount) + "."
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  valid-flag = NO.
                  LEAVE.
               END.

               RUN check-date-excel(INPUT tt-gl-jrn.DATE, OUTPUT tt-gl-jrn.period) NO-ERROR.

               IF ERROR-STATUS:ERROR THEN
               DO:
                  valid-flag = NO.
                  LEAVE.
               END.

               tt-gl-jrn.reverse = chWorkSheet:Range("D" + STRING(viRowCount)):VALUE NO-ERROR.

               IF ERROR-STATUS:ERROR THEN
               DO:
                  MESSAGE "Invalid Reverse Entry Value, in row " + STRING(viRowCount) + "."
                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                  valid-flag = NO.
                  LEAVE.
               END.

               IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE NE ? THEN
                  tt-gl-jrn.id = chWorkSheet:Range("A" + STRING(viRowCount)):VALUE.
               ELSE
               DO:
                   MESSAGE "Invalid Unique Identifier, in row " + STRING(viRowCount) + "."
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
                   valid-flag = NO.
                   LEAVE.
               END.

               RELEASE tt-gl-jrn.
               viRowCount = viRowCount + 1.
            END.
            ELSE IF chWorkSheet:Range("B" + STRING(viRowCount)):VALUE = "T" THEN /*Trailer*/
            DO:
               CREATE tt-gl-jrnl.
               ASSIGN tt-gl-jrnl.actnum = IF chWorkSheet:Range("C" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("C" + STRING(viRowCount)):VALUE
                                          ELSE ""
                      tt-gl-jrnl.dscr   = IF chWorkSheet:Range("D" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("D" + STRING(viRowCount)):VALUE
                                          ELSE ""
                      tt-gl-jrnl.tr-amt = IF chWorkSheet:Range("E" + STRING(viRowCount)):VALUE NE ? THEN
                                             chWorkSheet:Range("E" + STRING(viRowCount)):VALUE
                                          ELSE 0.

              /* to format actnum to integer when there are no dashes */
              IF length(trim(tt-gl-jrnl.actnum)) > 0 AND index(tt-gl-jrnl.actnum,"-") = 0 THEN DO:
                  ASSIGN i-actnum = INTE(tt-gl-jrnl.actnum) NO-ERROR.
                  IF ERROR-STATUS:ERROR = FALSE THEN
                     ASSIGN tt-gl-jrnl.actnum = string(i-actnum).
              END.

               IF NOT CAN-FIND(FIRST account WHERE
                  account.company EQ g_company AND
                  account.type    NE "T" AND
                  account.actnum  EQ tt-gl-jrnl.actnum) THEN
                  DO:
                     MESSAGE "Invalid Account #, in row " + STRING(viRowCount) + " - Company: " g_company + " Acct Num: " + tt-gl-jrnl.actnum + "."
                         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                     valid-flag = NO.
                     LEAVE.
                  END.

               IF chWorkSheet:Range("A" + STRING(viRowCount)):VALUE NE ? THEN
               DO:
                  v-id = chWorkSheet:Range("A" + STRING(viRowCount)):VALUE.

                  IF NOT CAN-FIND(FIRST tt-gl-jrnl WHERE
                     tt-gl-jrnl.id = v-id) THEN
                     v-line = 1.

                  ASSIGN
                     tt-gl-jrnl.id = v-id
                     tt-gl-jrnl.LINE = v-line.
               END.
               ELSE
               DO:
                  MESSAGE "Invalid Unique Identifier , in row " + STRING(viRowCount)
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  valid-flag = NO.
                  LEAVE.
               END.

               RELEASE tt-gl-jrnl.

               ASSIGN
                  v-line = v-line + 1
                  viRowCount = viRowCount + 1.
            END.
            ELSE
            DO:
               MESSAGE "Invalid Header/Trailer Value, in row " + STRING(viRowCount) + "."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
               valid-flag = NO.
               LEAVE.
            END.
         END.

         /*Free memory*/
         chWorkbook = chExcelApplication:Workbooks:CLOSE() no-error.
         RELEASE OBJECT chWorkbook NO-ERROR.
         RELEASE OBJECT chWorkSheet NO-ERROR.
         RELEASE OBJECT chExcelApplication NO-ERROR.

         IF valid-flag THEN
         DO:
            /*create records*/

            FOR EACH tt-gl-jrn:

               CREATE gl-jrn.
               ASSIGN 
                  gl-jrn.reverse = tt-gl-jrn.reverse
                  gl-jrn.tr-date = tt-gl-jrn.DATE
                  gl-jrn.company = g_company
                  gl-jrn.period  = tt-gl-jrn.period
                  gl-jrn.recur   = ll-recur
                  gl-jrn.from-reverse = NO
                  v-rowid = ROWID(gl-jrn)  
                  tt-gl-jrn.j-no = gl-jrn.j-no.
            END.

            FOR EACH tt-gl-jrnl,
                FIRST tt-gl-jrn WHERE
                      tt-gl-jrn.id = tt-gl-jrnl.id,
                FIRST gl-jrn WHERE
                      gl-jrn.j-no EQ tt-gl-jrn.j-no
                      EXCLUSIVE-LOCK:

                CREATE gl-jrnl.
                ASSIGN gl-jrnl.j-no = tt-gl-jrn.j-no
                       gl-jrnl.line = tt-gl-jrnl.LINE
                       gl-jrnl.actnum = tt-gl-jrnl.actnum
                       gl-jrnl.dscr = tt-gl-jrnl.dscr
                       gl-jrnl.tr-amt = tt-gl-jrnl.tr-amt.

                IF gl-jrnl.tr-amt GT 0 THEN
                   gl-jrn.tdeb = gl-jrn.tdeb  + tt-gl-jrnl.tr-amt.
                ELSE
                   gl-jrn.tcred = gl-jrn.tcred + tt-gl-jrnl.tr-amt.

                gl-jrn.tr-amt = gl-jrn.tdeb + gl-jrn.tcred.

                RELEASE gl-jrnl.
            END.

            MESSAGE "Excel File Imported." SKIP
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).

            IF v-rowid NE ? THEN
               RUN repos-query in WIDGET-HANDLE(char-hdl) (INPUT v-rowid).
         END.
      END.
   END.
   {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
  gl-jrn.freq    = cb_freq
  gl-jrn.reverse = tb_reverse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
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
   gl-jrn.company      = g_company
   gl-jrn.period       = g_period
   gl-jrn.recur        = ll-recur
   gl-jrn.reverse      = NO
   gl-jrn.from-reverse = NO.

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-adding-record THEN cb_freq:SCREEN-VALUE = "".
    tb_reverse:SCREEN-VALUE = STRING(gl-jrn.reverse).
  END.

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    IF AVAIL gl-jrn THEN
      ASSIGN
       cb_freq         = gl-jrn.freq
       tb_reverse      = gl-jrn.reverse
       tb_from-reverse = gl-jrn.from-reverse.
    ELSE
      ASSIGN
       cb_freq         = ""
       tb_reverse      = NO
       tb_from-reverse = NO.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"recurring-source",OUTPUT char-hdl).

  ll-recur = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-recur THEN 
      ASSIGN
       gl-jrn.tr-date:VISIBLE = NO
       gl-jrn.period:VISIBLE  = NO
       cb_freq:HIDDEN         = NO.
    ELSE
      ASSIGN
       cb_freq:SENSITIVE = NO
       cb_freq:HIDDEN    = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ll-new-record = adm-new-record.

  RUN check-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN disable-fields.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST gl-jrnl OF gl-jrn NO-LOCK NO-ERROR.
  IF NOT AVAIL gl-jrnl THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"adding-line-target",OUTPUT char-hdl).
     RUN auto-line-add IN WIDGET-HANDLE(char-hdl).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll AS LOG NO-UNDO.   
  DEF VAR char-hdl AS cha NO-UNDO.

  IF AVAIL gl-jrn THEN
  DO:
     IF NOT adm-new-record AND gl-jrn.posted THEN DO:
        MESSAGE "This invoice has been posted. No changes are allowed!"           
             VIEW-AS ALERT-BOX ERROR.
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
        RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
     END.
    /* ticket 15382  */
     /*IF USERID("nosweat") NE gl-jrn.user-id AND
        gl-jrn.user-id NE ""                THEN DO:
        MESSAGE "This Journal was created by User: " +
               TRIM(gl-jrn.user-id) + ", do you wish update anyway?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           UPDATE ll.
        IF NOT ll THEN DO:
           RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
           RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
       END.
     END.*/
  END.

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-recur THEN ENABLE cb_freq.
    IF NOT AVAIL gl-jrn OR NOT gl-jrn.from-reverse THEN ENABLE tb_reverse.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query V-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND CURRENT gl-jrn NO-LOCK.
  RUN dispatch ('display-fields').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "j-no" "gl-jrn" "j-no"}
  {src/adm/template/sndkycas.i "company" "gl-jrn" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "gl-jrn"}

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

