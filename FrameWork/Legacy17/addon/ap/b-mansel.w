&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

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

&SCOPED-DEFINE dataGridInclude dataGrid\addon\ap\b-mansel.i
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared }

assign
 cocode = g_company
 locode = g_loc.

DEF VAR ll-inv-displayed AS LOG NO-UNDO.

{ap/l-apinv1.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ap-chk
&Scoped-define FIRST-EXTERNAL-TABLE ap-chk


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-chk.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ap-sel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ap-sel.inv-no ap-sel.due-date ~
ap-sel.inv-bal ap-sel.disc-amt ap-sel.amt-paid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ap-sel.inv-no ~
ap-sel.disc-amt ap-sel.amt-paid 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ap-sel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ap-sel
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ap-sel WHERE ap-sel.company = ap-chk.company ~
  AND ap-sel.vend-no = ap-chk.vend-no ~
  AND ap-sel.man-check = ap-chk.man-check ~
  AND ap-sel.bank-code = ap-chk.bank-code ~
  AND ap-sel.check-no = ap-chk.check-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ap-sel WHERE ap-sel.company = ap-chk.company ~
  AND ap-sel.vend-no = ap-chk.vend-no ~
  AND ap-sel.man-check = ap-chk.man-check ~
  AND ap-sel.bank-code = ap-chk.bank-code ~
  AND ap-sel.check-no = ap-chk.check-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ap-sel
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ap-sel


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
      ap-sel
    FIELDS(ap-sel.inv-no
      ap-sel.due-date
      ap-sel.inv-bal
      ap-sel.disc-amt
      ap-sel.amt-paid) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ap-sel.inv-no COLUMN-LABEL "Invoice#" FORMAT "x(12)":U
      ap-sel.due-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
      ap-sel.inv-bal COLUMN-LABEL "Balance Due" FORMAT "->>,>>>,>>9.99":U
      ap-sel.disc-amt COLUMN-LABEL "Discount" FORMAT "->>,>>9.99":U
      ap-sel.amt-paid COLUMN-LABEL "Payment" FORMAT "->>,>>>,>>9.99":U
  ENABLE
      ap-sel.inv-no
      ap-sel.disc-amt
      ap-sel.amt-paid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.29
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.52 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 10.52 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.52 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.52 COL 2
     RECT-4 AT ROW 10.29 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.ap-chk
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB Browser-Table TEXT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ap-sel WHERE ASI.ap-chk ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _JoinCode[1]      = "ASI.ap-sel.company = ASI.ap-chk.company
  AND ASI.ap-sel.vend-no = ASI.ap-chk.vend-no
  AND ASI.ap-sel.man-check = ASI.ap-chk.man-check
  AND ASI.ap-sel.bank-code = ASI.ap-chk.bank-code
  AND ASI.ap-sel.check-no = ASI.ap-chk.check-no"
     _FldNameList[1]   > ASI.ap-sel.inv-no
"ap-sel.inv-no" "Invoice#" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ap-sel.due-date
"ap-sel.due-date" "Due Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ap-sel.inv-bal
"ap-sel.inv-bal" "Balance Due" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ap-sel.disc-amt
"ap-sel.disc-amt" "Discount" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ap-sel.amt-paid
"ap-sel.amt-paid" "Payment" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.
    CASE FOCUS:NAME:
        WHEN "inv-no" THEN DO:
            FOR EACH w-ap-sel WHERE ap-chk-rowid NE ROWID(ap-chk):
              DELETE w-ap-sel.
            END.
            RUN ap/l-apinv1.w (g_company, ap-chk.vend-no, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
            FOR EACH w-ap-sel:
              ap-chk-rowid = ROWID(ap-chk).
            END.
            /*IF lk-recid <> ? THEN RUN display-invoice (lk-recid).*/
        END.
    END CASE.

    RUN check-workfile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   /*{src/adm/template/brsleave.i}*/
   {brsleave.i}
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

  ll-inv-displayed = NO.

  IF CAN-FIND(FIRST w-ap-sel WHERE w-ap-sel.ap-chk-rowid EQ ROWID(ap-chk)) THEN
    RUN check-workfile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-sel.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-sel.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-sel.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
    IF LASTKEY = -1 THEN RETURN.

    FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = ap-chk.vend-no
                        AND ap-inv.inv-no = (ap-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND ap-inv.posted = YES
                        AND ap-inv.due <> 0 NO-LOCK NO-ERROR.
   IF NOT AVAIL ap-inv  THEN DO:
      MESSAGE "Invalid AP Invoice. Try Help. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF AVAIL ap-inv AND NOT ll-inv-displayed THEN RUN display-invoice (RECID(ap-inv)).

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
  {src/adm/template/row-list.i "ap-chk"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-chk"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add B-table-Win 
PROCEDURE auto-add :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  RUN auto-add IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-add-tt B-table-Win 
PROCEDURE auto-add-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF CAN-FIND(FIRST w-ap-sel) THEN RUN auto-add.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-save B-table-Win 
PROCEDURE auto-save :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl
                       (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
  RUN auto-save IN phandle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-workfile B-table-Win 
PROCEDURE check-workfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ap-sel FOR ap-sel.


  RELEASE ap-inv.
  FIND FIRST w-ap-sel NO-ERROR.
  IF AVAIL w-ap-sel THEN
  FIND ap-inv
      WHERE ROWID(ap-inv) EQ w-ap-sel.ap-inv-rowid
        AND NOT CAN-FIND(FIRST b-ap-sel
                         WHERE b-ap-sel.company EQ ap-inv.company
                           AND b-ap-sel.vend-no EQ ap-inv.vend-no
                           AND b-ap-sel.inv-no  EQ ap-inv.inv-no
                           AND ROWID(b-ap-sel)  NE ROWID(ap-sel))
      NO-LOCK NO-ERROR.
  IF AVAIL ap-inv THEN DO:
    RUN display-invoice (RECID(ap-inv)).
    RUN auto-save.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-tt B-table-Win 
PROCEDURE delete-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL ap-sel THEN DO:
    FIND FIRST w-ap-sel WHERE w-ap-sel.ap-chk-rowid EQ ROWID(ap-chk) NO-ERROR.
    IF AVAIL w-ap-sel THEN DELETE w-ap-sel.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-invoice B-table-Win 
PROCEDURE display-invoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  FIND ap-inv WHERE RECID(ap-inv) = ip-recid NO-LOCK NO-ERROR.
  IF NOT AVAIL ap-inv THEN RETURN.

  ASSIGN ap-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name} = ap-inv.inv-no
         ap-sel.due-date:SCREEN-VALUE = string(ap-inv.due-date,"99/99/9999")
         ap-sel.inv-bal:SCREEN-VALUE = STRING(ap-inv.due)
         ap-sel.amt-paid:SCREEN-VALUE = STRING(ap-inv.due)
         .

  IF (ap-sel.pre-date - ap-inv.inv-date) LE ap-inv.disc-days THEN DO:
     ap-sel.disc-amt:SCREEN-VALUE = STRING(round(ap-inv.disc-% * ap-inv.net / 100,2)).
  END.

  ll-inv-displayed = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld-pre-paid AS DEC NO-UNDO.
  DEF VAR ld-pre-disc AS DEC NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN ld-pre-paid = ap-sel.amt-paid
         ld-pre-disc = ap-sel.disc-amt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN update-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ap-sel.due-date = date(ap-sel.due-date:SCREEN-VALUE IN BROWSE {&browse-name})
         ap-sel.inv-bal  = DEC(ap-sel.inv-bal:SCREEN-VALUE IN BROWSE {&browse-name})
         .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN auto-add-tt.

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
  FIND FIRST bank WHERE bank.company = g_company
                    AND bank.bank-code = ap-chk.bank-code NO-LOCK NO-ERROR.
  ASSIGN ap-sel.company = g_company
         ap-sel.vend-no = ap-chk.vend-no
         ap-sel.check-no = ap-chk.check-no
         ap-sel.bank-code = ap-chk.bank-code
         ap-sel.man-check = YES
         ap-sel.pre-date = ap-chk.check-date
         ap-sel.actnum = IF AVAIL bank THEN bank.actnum ELSE "NO Account"
         . 

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

  RUN delete-tt.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN update-header.

  RUN reopen-header.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO ap-sel.inv-no IN BROWSE {&browse-name}.

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
  /* ======== validation ===========*/
  IF ap-sel.inv-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
     FIND FIRST ap-inv WHERE ap-inv.company = g_company
                        AND ap-inv.vend-no = ap-chk.vend-no
                        AND ap-inv.inv-no = (ap-sel.inv-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        AND ap-inv.posted = YES
                        AND ap-inv.due <> 0 NO-LOCK NO-ERROR.
     IF NOT AVAIL ap-inv THEN DO:
        MESSAGE "Invalid AP Invoice. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ap-sel.inv-no.
        RETURN NO-APPLY.
     END.
     IF AVAIL ap-inv AND NOT ll-inv-displayed THEN RUN display-invoice (RECID(ap-inv)).
  END.
  /*========= end vali ===========*/
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN delete-tt.

  RUN reopen-header.

  ASSIGN
   adm-adding-record = NO
   adm-new-record    = NO.

  RUN auto-add-tt.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-header B-table-Win 
PROCEDURE reopen-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl( THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl) (RECID(ap-chk)).

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
  {src/adm/template/snd-list.i "ap-chk"}
  {src/adm/template/snd-list.i "ap-sel"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-header B-table-Win 
PROCEDURE update-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR ld-tmp-amt AS DEC NO-UNDO.

   DEF BUFFER bf-sel FOR ap-sel.


   ld-tmp-amt = 0.
   FOR EACH bf-sel WHERE bf-sel.company = ap-chk.company
                    AND bf-sel.vend-no = ap-chk.vend-no
                    AND bf-sel.man-check = ap-chk.man-check
                    AND bf-sel.bank-code = ap-chk.bank-code
                    AND bf-sel.check-no = ap-chk.check-no NO-LOCK:
         ld-tmp-amt = ld-tmp-amt + bf-sel.amt-paid.
   END.
   FIND CURRENT ap-chk EXCLUSIVE-LOCK.
   ap-chk.check-amt = ld-tmp-amt.
   FIND CURRENT ap-chk NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

