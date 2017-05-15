&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/fgcat.w

  Description: from VIEWER.W - Template for SmartViewer Objects

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
{custom/format.i}

{sys/inc/VAR.i  NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
    &Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&SCOPED-DEFINE proc-enable RUN proc-enable.
&SCOPED-DEFINE create-more viewers/fgcatcreate

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
&Scoped-define EXTERNAL-TABLES fgcat
&Scoped-define FIRST-EXTERNAL-TABLE fgcat


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR fgcat.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS fgcat.dscr fgcat.glacc 
&Scoped-define ENABLED-TABLES fgcat
&Scoped-define FIRST-ENABLED-TABLE fgcat
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS fgcat.procat fgcat.dscr fgcat.glacc 
&Scoped-define DISPLAYED-TABLES fgcat
&Scoped-define FIRST-DISPLAYED-TABLE fgcat
&Scoped-Define DISPLAYED-OBJECTS cat-format v-charge v-gl-rm v-gl-fg F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS fgcat.procat 
&Scoped-define F1 F1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE cat-format AS LOGICAL FORMAT "Fraction/Decimal":U INITIAL NO 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE v-charge AS CHARACTER FORMAT "X(20)":U 
     LABEL "Misc Charge" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE v-gl-fg AS CHARACTER FORMAT "X(20)":U 
     LABEL "FG COGS Expense GL#" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE v-gl-rm AS CHARACTER FORMAT "X(20)":U 
     LABEL "RM Board Expense GL#" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 6.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fgcat.procat AT ROW 1.24 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     fgcat.dscr AT ROW 1.24 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     cat-format AT ROW 2.43 COL 12 COLON-ALIGNED HELP
          "Enter to print box & sheet dimensions in Decimal or Fraction"
     fgcat.glacc AT ROW 2.43 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     v-charge AT ROW 3.62 COL 17 COLON-ALIGNED
     v-gl-rm AT ROW 4.62 COL 30 COLON-ALIGNED WIDGET-ID 2
     v-gl-fg AT ROW 5.62 COL 30 COLON-ALIGNED WIDGET-ID 4
     F1 AT ROW 2.43 COL 72 NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.fgcat
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.24
         WIDTH              = 79.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cat-format IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fgcat.procat IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v-charge IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-gl-fg IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-gl-rm IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lv-handle AS HANDLE NO-UNDO.

   CASE FOCUS:NAME :
    when "v-charge" then do:
           run windows/l-surchg.w 
                 (g_company,focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
     end.
     WHEN "v-gl-rm" OR WHEN "v-gl-fg" THEN DO:
        RUN windows/l-acct2.w (g_company, "E", focus:screen-value in frame {&frame-name}, OUTPUT char-val).
        IF char-val NE "" THEN
           FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
     END.
     otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.

           end.   /* g_lookup-var <> "" */
           apply "entry" to lv-handle.
           return no-apply.

     end.  /* otherwise */
  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fgcat.glacc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fgcat.glacc V-table-Win
ON LEAVE OF fgcat.glacc IN FRAME F-Main /* GL Account */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-glacc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fgcat.glacc V-table-Win
ON VALUE-CHANGED OF fgcat.glacc IN FRAME F-Main /* GL Account */
DO:
  FIND account
        WHERE account.company EQ cocode
          AND account.actnum  BEGINS {&self-name}:SCREEN-VALUE
          AND account.TYPE    EQ "R"
        NO-LOCK NO-ERROR.
  IF AVAIL account THEN DO:
    {&self-name}:SCREEN-VALUE = account.actnum.
    APPLY "tab" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fgcat.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fgcat.procat V-table-Win
ON LEAVE OF fgcat.procat IN FRAME F-Main /* Category */
DO:
   IF LASTKEY <> -1 AND fgcat.procat:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Category cannot be blank. Try again." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-charge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-charge V-table-Win
ON LEAVE OF v-charge IN FRAME F-Main /* Misc Charge */
DO:
   IF LASTKEY <> -1 THEN DO:
      RUN valid-charge NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-gl-fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-gl-fg V-table-Win
ON LEAVE OF v-gl-fg IN FRAME F-Main /* FG COGS Expense GL# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN valid-fg-glacc NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-gl-fg V-table-Win
ON VALUE-CHANGED OF v-gl-fg IN FRAME F-Main /* FG COGS Expense GL# */
DO:
   FIND account
        WHERE account.company EQ cocode
          AND account.actnum  BEGINS {&self-name}:SCREEN-VALUE
          AND account.TYPE    EQ "E"
        NO-LOCK NO-ERROR.
  IF AVAIL account THEN DO:
     {&self-name}:SCREEN-VALUE = account.actnum.
     APPLY "tab" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-gl-rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-gl-rm V-table-Win
ON LEAVE OF v-gl-rm IN FRAME F-Main /* RM Board Expense GL# */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN valid-rm-glacc NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-gl-rm V-table-Win
ON VALUE-CHANGED OF v-gl-rm IN FRAME F-Main /* RM Board Expense GL# */
DO:
   FIND account
        WHERE account.company EQ cocode
          AND account.actnum  BEGINS {&self-name}:SCREEN-VALUE
          AND account.TYPE    EQ "E"
        NO-LOCK NO-ERROR.
  IF AVAIL account THEN DO:
     {&self-name}:SCREEN-VALUE = account.actnum.
     APPLY "tab" TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  RUN dispatch ('add-record').
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
  {src/adm/template/row-list.i "fgcat"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "fgcat"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-update AS LOG INIT FALSE NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER sman-mtx-tmp FOR sman-mtx.
  ASSIGN FRAME {&FRAME-NAME} v-charge v-gl-rm v-gl-fg.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/fgcat.i}

  if adm-new-record then
  for each sman-mtx
      where sman-mtx.company eq cocode
      break by sman-mtx.sman by sman-mtx.custype:
    if first-of(sman-mtx.custype) or first-of(sman-mtx.sman) then
      v-update = no.

    if v-update then next.

    do li = 1 to 10:
      if li gt 10 then leave.
      if sman-mtx.procat[li] ne "" then next.
      else do:
        assign
         sman-mtx.procat[li] = fgcat.procat
         sman-mtx.dscr[li]   = fgcat.dscr
         sman-mtx.comm[li]   = fgcat.comm
        v-update = yes.
        leave.
      end.
    end.

    if last-of(sman-mtx.custype) and v-update eq no then do:
      create sman-mtx-tmp.
      assign
       sman-mtx-tmp.company = cocode
       sman-mtx-tmp.sman = sman-mtx.sman
       sman-mtx-tmp.custype = sman-mtx.custype
       sman-mtx-tmp.custype-no = sman-mtx.custype-no + 1
       sman-mtx-tmp.procat[1] = fgcat.procat
       sman-mtx-tmp.dscr[1] = fgcat.dscr.
       /* sman-mtx-tmp.comm[1] = fgcat.comm. */
    end.
  end.

  FIND FIRST reftable WHERE reftable.reftable EQ "chargecode"
           AND reftable.company  EQ fgcat.company
           AND reftable.loc      EQ fgcat.procat
           /*AND reftable.code     EQ */
           NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
     CREATE reftable.
     ASSIGN reftable.reftable = "ChargeCode"
            reftable.company = fgcat.company
            reftable.loc = fgcat.procat.
  END.
  ASSIGN
  reftable.CODE = v-charge
  reftable.code2 = v-gl-rm
  reftable.dscr = v-gl-fg.


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
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
  END.

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
  {methods/viewers/create/fgcat.i}

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    cat-format:SCREEN-VALUE = STRING(fgcat.commrate EQ 1,"yes/no").
  END.

  IF NOT adm-new-record THEN
  DO:
    FIND FIRST reftable WHERE
         reftable.reftable EQ "chargecode" AND
         reftable.company  EQ fgcat.company AND
         reftable.loc      EQ fgcat.procat
         NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
       ASSIGN
          v-charge = reftable.CODE
          v-gl-rm  = reftable.code2
          v-gl-fg  = reftable.dscr.
    ELSE
       ASSIGN
          v-charge = ""
          v-gl-rm = ""
          v-gl-fg = "".
  END.

  DISPLAY v-charge v-gl-rm v-gl-fg WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {&methods/lValidateError.i YES}
  IF fgcat.procat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
     MESSAGE "Category cannot be blank. Try again." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO fgcat.procat.
     RETURN .
  END.
  {&methods/lValidateError.i NO}
  RUN valid-glacc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  RUN valid-rm-glacc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN valid-fg-glacc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  RUN valid-charge NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE v-charge v-gl-rm v-gl-fg WITH FRAME {&FRAME-NAME}.
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
  ENABLE v-charge v-gl-fg v-gl-rm WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "fgcat"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-charge V-table-Win 
PROCEDURE valid-charge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST surcharge WHERE surcharge.company = g_company
                         AND surcharge.charge = v-charge:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    AND v-charge:SCREEN-VALUE <> ""
  THEN DO:
     MESSAGE "Invalid Misc Charge. Try Help." VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO v-charge.
     RETURN ERROR.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fg-glacc V-table-Win 
PROCEDURE valid-fg-glacc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
      IF NOT v-gl-fg:SCREEN-VALUE EQ "" AND
         NOT CAN-FIND(FIRST account
                      WHERE account.company EQ cocode
                        AND account.actnum  EQ v-gl-fg:SCREEN-VALUE
                        AND account.TYPE    EQ "E") THEN DO:
         MESSAGE "Invalid FG Board Expense GL#, try help..."
                 VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-gl-fg.
         RETURN ERROR.
      END.
   END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-glacc V-table-Win 
PROCEDURE valid-glacc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-FIND(FIRST account
                    WHERE account.company EQ cocode
                      AND account.actnum  EQ fgcat.glacc:SCREEN-VALUE
                      AND account.TYPE    EQ "R") THEN DO:
      MESSAGE "Invalid Revenue Account#, try help..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fgcat.glacc.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-glacc V-table-Win 
PROCEDURE valid-rm-glacc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
      IF NOT v-gl-rm:SCREEN-VALUE EQ "" AND
         NOT CAN-FIND(FIRST account
                      WHERE account.company EQ cocode
                        AND account.actnum  EQ v-gl-rm:SCREEN-VALUE
                        AND account.TYPE    EQ "E") THEN DO:
         MESSAGE "Invalid RM Board Expense GL#, try help..."
                 VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-gl-rm.
         RETURN ERROR.
      END.
   END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

