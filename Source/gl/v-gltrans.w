&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: gl/v-gltrans.w

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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/*&scoped-def gltrans-maint gltrans*/

{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE BUFFER bf-gltrans FOR gltrans .
DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
 RUN util/Validate.p PERSISTENT SET hdValidator.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdValidator).

&Scoped-define proc-enable proc-enable

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
&Scoped-define EXTERNAL-TABLES gltrans
&Scoped-define FIRST-EXTERNAL-TABLE gltrans


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR gltrans.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS gltrans.tr-date gltrans.actnum ~
gltrans.tr-dscr gltrans.jrnl gltrans.tr-amt 
&Scoped-define ENABLED-TABLES gltrans
&Scoped-define FIRST-ENABLED-TABLE gltrans
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 
&Scoped-Define DISPLAYED-FIELDS gltrans.trnum gltrans.tr-date ~
gltrans.actnum gltrans.tr-dscr gltrans.jrnl gltrans.yr gltrans.period ~
gltrans.tr-amt 
&Scoped-define DISPLAYED-TABLES gltrans
&Scoped-define FIRST-DISPLAYED-TABLE gltrans
&Scoped-Define DISPLAYED-OBJECTS acc_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

&Scoped-define calendarPopup btnCalendar-1 

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
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 109 BY 16.33.

DEFINE VARIABLE acc_dscr AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     gltrans.trnum AT ROW 2.52 COL 24.6 COLON-ALIGNED
          LABEL "Run #"
          VIEW-AS FILL-IN 
          SIZE 12.4 BY 1
          BGCOLOR 15 
     gltrans.tr-date AT ROW 3.76 COL 24.6 COLON-ALIGNED
          LABEL "Tx Date"
          VIEW-AS FILL-IN 
          SIZE 21.4 BY 1
          BGCOLOR 15 
     btnCalendar-1 AT ROW 3.76 COL 49.2
     gltrans.actnum AT ROW 4.95 COL 24.6 COLON-ALIGNED 
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 15 
     acc_dscr AT ROW 4.95 COL 66 COLON-ALIGNED NO-LABEL
     gltrans.tr-dscr AT ROW 6.24 COL 24.6 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
          BGCOLOR 15 
     gltrans.jrnl AT ROW 7.48 COL 24.6 COLON-ALIGNED
          LABEL "Journal"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     gltrans.yr AT ROW 8.76 COL 24.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     gltrans.period AT ROW 10.05 COL 24.6 COLON-ALIGNED
          LABEL "Period"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     gltrans.tr-amt AT ROW 11.43 COL 24.6 COLON-ALIGNED
          LABEL "Amount" FORMAT "->>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 15 
     RECT-1 AT ROW 1 COL 1
     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.gltrans
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
         HEIGHT             = 17.86
         WIDTH              = 161.2.
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

/* SETTINGS FOR FILL-IN gltrans.actnum IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN gltrans.jrnl IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN gltrans.period IN FRAME F-Main
   EXP-LABEL   NO-ENABLE                                                */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN gltrans.tr-amt IN FRAME F-Main
   2 EXP-LABEL FORMAT-EXIT                                              */
/* SETTINGS FOR FILL-IN gltrans.tr-date IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN gltrans.tr-dscr IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN gltrans.trnum IN FRAME F-Main
   EXP-LABEL   NO-ENABLE                                                */
/* SETTINGS FOR FILL-IN gltrans.yr IN FRAME F-Main
   EXP-LABEL   NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN acc_dscr IN FRAME F-Main
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
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE riLookup AS RECID NO-UNDO.
  DEFINE VARIABLE lv-handle AS HANDLE NO-UNDO.
  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

  {&methods/lValidateError.i YES}
  CASE FOCUS:NAME :
   WHEN "actnum" THEN DO:
        RUN windows/l-acct.w (cocode,"", gltrans.actnum:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" THEN 
            ASSIGN
            gltrans.actnum:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val)
            acc_dscr:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(2,char-val).
    END.
   
    OTHERWISE DO:
      lv-handle = FOCUS:HANDLE.
      RUN applhelp.p.
      IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
      APPLY "entry" TO lv-handle.
      RETURN NO-APPLY.
    END.
  END CASE.  
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gltrans.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gltrans.actnum V-table-Win
ON LEAVE OF gltrans.actnum IN FRAME F-Main /* Account Number */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   
   IF LASTKEY <> -1 AND gltrans.actnum:SCREEN-VALUE <> "" THEN DO:
      RUN valid-act-no ( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i gltrans.tr-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gltrans.tr-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gltrans.tr-date V-table-Win
ON HELP OF gltrans.tr-date IN FRAME F-Main /* Tx Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME gltrans.tr-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gltrans.tr-date V-table-Win
ON LEAVE OF gltrans.tr-date IN FRAME F-Main /* Tx Date */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .

   IF LASTKEY <> -1 THEN do:
    RUN valid-date ( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    IF date(gltrans.tr-date:SCREEN-VALUE) NE ? THEN
        ASSIGN
         gltrans.period:SCREEN-VALUE = string(MONTH(date(gltrans.tr-date:SCREEN-VALUE)),"99")
         gltrans.yr:SCREEN-VALUE = string(YEAR(date(gltrans.tr-date:SCREEN-VALUE)),"9999") .
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
   RUN dispatch ('add-record').
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

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "gltrans"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "gltrans"}

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
  DEFINE VARIABLE iTrNum AS INTEGER NO-UNDO .
  DEFINE BUFFER bf-gltrans FOR gltrans.
    
  /* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF adm-new-record OR  adm-adding-record THEN DO: /* add or copy */
          FIND FIRST gl-ctrl EXCLUSIVE-LOCK
              WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
          IF AVAIL gl-ctrl THEN DO:
              ASSIGN iTrNum        = gl-ctrl.trnum + 1
                     gl-ctrl.trnum = iTrNum
                     gltrans.trnum    = iTrNum
                     gltrans.trnum:SCREEN-VALUE  = string(iTrNum) .
              RELEASE gl-ctrl.       
          END. /* IF AVAIL gl-ctrl */
      END.
       IF date(gltrans.tr-date:SCREEN-VALUE) NE ? THEN
        ASSIGN
         gltrans.period = MONTH(date(gltrans.tr-date:SCREEN-VALUE))
         gltrans.yr = YEAR(date(gltrans.tr-date:SCREEN-VALUE)) .
  END.
  

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
  DISABLE ALL WITH FRAME {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  adm-adding-record = NO .
  adm-new-record = NO .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  gltrans.company = cocode.
  
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-company LIKE gltrans.company NO-UNDO.
  DEF VAR lv-tr-num  LIKE gltrans.trnum  NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
    

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  IF AVAIL gltrans THEN
      FIND FIRST account NO-LOCK
      WHERE account.company = g_company
      AND account.actnum = gltrans.actnum NO-ERROR .
  IF AVAIL account THEN DO:
      acc_dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = account.dscr .
  END.
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNewRecord AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
    DEFINE VARIABLE rdRowidLevel AS ROWID NO-UNDO .
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
   
    IF gltrans.actnum:SCREEN-VALUE <> "" THEN DO:
        RUN valid-act-no ( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    RUN valid-date ( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

  END.
 
  DISABLE ALL WITH FRAME {&frame-name}.

  IF adm-adding-record THEN lNewRecord = YES .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF lNewRecord THEN DO:
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
            RUN repo-query2 IN WIDGET-HANDLE(char-hdl) (gltrans.trnum).
  END.     
  adm-adding-record = NO .
  adm-new-record = NO .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
------------------------------------------------------------------------------*/

     /*RUN getSourceAttributes.
                
     IF cgltransSourceFrom = "EST" THEN DO WITH FRAME {&frame-name}:
        DISABLE gltrans.ItemType gltrans.customerID gltrans.ItemID gltrans.estimateNo gltrans.formNo 
                gltrans.blankNo /*gltrans.effectiveDate gltrans.ExpirationDate*/.
           
     END.
     ELSE IF cgltransSourceFrom = "OF" THEN DO WITH FRAME {&frame-name}:
        DISABLE /*gltrans.ItemType*/ gltrans.customerID gltrans.ItemID 
            /*gltrans.estimateNo gltrans.formNo gltrans.blankNo gltrans.effectiveDate gltrans.ExpirationDate*/.           
     END.
     ELSE IF cgltransSourceFrom NE "" THEN DO WITH FRAME {&frame-name}:
         DISABLE /*gltrans.ItemType*/ gltrans.ItemID gltrans.estimateNo gltrans.formNo 
             gltrans.blankNo 
                    /*gltrans.effectiveDate gltrans.ExpirationDate*/ .
     END.  */
    MESSAGE "hello " VIEW-AS ALERT-BOX ERROR .
DO WITH FRAME {&frame-name}:
    DISABLE gltrans.trnum.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME*/



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
  {src/adm/template/snd-list.i "gltrans"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-act-no V-table-Win 
PROCEDURE valid-act-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER oplReturnType AS  LOGICAL NO-UNDO .
  {methods/lValidateError.i YES}
  FIND FIRST account NO-LOCK
      WHERE account.company = g_company
        AND account.actnum = gltrans.actnum:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR .
  IF NOT AVAIL account THEN DO:
      MESSAGE "Invalid Account. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO gltrans.actnum.
      oplReturnType = YES . 
  END.         
  ELSE DO:
    acc_dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = account.dscr .
  END.
  
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date V-table-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplReturnType AS  LOGICAL NO-UNDO .

  {methods/lValidateError.i YES}
  IF  date(gltrans.tr-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}) EQ ? THEN DO:
      MESSAGE "Please enter a valid date." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO gltrans.tr-date.
      oplReturnType = YES .
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



