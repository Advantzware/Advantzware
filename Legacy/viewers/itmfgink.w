&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/itmfgink.w

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
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR lv-pr-types AS CHAR INIT "FGLO" NO-UNDO.
DEF VAR lv-pr-list AS CHAR INIT ",Flexo,Gravure,Letterpress,Offset" NO-UNDO.
DEF VAR lv-cover% AS INT NO-UNDO.

&SCOPED-DEFINE enable-itemfg-ink enable-itemfg-ink



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg-ink
&Scoped-define FIRST-EXTERNAL-TABLE itemfg-ink


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg-ink.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg-ink.rm-i-no itemfg-ink.dscr ~
itemfg-ink.pass itemfg-ink.in-out itemfg-ink.cover% 
&Scoped-define ENABLED-TABLES itemfg-ink
&Scoped-define FIRST-ENABLED-TABLE itemfg-ink
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS itemfg-ink.rm-i-no itemfg-ink.dscr ~
itemfg-ink.pass itemfg-ink.in-out itemfg-ink.cover% 
&Scoped-define DISPLAYED-TABLES itemfg-ink
&Scoped-define FIRST-DISPLAYED-TABLE itemfg-ink
&Scoped-Define DISPLAYED-OBJECTS fi_press-type fi_occurs 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS fi_occurs 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.itemfg-ink.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_occurs AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Occurs" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fi_press-type AS CHARACTER FORMAT "X(15)":U 
     LABEL "Press" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg-ink.rm-i-no AT ROW 1.24 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 6
     fi_press-type AT ROW 1.24 COL 42 COLON-ALIGNED
     itemfg-ink.dscr AT ROW 2.43 COL 13 COLON-ALIGNED
          LABEL "Desc"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
          FONT 6
     itemfg-ink.pass AT ROW 3.62 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          FONT 6
     itemfg-ink.in-out AT ROW 4.81 COL 15 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Inside", yes,
"Outside", no
          SIZE 33 BY .95
     itemfg-ink.cover% AT ROW 6 COL 13 COLON-ALIGNED
          LABEL "Cover %"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          FONT 6
     fi_occurs AT ROW 6 COL 35 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg-ink
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg-ink.cover% IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN itemfg-ink.dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_occurs IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_press-type IN FRAME F-Main
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


  DO WITH FRAME {&FRAME-NAME}:
    CASE FOCUS:NAME:
      WHEN "rm-i-no" then do:        
        RUN windows/l-item.w (g_company, " ", "I,V", FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" AND itemfg-ink.rm-i-no:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
          itemfg-ink.rm-i-no:SCREEN-VALUE = ENTRY(1,char-val).
          RUN new-rm-i-no.
        END.
      END.
    END CASE.
  END.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg-ink.cover%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg-ink.cover% V-table-Win
ON LEAVE OF itemfg-ink.cover% IN FRAME F-Main /* Cover % */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cover% NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_occurs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_occurs V-table-Win
ON LEAVE OF fi_occurs IN FRAME F-Main /* Occurs */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-fi_occurs NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg-ink.pass
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg-ink.pass V-table-Win
ON LEAVE OF itemfg-ink.pass IN FRAME F-Main /* Pass */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-pass NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg-ink.rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg-ink.rm-i-no V-table-Win
ON ENTRY OF itemfg-ink.rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  IF adm-new-record THEN
  itemfg-ink.cover%:SCREEN-VALUE = STRING(lv-cover%).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg-ink.rm-i-no V-table-Win
ON LEAVE OF itemfg-ink.rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-rm-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg-ink.rm-i-no V-table-Win
ON VALUE-CHANGED OF itemfg-ink.rm-i-no IN FRAME F-Main /* RM Item# */
DO:
  RUN new-rm-i-no.
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
  {src/adm/template/row-list.i "itemfg-ink"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg-ink"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
    DISABLE fi_occurs.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ebfgBuild V-table-Win 
PROCEDURE ebfgBuild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "ebfg-target", OUTPUT char-hdl).
  RUN ebfgBuild IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-itemfg-ink V-table-Win 
PROCEDURE enable-itemfg-ink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_occurs.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (NO).

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
  RUN disable-fields.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR op-company LIKE itemfg.company NO-UNDO.
  DEF VAR op-i-no    LIKE itemfg.i-no    NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/CREATE/itemfg-ink.i}
  itemfg-ink.cover% = lv-cover%.

  DO WITH FRAME {&FRAME-NAME}:
    fi_occurs:SCREEN-VALUE = "1".
  END.

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
  RELEASE item.
  IF AVAIL itemfg-ink THEN DO:
    FIND FIRST item
          WHERE item.company EQ itemfg-ink.company
            AND item.i-no    EQ itemfg-ink.rm-i-no
          NO-LOCK NO-ERROR.
    fi_press-type = IF AVAIL item THEN
                      ENTRY(INDEX(lv-pr-types,item.press-type) + 1,lv-pr-list)
                    ELSE "".

    IF NOT adm-new-record THEN RUN reftable-values (YES).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-cover% = IF AVAILABLE itemfg-ink THEN itemfg-ink.cover% ELSE 100.

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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     itemfg-ink.rm-i-no:BGCOLOR = ?
     itemfg-ink.rm-i-no:FGCOLOR = ?
     itemfg-ink.in-out:BGCOLOR  = ?
     itemfg-ink.in-out:FGCOLOR  = ?.
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
  DEF BUFFER b-fgink FOR itemfg-ink.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-rm-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST b-fgink
        WHERE b-fgink.company EQ itemfg.company
          AND b-fgink.i-no    EQ itemfg.i-no
          AND b-fgink.rm-i-no EQ itemfg-ink.rm-i-no:SCREEN-VALUE
          AND b-fgink.in-out  EQ (itemfg-ink.in-out:SCREEN-VALUE EQ "yes")
          AND ROWID(b-fgink)  NE ROWID(itemfg-ink)
        NO-LOCK NO-ERROR.
    IF AVAIL b-fgink THEN DO:
      MESSAGE "Ink already entered..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg-ink.rm-i-no.
      RETURN ERROR.
    END.
  END.
  {&methods/lValidateError.i NO}
  RUN valid-pass NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-cover% NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  RUN valid-fi_occurs NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-fields.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "record-source", OUTPUT char-hdl).
  RUN repo-browser IN WIDGET-HANDLE(char-hdl) (ROWID(itemfg-ink)).
  RUN ebfgBuild.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rm-i-no V-table-Win 
PROCEDURE new-rm-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ itemfg-ink.rm-i-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN
      ASSIGN
       itemfg-ink.dscr:SCREEN-VALUE = 
           IF item.est-dscr NE "" THEN item.est-dscr
           ELSE
           IF item.i-dscr NE "" THEN item.i-dscr ELSE item.i-name
       fi_press-type:SCREEN-VALUE   = ENTRY(INDEX(lv-pr-types,item.press-type) + 1,lv-pr-list).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.


  IF AVAIL itemfg-ink THEN DO:
/*    FIND FIRST reftable {&where-occurs} NO-ERROR.*/
/*    IF NOT AVAIL reftable THEN DO:               */
/*      CREATE reftable.                           */
/*      ASSIGN                                     */
/*       reftable.rec_key  = itemfg-ink.rec_key    */
/*       reftable.reftable = "itemfg-ink.occurs"   */
/*       reftable.company  = itemfg-ink.company    */
/*       reftable.val[1]   = 1.                    */
/*    END.                                         */
/*    IF ip-display THEN                           */
/*      fi_occurs = reftable.val[1].               */
/*    ELSE                                         */
/*      reftable.val[1] = fi_occurs.               */
/*                                                 */
/*    FIND CURRENT reftable NO-LOCK.               */
    IF ip-display THEN
      fi_occurs = itemfg-ink.occurs.
    ELSE
      itemfg-ink.occurs = fi_occurs.

    FIND CURRENT itemfg-ink NO-LOCK.

  END.

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
  {src/adm/template/sndkycas.i "company" "itemfg-ink" "company"}

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
  {src/adm/template/snd-list.i "itemfg-ink"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cover% V-table-Win 
PROCEDURE valid-cover% :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(itemfg-ink.cover%:SCREEN-VALUE) LT 1   OR
       DEC(itemfg-ink.cover%:SCREEN-VALUE) GT 100 THEN DO:
      MESSAGE "Cover% must be greater than zero and less than or equal to 100..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg-ink.cover%.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fi_occurs V-table-Win 
PROCEDURE valid-fi_occurs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(fi_occurs:SCREEN-VALUE) LT 1 THEN DO:
      MESSAGE TRIM(fi_occurs:LABEL) + " must be greater than zero..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi_occurs.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-pass V-table-Win 
PROCEDURE valid-pass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(itemfg-ink.pass:SCREEN-VALUE) LT 1 THEN DO:
      MESSAGE TRIM(fi_occurs:LABEL) + " must be greater than zero..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg-ink.pass.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rm-i-no V-table-Win 
PROCEDURE valid-rm-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST item
        WHERE item.company EQ g_company
          AND item.i-no    EQ itemfg-ink.rm-i-no:SCREEN-VALUE
          AND INDEX("IV",item.mat-type) GT 0
          AND INDEX("FGLO",item.press-type) GT 0
        NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO:
      MESSAGE "Invalid Raw Material, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg-ink.rm-i-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

