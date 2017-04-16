&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: browsers\custpart.w 

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
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cust-part reftable

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table cust-part.cust-no cust-part.part-no cust-part.spare-char-1 reftable.code2 reftable.dscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table cust-part.cust-no ~
cust-part.part-no ~
cust-part.spare-char-1 ~
reftable.code2 ~
reftable.dscr   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table cust-part reftable
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table cust-part
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br_table reftable
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH cust-part WHERE cust-part.company = itemfg.company   AND cust-part.i-no = itemfg.i-no NO-LOCK, ~
           FIRST reftable OUTER-JOIN WHERE reftable.reftable = "cp-lab-p"   AND reftable.company = cust-part.company   AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH cust-part WHERE cust-part.company = itemfg.company   AND cust-part.i-no = itemfg.i-no NO-LOCK, ~
           FIRST reftable OUTER-JOIN WHERE reftable.reftable = "cp-lab-p"   AND reftable.company = cust-part.company   AND reftable.loc = cust-part.i-no   AND reftable.code = cust-part.cust-no NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table cust-part reftable
&Scoped-define FIRST-TABLE-IN-QUERY-br_table cust-part
&Scoped-define SECOND-TABLE-IN-QUERY-br_table reftable


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
company||y|asi.cust-part.company
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
      cust-part, 
      reftable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      cust-part.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            WIDTH 13
      cust-part.part-no COLUMN-LABEL "Customer Part#" FORMAT "x(15)":U
            WIDTH 23
      cust-part.spare-char-1 COLUMN-LABEL "Sls Rep" FORMAT "x(7)":U
      reftable.code2 COLUMN-LABEL "Case Label" FORMAT "X(80)":U
      reftable.dscr  COLUMN-LABEL "Pallet Label" FORMAT "X(50)":U
  ENABLE
      cust-part.cust-no
      cust-part.part-no
      cust-part.spare-char-1
      reftable.code2
      reftable.dscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 57 BY 6.71
         BGCOLOR 8 FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.itemfg
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
         HEIGHT             = 6.76
         WIDTH              = 75.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH cust-part WHERE cust-part.company = itemfg.company
  AND cust-part.i-no = itemfg.i-no NO-LOCK,
    FIRST reftable OUTER-JOIN WHERE reftable.reftable = "cp-lab-p"
  AND reftable.company = cust-part.company
  AND reftable.loc = cust-part.i-no
  AND reftable.code = cust-part.cust-no NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[1]      = "asi.cust-part.company = asi.itemfg.company
  AND asi.cust-part.i-no = asi.itemfg.i-no"
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
ON HELP OF br_table IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "cust-no" THEN DO:
     
      RUN windows/l-cust.w (itemfg.company, FOCUS:SCREEN-VALUE, OUTPUT char-val).
      IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN
      /*         FOCUS:SCREEN-VALUE = ENTRY(1,char-val). aj */  
       cust-part.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val). 
      
    END.
    WHEN "spare-char-1" THEN DO:
       run windows/l-sman.w (itemfg.company, output char-val).
       if char-val ne "" THEN    
          cust-part.spare-char-1:screen-value IN BROWSE {&browse-name} = entry(1,char-val).

    END.
  END. 
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

DEF VAR chFile AS CHAR FORMAT "X(80)" NO-UNDO.
DEF VAR ll-ok AS LOG NO-UNDO.

/* gdm - 11050804 */
DEF VAR v-path AS CHAR NO-UNDO INIT 'c:\'.


ON 'help':U OF reftable.code2 IN BROWSE {&browse-name}
DO:
    
    /* gdm - 11050804 */
    
    ASSIGN v-path = TRIM(reftable.code2).

    IF TRIM(v-path) EQ "" 
      THEN
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cust-part.company
              AND sys-ctrl.name EQ "CASLABEL" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.char-fld).


    RUN sys\ref\char-fld-help.w(INPUT cust-part.company,
                                INPUT v-path,
                                OUTPUT chFile).
/*    
    IF TRIM(reftable.code2) NE ''
      THEN
        ASSIGN v_path = TRIM(reftable.code2).

    IF TRIM(v_path) NE '' OR TRIM(v_path) NE 'c:\' THEN DO:
        ASSIGN
            v_path = REPLACE(v_path,'/','\')
            v_pos  = R-INDEX(v_path,'\')
            v_path = SUBSTRING(v_path,1,v_pos).

    END.

    SYSTEM-DIALOG GET-FILE chFile 
                 TITLE "Select Label Matrix Label File"
                 FILTERS "Label Matrix (*.qdf) " "*.qdf"
                 INITIAL-DIR v_path
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

   IF ll-ok THEN
*/   
    ASSIGN reftable.code2:SCREEN-VALUE IN BROWSE {&browse-name} = chFile.
 
END.
/* gdm - 04090909 */
ON 'help':U OF reftable.dscr IN BROWSE {&browse-name}
DO:
    
    /* gdm - 11050804 */
    
    ASSIGN v-path = TRIM(reftable.dscr).

    IF TRIM(v-path) EQ "" 
      THEN
        FIND FIRST sys-ctrl NO-LOCK 
            WHERE sys-ctrl.company EQ cust-part.company
              AND sys-ctrl.name EQ "BARDIR" NO-ERROR.
        IF AVAIL sys-ctrl THEN
            ASSIGN v-path = TRIM(sys-ctrl.descrip).


    RUN sys\ref\char-fld-help.w(INPUT cust-part.company,
                                INPUT v-path,
                                OUTPUT chFile).

    ASSIGN reftable.dscr:SCREEN-VALUE IN BROWSE {&browse-name} = chFile.
 
END.
/* gdm - 04090909 end */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT AVAIL reftable THEN
     create reftable.

  assign reftable.reftable = "cp-lab-p"
         reftable.company = cust-part.company
         reftable.loc = cust-part.i-no
         reftable.code = cust-part.cust-no.
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
   cust-part.company = itemfg.company
   cust-part.i-no    = itemfg.i-no.

  create reftable.

  assign reftable.reftable = "cp-lab-p"
         reftable.company = cust-part.company
         reftable.loc = cust-part.i-no
         reftable.code = cust-part.cust-no.

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
/*     IF cust-part.cust-no EQ FILL("*",20) THEN RETURN ERROR. */
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 RUN local-open-query.

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
/*   IF AVAIL cust-part AND cust-part.cust-no EQ FILL("*",20) THEN RETURN ERROR. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-cust-part FOR cust-part.
  DEFINE BUFFER b-reftable FOR reftable.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH b-cust-part fields(company i-no cust-no) WHERE
      b-cust-part.company = itemfg.company AND
      b-cust-part.i-no = itemfg.i-no
      NO-LOCK:

      IF NOT CAN-FIND(FIRST reftable WHERE
         reftable.reftable = "cp-lab-p" AND
         reftable.company = b-cust-part.company AND
         reftable.loc = b-cust-part.i-no AND
         reftable.code = b-cust-part.cust-no) THEN
         DO:
            CREATE b-reftable.
            ASSIGN
               b-reftable.reftable = "cp-lab-p"
               b-reftable.company = b-cust-part.company
               b-reftable.loc = b-cust-part.i-no
               b-reftable.code = b-cust-part.cust-no.
            RELEASE b-reftable.
         END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-ans AS LOG NO-UNDO.
  DEF VAR v-ask AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
 
  IF cust-part.cust-no EQ FILL("*",20) THEN DO:
     MESSAGE "Record is not allowed to be changed.      "
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "entry" TO cust-part.cust-no IN BROWSE {&browse-name}.
     RETURN ERROR.
  END.
     
  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-part-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-sman NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  v-ask = NOT adm-new-record AND
          cust-part.part-no NE cust-part.part-no:SCREEN-VALUE IN BROWSE {&browse-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*gets rid of updating an existing cp before label mod and adding new
    customer part error*/
  RUN local-row-changed.

  IF v-ask THEN
  DO:
      MESSAGE "Update Orders?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ll-ans.

      IF ll-ans THEN
      DO:
         FOR EACH oe-ordl WHERE
             oe-ordl.company EQ cust-part.company AND
             oe-ordl.i-no EQ cust-part.i-no AND
             oe-ordl.cust-no EQ cust-part.cust-no
             USE-INDEX item-ord
             EXCLUSIVE-LOCK:

             oe-ordl.part-no = cust-part.part-no.
         END.
      END.
  END.

  
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
  {src/adm/template/sndkycas.i "company" "cust-part" "company"}

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
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "cust-part"}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no B-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-cust-part FOR cust-part.

  DEF VAR lv-cust-no LIKE cust-part.cust-no NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     ASSIGN
      cust-part.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} =
         CAPS(cust-part.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}).
      lv-cust-no = cust-part.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}.
     
     IF lv-msg EQ "" THEN
       IF lv-cust-no EQ "" THEN lv-msg = "may not be spaces".
     
     IF lv-msg EQ "" THEN
       IF NOT CAN-FIND(FIRST cust
                       WHERE cust.company EQ cust-part.company
                         AND cust.cust-no EQ lv-cust-no) THEN
         lv-msg = "is invalid, try help".
     
     IF lv-msg EQ "" THEN
       IF CAN-FIND(FIRST b-cust-part
                   WHERE b-cust-part.company EQ cust-part.company
                     AND b-cust-part.i-no    EQ cust-part.i-no
                     AND b-cust-part.cust-no EQ lv-cust-no
                     AND ROWID(b-cust-part)  NE ROWID(cust-part)) THEN
         lv-msg = "already exists for FG Item:" + cust-part.i-no.
     
     IF lv-msg NE "" THEN DO:
       MESSAGE TRIM(cust-part.cust-no:LABEL IN BROWSE {&browse-name}) + " " +
               TRIM(lv-msg) + "..."
           VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO cust-part.cust-no IN BROWSE {&browse-name}. 
       RETURN ERROR.
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no B-table-Win 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    cust-part.part-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(cust-part.part-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF cust-part.part-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN DO:
      MESSAGE TRIM(cust-part.part-no:LABEL IN BROWSE {&browse-name}) +
              " may not be spaces..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cust-part.part-no IN BROWSE {&browse-name}. 
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman B-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    /* Blank is valid */
    IF cust-part.spare-char-1:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      RETURN.
    FIND FIRST sman NO-LOCK WHERE sman.company EQ cust-part.company
                              AND sman.sman    EQ cust-part.spare-char-1:SCREEN-VALUE IN BROWSE {&browse-name} NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
      MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cust-part.spare-char-1 IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

