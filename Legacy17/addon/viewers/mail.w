&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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
def buffer bf-maillist for maillist.
define shared var g_company as cha no-undo.

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
&Scoped-define EXTERNAL-TABLES maillist
&Scoped-define FIRST-EXTERNAL-TABLE maillist


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR maillist.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS maillist.list-date maillist.list-name ~
maillist.list-for 
&Scoped-define ENABLED-TABLES maillist
&Scoped-define FIRST-ENABLED-TABLE maillist
&Scoped-Define ENABLED-OBJECTS RECT-5 
&Scoped-Define DISPLAYED-FIELDS maillist.list-no maillist.list-date ~
maillist.active maillist.list-name maillist.list-for maillist.sman 
&Scoped-define DISPLAYED-TABLES maillist
&Scoped-define FIRST-DISPLAYED-TABLE maillist


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS maillist.list-for maillist.sman 

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
DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)":U 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 108 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     maillist.list-no AT ROW 1.24 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     maillist.list-date AT ROW 1.24 COL 39 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     maillist.active AT ROW 1.24 COL 84
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     maillist.list-name AT ROW 2.19 COL 12 COLON-ALIGNED
          LABEL "Title"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     maillist.list-for AT ROW 3.38 COL 14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "All", "A":U,
"SalesRep", "S":U,
"Type", "T":U
          SIZE 35 BY .95
     maillist.sman AT ROW 3.38 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     begin_type AT ROW 3.38 COL 68 COLON-ALIGNED
     end_type AT ROW 3.38 COL 90 COLON-ALIGNED
     "List for:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.62 COL 3
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: maillist
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
         HEIGHT             = 7.76
         WIDTH              = 111.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR TOGGLE-BOX maillist.active IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       maillist.active:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN begin_type IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       begin_type:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN end_type IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       end_type:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET maillist.list-for IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN maillist.list-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN maillist.list-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN maillist.sman IN FRAME F-Main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME begin_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_type V-table-Win
ON HELP OF begin_type IN FRAME F-Main /* From */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-csttyp.w (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_type V-table-Win
ON HELP OF end_type IN FRAME F-Main /* To */
DO:
  DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-csttyp.w (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maillist.list-for
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maillist.list-for V-table-Win
ON LEAVE OF maillist.list-for IN FRAME F-Main /* List For */
DO:
   if maillist.list-for:screen-value = "S" then 
   do: 
      enable maillist.sman with frame {&frame-name}.
      apply "entry" to maillist.sman in frame {&frame-name}.   
   end.
   else disable maillist.sman with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maillist.list-for V-table-Win
ON VALUE-CHANGED OF maillist.list-for IN FRAME F-Main /* List For */
DO:   


   if maillist.list-for:screen-value = "S" then 
   do: 
      enable maillist.sman with frame {&frame-name}.
      apply "entry" to maillist.sman in frame {&frame-name}.   
      DISABLE begin_type END_type WITH FRAME {&FRAME-NAME}.
      ASSIGN begin_type:HIDDEN IN FRAME {&FRAME-NAME} = YES
             END_type:HIDDEN = YES.
   end.
   ELSE IF maillist.list-for:screen-value = "T" then DO:
       ASSIGN begin_type:HIDDEN = NO
              END_type:HIDDEN = NO
              begin_type:SENSITIVE = YES
              END_type:SENSITIVE = YES.

      apply "entry" to begin_type in frame {&frame-name}.   
   END.
   else do:
       disable maillist.sman with frame {&frame-name}.
       DISABLE begin_type END_type WITH FRAME {&FRAME-NAME}.
       ASSIGN begin_type:HIDDEN IN FRAME {&FRAME-NAME} = YES
              END_type:HIDDEN = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maillist.list-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maillist.list-name V-table-Win
ON LEAVE OF maillist.list-name IN FRAME F-Main /* Title */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and
       maillist.list-name:screen-value <> "" and
       can-find(first bf-maillist where bf-maillist.list-name = self:screen-value and
                                        recid(bf-maillist) <> recid(maillist))
    then do:
         message "Title already exists. Use other title please." view-as alert-box.
         return no-apply.
    end.    
    {&methods/lValidateError.i NO}                                
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maillist.sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maillist.sman V-table-Win
ON HELP OF maillist.sman IN FRAME F-Main /* Salesman */
DO:

   def var char-val as cha no-undo.
   run windows/l-sman.w (g_company,output char-val).
   if char-val <> "" then assign self:screen-value = entry(1,char-val).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-mailcont V-table-Win 
PROCEDURE add-mailcont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
session:set-wait-state("general").
for each contact where /*contact.maillist*/ 
                            ((maillist.list-for = "S" AND contact.sman = maillist.sman) or
                              (maillist.list-for = "T" AND contact.type >= begin_type
                               AND contact.type <= END_type) OR
                             maillist.list-for = "A")
                           no-lock:
         create mailcont.
         assign mailcont.list-no = maillist.list-no
                mailcont.contact-rec = recid(contact)
                mailcont.first-name = contact.first-name
                mailcont.middle-initial = contact.middle-initial
                mailcont.last-name = contact.last-name
                mailcont.cust-no = contact.cust-no
                mailcont.cust-name = contact.cust-name
                mailcont.contact-title = contact.contact-title
                mailcont.maillist  = contact.maillist.
end.
session:set-wait-state("").

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
  {src/adm/template/row-list.i "maillist"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "maillist"}

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
  def var li-prev-list-no as int no-undo.
  def buffer bf-cont for mailcont .

  /* Code placed here will execute PRIOR to standard behavior. */
  li-prev-list-no = maillist.list-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if adm-new-record and not adm-adding-record then DO TRANSACTION:  /* copy */
     for each bf-cont where bf-cont.list-no = li-prev-list-no no-lock:
         create mailcont.
         buffer-copy bf-cont except bf-cont.list-no to mailcont.
         assign mailcont.list-no = maillist.list-no.
     end.      
  end.
  /* moved to update-record due to locktable overflow
  if adm-adding-record then do:
     session:set-wait-state("general").
     RUN add-mailcont.

     session:set-wait-state("").
  end.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-maillist for maillist.
  def var li-next-list-no as int no-undo.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  find last bf-maillist use-index maillist no-lock no-error.
  li-next-list-no = if avail bf-maillist then  bf-maillist.list-no + 1 else 1.

  assign maillist.list-no = li-next-list-no
         maillist.list-date = today
         maillist.active = yes
         maillist.list-for = "A".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  disable maillist.sman begin_type end_type with frame {&frame-name}.
  MESSAGE "Delete Currently Selected Record?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE response AS LOGICAL.
  IF NOT response THEN return no-apply.

  session:set-wait-state("general").
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     session:set-wait-state("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if not avail maillist then return.

  if maillist.list-for = "s" then enable maillist.sman with frame {&frame-name}.
  if not adm-adding-record then do: 
     disable maillist.list-for maillist.sman with frame {&frame-name}.
  end.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 def var ll-new-record as log no-undo.
 def var ll-add-record as log no-undo.
 DEF VAR lv-error AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  ASSIGN ll-new-record = adm-new-record
         ll-add-record = adm-adding-record.
  {&methods/lValidateError.i YES}
  do with frame {&frame-name} :
     if maillist.list-name:screen-value <> "" and
        can-find(first bf-maillist where bf-maillist.list-name = maillist.list-name:screen-value and
                                        recid(bf-maillist) <> recid(maillist))
    then do:
         message "Title already exists. Use other title please." view-as alert-box.
         apply "entry" to maillist.list-name.
         return no-apply.
    end.                       

    IF maillist.list-for:SCREEN-VALUE = "T" AND
       (begin_type:SCREEN-VALUE <> "" OR end_type:SCREEN-VALUE <> "") THEN DO:
       lv-error = 0.
       IF begin_type:SCREEN-VALUE <> "" THEN DO:
          FIND FIRST custype WHERE custype.company = g_company AND
                                   custype.custYPE = begin_type:SCREEN-VALUE NO-LOCK NO-ERROR.
          IF NOT AVAIL custype THEN lv-error = 1.
       END.
       IF begin_type:SCREEN-VALUE <> "" THEN DO:
          FIND FIRST custype WHERE custype.company = g_company AND
                                   custype.cusTYPE = begin_type:SCREEN-VALUE NO-LOCK NO-ERROR.
          IF NOT AVAIL custype THEN lv-error = 2.
       END.
       IF lv-error > 0 THEN DO:
           MESSAGE "Invalid Customer Type. Try Help." VIEW-AS ALERT-BOX ERROR.
           IF lv-error = 1 THEN APPLY "entry" TO begin_type.
           ELSE IF lv-error = 2 THEN APPLY "entry" TO END_type.
           RETURN.
       END.
    END.
  end.
  {&methods/lValidateError.i NO}
  ASSIGN begin_type END_type.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if ll-add-record then do:
     RUN add-mailcont.     
  end.

  disable maillist.sman begin_type end_type with frame {&frame-name}.
  ASSIGN begin_type:HIDDEN IN FRAME {&FRAME-NAME}= YES
         END_type:HIDDEN = YES.

  if ll-new-record then  do:
     def var char-hdl as cha no-undo.
     run get-link-handle in adm-broker-hdl (this-procedure,"record-source",output char-hdl).
     run dispatch in widget-handle(char-hdl) ('row-changed').
  end.   

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
  {src/adm/template/snd-list.i "maillist"}

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

