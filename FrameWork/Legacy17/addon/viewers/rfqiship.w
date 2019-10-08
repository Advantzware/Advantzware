&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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

def var char-val as cha no-undo.
def var lv-ind like style.industry no-undo.

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
&Scoped-define EXTERNAL-TABLES rfqitem
&Scoped-define FIRST-EXTERNAL-TABLE rfqitem


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfqitem.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.ship-id rfqitem.carrier ~
rfqitem.cas-no rfqitem.tr-no rfqitem.weight-m rfqitem.cas-cost ~
rfqitem.tr-cost rfqitem.cas-cnt rfqitem.tr-cnt rfqitem.cas-len ~
rfqitem.tr-len rfqitem.cas-wid rfqitem.tr-wid rfqitem.cas-dep ~
rfqitem.tr-dep rfqitem.cas-pal rfqitem.tr-cas rfqitem.cas-wt 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-15 
&Scoped-Define DISPLAYED-FIELDS rfqitem.ship-id rfqitem.carrier ~
rfqitem.carr-dscr rfqitem.cas-no rfqitem.tr-no rfqitem.weight-m ~
rfqitem.cas-cost rfqitem.tr-cost rfqitem.cas-cnt rfqitem.tr-cnt ~
rfqitem.cas-len rfqitem.tr-len rfqitem.cas-wid rfqitem.tr-wid ~
rfqitem.cas-dep rfqitem.tr-dep rfqitem.cas-pal rfqitem.tr-cas ~
rfqitem.cas-wt 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS ship-name 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE VARIABLE ship-name AS CHARACTER FORMAT "x(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 118.2 BY 10.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.ship-id AT ROW 1.48 COL 19 COLON-ALIGNED
          LABEL "Ship To"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     ship-name AT ROW 1.48 COL 39 COLON-ALIGNED NO-LABEL
     rfqitem.carrier AT ROW 2.67 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.carr-dscr AT ROW 2.67 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 7 FGCOLOR 15 
     rfqitem.cas-no AT ROW 3.62 COL 19 COLON-ALIGNED
          LABEL "Packing Code" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.tr-no AT ROW 3.62 COL 58 COLON-ALIGNED
          LABEL "Unit#"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     rfqitem.weight-m AT ROW 3.62 COL 102 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     rfqitem.cas-cost AT ROW 4.57 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-cost AT ROW 4.57 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-cnt AT ROW 5.52 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-cnt AT ROW 5.52 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-len AT ROW 6.48 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-len AT ROW 6.48 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-wid AT ROW 7.43 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-wid AT ROW 7.43 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-dep AT ROW 8.38 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-dep AT ROW 8.38 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-pal AT ROW 9.33 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.tr-cas AT ROW 9.33 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.cas-wt AT ROW 10.29 COL 19 COLON-ALIGNED FORMAT ">>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RECT-15 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfqitem
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
         HEIGHT             = 11.62
         WIDTH              = 118.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

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

/* SETTINGS FOR FILL-IN rfqitem.carr-dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.cas-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rfqitem.cas-wt IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rfqitem.ship-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ship-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.tr-no IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON GO OF FRAME F-Main
DO:
   def var out-hdl-str as cha no-undo.
   run get-link-handle in adm-broker-hdl (this-procedure,"TABLEIO-SOURCE",output out-hdl-str).   
   run notify in widget-handle(out-hdl-str) ("update-record"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   if not avail rfq then find rfq of rfqitem no-lock.
   case focus:name :
        when "carrier" then do:
             run windows/l-carrier.w  
                 (rfqitem.company,rfqitem.loc,focus:screen-value, output char-val).
             if char-val <> "" then
                assign rfqitem.carrier:screen-value in frame {&frame-name} = entry(1,char-val)
                       carr-dscr:screen-value in frame {&frame-name} = entry(2,char-val)
                       .
             return no-apply.              

        end.
        when "cas-no" then do:
           find style where style.company = rfqitem.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,"","C",focus:screen-value, output char-val).
           if char-val <> "" then do:
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*rfqitem.cas-cost:Screen-value = */
                                        rfqitem.cas-cnt:Screen-value = string(item.box-case)
                                        rfqitem.cas-len:Screen-value = string(item.case-l)
                                        rfqitem.cas-wid:Screen-value = string(item.case-w)
                                        rfqitem.cas-dep:Screen-value = string(item.case-d)
                                        rfqitem.cas-pal:Screen-value = string(item.case-pall)
                                        rfqitem.cas-wt:Screen-value = string(item.avg-w)         
                                        .
           end.   
           return no-apply.   
     end.   
     when "tr-no" then do:
           find style where style.company = rfqitem.company and
                            style.style = rfqitem.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (rfqitem.company,"","D",focus:screen-value, output char-val).
           if char-val <> "" then do:
              find item where item.company = rfqitem.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*rfqitem.cas-cost:Screen-value = */
                                        rfqitem.tr-len:Screen-value = string(item.case-l)
                                        rfqitem.tr-wid:Screen-value = string(item.case-w)
                                        rfqitem.tr-dep:Screen-value = string(item.case-d)
                                        .
           end.
           return no-apply.   
     end.   
     when "ship-id" then do:
           run windows/l-shipto.w (rfqitem.company,rfqitem.loc,rfq.cust-no,focus:screen-value, output char-val).
           if char-val <> "" then DO:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     ship-name:screen-value in frame {&frame-name} = entry(2,char-val)
                     rfqitem.carrier:screen-value in frame {&frame-name} = entry(3,char-val)
                     .
              find carrier where carrier.company = rfqitem.company and 
                                 carrier.loc     = rfqitem.loc and
                                 carrier.carrier = rfqitem.carrier:screen-value in frame {&frame-name}
                                 no-lock no-error.
               carr-dscr:screen-value in frame {&frame-name} = if avail carrier then carrier.dscr else "".

           end.
           return no-apply.   
     end.
    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON return OF FRAME F-Main
anywhere
DO:
  /*  use     session:data-entry-return = true.  /* return key will be like tab key */

   def var lv-wh as handle no-undo.

    lv-wh = focus:next-tab-item.
    do while lv-wh:sensitive = no :
       lv-wh = lv-wh:next-tab-item.
    end.
    apply "entry" to lv-wh.
    return no-apply.
    */

    return .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.cas-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.cas-no V-table-Win
ON LEAVE OF rfqitem.cas-no IN FRAME F-Main /* Packing Code */
DO:
    find item where item.company = rfqitem.company and
                    item.i-no = self:screen-value
              no-lock no-error.
    if avail item and item.mat-type = "C" 
    then assign /*rfqitem.cas-cost:Screen-value = */
                              rfqitem.cas-cnt:Screen-value = string(item.box-case)
                              rfqitem.cas-len:Screen-value = string(item.case-l)
                              rfqitem.cas-wid:Screen-value = string(item.case-w)
                              rfqitem.cas-dep:Screen-value = string(item.case-d)
                              rfqitem.cas-pal:Screen-value = string(item.case-pall)
                              rfqitem.cas-wt:Screen-value = string(item.avg-w)         
                              .
    else if lastkey <> -1 and rfqitem.cas-no:screen-value <> "" then do:
    {&methods/lValidateError.i YES}
         message "Invalid Packing Code. Try Help." view-as alert-box error.
         return  no-apply.
    {&methods/lValidateError.i NO}
    end.                           

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.tr-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.tr-no V-table-Win
ON LEAVE OF rfqitem.tr-no IN FRAME F-Main /* Unit# */
DO:
    find item where item.company = rfqitem.company and
                    item.i-no = self:screen-value
              no-lock no-error.
    if avail item and item.mat-type = "D"
    then assign /*rfqitem.cas-cost:Screen-value = */
                              rfqitem.tr-len:Screen-value = string(item.case-l)
                              rfqitem.tr-wid:Screen-value = string(item.case-w)
                              rfqitem.tr-dep:Screen-value = string(item.case-d)
                              .
    else if lastkey <> -1 and rfqitem.tr-no:screen-value <> "" then do:
     {&methods/lValidateError.i YES}
         message "Invalid Unit#. Try Help." view-as alert-box error.
         return  no-apply.
     {&methods/lValidateError.i NO}
    end.                           


END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
    session:data-entry-return = true.  /* return key will be like tab key */
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "rfqitem"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfqitem"}

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

  find first shipto where shipto.company = rfqitem.company and
                    shipto.ship-id = rfqitem.ship-id
                    no-lock no-error.
  ship-name:screen-value in frame {&frame-name} = if avail shipto then shipto.ship-name else "".

  find carrier where carrier.company = rfqitem.company and 
                     carrier.loc     = rfqitem.loc and
                     carrier.carrier = rfqitem.carrier
                                 no-lock no-error.
  carr-dscr:screen-value in frame {&frame-name} = if avail carrier then carrier.dscr else "".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /* check mode where in update */
  DEFINE VARIABLE vlChangePages AS LOGICAL NO-UNDO.  
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages.
    RUN dispatch IN THIS-PROCEDURE (IF vlChangePages THEN
                                      'update-record':U
                                    ELSE
                                      'cancel-record':U).
  END.


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

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
{&methods/lValidateError.i YES}{&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */
  do with frame {&frame-name}:
     if rfqitem.cas-no:screen-value <> "" 
        and not can-find(first item where item.company = rfqitem.company and
                                       item.i-no = rfqitem.cas-no:screen-value and
                                       item.mat-type = "C")
     then do:
         message "Invalid Packing Code. Try Help." view-as alert-box error.
         apply "entry" to rfqitem.cas-no.
         return  no-apply.
     end.                           
     if rfqitem.tr-no:screen-value <> "" 
        and not can-find(first item where item.company = rfqitem.company and
                                       item.i-no = rfqitem.tr-no:screen-value and
                                       item.mat-type = "D")
     then do:
         message "Invalid Unit#. Try Help." view-as alert-box error.
         apply "entry" to rfqitem.tr-no.
         return  no-apply.
     end.                           
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
{&methods/lValidateError.i NO}

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
  {src/adm/template/snd-list.i "rfqitem"}

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

