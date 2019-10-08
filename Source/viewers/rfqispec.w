&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
def var char-val as cha no-undo.

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
&Scoped-Define ENABLED-FIELDS rfqitem.i-name rfqitem.part-dscr1 ~
rfqitem.part-dscr2 rfqitem.part-dscr3 rfqitem.stock-no rfqitem.plate-no ~
rfqitem.die-no rfqitem.cad-no rfqitem.upc-no rfqitem.spc-no 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-10 
&Scoped-Define DISPLAYED-FIELDS rfqitem.est-no rfqitem.part-no ~
rfqitem.i-name rfqitem.part-dscr1 rfqitem.part-dscr2 rfqitem.part-dscr3 ~
rfqitem.procat rfqitem.stock-no rfqitem.plate-no rfqitem.die-no ~
rfqitem.cad-no rfqitem.upc-no rfqitem.spc-no 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS cat-desc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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
DEFINE VARIABLE cat-desc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .95
     BGCOLOR 7 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 116 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.est-no AT ROW 1.48 COL 95 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     rfqitem.part-no AT ROW 1.48 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     rfqitem.i-name AT ROW 2.67 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          FONT 4
     rfqitem.part-dscr1 AT ROW 3.67 COL 13 COLON-ALIGNED
          LABEL "Description"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          FONT 4
     rfqitem.part-dscr2 AT ROW 4.67 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          FONT 4
     rfqitem.part-dscr3 AT ROW 5.67 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          FONT 4
     rfqitem.procat AT ROW 7.67 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     rfqitem.stock-no AT ROW 2.91 COL 95 COLON-ALIGNED
          LABEL "F.G Item#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     rfqitem.plate-no AT ROW 3.86 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     rfqitem.die-no AT ROW 4.81 COL 95 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     rfqitem.cad-no AT ROW 5.76 COL 95 COLON-ALIGNED
          LABEL "CAD/Sample#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     rfqitem.upc-no AT ROW 6.71 COL 95 COLON-ALIGNED
          LABEL "UPC#"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     rfqitem.spc-no AT ROW 7.67 COL 95 COLON-ALIGNED
          LABEL "SPC/QC Code"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     cat-desc AT ROW 7.67 COL 25 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         HEIGHT             = 9.67
         WIDTH              = 117.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN rfqitem.cad-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN cat-desc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.est-no IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rfqitem.part-dscr1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.part-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.procat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.spc-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.stock-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rfqitem.upc-no IN FRAME F-Main
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
   case focus:name :
      when "stock-no" then do:
           run windows/l-itemfg.w  (rfqitem.company, "", focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "plate-no" or when "die-no" then do:
           run windows/l-matpr.w  (rfqitem.company,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "cad-no" then do:
           run windows/l-itemfc.w  (rfqitem.company,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "upc-no" then do:     
           run windows/l-itemfu.w  (rfqitem.company,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "spc-no" then do:
           run windows/l-itemfs.w  (rfqitem.company,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.
      when "procat" then do:
           run windows/l-fgcat.w  (rfqitem.company,focus:screen-value, output char-val). 
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
      end.

  
   end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON return OF FRAME F-Main
anywhere
DO:
  /* wiil replaced to use session:data-entry-return = true.  
   def var lv-wh as handle no-undo.
   
   lv-wh = focus:next-tab-item.
   do while lv-wh:sensitive = no :
       lv-wh = lv-wh:next-tab-item.
   end.

   apply "entry" to lv-wh.
   return no-apply. 
   */
   return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.procat V-table-Win
ON LEAVE OF rfqitem.procat IN FRAME F-Main /* Category */
DO:
    FIND fgcat where fgcat.company = rfqitem.company and
                     fgcat.procat = rfqitem.procat:SCREEN-VALUE in frame {&frame-name}
         no-lock no-error.      
    cat-desc = IF NOT AVAILABLE fgcat THEN "" ELSE fgcat.dscr.

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

  FIND fgcat where fgcat.company = rfqitem.company and
                   fgcat.procat = rfqitem.procat  no-lock no-error.      
  cat-desc:screen-value in frame {&frame-name} =
             IF NOT AVAILABLE fgcat THEN "" ELSE fgcat.dscr.

  /* Code placed here will execute AFTER standard behavior.    */

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

