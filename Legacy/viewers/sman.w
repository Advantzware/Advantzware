&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/sman.w

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

{custom/gcompany.i}

&SCOPED-DEFINE proc-enable proc-enable

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
&Scoped-define EXTERNAL-TABLES sman
&Scoped-define FIRST-EXTERNAL-TABLE sman


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sman.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS sman.sname sman.commbasis sman.scomm ~
sman.netpct sman.territory sman.hasMembers
&Scoped-define ENABLED-TABLES sman
&Scoped-define FIRST-ENABLED-TABLE sman
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS sman.sman sman.sname sman.commbasis ~
sman.scomm sman.netpct sman.territory sman.hasMembers
&Scoped-define DISPLAYED-TABLES sman
&Scoped-define FIRST-DISPLAYED-TABLE sman
&Scoped-Define DISPLAYED-OBJECTS terr_dscr F1 btnGroupLookup

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS sman.sman 
&Scoped-define DISPLAY-FIELD sman.territory 
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
DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE terr_dscr AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 6.2.


DEFINE BUTTON btnGroupLookup 
     IMAGE-UP FILE "Graphics/16x16/find.bmp":U
     LABEL "" 
     SIZE 4.4 BY 1.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sman.sman AT ROW 1.24 COL 17.6 COLON-ALIGNED
          LABEL "SalesGrp"
          VIEW-AS FILL-IN 
          SIZE 8.4 BY 1
          BGCOLOR 15 FONT 4
     sman.sname AT ROW 1.24 COL 34.2 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
          BGCOLOR 15 FONT 4
     sman.commbasis AT ROW 2.43 COL 17.6 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "Gross Profit","G",
                     "Selling Price","S",
                     "Margin","M"
          DROP-DOWN-LIST
          SIZE 22 BY 1
          FONT 4
     sman.scomm AT ROW 3.62 COL 17.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     sman.netpct AT ROW 3.62 COL 36 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          FONT 4
     sman.territory AT ROW 4.81 COL 17.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     terr_dscr AT ROW 4.81 COL 27 COLON-ALIGNED NO-LABEL
     sman.hasMembers AT ROW 6.00 COL 18.6 COLON-ALIGNED
         LABEL "Has Group Members"
         VIEW-AS TOGGLE-BOX
         SIZE 30 BY .81
     btnGroupLookup AT ROW 6.00 COL 51
     F1 AT ROW 4.81 COL 26 NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.sman
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
         HEIGHT             = 5
         WIDTH              = 65.
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

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN sman.sman IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN sman.sname IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sman.territory IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN terr_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGroupLookup IN FRAME Corr
                                                                         */
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

&Scoped-define SELF-NAME sman.territory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman.territory V-table-Win
ON LEAVE OF sman.territory IN FRAME F-Main /* Sales Territory */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman.hasMembers V-table-Win
ON VALUE-CHANGED OF sman.hasMembers IN FRAME F-Main /* HsaMembers */
DO:
    DEFINE VARIABLE v-date-change-reason AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-added-rowid AS ROWID NO-UNDO .
    
    IF sman.hasMembers:SCREEN-VALUE EQ "Yes" AND AVAIL sman THEN DO:
        RUN oe/d-salegrp.w (INPUT sman.rec_key, INPUT sman.sman:SCREEN-VALUE )  .
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGroupLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupLookup V-table-Win
ON CHOOSE OF btnGroupLookup IN FRAME F-Main
DO:
  
    IF sman.hasMembers:SCREEN-VALUE EQ "Yes" AND AVAIL sman THEN DO:
        RUN oe/d-salegrp.w (INPUT sman.rec_key, INPUT sman.sman:SCREEN-VALUE )  .
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
  {src/adm/template/row-list.i "sman"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sman"}

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
  def var page-no as int no-undo.
  def var i as int no-undo.
  def var ls-prev-sman like sman.sman no-undo.
  def buffer bf-mtx for smanmtrx.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  ls-prev-sman = sman.sman.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  if adm-adding-record then do:
     for each custype where custype.company EQ sman.company no-lock:
         page-no = 1.
        /* old database
         create sman-mtx.
         assign sman-mtx.company    = sman.company
               sman-mtx.sman       = sman.sman
               sman-mtx.custype    = custype.custype
               sman-mtx.type-comm  = sman.scomm
               sman-mtx.custype-no = page-no.
        */
         IF NOT CAN-FIND(FIRST smanmtrx WHERE smanmtrx.company = sman.company
                            AND smanmtrx.sman = sman.sman AND smanmtrx.custype = custype.custype
                            AND smanmtrx.procat = '')
         THEN DO:
             CREATE smanmtrx.
             ASSIGN smanmtrx.company = sman.company
                smanmtrx.sman = sman.sman
                smanmtrx.custype = custype.custype
                smanmtrx.commbasis = sman.commbasis
                smanmtrx.netpct = sman.netpct
                smanmtrx.comm = sman.scomm.
         END.

         i = 1.
        for each fgcat where fgcat.company eq sman.company no-lock:
            /*
            if i gt 10 then do:
               assign  page-no = page-no + 1
                       i = 1.
               create sman-mtx.
               assign sman-mtx.company = sman.company
                   sman-mtx.sman    = sman.sman
                   sman-mtx.custype = custype.custype
                   sman-mtx.type-comm = sman.scomm
                   sman-mtx.custype-no = page-no.
            end.
        
            assign sman-mtx.procat[i] = fgcat.procat
                   sman-mtx.dscr[i]   = fgcat.dscr
                   sman-mtx.comm[i]   = sman.scomm
                   i                  = i + 1.
            */
           CREATE smanmtrx.
           ASSIGN smanmtrx.company = sman.company
                  smanmtrx.sman = sman.sman
                  smanmtrx.custype = custype.custype
                  smanmtrx.commbasis = sman.commbasis
                  smanmtrx.netpct = sman.netpct
                  smanmtrx.comm = sman.scomm
                  smanmtrx.procat = fgcat.procat.

        end.  /* each fgcat */
      end. /* each custype */
  end.  /* adding-record */
  
  else if adm-new-record then do:  /* copy */
       /*for each sman-mtx where sman-mtx.company = sman.company and
                               sman-mtx.sman = ls-prev-sman
                               no-lock:
           create bf-mtx.
           buffer-copy sman-mtx except sman-mtx.sman to bf-mtx.
           assign bf-mtx.sman = sman.sman.
       end.
       */
      FOR EACH smanmtrx NO-LOCK WHERE smanmtrx.company = sman.company AND
                              smanmtrx.sman = ls-prev-sman :
           create bf-mtx.
           buffer-copy smanmtrx except smanmtrx.sman to bf-mtx.
           assign bf-mtx.sman = sman.sman.

      END.
  end.

  DISABLE btnGroupLookup WITH FRAME {&FRAME-NAME}.

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
  {methods/viewers/create/sman.i}

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FOR EACH smanmtrx WHERE
      smanmtrx.company = sman.company AND
      smanmtrx.sman = sman.sman
      EXCLUSIVE-LOCK:
        
      smanmtrx.commbasis = sman.commbasis.

      IF sman.commbasis NE "M" THEN
         smanmtrx.netpct = 0.
  END.
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
  {src/adm/template/snd-list.i "sman"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
    ENABLE btnGroupLookup WITH FRAME {&FRAME-NAME}.

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

