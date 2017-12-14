&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers\itemfgt.w

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
{sys/inc/VAR.i NEW SHARED}

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.
ASSIGN
 cocode = g_company
 locode = g_loc.

{ce/msfcalc.i}

{sys/inc/f16to32.i}

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
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS itemfg.l-score[50] itemfg.t-len ~
itemfg.w-score[50] itemfg.t-wid itemfg.d-score[50] itemfg.t-sqin ~
itemfg.t-sqft itemfg.spare-char-3 
&Scoped-define ENABLED-TABLES itemfg
&Scoped-define FIRST-ENABLED-TABLE itemfg
&Scoped-Define ENABLED-OBJECTS RECT-3 
&Scoped-Define DISPLAYED-FIELDS itemfg.q-ptd itemfg.q-ord-ytd itemfg.u-ord ~
itemfg.q-prod-ptd itemfg.q-prod-ytd itemfg.u-prod itemfg.q-ship-ptd ~
itemfg.q-ship-ytd itemfg.u-ship itemfg.q-inv-ptd itemfg.q-inv-ytd ~
itemfg.u-inv itemfg.ytd-msf itemfg.lyytd-msf itemfg.l-score[50] ~
itemfg.t-len itemfg.w-score[50] itemfg.t-wid itemfg.d-score[50] ~
itemfg.t-sqin itemfg.t-sqft itemfg.spare-char-3 
&Scoped-define DISPLAYED-TABLES itemfg
&Scoped-define FIRST-DISPLAYED-TABLE itemfg
&Scoped-Define DISPLAYED-OBJECTS ld-ptd-msf fi_blank-len v-factor-item ~
fi_blank-wid fiSlsRepName 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS v-factor-item 

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
DEFINE VARIABLE fiSlsRepName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fi_blank-len AS DECIMAL FORMAT "->>,>>9.999<<<":U INITIAL 0 
     LABEL "Blank Length" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_blank-wid AS DECIMAL FORMAT "->>,>>9.999<<<":U INITIAL 0 
     LABEL "Blank Width" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE ld-ptd-msf AS DECIMAL FORMAT "->>>,>>>,>>9" INITIAL 0 
     LABEL "Total MSF" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 14.52.

DEFINE VARIABLE v-factor-item AS LOGICAL INITIAL no 
     LABEL "Factor Invoice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     itemfg.q-ptd AT ROW 2.67 COL 23 COLON-ALIGNED
          LABEL "Quantity Ordered" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-ord-ytd AT ROW 2.67 COL 47 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.u-ord AT ROW 2.67 COL 72 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-prod-ptd AT ROW 3.62 COL 23 COLON-ALIGNED FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-prod-ytd AT ROW 3.62 COL 47 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.u-prod AT ROW 3.62 COL 72 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-ship-ptd AT ROW 4.57 COL 23 COLON-ALIGNED FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-ship-ytd AT ROW 4.57 COL 47 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.u-ship AT ROW 4.57 COL 72 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-inv-ptd AT ROW 5.52 COL 23 COLON-ALIGNED
          LABEL "Quantity Invoiced" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.q-inv-ytd AT ROW 5.52 COL 47 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.u-inv AT ROW 5.52 COL 72 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     ld-ptd-msf AT ROW 6.48 COL 23 COLON-ALIGNED HELP
          "Enter period-to-date MSF"
     itemfg.ytd-msf AT ROW 6.48 COL 47 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.lyytd-msf AT ROW 6.48 COL 72 COLON-ALIGNED NO-LABEL FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     itemfg.l-score[50] AT ROW 8.62 COL 30 COLON-ALIGNED
          LABEL "Box Length" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.t-len AT ROW 8.62 COL 72 COLON-ALIGNED
          LABEL "Style Sq. In. Length" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.w-score[50] AT ROW 9.57 COL 30 COLON-ALIGNED
          LABEL "Box Width" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.t-wid AT ROW 9.57 COL 72 COLON-ALIGNED
          LABEL "Style Sq. In. Width" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.d-score[50] AT ROW 10.52 COL 30 COLON-ALIGNED
          LABEL "Box Depth" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.t-sqin AT ROW 10.52 COL 72 COLON-ALIGNED
          LABEL "Total Square Inches" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     itemfg.t-sqft AT ROW 11.48 COL 72 COLON-ALIGNED
          LABEL "Total Sq Feet" FORMAT "->>,>>9.999<<"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     fi_blank-len AT ROW 11.95 COL 30 COLON-ALIGNED WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     v-factor-item AT ROW 12.67 COL 74
     fi_blank-wid AT ROW 12.95 COL 30 COLON-ALIGNED WIDGET-ID 4
     itemfg.spare-char-3 AT ROW 14.33 COL 30 COLON-ALIGNED HELP
          "" WIDGET-ID 6
          LABEL "Sales Rep"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     fiSlsRepName AT ROW 14.33 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "P T D" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 32
          FONT 6
     "Y T D" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.95 COL 57
          FONT 6
     "LAST YR." VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.95 COL 78
          FONT 6
     RECT-3 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.itemfg
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
         HEIGHT             = 18.33
         WIDTH              = 105.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN itemfg.d-score[50] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fiSlsRepName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_blank-len IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_blank-wid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.l-score[50] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ld-ptd-msf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN itemfg.lyytd-msf IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg.q-inv-ptd IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg.q-inv-ytd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.q-ord-ytd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.q-prod-ptd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.q-prod-ytd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.q-ptd IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg.q-ship-ptd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.q-ship-ytd IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.spare-char-3 IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN itemfg.t-len IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.t-sqft IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.t-sqin IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.t-wid IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.u-inv IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.u-ord IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN itemfg.u-prod IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.u-ship IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX v-factor-item IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN itemfg.w-score[50] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN itemfg.ytd-msf IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
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

&Scoped-define SELF-NAME itemfg.spare-char-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.spare-char-3 V-table-Win
ON HELP OF itemfg.spare-char-3 IN FRAME F-Main /* Sales Rep */
DO:
  run windows/l-sman.w (g_company, output char-val).
  if char-val ne "" THEN    
      itemfg.spare-char-3:screen-value = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.spare-char-3 V-table-Win
ON LEAVE OF itemfg.spare-char-3 IN FRAME F-Main /* Sales Rep */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-sman NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.spare-char-3 V-table-Win
ON VALUE-CHANGED OF itemfg.spare-char-3 IN FRAME F-Main /* Sales Rep */
DO:
  DO WITH FRAME {&FRAME-NAME}:

    FIND FIRST sman NO-LOCK WHERE sman.company EQ cocode
                              AND sman.sman    EQ itemfg.spare-char-3:SCREEN-VALUE NO-ERROR.
    IF AVAIL sman THEN
      fiSlsRepName:SCREEN-VALUE = sman.sname.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME itemfg.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL itemfg.t-wid V-table-Win
ON LEAVE OF itemfg.t-wid IN FRAME F-Main /* Style Sq. In. Width */
DO:
    message "Do you want to calculate Sq Inches?" view-as alert-box question
            button yes-no update ll-ans as log.
    if ll-ans then itemfg.t-sqin:screen-value = string(dec(itemfg.t-len:screen-value) *
                                                       dec(itemfg.t-wid:screen-value) ).         


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
  cocode = g_company.
  session:data-entry-return = yes.
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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  itemfg.t-sqft = if v-corr then itemfg.t-sqin * .007
                            else itemfg.t-sqin / 144 .

  
   ASSIGN itemfg.factored = v-factor-item.

   FIND CURRENT reftable NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLength AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cWidth AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-eb FOR eb.

  /* Code placed here will execute PRIOR to standard behavior. */
  if avail itemfg and g_period <> 0 then ld-ptd-msf = itemfg.ptd-msf[g_period].  
  ASSIGN 
      fi_blank-len = 0
      fi_blank-wid = 0.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL itemfg THEN DO:
/*    FIND FIRST reftable WHERE reftable.reftable EQ "FACTORED"  */
/*                        AND reftable.company  EQ itemfg.company*/
/*                        AND reftable.loc      EQ ""            */
/*                        AND reftable.code     EQ itemfg.i-no   */
/*                        NO-LOCK NO-ERROR.                      */
    IF itemfg.est-no NE "" THEN
        FIND FIRST bf-eb 
            WHERE bf-eb.company EQ itemfg.company
              AND bf-eb.est-no EQ itemfg.est-no
              AND bf-eb.stock-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.
    ASSIGN 
        cWidth = IF AVAIL bf-eb THEN string(bf-eb.t-wid) ELSE string(itemfg.t-wid)
        cLength = IF AVAIL bf-eb THEN string(bf-eb.t-len) ELSE string(itemfg.t-len). 
  END.

  ASSIGN 
      fi_blank-len:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cLength.
      fi_blank-wid:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cWidth.
/*      v-factor-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL reftable THEN reftable.code2 ELSE "NO".*/
      v-factor-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL itemfg THEN STRING(itemfg.factored) ELSE "NO".

  IF v-cecscrn-char = "Decimal" THEN
     ASSIGN
        itemfg.l-score[50]:FORMAT = "->>,>>9.999<<<"
        itemfg.w-score[50]:FORMAT = "->>,>>9.999<<<"
        itemfg.d-score[50]:FORMAT = "->>,>>9.999<<<"
        itemfg.t-len:FORMAT = "->>,>>9.999<<<"
        itemfg.t-wid:FORMAT = "->>,>>9.999<<<"
        itemfg.t-sqin:FORMAT = "->>,>>9.999<<<"
        itemfg.t-sqft:FORMAT = "->>,>>9.999<<<"
        itemfg.l-score[50]:WIDTH = 15.60
        itemfg.w-score[50]:WIDTH = 15.60
        itemfg.d-score[50]:WIDTH = 15.60
        itemfg.t-len:WIDTH = 15.60
        itemfg.t-wid:WIDTH = 15.60
        itemfg.t-sqin:WIDTH = 15.60
        itemfg.t-sqft:WIDTH = 15.60.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var v-item like itemfg.i-no no-undo.
  def var ll-ans as log no-undo.
  def buffer b-itemfg for itemfg.
  def var tmpstore as cha no-undo.  /* for include file ce/updfgdim.i */
  def var i as int no-undo.


  /* Code placed here will execute PRIOR to standard behavior. */
  IF itemfg.est-no NE "" THEN DO:
    MESSAGE "IMPORT Estimate Info (Part#, Unit Count, Style, Die#, Plate#, Dimensions, etc.) for FG?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll-ans.

    IF ll-ans THEN DO:
      MESSAGE "OneItem: " trim(itemfg.i-no) + "(YES) or  ALLITEMs? (NO) "
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans .
      v-item = IF ll-ans THEN itemfg.i-no ELSE "*". 

      SESSION:SET-WAIT-STATE ("general").

      FOR EACH b-itemfg
          WHERE b-itemfg.company      EQ cocode
            AND b-itemfg.i-no         MATCHES v-item
            AND TRIM(b-itemfg.est-no) NE "":

        STATUS DEFAULT "Processing Item: " + trim(b-itemfg.i-no).

        RELEASE eb.
        FIND FIRST est
            WHERE est.company EQ cocode
              AND est.est-no  EQ b-itemfg.est-no
            NO-LOCK NO-ERROR.
        IF AVAIL est THEN
        FOR EACH eb NO-LOCK
            WHERE eb.company  EQ est.company
              AND eb.est-no   EQ est.est-no
              AND eb.stock-no EQ b-itemfg.i-no
            BY eb.form-no:
          LEAVE.
        END.
        IF NOT AVAIL eb AND AVAIL est AND (est.est-type EQ 1 OR est.est-type EQ 5) THEN
        FIND FIRST eb
            WHERE eb.company EQ est.company
              AND eb.est-no  EQ est.est-no
              AND eb.form-no NE 0
            NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN
        FOR EACH eb
            WHERE eb.company  EQ cocode
              AND eb.loc      EQ g_loc
              AND eb.stock-no eq b-itemfg.i-no
            USE-INDEX stock NO-LOCK,
            FIRST est
            WHERE est.company EQ eb.company
              AND est.est-no  EQ eb.est-no
            NO-LOCK
            BY eb.est-no DESC:
          LEAVE.
        END.

        IF AVAIL eb THEN DO:
          ASSIGN
           b-itemfg.cad-no     = CAPS(eb.cad-no)
           b-itemfg.spc-no     = CAPS(eb.spc-no)
           b-itemfg.upc-no     = CAPS(eb.upc-no)
           b-itemfg.plate-no   = CAPS(eb.plate-no)
           b-itemfg.die-no     = CAPS(eb.die-no)
           b-itemfg.cust-no    = CAPS(eb.cust-no)
           b-itemfg.part-no    = CAPS(eb.part-no)
           b-itemfg.case-count = eb.cas-cnt.

          FIND FIRST sys-ctrl
              WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "OECOUNT"
              NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND NOT sys-ctrl.log-fld THEN
            b-itemfg.case-count = IF eb.tr-cnt NE 0 THEN eb.tr-cnt
                                  ELSE (eb.cas-cnt * eb.cas-pal).

          IF b-itemfg.isaset AND eb.form-no EQ 0 THEN
            RUN fg/updsetdm.p (RECID(eb)).
          ELSE DO:
            {sys/inc/updfgdim.i "eb" "b-"}
          END.
        END.
      END.  /* for each b-itemfg */
    END.

    STATUS DEFAULT "".
    SESSION:SET-WAIT-STATE ("").
  END.

  ELSE
  if avail itemfg and itemfg.isaset and
                      can-find(fg-set where fg-set.company EQ g_company and
                                            fg-set.set-no EQ itemfg.i-no AND
                                            fg-set.part-no NE itemfg.i-no)
  then do:
       message "Update Width, Length & Weight via Set Components?"
               view-as alert-box question button yes-no update ll-ans.
       if ll-ans  then run fg/updsetdm.p (recid(itemfg)).      
  end.

  FIND CURRENT itemfg NO-LOCK.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ENABLE v-factor-item WITH FRAME {&FRAME-NAME}.
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
  RUN GET-ATTRIBUTE("FIELDS-ENABLED":U).
  IF RETURN-VALUE = "YES":U THEN
  DO:
    MESSAGE "Would you like to save changes before changing pages?":U
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlChangePages as log.
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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-sman NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disABLE v-factor-item WITH FRAME {&FRAME-NAME}.


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
  {src/adm/template/snd-list.i "itemfg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman V-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    /* Blank is valid */
    IF itemfg.spare-char-3:SCREEN-VALUE EQ "" THEN 
      RETURN.

    FIND FIRST sman NO-LOCK WHERE sman.company EQ cocode
                              AND sman.sman    EQ itemfg.spare-char-3:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
      MESSAGE "Invalid Sales Rep, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO itemfg.spare-char-3.
      RETURN ERROR.
    END.
    ELSE DO:
      fiSlsRepName:SCREEN-VALUE = sman.sname.
    END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

