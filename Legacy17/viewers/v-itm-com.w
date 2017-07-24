&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
{custom/globdefs.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/* &scoped-def oe-prmtx-maint oe-prmtx */

{sys/inc/var.i new shared}

ASSIGN
   cocode = g_company
   locode = g_loc.

DEF VAR v-invalid   AS LOG  NO-UNDO.
DEF VAR v-cust-no  AS CHAR NO-UNDO.

&scoped-define proc-enable proc-enable

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
&Scoped-define EXTERNAL-TABLES item-comm
&Scoped-define FIRST-EXTERNAL-TABLE item-comm


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item-comm.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item-comm.cust-no item-comm.i-no ~
item-comm.part-no item-comm.i-name item-comm.set-sales-price ~
item-comm.zz-char[2] item-comm.base-cost item-comm.zz-char[3] ~
item-comm.zz-dec[1] item-comm.fixed-gross-profit item-comm.overhead-percent ~
item-comm.misc-percent item-comm.freight-percent ~
item-comm.industrial-percent item-comm.warehouse-percent ~
item-comm.comm-rate-percent item-comm.zz-char[1] 
&Scoped-define ENABLED-TABLES item-comm
&Scoped-define FIRST-ENABLED-TABLE item-comm
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS item-comm.cust-no item-comm.i-no ~
item-comm.part-no item-comm.i-name item-comm.set-sales-price ~
item-comm.zz-char[2] item-comm.base-cost item-comm.zz-char[3] ~
item-comm.zz-dec[1] item-comm.fixed-gross-profit item-comm.overhead-percent ~
item-comm.misc-percent item-comm.freight-percent ~
item-comm.industrial-percent item-comm.warehouse-percent ~
item-comm.comm-rate-percent item-comm.zz-char[1] 
&Scoped-define DISPLAYED-TABLES item-comm
&Scoped-define FIRST-DISPLAYED-TABLE item-comm
&Scoped-Define DISPLAYED-OBJECTS TG_lock-base-cost 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS TG_lock-base-cost 
&Scoped-define ADM-ASSIGN-FIELDS TG_lock-base-cost 
&Scoped-define DISPLAY-FIELD TG_lock-base-cost 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company|y|y|ASI.oe-prmtx.company
uom||y|ASI.oe-prmtx.uom[1]
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,uom"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 134 BY 16.81.

DEFINE VARIABLE TG_lock-base-cost AS LOGICAL INITIAL no 
     LABEL "Lock Base Cost" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     item-comm.cust-no AT ROW 1.38 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     item-comm.i-no AT ROW 2.38 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     item-comm.part-no AT ROW 3.38 COL 25 COLON-ALIGNED FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     item-comm.i-name AT ROW 4.38 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY 1
     item-comm.set-sales-price AT ROW 5.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.zz-char[2] AT ROW 5.67 COL 52.8 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 8.2 BY 1
     item-comm.base-cost AT ROW 6.76 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.zz-char[3] AT ROW 6.76 COL 52.8 COLON-ALIGNED
          LABEL "UOM" FORMAT "x(4)"
          VIEW-AS FILL-IN 
          SIZE 8.2 BY 1
     TG_lock-base-cost AT ROW 6.95 COL 66 WIDGET-ID 2
     item-comm.zz-dec[1] AT ROW 8.14 COL 25 COLON-ALIGNED HELP
          ""
          LABEL "Rebate%" FORMAT ">>>9.99<"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.fixed-gross-profit AT ROW 9.71 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.overhead-percent AT ROW 10.91 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.misc-percent AT ROW 12.05 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.freight-percent AT ROW 13.19 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.industrial-percent AT ROW 14.33 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.warehouse-percent AT ROW 15.48 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.comm-rate-percent AT ROW 16.62 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     item-comm.zz-char[1] AT ROW 1.38 COL 43.2 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
           VIEW-AS TEXT 
          SIZE 56.6 BY 1
     RECT-1 AT ROW 1.19 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.item-comm
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 17.71
         WIDTH              = 136.8.
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

ASSIGN 
       item-comm.i-name:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN item-comm.part-no IN FRAME F-Main
   EXP-FORMAT                                                           */
ASSIGN 
       item-comm.part-no:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TG_lock-base-cost IN FRAME F-Main
   NO-ENABLE 1 2 4                                                      */
/* SETTINGS FOR FILL-IN item-comm.zz-char[1] IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN item-comm.zz-char[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN item-comm.zz-char[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN item-comm.zz-dec[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
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
/*   def var char-val as cha no-undo.                                                    */
/*   def var lv-handle as handle no-undo.                                                */
/*                                                                                       */
/*                                                                                       */
/*   case focus:name :                                                                   */
/*     when "uom" then do:                                                               */
/*       run windows/l-stduom.w (cocode, uom-list, focus:screen-value, output char-val). */
/*       if char-val ne "" then                                                          */
/*         focus:screen-value in frame {&frame-name} = entry(1,char-val).                */
/*     end.                                                                              */
/*                                                                                       */
/*     otherwise do:                                                                     */
/*       lv-handle = focus:handle.                                                       */
/*       run applhelp.p.                                                                 */
/*                                                                                       */
/*       if g_lookup-var ne "" then lv-handle:screen-value = g_lookup-var.               */
/*                                                                                       */
/*       apply "entry" to lv-handle.                                                     */
/*       return no-apply.                                                                */
/*     end.                                                                              */
/*   end case.                                                                           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.cust-no V-table-Win
ON HELP OF item-comm.cust-no IN FRAME F-Main /* Cust. # */
DO:
   DEF VAR v-char-val AS CHAR NO-UNDO.

   RUN windows/l-cust.w (cocode,FOCUS:SCREEN-VALUE,OUTPUT v-char-val).

   IF v-char-val <> "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,v-char-val) THEN 
      ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
   RETURN NO-APPLY.          

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.cust-no V-table-Win
ON LEAVE OF item-comm.cust-no IN FRAME F-Main /* Cust. # */
DO: 
   IF LASTKEY <> -1 THEN DO:
      RUN valid-cust-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN NO-APPLY.
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.fixed-gross-profit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.fixed-gross-profit V-table-Win
ON LEAVE OF item-comm.fixed-gross-profit IN FRAME F-Main /* Fixed Gross Profit% */
OR RETURN OF item-comm.fixed-gross-profit
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.freight-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.freight-percent V-table-Win
ON LEAVE OF item-comm.freight-percent IN FRAME F-Main /* Freight% */
OR RETURN OF item-comm.freight-percent
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.i-no V-table-Win
ON HELP OF item-comm.i-no IN FRAME F-Main /* Item No */
DO:
   DEF VAR v-char-val AS CHAR FORMAT "X(10)" NO-UNDO.

   RUN windows/l-itemfg.w (cocode, "", FOCUS:SCREEN-VALUE, OUTPUT v-char-val).

   IF v-char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,v-char-val) THEN DO:
      FOCUS:SCREEN-VALUE = ENTRY(1,v-char-val).
      FIND FIRST itemfg WHERE itemfg.company EQ cocode
                          AND itemfg.i-no    EQ item-comm.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN
         ASSIGN
            item-comm.i-name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.i-name
            item-comm.part-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = itemfg.part-no.

      APPLY "value-changed" TO FOCUS.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.i-no V-table-Win
ON LEAVE OF item-comm.i-no IN FRAME F-Main /* Item No */
DO:
   IF LASTKEY <> -1 THEN DO:
      RUN valid-i-no NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN NO-APPLY.
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.industrial-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.industrial-percent V-table-Win
ON LEAVE OF item-comm.industrial-percent IN FRAME F-Main /* Industrial% */
OR RETURN OF item-comm.industrial-percent
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.misc-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.misc-percent V-table-Win
ON LEAVE OF item-comm.misc-percent IN FRAME F-Main /* Misc% */
OR RETURN OF item-comm.misc-percent
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.overhead-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.overhead-percent V-table-Win
ON LEAVE OF item-comm.overhead-percent IN FRAME F-Main /* Overhead% */
OR RETURN OF item-comm.overhead-percent
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.warehouse-percent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.warehouse-percent V-table-Win
ON LEAVE OF item-comm.warehouse-percent IN FRAME F-Main /* Warehouse% */
OR RETURN OF item-comm.warehouse-percent
DO:
   RUN calculate-commission-rate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.zz-char[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.zz-char[2] V-table-Win
ON LEAVE OF item-comm.zz-char[2] IN FRAME F-Main /* UOM */
DO:
   IF LASTKEY <> -1 THEN DO:
      RUN valid-set-sales-price-uom NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN NO-APPLY.
      END.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-comm.zz-char[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-comm.zz-char[3] V-table-Win
ON LEAVE OF item-comm.zz-char[3] IN FRAME F-Main /* UOM */
DO:
   IF LASTKEY <> -1 THEN DO:
      RUN valid-base-cost-uom NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN NO-APPLY.
      END.
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  session:data-entry-return = yes.

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
  {src/adm/template/row-list.i "item-comm"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item-comm"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculate-commission-rate V-table-Win 
PROCEDURE calculate-commission-rate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR v-comm-rate AS DECI NO-UNDO.
DEF VAR v-fixed-gross-profit AS DECI NO-UNDO.
DEF VAR v-freight-percent  AS DECI NO-UNDO.
DEF VAR v-industrial-percent AS DECI NO-UNDO.
DEF VAR v-misc-percent       AS DECI NO-UNDO.
DEF VAR v-overhead-percent   AS DECI NO-UNDO.
DEF VAR v-warehouse-percent  AS DECI NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
   ASSIGN {&displayed-objects}.
   ASSIGN
      v-fixed-gross-profit = DECI(item-comm.fixed-gross-profit:SCREEN-VALUE)
      v-freight-percent    = DECI(item-comm.freight-percent:SCREEN-VALUE)  
      v-industrial-percent = DECI(item-comm.industrial-percent:SCREEN-VALUE)
      v-misc-percent       = DECI(item-comm.misc-percent:SCREEN-VALUE)  
      v-overhead-percent   = DECI(item-comm.overhead-percent:SCREEN-VALUE)
      v-warehouse-percent  = DECI(item-comm.warehouse-percent:SCREEN-VALUE).

   IF v-fixed-gross-profit > 0 THEN DO:
      v-comm-rate =  v-fixed-gross-profit
                  - (v-freight-percent 
                  +  v-industrial-percent
                  +  v-misc-percent
                  +  v-overhead-percent
                  +  v-warehouse-percent).
   END.

END.
item-comm.comm-rate-percent:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(v-comm-rate). 
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
  item-comm.zz-char[2] = CAPS(item-comm.zz-char[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  item-comm.zz-char[3] = CAPS(item-comm.zz-char[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  IF TG_lock-base-cost:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES" THEN
     item-comm.zz-char[4] = "YES".
  ELSE
     item-comm.zz-char[4] = "NO".

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
  disable all with frame {&frame-name}.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   item-comm.company = cocode.
   item-comm.cust-no = v-cust-no.


  IF adm-adding-record THEN DO:
     ASSIGN
        item-comm.create-date = TODAY
        item-comm.create-time = TIME 
        item-comm.create-user-id = USERID("nosweat")
        item-comm.rec_key = STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99") + STRING(DAY(TODAY), "99") + STRING(TIME).
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
/*   ENABLE TG_lock-base-cost WITH FRAME {&FRAME-NAME}. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 IF AVAIL item-comm THEN
  IF item-comm.zz-char[4] = "YES" THEN
     TG_lock-base-cost:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES".
  ELSE
     TG_lock-base-cost:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".


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

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     RETURN NO-APPLY.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     RETURN NO-APPLY.

  RUN valid-set-sales-price-uom NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
     RETURN NO-APPLY.

  disable all with frame {&frame-name}.

  IF adm-adding-record THEN v-cust-no = item-comm.cust-no:SCREEN-VALUE.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

DO WITH FRAME {&FRAME-NAME}.
    ENABLE TG_lock-base-cost.
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
  {src/adm/template/snd-list.i "item-comm"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-base-cost-uom V-table-Win 
PROCEDURE valid-base-cost-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-msg AS CHAR FORMAT "X(15)".

  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
   v-msg = "".

   IF item-comm.zz-char[3]:SCREEN-VALUE = "L" OR
      item-comm.zz-char[3]:SCREEN-VALUE = "LB" OR
      item-comm.zz-char[3]:SCREEN-VALUE = "CS" OR
      item-comm.zz-char[3]:SCREEN-VALUE = "C" OR 
      item-comm.zz-char[3]:SCREEN-VALUE = "EA" OR
      item-comm.zz-char[3]:SCREEN-VALUE = "M" THEN
      v-msg = "".
   ELSE
      v-msg = "is invalid".

   IF v-msg NE "" THEN DO:
      MESSAGE "Sorry, " + TRIM(item-comm.zz-char[3]:LABEL) + " " + TRIM(v-msg)
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item-comm.zz-char[3].
      RETURN ERROR.
   END.
   ELSE
      ASSIGN item-comm.zz-char[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = CAPS(item-comm.zz-char[3]:SCREEN-VALUE).
END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-cust FOR cust.

  {methods/lValidateError.i YES}
   IF NOT CAN-FIND(FIRST cust WHERE cust.company = g_company
                                AND cust.cust-no = item-comm.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
      MESSAGE "Invalid Customer#. " VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   ELSE DO:
      FIND FIRST b-cust WHERE b-cust.company = cocode
                          AND b-cust.cust-no EQ item-comm.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.

      IF AVAIL b-cust THEN DO:
         ASSIGN
            item-comm.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-cust.cust-no
            item-comm.zz-char[1]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-cust.name.
         APPLY "entry" TO item-comm.i-no.
      END.
   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF BUFFER b-item-comm FOR item-comm.
   DEF BUFFER b-itemfg    FOR itemfg.

   DEF VAR v-msg AS CHAR NO-UNDO.

  {methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
      v-msg = "".

      IF item-comm.i-no:SCREEN-VALUE EQ "" THEN 
         v-msg = "may not be spaces".
      ELSE DO:
         FIND FIRST b-itemfg WHERE b-itemfg.company EQ cocode
                               AND b-itemfg.i-no    EQ item-comm.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
         IF NOT AVAIL b-itemfg THEN 
            v-msg = "does not exist".
         ELSE DO:
            IF CAN-FIND(FIRST b-item-comm WHERE b-item-comm.company = cocode
                                            AND b-item-comm.cust-no = item-comm.cust-no:SCREEN-VALUE
                                            AND b-item-comm.i-no    = item-comm.i-no:SCREEN-VALUE
                                            AND ROWID(b-item-comm)  <> ROWID(item-comm)) THEN 
               v-msg = "already exists".
         END.
      END.

      IF v-msg NE "" THEN DO:
         MESSAGE "Sorry, " + TRIM(item-comm.i-no:LABEL) + " " + TRIM(v-msg)
            VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO item-comm.i-no.
         RETURN ERROR.
      END.
      ELSE DO:
         ASSIGN
            item-comm.i-name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-itemfg.i-name
            item-comm.part-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-itemfg.part-no
            .
         IF item-comm.zz-char[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""  THEN
               item-comm.zz-char[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-itemfg.sell-uom. 
         IF item-comm.zz-char[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
               item-comm.zz-char[3]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-itemfg.prod-uom.
      END.
   END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-set-sales-price-uom V-table-Win 
PROCEDURE valid-set-sales-price-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-msg AS CHAR FORMAT "X(15)".

  {methods/lValidateError.i YES}
DO WITH FRAME {&FRAME-NAME}:
   v-msg = "".

   IF item-comm.zz-char[2]:SCREEN-VALUE = "L" OR
      item-comm.zz-char[2]:SCREEN-VALUE = "LB" OR
      item-comm.zz-char[2]:SCREEN-VALUE = "CS" OR
      item-comm.zz-char[2]:SCREEN-VALUE = "C" OR 
      item-comm.zz-char[2]:SCREEN-VALUE = "EA" OR
      item-comm.zz-char[2]:SCREEN-VALUE = "M" THEN
      v-msg = "".
   ELSE
      v-msg = "is invalid".

   IF v-msg NE "" THEN DO:
      MESSAGE "Sorry, " + TRIM(item-comm.zz-char[2]:LABEL) + " " + TRIM(v-msg)
         VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO item-comm.zz-char[2].
      RETURN ERROR.
   END.
   ELSE
      ASSIGN item-comm.zz-char[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} = CAPS(item-comm.zz-char[2]:SCREEN-VALUE).
END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

