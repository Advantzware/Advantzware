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

  File: addon\viewers\rfqsize.w

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
{cec/descalc.i "new"}

def new shared temp-table formule field formule as dec extent 12.
def var ll-auto-calc-selected as log no-undo.
def var lv-is-corr as log no-undo.
define new shared buffer xritem for rfqitem.
def var k_frac as dec init 6.25 no-undo.
DEF VAR lv-panels AS LOG NO-UNDO.
DEF VAR ld-k-wid-array LIKE rfqitem.k-wid-array2 NO-UNDO.
DEF VAR ld-k-len-array LIKE rfqitem.k-len-array2 NO-UNDO.
DEF TEMP-TABLE tt-array NO-UNDO FIELD tt-dec AS DEC FIELD tt-type AS CHAR.
def new shared var cocode as cha no-undo.

{custom/globdefs.i}

assign cocode = g_company.

{rfq/msfcalc.i}   /* v-corr */
{sys/inc/f16to32.i}

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
&Scoped-Define ENABLED-FIELDS rfqitem.tab-in rfqitem.len rfqitem.wid ~
rfqitem.dep rfqitem.dust rfqitem.fpanel rfqitem.tuck rfqitem.adhesive ~
rfqitem.k-wid rfqitem.k-len rfqitem.lock rfqitem.gluelap rfqitem.lin-in ~
rfqitem.t-wid rfqitem.t-len 
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS rfqitem.part-no rfqitem.i-name ~
rfqitem.style rfqitem.tab-in rfqitem.len rfqitem.wid rfqitem.dep ~
rfqitem.dust rfqitem.fpanel rfqitem.tuck rfqitem.adhesive rfqitem.k-wid ~
rfqitem.k-len rfqitem.lock rfqitem.gluelap rfqitem.lin-in rfqitem.t-wid ~
rfqitem.t-len 
&Scoped-define DISPLAYED-TABLES rfqitem
&Scoped-define FIRST-DISPLAYED-TABLE rfqitem
&Scoped-Define DISPLAYED-OBJECTS lv-sqin style-desc 

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
DEFINE VARIABLE lv-sqin AS DECIMAL FORMAT "->>,>>9.99<<":U INITIAL 0 
     LABEL "Blank Sq.In" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE style-desc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 40 BY .95
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 103.8 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.part-no AT ROW 1.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 7 
     rfqitem.i-name AT ROW 1.24 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 7 
     rfqitem.style AT ROW 2.67 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     rfqitem.tab-in AT ROW 2.67 COL 87 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     rfqitem.len AT ROW 4.33 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.wid AT ROW 4.33 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.dep AT ROW 4.33 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.dust AT ROW 5.29 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.fpanel AT ROW 5.29 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.tuck AT ROW 5.29 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.adhesive AT ROW 6.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     rfqitem.k-wid AT ROW 6.24 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.k-len AT ROW 6.24 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.lock AT ROW 7.19 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.gluelap AT ROW 7.19 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.lin-in AT ROW 7.19 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     rfqitem.t-wid AT ROW 8.14 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     rfqitem.t-len AT ROW 8.14 COL 49 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     lv-sqin AT ROW 8.14 COL 86 COLON-ALIGNED
     style-desc AT ROW 2.67 COL 36 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
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
         HEIGHT             = 9.38
         WIDTH              = 104.4.
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

/* SETTINGS FOR FILL-IN rfqitem.i-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-sqin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.part-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rfqitem.style IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-desc IN FRAME F-Main
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
ON GO OF FRAME F-Main
DO:

   def var out-hdl-str as cha no-undo.
   run get-link-handle in adm-broker-hdl (this-procedure,"TABLEIO-SOURCE",output out-hdl-str).   
   run notify in widget-handle(out-hdl-str) ("update-record"). 
   run enable-auto-calc in widget-handle(out-hdl-str).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
     def var lv-ind like style.industry no-undo.
     def var char-val as cha no-undo.

     case focus:name :
          when "adhesive" then do:
                find style where style.company = rfq.company and
                                 style.style = rfqitem.style
                                 no-lock no-error.   
                if avail style then lv-ind = style.industry.
                else lv-ind = "".  
                run windows/l-item.w (rfqitem.company,lv-ind,"G", focus:screen-value,output char-val).
                if char-val <> "" then do:
                   assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                          .
               /* not sure to update k-wid,len  ======= may be for corrugated
                   find item where item.company = rfqitem.company and
                                   item.i-no = entry(1,char-val)
                                   no-lock no-error.
                   if avail item then 
                        assign rfqitem.k-wid:screen-value in frame {&frame-name} = if item.r-wid <> 0 then string(item.r-wid) else string(item.s-wid)
                               rfqitem.k-len:screen-value in frame {&frame-name} = string(item.s-len) 
                               . 
                 ===============  */              
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
  /* will use session:data-entry-return  not to have widget error  YSK  
    def var lv-wh as widget-handle no-undo.
    lv-wh = focus:next-tab-item.
    apply "entry" to lv-wh.
    return no-apply.
  */
  return.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.dep V-table-Win
ON LEAVE OF rfqitem.dep IN FRAME F-Main /* Depth */
DO:
  IF LASTKEY NE -1 THEN DO:
        IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.dust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.dust V-table-Win
ON LEAVE OF rfqitem.dust IN FRAME F-Main /* Dust Flap */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.fpanel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.fpanel V-table-Win
ON LEAVE OF rfqitem.fpanel IN FRAME F-Main /* Fifth Panel */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.gluelap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.gluelap V-table-Win
ON LEAVE OF rfqitem.gluelap IN FRAME F-Main /* Glue Lap */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.k-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.k-len V-table-Win
ON LEAVE OF rfqitem.k-len IN FRAME F-Main /* DK Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.k-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.k-wid V-table-Win
ON LEAVE OF rfqitem.k-wid IN FRAME F-Main /* DK Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.len V-table-Win
ON LEAVE OF rfqitem.len IN FRAME F-Main /* Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.lin-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.lin-in V-table-Win
ON LEAVE OF rfqitem.lin-in IN FRAME F-Main /* Lin Inches */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.lock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.lock V-table-Win
ON LEAVE OF rfqitem.lock IN FRAME F-Main /* Lock Tab */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.t-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.t-len V-table-Win
ON ENTRY OF rfqitem.t-len IN FRAME F-Main /* Blank Length */
DO:
   DEF VAR ld-total AS DEC NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   IF lv-is-corr AND
      lv-panels THEN DO:

      EMPTY TEMP-TABLE tt-array.

      DO i = 1 TO EXTENT(ld-k-len-array):
        CREATE tt-array.
        ASSIGN
         tt-dec  = ld-k-len-array[i]
         tt-type = lv-k-len-scr-type[i].
      END.
                            /*is 16th*/ 
      RUN cec/d-panels.w (YES, "Length Panels", INPUT-OUTPUT TABLE tt-array).

      i = 0.
      FOR EACH tt-array:
        i = i + 1.
        IF i GT EXTENT(ld-k-len-array) THEN LEAVE.
        ASSIGN
         ld-k-len-array[i]    = tt-dec
         ld-total             = ld-total + ld-k-len-array[i]
         lv-k-len-scr-type[i] = tt-type.
      END.

      SELF:SCREEN-VALUE = STRING({sys/inc/k16.i ld-total}).

      RUN leave-dim (SELF) NO-ERROR.

      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.t-len V-table-Win
ON LEAVE OF rfqitem.t-len IN FRAME F-Main /* Blank Length */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN leave-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.t-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.t-wid V-table-Win
ON ENTRY OF rfqitem.t-wid IN FRAME F-Main /* Blank Width */
DO:
   DEF VAR ld-total AS DEC NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

   IF lv-is-corr AND
      lv-panels THEN DO:

      EMPTY TEMP-TABLE tt-array.

      DO i = 1 TO EXTENT(ld-k-wid-array):

        CREATE tt-array.
        ASSIGN
         tt-dec  = ld-k-wid-array[i]
         tt-type = lv-k-wid-scr-type[i].
      END.

      RUN cec/d-panels.w (YES, "Width Panels", INPUT-OUTPUT TABLE tt-array).

      i = 0.

      FOR EACH tt-array:
        i = i + 1.
        IF i GT EXTENT(ld-k-wid-array) THEN LEAVE.
        ASSIGN
         ld-k-wid-array[i]    = tt-dec
         ld-total             = ld-total + ld-k-wid-array[i]
         lv-k-wid-scr-type[i] = tt-type.
      END.

      SELF:SCREEN-VALUE = STRING({sys/inc/k16.i ld-total}).

      RUN leave-dim (SELF) NO-ERROR.

      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.t-wid V-table-Win
ON LEAVE OF rfqitem.t-wid IN FRAME F-Main /* Blank Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN leave-dim (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.tuck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.tuck V-table-Win
ON LEAVE OF rfqitem.tuck IN FRAME F-Main /* Tuck */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rfqitem.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rfqitem.wid V-table-Win
ON LEAVE OF rfqitem.wid IN FRAME F-Main /* Width */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (FOCUS) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
  session:data-entry-return = true.  /* return key will be like tab key */

  find first sys-ctrl where
       sys-ctrl.company eq cocode AND
       sys-ctrl.name eq "PANELS"
       no-lock no-error.

  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "PANELS"
            sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
            sys-ctrl.log-fld = yes.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  lv-panels = sys-ctrl.log-fld.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-sqin V-table-Win 
PROCEDURE assign-sqin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN lv-sqin.

    IF lv-is-corr THEN
      IF v-corr THEN lv-sqin = lv-sqin / .007.
                ELSE lv-sqin = lv-sqin * 144.

    rfqitem.t-sqin = lv-sqin.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE auto-calc V-table-Win 
PROCEDURE auto-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ll-auto-calc-selected = yes.
   run dispatch ('enable-fields').
   disable rfqitem.t-wid rfqitem.t-len lv-sqin
           with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size V-table-Win 
PROCEDURE calc-blank-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR i as int no-undo.
   DEF VAR j as int no-undo.
   DEF VAR v-score-char like v-lscore-c extent 100.

   /* calc blank W,L SqIn */
   if not avail rfq then find first rfq where rfq.company = rfqitem.company and
                                        rfq.rfq-no = rfqitem.rfq-no
                                        no-lock.        

   if lv-is-corr then do:
      find first reftable
          where reftable.reftable eq "STYFLU"
            and reftable.company  eq rfqitem.style
            and reftable.loc      eq rfqitem.flute
            and reftable.code     eq "DIM-FIT"
          no-lock no-error.

      DO i = 1 TO EXTENT(rfqitem.k-wid-array2):
         ASSIGN
           ld-k-wid-array[i]    = rfqitem.k-wid-array2[i]
           lv-k-wid-scr-type[i] = rfqitem.k-wid-scr-type2[i].
      END.
      DO i = 1 TO EXTENT(rfqitem.k-len-array2):
         ASSIGN
           ld-k-len-array[i]    = rfqitem.k-len-array2[i]
           lv-k-len-scr-type[i] = rfqitem.k-len-scr-type2[i].
      END.

      find style where style.company = rfqitem.company and
                       style.style = rfqitem.style
                       no-lock no-error.
      if avail style and style.type <> "F" then run calc-blank-size2.

      run rfq/u2kinc1c.p (recid(rfq), recid(rfqitem)).
      run rfq/u2kinc2c.p (recid(rfq), recid(rfqitem)).

      find first formule no-lock.
      assign rfqitem.t-wid = (formule.formule[1])
             rfqitem.t-len = (formule.formule[2])
             rfqitem.k-wid-array2 = 0
             rfqitem.k-len-array2 = 0.

      if not lv-panels or style.type = "F" then 
         assign rfqitem.k-wid-array2[1] = rfqitem.t-wid
                rfqitem.k-len-array2[1] = rfqitem.t-len.
      ELSE
      DO:
         run rfq/descalc2.p (recid(rfqitem)).

         DO i = 1 TO EXTENT(rfqitem.k-wid-scr-type2):
           ASSIGN
            rfqitem.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            rfqitem.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" then
            assign  rfqitem.k-wid-array2[1] = rfqitem.t-wid
                    rfqitem.k-len-array2[1] = rfqitem.t-len.
         else do:
           i = 0.
           for each w-box-design-line:
              i = i + 1.
              rfqitem.k-wid-array2[i] = w-box-design-line.wscore-d.
                 {sys/inc/k16bb.i rfqitem.k-wid-array2[i]} 
           end.
           assign  v-score-char    = ""
                   j               = 1.

           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt EXTENT(rfqitem.k-len-array2) then leave.
           end.
           DO i = 1 TO EXTENT(rfqitem.k-len-array2):
              rfqitem.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i rfqitem.k-len-array2[i]}. 
           end.
         end.  /* else v-lscore */
      END.

   end.
   else do:
      run rfq/u2kinc1f.p (recid(rfq), recid(rfqitem)).
      run rfq/u2kinc2f.p (recid(rfq), recid(rfqitem)).
      find first formule no-lock.
      assign rfqitem.t-wid = (formule.formule[1])
             rfqitem.t-len = (formule.formule[2]).
   end.

/* will be displayed automatically for folding or corrware*/ 
   IF AVAIL formule THEN DO:
     rfqitem.t-wid:screen-value in frame {&frame-name} = string(formule.formule[1]).
     rfqitem.t-len:screen-value in frame {&frame-name} = string(formule.formule[2]).

     RUN calc-sqin (formule.formule[7], formule.formule[8], NO).

     RUN assign-sqin.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-blank-size2 V-table-Win 
PROCEDURE calc-blank-size2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   find xritem where recid(xritem) = recid(rfqitem) no-lock.
   {rfq/u2estc.i rfqitem.gluelap 1}

   {rfq/u2estc.i rfqitem.k-wid 2}
   find first item where item.company = rfq.company
                    and item.i-no eq rfqitem.adhesive
                  no-lock no-error.
   if avail item then do:  
            if item.mat-type eq "G" then do:
                    if rfqitem.tab-in:screen-value in frame {&frame-name} eq "In" then do:
                       {rfq/u2estc.i rfqitem.k-len 3}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if rfqitem.tab-in:screen-value in frame {&frame-name} eq "In" then do:
                       {rfq/u2estc.i rfqitem.k-len 5}
                    end.
                    else do:
                       {rfq/u2estc.i rfqitem.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    rfqitem.tab-in:screen-value in frame {&frame-name} = "In".
                    {rfq/u2estc.i rfqitem.k-len 7}
            end.
    end.
    else do:
                 rfqitem.tab-in:screen-value in frame {&frame-name} = "In".
                 {rfq/u2estc.i rfqitem.k-len 7}
    end.

    if rfqitem.len:screen-value in frame {&frame-name} eq rfqitem.wid:screen-value in frame {&frame-name}
    then do:
                 {rfq/u2estc.i rfqitem.k-wid 2 dim-fit}
    end.
    else do:
                 {rfq/u2estc.i rfqitem.k-wid 2}
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-sqin V-table-Win 
PROCEDURE calc-sqin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-dim-w AS DEC NO-UNDO.
  DEF INPUT PARAM ip-dim-l AS DEC NO-UNDO.
  DEF INPUT PARAM ip-frac AS LOG NO-UNDO.

  DEF VAR ld AS DEC EXTENT 3 NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ld[1] = ip-dim-w
     ld[2] = ip-dim-l.

    IF lv-is-corr AND ip-frac THEN DO li = 1 TO EXTENT(ld):
      ld[li] = TRUNC(ld[li],0) + ((ld[li] - TRUNC(ld[li],0)) * 6.25).
    END.

    ld[3] = ld[1] * ld[2].

    IF lv-is-corr THEN
      IF v-corr THEN ld[3] = ld[3] * .007.
                ELSE ld[3] = ld[3] / 144.

    lv-sqin:SCREEN-VALUE = STRING(ld[3]).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-dim V-table-Win 
PROCEDURE leave-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-is-corr THEN DO:
      RUN valid-16ths (ip-focus) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    END.

    RUN calc-sqin (DEC(rfqitem.t-wid:SCREEN-VALUE),
                   DEC(rfqitem.t-len:SCREEN-VALUE),
                   YES). 
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

  DEF VAR viCount AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


  if  lv-is-corr then do:

     {sys/inc/k16bb.i rfqitem.t-wid  } 
     {sys/inc/k16bb.i rfqitem.t-len  } 
     {sys/inc/k16bb.i rfqitem.wid  } 
     {sys/inc/k16bb.i rfqitem.len  } 
     {sys/inc/k16bb.i rfqitem.dep  } 
     {sys/inc/k16bb.i rfqitem.dust  } 
     {sys/inc/k16bb.i rfqitem.fpanel  } 
     {sys/inc/k16bb.i rfqitem.tuck  } 
     {sys/inc/k16bb.i rfqitem.k-wid  } 
     {sys/inc/k16bb.i rfqitem.k-len  } 
     {sys/inc/k16bb.i rfqitem.gluelap  } 
     {sys/inc/k16bb.i rfqitem.lock  } 
     {sys/inc/k16bb.i rfqitem.lin-in  }

  end.

  if ll-auto-calc-selected then do:
     run calc-blank-size.
     /*/*if not lv-is-corr then */
         assign rfqitem.t-len = decimal(rfqitem.t-len:screen-value in frame {&frame-name})
                rfqitem.t-wid = decimal(rfqitem.t-wid:screen-value in frame {&frame-name})
                rfqitem.t-sqin = lv-sqin .   
     */
     ll-auto-calc-selected = no. 
  end.
  ELSE
  DO:
     DO viCount = 1 TO EXTENT(ld-k-wid-array):
        rfqitem.k-wid-scr-type2[viCount] = lv-k-wid-scr-type[viCount].
        IF rfqitem.k-wid-array2[viCount] NE ld-k-wid-array[viCount] THEN
           rfqitem.k-wid-array2[viCount] = ld-k-wid-array[viCount].
     END.

     DO viCount = 1 TO EXTENT(ld-k-len-array):
        rfqitem.k-len-scr-type2[viCount] = lv-k-len-scr-type[viCount].
        IF rfqitem.k-len-array2[viCount] NE ld-k-len-array[viCount] THEN
           rfqitem.k-len-array2[viCount] = ld-k-len-array[viCount].
     END.
  END.

  RUN assign-sqin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   DISABLE lv-sqin WITH FRAME {&FRAME-NAME}.

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
  RUN reset-labels.

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
  ENABLE lv-sqin WITH FRAME {&FRAME-NAME}.

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
  DEF VAR hd1 AS HANDLE NO-UNDO.
  DEF VAR hd2 AS HANDLE NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  /* validation */
  IF lv-is-corr THEN DO: 
    ASSIGN
     hd1 = FRAME {&FRAME-NAME}:HANDLE
     hd1 = hd1:FIRST-CHILD
     hd2 = hd1:FIRST-CHILD.

     DO WHILE VALID-HANDLE(hd2):
       IF hd2:TYPE EQ "fill-in"      AND
          hd2:DATA-TYPE EQ "decimal" AND
          hd2:NAME NE "lv-sqin"      THEN DO:

         RUN valid-16ths (hd2) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
       END.

       hd2 = hd2:NEXT-SIBLING.
     END.       
  END.  /* lv-is-corr */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  find first eb where
       eb.company = rfqitem.company AND
       eb.est-no = rfqitem.est-no AND
       eb.form-no = rfqitem.form-no AND
       eb.blank-no = rfqitem.blank-no
       EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL eb THEN
  DO:
     ASSIGN
        eb.t-wid = decimal(rfqitem.t-wid:screen-value in frame {&frame-name})
        eb.t-len = decimal(rfqitem.t-len:screen-value in frame {&frame-name}).
     RELEASE eb.
  END.

  find item where item.company = rfqitem.company and
                  item.i-no = rfqitem.adhesive:screen-value in frame {&frame-name}
                   no-lock no-error.

  assign /* not any more - field name is changed to STAMP from Glue 
            rfqitem.leaf[3] = rfqitem.adhesive:screen-value in frame {&frame-name}
            rfqitem.leaf-dscr[3] = if avail item then item.i-name else ""
            rfqitem.leaf-w[3] = decimal(rfqitem.k-wid:screen-value in frame {&frame-name})
            rfqitem.leaf-l[3] = decimal(rfqitem.k-len:screen-value in frame {&frame-name})
         */  
         /*
            rfqitem.t-sqin = if lv-sqin:label = "Blank Sq.Ft" 
                             then decimal(lv-sqin:screen-value in frame {&frame-name}) / 0.007
                             else decimal(lv-sqin:screen-value in frame {&frame-name})
           */
            . 


  run dispatch ('display-fields').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-labels V-table-Win 
PROCEDURE reset-labels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var cocode as cha no-undo.
  DEF VAR li AS INT NO-UNDO.

  if not avail rfqitem then return.

  find style where style.company = rfqitem.company and
                   style.style = rfqitem.style
                   no-lock no-error.
  if avail style then
     style-desc:screen-value in frame {&frame-name} = style.dscr.   

  if avail style and style.industry = "2" /* Corrugate Box */ 
  then do:
       assign rfqitem.dust:label in frame {&frame-name} = "Top/Dust Flap"
              rfqitem.fpanel:label in frame {&frame-name} = "Bottom Flap"
              rfqitem.adhesive:label in frame {&frame-name} = "Joint Material"
              rfqitem.k-wid:label in frame {&frame-name} = "Scores on Width"
              rfqitem.k-len:label in frame {&frame-name} = "Scores on Length"
              rfqitem.gluelap:label in frame {&frame-name} = "Joint Tab Width"
              rfqitem.lin-in:label in frame {&frame-name} = "Joint Length"
              lv-sqin:label in frame {&frame-name} = "Blank Sq.Ft"
              lv-sqin:screen-value in frame {&frame-name} = string(if v-corr then rfqitem.t-sqin * 0.007 else rfqitem.t-sqin / 144).

        IF v-cecscrn-char NE "Decimal" THEN
           ASSIGN
              rfqitem.len:format in frame {&frame-name} = ">>9.99"
              rfqitem.wid:format in frame {&frame-name} = ">>9.99"
              rfqitem.dep:format in frame {&frame-name} = ">>9.99"
              rfqitem.dust:format in frame {&frame-name} = ">>9.99"
              rfqitem.fpanel:format in frame {&frame-name} = ">>9.99"
              rfqitem.tuck:format in frame {&frame-name} = ">>9.99"
              rfqitem.k-wid:format in frame {&frame-name} = ">>9.99"
              rfqitem.k-len:format in frame {&frame-name} = ">>9.99"
              rfqitem.gluelap:format in frame {&frame-name} = ">>9.99"
              rfqitem.lock:format in frame {&frame-name} = ">>9.99"
              rfqitem.lin-in:format in frame {&frame-name} = ">>9.99"
              rfqitem.t-len:format in frame {&frame-name} = ">>9.99"
              rfqitem.t-wid:format in frame {&frame-name} = ">>9.99".
        ELSE
           ASSIGN
              rfqitem.len:format in frame {&frame-name} = ">>9.999999"
              rfqitem.wid:format in frame {&frame-name} = ">>9.999999"
              rfqitem.dep:format in frame {&frame-name} = ">>9.999999"
              rfqitem.dust:format in frame {&frame-name} = ">>9.999999"
              rfqitem.fpanel:format in frame {&frame-name} = ">>9.999999"
              rfqitem.tuck:format in frame {&frame-name} = ">>9.999999"
              rfqitem.k-wid:format in frame {&frame-name} = ">>9.999999"
              rfqitem.k-len:format in frame {&frame-name} = ">>9.999999"
              rfqitem.gluelap:format in frame {&frame-name} = ">>9.999999"
              rfqitem.lock:format in frame {&frame-name} = ">>9.999999"
              rfqitem.lin-in:format in frame {&frame-name} = ">>9.999999"
              rfqitem.t-len:format in frame {&frame-name} = ">>9.999999"
              rfqitem.t-wid:format in frame {&frame-name} = ">>9.999999".

        assign lv-is-corr = yes
               rfqitem.tab-in:hidden in frame {&frame-name} = NO
               rfqitem.len:screen-value = /*string(trunc(rfqitem.len,0) + ((rfqitem.len - trunc(rfqitem.len,0)) / K_FRAC) ). */
                                          string( {sys/inc/k16.i rfqitem.len } )
               rfqitem.wid:screen-value = string( {sys/inc/k16.i rfqitem.wid } )
               rfqitem.dep:screen-value = string({sys/inc/k16.i rfqitem.dep    } )
               rfqitem.dust:screen-value = string({sys/inc/k16.i rfqitem.dust   } )
               rfqitem.fpanel:screen-value = string({sys/inc/k16.i rfqitem.fpanel } )
               rfqitem.tuck:screen-value = string({sys/inc/k16.i rfqitem.tuck   } )
               rfqitem.k-wid:screen-value = string({sys/inc/k16.i rfqitem.k-wid  } )
               rfqitem.k-len:screen-value = string({sys/inc/k16.i rfqitem.k-len  } )
               rfqitem.gluelap:screen-value = string({sys/inc/k16.i rfqitem.gluelap   } )
               rfqitem.lock:screen-value = string({sys/inc/k16.i rfqitem.lock   } )
               rfqitem.lin-in:screen-value = string({sys/inc/k16.i rfqitem.lin-in } )
               rfqitem.t-len:screen-value = string({sys/inc/k16.i rfqitem.t-len  } )
               rfqitem.t-wid:screen-value = string({sys/inc/k16.i rfqitem.t-wid  } ).

        DO li = 1 TO EXTENT(rfqitem.k-wid-array2):
           ASSIGN
              ld-k-wid-array[li]    = rfqitem.k-wid-array2[li]
              lv-k-wid-scr-type[li] = rfqitem.k-wid-scr-type2[li].
        END.
        DO li = 1 TO EXTENT(rfqitem.k-len-array2):
           ASSIGN
              ld-k-len-array[li]    = rfqitem.k-len-array2[li]
              lv-k-len-scr-type[li] = rfqitem.k-len-scr-type2[li].
        END.
  end.
  else do:
       assign rfqitem.dust:label in frame {&frame-name} = "Dust Flap"
              rfqitem.fpanel:label in frame {&frame-name} = "5th Panel"
              rfqitem.adhesive:label in frame {&frame-name} = "Adhesive"
              rfqitem.k-wid:label in frame {&frame-name} = "DK Width"
              rfqitem.k-len:label in frame {&frame-name} = "DK Length"
              rfqitem.gluelap:label in frame {&frame-name} = "Glue Lap"
              rfqitem.lin-in:label in frame {&frame-name} = "Lin Inches"
              lv-sqin:label in frame {&frame-name} = "Blank Sq.In"
              lv-sqin:screen-value in frame {&frame-name} = string(rfqitem.t-sqin)
              lv-is-corr = no
              rfqitem.len:format in frame {&frame-name} = ">9.99999"
              rfqitem.wid:format in frame {&frame-name} = ">9.99999"
              rfqitem.dep:format in frame {&frame-name} = ">9.99999"
              rfqitem.dust:format in frame {&frame-name} = ">9.99999"
              rfqitem.fpanel:format in frame {&frame-name} = ">9.99999"
              rfqitem.tuck:format in frame {&frame-name} = ">9.99999"
              rfqitem.k-wid:format in frame {&frame-name} = ">9.99999"
              rfqitem.k-len:format in frame {&frame-name} = ">9.99999"
              rfqitem.gluelap:format in frame {&frame-name} = ">9.99999"
              rfqitem.lock:format in frame {&frame-name} = ">9.99999"
              rfqitem.lin-in:format in frame {&frame-name} = ">9.99999"
              rfqitem.t-len:format in frame {&frame-name} = ">9.99999"
              rfqitem.t-wid:format in frame {&frame-name} = ">9.99999"
              rfqitem.tab-in:hidden in frame {&frame-name} = yes.      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-16ths V-table-Win 
PROCEDURE valid-16ths :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-focus AS HANDLE NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ip-focus:SCREEN-VALUE) - TRUNC(DEC(ip-focus:SCREEN-VALUE),0) GE 0.16
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL)                      +
              " may not have more than .15 as decimal," +
              " field is (inches.16ths)..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

