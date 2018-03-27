&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/rm-rdtl.w

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
def new shared var cocode    as   char  format "x(3)"  initial "001" no-undo.
def var locode     as   char  format "x(5)"  initial "MAIN" no-undo.
def var v-locode like locode no-undo.
def var char-val as cha no-undo.

ASSIGN cocode = g_company .
 FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.user_id = USERID(LDBNAME(1)) AND
        usercomp.company = cocode AND
        usercomp.loc NE "" AND
        usercomp.loc_default = yes
        NO-ERROR.
    ASSIGN
        cocode = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".  


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rm-rdtl rm-rcpt
&Scoped-define FIRST-EXTERNAL-TABLE rm-rdtl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rm-rdtl, rm-rcpt.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rm-rdtl.s-num rm-rdtl.qty rm-rdtl.cost ~
rm-rdtl.loc rm-rdtl.loc-bin rm-rdtl.tag 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}s-num ~{&FP2}s-num ~{&FP3}~
 ~{&FP1}qty ~{&FP2}qty ~{&FP3}~
 ~{&FP1}cost ~{&FP2}cost ~{&FP3}~
 ~{&FP1}loc ~{&FP2}loc ~{&FP3}~
 ~{&FP1}loc-bin ~{&FP2}loc-bin ~{&FP3}~
 ~{&FP1}tag ~{&FP2}tag ~{&FP3}
&Scoped-define ENABLED-TABLES rm-rdtl
&Scoped-define FIRST-ENABLED-TABLE rm-rdtl
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS rm-rdtl.s-num rm-rdtl.job-no ~
rm-rdtl.job-no2 rm-rdtl.qty rm-rdtl.cost rm-rdtl.loc rm-rdtl.loc-bin ~
rm-rdtl.tag 
&Scoped-Define DISPLAYED-OBJECTS lv_pur-uom cost-uom ext-cost 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ROW-AVAILABLE lv_pur-uom 

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
DEFINE VARIABLE cost-uom AS CHARACTER FORMAT "X(256)":U 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE ext-cost AS DECIMAL FORMAT "->>,>>,>>9.99":U INITIAL 0 
     LABEL "Ext. Cost" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE lv_pur-uom AS CHARACTER FORMAT "x(3)" 
     LABEL "UOM" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 108 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rm-rdtl.s-num AT ROW 1.48 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          FONT 4
     rm-rdtl.job-no AT ROW 1.48 COL 32 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     rm-rdtl.job-no2 AT ROW 1.48 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     rm-rdtl.qty AT ROW 2.57 COL 14 COLON-ALIGNED
          LABEL "Qty"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          FONT 4
     lv_pur-uom AT ROW 2.67 COL 43 COLON-ALIGNED
     rm-rdtl.cost AT ROW 3.52 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          FONT 4
     cost-uom AT ROW 3.62 COL 43 COLON-ALIGNED
     ext-cost AT ROW 4.48 COL 14 COLON-ALIGNED
     rm-rdtl.loc AT ROW 6.24 COL 14 COLON-ALIGNED
          LABEL "Warehouse"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          FGCOLOR 7 FONT 4
     rm-rdtl.loc-bin AT ROW 6.24 COL 32 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          FONT 4
     rm-rdtl.tag AT ROW 6.24 COL 54 COLON-ALIGNED
          LABEL "Tag"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          FONT 4
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.rm-rdtl,ASI.rm-rcpt
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.38
         WIDTH              = 143.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cost-uom IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ext-cost IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rm-rdtl.job-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rm-rdtl.job-no2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rm-rdtl.loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtl.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN lv_pur-uom IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN rm-rdtl.qty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN rm-rdtl.tag IN FRAME F-Main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   case focus:name :
      when "loc" then do:
           run rm/l-loc.w (rm-rdtl.company,focus:screen-value, output char-val).
           if char-val <> "" then do with frame {&frame-name}:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rm-rdtl.loc-bin:screen-value = entry(2,char-val)
                     .
             
           end.
           return no-apply.   
     end.
     when "loc-bin" then do:
           run rm/l-locbin.w (rm-rdtl.company,rm-rdtl.loc:screen-value, output char-val).
           if char-val <> "" then do with frame {&frame-name}:
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                     rm-rdtl.loc:screen-value = entry(2,char-val)
                     rm-rdtl.qty:screen-value = entry(3,char-val)
                     rm-rdtl.tag:screen-value = entry(4,char-val)
                     .
             
           end.
           return no-apply.   
     end.
   end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtl.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtl.cost V-table-Win
ON LEAVE OF rm-rdtl.cost IN FRAME F-Main /* Costs */
DO:
  def var v-qty like rm-rdtl.qty no-undo.  
  
  run rm/convquom.p 
        ("M", "EA", 0, 0, 0, 0, 
         (input rm-rdtl.qty), output v-qty).  
  
  assign ext-cost = v-qty * rm-rdtl.cost. 
  display ext-cost with frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rdtl.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rdtl.qty V-table-Win
ON LEAVE OF rm-rdtl.qty IN FRAME F-Main /* Qty */
DO:
  def var v-qty like rm-rdtl.qty no-undo.  
  
  run rm/convquom.p 
        ("M", "EA", 0, 0, 0, 0, 
         (input rm-rdtl.qty), output v-qty).  
  
  assign ext-cost = v-qty * rm-rdtl.cost. 
  display ext-cost with frame {&frame-name}.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "rm-rdtl"}
  {src/adm/template/row-list.i "rm-rcpt"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rm-rdtl"}
  {src/adm/template/row-find.i "rm-rcpt"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-detail V-table-Win 
PROCEDURE create-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-cost like item.avg-cost no-undo.
  def var v-locbin like item.loc-bin no-undo.
  
  find first item where item.company = rm-rcpt.company and
                        item.i-no = rm-rcpt.i-no
                        no-lock no-error.
  v-cost = if avail item then item.avg-cost else 0.  /* item.last-cost ??? */
  v-locbin = if avail item then item.loc-bin else "".
  if v-locbin = "" then do:
       /* get logic from rm/rm-rdtl.a */
  end.                      
  assign rm-rdtl.company = rm-rcpt.company
         rm-rdtl.r-no    = rm-rcpt.r-no
         rm-rdtl.loc     = rm-rcpt.loc
         rm-rdtl.job-no  = rm-rcpt.job-no
         rm-rdtl.job-no2 = rm-rcpt.job-no2
         rm-rdtl.rita-code = "R"
         rm-rdtl.cost = v-cost
         rm-rdtl.loc-bin = v-locbin.
  display rm-rdtl.job-no rm-rdtl.job-no2 rm-rdtl.loc with frame {&frame-name}.
                   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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
  /*{methods/viewers/create/rm-rdtl.i}
  */
  run create-detail.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable V-table-Win 
PROCEDURE local-disable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
     disable cost-uom lv_pur-uom with frame {&frame-name}. 
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
  def var v-qty like rm-rdtl.qty no-undo.  
  if avail rm-rdtl then   
  run rm/convquom.p 
        ("M", "EA", 0, 0, 0, 0, 
         decimal(rm-rdtl.qty:screen-value in frame {&frame-name}), output v-qty).  
  
  assign ext-cost = v-qty * decimal(rm-rdtl.cost:screen-value in frame {&frame-name}). 
  display ext-cost with frame {&frame-name}.
   
 
 assign lv_pur-uom = rm-rcpt.pur-uom
        cost-uom = rm-rcpt.pur-uom.
 disp lv_pur-uom with frame {&frame-name}  .
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "rm-rdtl"}
  {src/adm/template/snd-list.i "rm-rcpt"}

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


