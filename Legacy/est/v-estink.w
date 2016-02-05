&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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
def var ll-update-pack as log no-undo.
def var ll-unit-calc as log no-undo.
&scoped-define est-pack PACK  /* for disable pack */
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.
{custom/globdefs.i}

def new shared buffer xest for est.
def new shared buffer xef  for ef.
def new shared buffer xeb  for eb.
def var k_frac as dec init 6.25 no-undo.

ASSIGN cocode = g_company
       locode = g_loc.
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
&Scoped-define EXTERNAL-TABLES eb
&Scoped-define FIRST-EXTERNAL-TABLE eb


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR eb.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS eb.i-col eb.i-pass eb.i-coat eb.i-coat-p ~
eb.est-no eb.i-coldscr eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] eb.i-%[1] ~
eb.i-ps[2] eb.i-code[2] eb.i-dscr[2] eb.i-%[2] eb.i-ps[3] eb.i-code[3] ~
eb.i-dscr[3] eb.i-%[3] eb.i-ps[4] eb.i-code[4] eb.i-dscr[4] eb.i-%[4] ~
eb.i-ps[5] eb.i-code[5] eb.i-dscr[5] eb.i-%[5] eb.i-ps[6] eb.i-code[6] ~
eb.i-dscr[6] eb.i-%[6] eb.i-ps[7] eb.i-code[7] eb.i-dscr[7] eb.i-%[7] ~
eb.i-ps[8] eb.i-code[8] eb.i-dscr[8] eb.i-%[8] eb.i-ps[9] eb.i-code[9] ~
eb.i-dscr[9] eb.i-%[9] eb.i-ps[10] eb.i-code[10] eb.i-dscr[10] eb.i-%[10] 
&Scoped-define ENABLED-TABLES eb
&Scoped-define FIRST-ENABLED-TABLE eb
&Scoped-Define ENABLED-OBJECTS RECT-29 
&Scoped-Define DISPLAYED-FIELDS eb.i-col eb.i-pass eb.i-coat eb.i-coat-p ~
eb.est-no eb.i-coldscr eb.i-ps[1] eb.i-code[1] eb.i-dscr[1] eb.i-%[1] ~
eb.i-ps[2] eb.i-code[2] eb.i-dscr[2] eb.i-%[2] eb.i-ps[3] eb.i-code[3] ~
eb.i-dscr[3] eb.i-%[3] eb.i-ps[4] eb.i-code[4] eb.i-dscr[4] eb.i-%[4] ~
eb.i-ps[5] eb.i-code[5] eb.i-dscr[5] eb.i-%[5] eb.i-ps[6] eb.i-code[6] ~
eb.i-dscr[6] eb.i-%[6] eb.i-ps[7] eb.i-code[7] eb.i-dscr[7] eb.i-%[7] ~
eb.i-ps[8] eb.i-code[8] eb.i-dscr[8] eb.i-%[8] eb.i-ps[9] eb.i-code[9] ~
eb.i-dscr[9] eb.i-%[9] eb.i-ps[10] eb.i-code[10] eb.i-dscr[10] eb.i-%[10] 
&Scoped-define DISPLAYED-TABLES eb
&Scoped-define FIRST-DISPLAYED-TABLE eb


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
DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 114 BY 14.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     eb.i-col AT ROW 1.71 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-pass AT ROW 1.71 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat AT ROW 1.71 COL 56 COLON-ALIGNED
          LABEL "Coatings"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-coat-p AT ROW 1.71 COL 77 COLON-ALIGNED
          LABEL "Passes"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.est-no AT ROW 1.71 COL 100 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     eb.i-coldscr AT ROW 2.67 COL 9 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36 BY 1
     eb.i-ps[1] AT ROW 4.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[1] AT ROW 4.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[1] AT ROW 4.81 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[1] AT ROW 4.81 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[2] AT ROW 5.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[2] AT ROW 5.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[2] AT ROW 5.81 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[2] AT ROW 5.81 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[3] AT ROW 6.81 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[3] AT ROW 6.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[3] AT ROW 6.81 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[3] AT ROW 6.81 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[4] AT ROW 7.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[4] AT ROW 7.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[4] AT ROW 7.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[4] AT ROW 7.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[5] AT ROW 8.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[5] AT ROW 8.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[5] AT ROW 8.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[5] AT ROW 8.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[6] AT ROW 9.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[6] AT ROW 9.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[6] AT ROW 9.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[6] AT ROW 9.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     eb.i-ps[7] AT ROW 10.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[7] AT ROW 10.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[7] AT ROW 10.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[7] AT ROW 10.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[8] AT ROW 11.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[8] AT ROW 11.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[8] AT ROW 11.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[8] AT ROW 11.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[9] AT ROW 12.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[9] AT ROW 12.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[9] AT ROW 12.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[9] AT ROW 12.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     eb.i-ps[10] AT ROW 13.91 COL 2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     eb.i-code[10] AT ROW 13.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     eb.i-dscr[10] AT ROW 13.91 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     eb.i-%[10] AT ROW 13.91 COL 77 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     "Code" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.1 COL 15
     "PS" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 4.1 COL 4
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 4.1 COL 81
     "Description" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 4.1 COL 36
     RECT-29 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.eb
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
         WIDTH              = 145.4.
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

/* SETTINGS FOR FILL-IN eb.i-coat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coat-p IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN eb.i-coldscr IN FRAME F-Main
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
ON HELP OF FRAME F-Main
DO:
/*
    def var lv-ind like style.industry no-undo.
    def var lv-handle as handle no-undo.
    def var char-val as cha no-undo.    
    lv-handle = focus:handle.
   
    case focus:name :
         when "i-code" then do:
             find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
             if avail style then lv-ind = style.industry.
             else lv-ind = "".  
             run windows/l-item2.w (eb.company, lv-ind, "I",focus:screen-value, output char-val).
             if char-val <> "" then do:
                  assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                  case focus:index:        
                       when 1 then eb.i-dscr[1]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 2 then eb.i-dscr[2]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 3 then eb.i-dscr[3]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 4 then eb.i-dscr[4]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 5 then eb.i-dscr[5]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 6 then eb.i-dscr[6]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 7 then eb.i-dscr[7]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 8 then eb.i-dscr[8]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 9 then eb.i-dscr[9]:screen-value in frame {&frame-name} = entry(2,char-val) .
                       when 10 then eb.i-dscr[10]:screen-value in frame {&frame-name} = entry(2,char-val) .
                  end case.       
             end.         
             return no-apply.                       
                            
         end.
         when "cas-no" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","C",focus:screen-value, output char-val).
           if char-val <> "" then do:
              find item where item.company = eb.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.cas-cnt:Screen-value = string(item.box-case)
                                        eb.cas-len:Screen-value = string(item.case-l)
                                        eb.cas-wid:Screen-value = string(item.case-w)
                                        eb.cas-dep:Screen-value = string(item.case-d)
                                        eb.cas-pal:Screen-value = string(item.case-pall)
                                        eb.cas-wt:Screen-value = string(item.avg-w)         
                                        .
           end.   
           return no-apply.   
         end.   
         when "tr-no" then do:
           find style where style.company = eb.company and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then lv-ind = style.industry.
           else lv-ind = "".  
           run windows/l-item.w (eb.company,"","D",focus:screen-value, output char-val).
           if char-val <> "" then do:
              find item where item.company = eb.company and
                              item.i-no = entry(1,char-val)
                              no-lock no-error.
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
              if avail item then assign /*eb.cas-cost:Screen-value = */
                                        eb.tr-len:Screen-value = string(item.case-l)
                                        eb.tr-wid:Screen-value = string(item.case-w)
                                        eb.tr-dep:Screen-value = string(item.case-d)
                                        .
           end.
           return no-apply.   
        end.   
        when "carrier" then do:
             run windows/l-carrier.w  
                 (eb.company,eb.loc,focus:screen-value, output char-val).
             if char-val <> "" then
                assign eb.carrier:screen-value in frame {&frame-name} = entry(1,char-val)
                       eb.carr-dscr:screen-value in frame {&frame-name} = entry(2,char-val)
                       .
             return no-apply.              
             
        end.
        when "dest-code" then do:
           run windows/l-delzon.w 
              (eb.company,eb.loc,eb.carrier:screen-value in frame {&frame-name},focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
        end.
    end case.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[10] V-table-Win
ON LEAVE OF eb.i-code[10] IN FRAME F-Main /* Code[10] */
DO:
   if eb.i-ps[10]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[10]:screen-value in frame {&frame-name} = item.i-name.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[1] V-table-Win
ON LEAVE OF eb.i-code[1] IN FRAME F-Main /* Code[1] */
DO:
  if eb.i-ps[1]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[1]:screen-value in frame {&frame-name} = item.i-name.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[2] V-table-Win
ON LEAVE OF eb.i-code[2] IN FRAME F-Main /* Code[2] */
DO:
  if eb.i-ps[2]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[2]:screen-value in frame {&frame-name} = item.i-name.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[3] V-table-Win
ON LEAVE OF eb.i-code[3] IN FRAME F-Main /* Code[3] */
DO:
  if eb.i-ps[3]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[3]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[4] V-table-Win
ON LEAVE OF eb.i-code[4] IN FRAME F-Main /* Code[4] */
DO:
  if eb.i-ps[4]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[4]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[5] V-table-Win
ON LEAVE OF eb.i-code[5] IN FRAME F-Main /* Code[5] */
DO:
  if eb.i-ps[5]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[5]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[6] V-table-Win
ON LEAVE OF eb.i-code[6] IN FRAME F-Main /* Code[6] */
DO:
  if eb.i-ps[6]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[6]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[7] V-table-Win
ON LEAVE OF eb.i-code[7] IN FRAME F-Main /* Code[7] */
DO:
  if eb.i-ps[7]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[7]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[8] V-table-Win
ON LEAVE OF eb.i-code[8] IN FRAME F-Main /* Code[8] */
DO:
  if eb.i-ps[8]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[8]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.i-code[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.i-code[9] V-table-Win
ON LEAVE OF eb.i-code[9] IN FRAME F-Main /* Code[9] */
DO:
  if eb.i-ps[9]:screen-value <> "0" and lastkey <> -1  then do:

     find item where item.company = eb.company and
                   item.i-no = self:screen-value
                   no-lock no-error.
     if avail item and item.mat-type  <> "I" then do:
        message "Invalid Material Type. Try Help." view-as alert-box error.
        return no-apply.
     end.
     if not avail item then do:
        message "Invalid Item. Try Help." view-as alert-box error.
        return no-apply.
     end.
                   
     if avail item then  eb.i-dscr[9]:screen-value in frame {&frame-name} = item.i-name.
  end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
assign cocode = g_company
       locode = g_loc.
{sys/inc/f3help.i}   
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
  {src/adm/template/row-list.i "eb"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "eb"}

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
/*
  do with frame {&frame-name} :
    assign eb.cas-no eb.cas-cost eb.cas-cnt eb.cas-len eb.cas-wid
           eb.cas-dep eb.cas-pal eb.cas-wt
           eb.tr-no
           eb.tr-cost eb.tr-cnt eb.tr-len eb.tr-wid eb.tr-dep
           eb.tr-cas 
           eb.carrier eb.carr-dscr eb.weight-m eb.dest-code
           eb.fr-out-c eb.fr-out-m eb.chg-method
           .
  
  end.
  if ll-unit-calc then do:
     def var lv-cas-pal like eb.cas-pal no-undo.
     def var lv-tr-cnt like eb.tr-cnt no-undo.
     def var lv-error as log no-undo.
     
     find xest where xest.company = eb.company and
                     xest.est-no = eb.est-no .
     find xeb where recid(xeb) = recid(eb).                
     run cec/kpallet.p (recid(xeb), output lv-cas-pal, output lv-tr-cnt, output lv-numstack, output lv-stackcode, output lv-error).
     if lv-error then do:
        message "An error occured while attempting to calculate the number of pallets. "
                skip
                "Please review any previous error messages for more information." 
                 view-as alert-box error.
        assign eb.cas-pal:screen-value in frame {&frame-name} = ?
               eb.tr-cnt:screen-value = ?
               lv-numstack:screen-value = ?
               lv-stackcode:screen-value = ?
               eb.cas-pal = ?
               eb.tr-cnt = ?
               .
        return .           
     end.
     assign eb.cas-pal:screen-value = string(lv-cas-pal)
            eb.tr-cnt:screen-value = string(lv-tr-cnt)
            lv-numstack:screen-value = string(lv-numstack)
            lv-stackcode:screen-value = string(lv-stackcode)
            eb.cas-pal = lv-cas-pal
            eb.tr-cnt = lv-tr-cnt.

  end.

  if ll-update-pack then do:
     eb.tr-cnt = eb.cas-cnt * eb.cas-pal.
     display eb.tr-cnt with frame {&frame-name}.
  end.
  {sys/inc/k16bb.i eb.cas-wid  } 
  {sys/inc/k16bb.i eb.cas-len  } 
  {sys/inc/k16bb.i eb.cas-dep  } 
  {sys/inc/k16bb.i eb.tr-wid  } 
  {sys/inc/k16bb.i eb.tr-len  } 
  {sys/inc/k16bb.i eb.tr-dep  } 

*/
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
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-num-of-code as int no-undo.
   
  /* Code placed here will execute PRIOR to standard behavior. */
/*
     li-num-of-code = 0.
     if eb.i-code[1]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[2]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[3]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[4]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[5]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[6]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[7]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[8]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[9]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.
     if eb.i-code[10]:screen-value in frame {&frame-name} <> "" then li-num-of-code = li-num-of-code + 1.

     if li-num-of-code <> (integer(eb.i-col:screen-value) + 
                          integer(eb.i-coat:screen-value) )
     then do:
          message "Invalid Number of Color and Coating." view-as alert-box.
          apply "entry" to eb.i-col.
          return no-apply.
     end.                     
     if eb.carrier:screen-value <> "" and
        not can-find(first carrier where carrier.carrier = eb.carrier:screen-value)
    then do:
         message "Invalid Carrier. Try Help." view-as alert-box error.
         apply "entry" to eb.carrier.
         return no-apply.
    end.
    if eb.cas-no:screen-value <> "" and
       not can-find(item where item.company = eb.company and item.i-no = eb.cas-no:screen-value)
    then do:
            message "Invalid Packing Code. Try Help." view-as alert-box.
            apply "entry" to eb.cas-no.
            return no-apply.
    end.
    if eb.tr-no:screen-value <> "" and
       not can-find(item where item.company = eb.company and item.i-no = eb.tr-no:screen-value)
    then do:
            message "Invalid Unit#. Try Help." view-as alert-box error.
            apply "entry" to eb.tr-no.
            return no-apply.
    end.
    if decimal(eb.cas-len:screen-value) - trunc(decimal(eb.cas-len:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.cas-len.
      return no-apply.
   end.
    if decimal(eb.cas-wid:screen-value) - trunc(decimal(eb.cas-wid:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.cas-wid.
      return no-apply.
   end.
    if decimal(eb.cas-dep:screen-value) - trunc(decimal(eb.cas-dep:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.cas-dep.
      return no-apply.
   end.
    if decimal(eb.tr-len:screen-value) - trunc(decimal(eb.tr-len:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.tr-len.
      return no-apply.
   end.
    if decimal(eb.tr-wid:screen-value) - trunc(decimal(eb.tr-wid:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.tr-wid.
      return no-apply.
   end.
    if decimal(eb.tr-dep:screen-value) - trunc(decimal(eb.tr-dep:screen-value),0) >= 0.16 
   then do:
      message "Can not have more than .15 as decimal, field is (inches.16ths) "
          view-as alert-box error.
      apply "entry" to eb.tr-dep.
      return no-apply.
   end.

*/
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reset-ink V-table-Win 
PROCEDURE reset-ink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  message "Are you sure you want to reset all ink codes?" view-as alert-box question
         button yes-no update ll-ans as log.
  if not ll-ans then return no-apply.       

  def buffer bf-eb for eb .
/*********** copied from uest3.p ***********/

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.
           
      find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).

         IF style.material[2] NE "" THEN
         find first item where item.company = eb.company and
                    item.i-no = style.material[2] no-lock no-error.

         if avail item then k = integer(style.material[3]).

         IF style.material[6] NE "" THEN
         find first alt-item where alt-item.company  = eb.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.
   
      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (integer(eb.i-col:screen-value in frame {&frame-name})
          + integer(eb.i-coat:screen-value in frame {&frame-name})  ) 
          / integer(eb.i-pass:screen-value in frame {&frame-name})
      counter = 1
      choice = true.
      {sys/inc/roundup.i j}
      
/*    do i = 1 to 10:
       if eb.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
 
      if choice then do i = 1 to 10:
         if i le integer(eb.i-col:screen-value) then do with frame {&frame-name}:
              find item where recid(item) = save_id no-lock no-error.
             /* assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.est-dscr
                     bf-eb.i-%[i]    = k.
             */
             case string(i) :
                when "1" then assign eb.i-ps[1]:screen-value   = string(counter)
                                     eb.i-code[1]:screen-value = item.i-no
                                     eb.i-dscr[1]:screen-value = item.est-dscr
                                     eb.i-%[1]:screen-value    = string(k).
                when "2" then assign eb.i-ps[2]:screen-value   = string(counter)
                                     eb.i-code[2]:screen-value = item.i-no
                                     eb.i-dscr[2]:screen-value = item.est-dscr
                                     eb.i-%[2]:screen-value    = string(k).
                when "3" then assign eb.i-ps[3]:screen-value   = string(counter)
                                     eb.i-code[3]:screen-value = item.i-no
                                     eb.i-dscr[3]:screen-value = item.est-dscr
                                     eb.i-%[3]:screen-value    = string(k).
                when "4" then assign eb.i-ps[4]:screen-value   = string(counter)
                                     eb.i-code[4]:screen-value = item.i-no
                                     eb.i-dscr[4]:screen-value = item.est-dscr
                                     eb.i-%[4]:screen-value    = string(k).
                when "5" then assign eb.i-ps[5]:screen-value   = string(counter)
                                     eb.i-code[5]:screen-value = item.i-no
                                     eb.i-dscr[5]:screen-value = item.est-dscr
                                     eb.i-%[5]:screen-value    = string(k).
                when "6" then assign eb.i-ps[6]:screen-value   = string(counter)
                                     eb.i-code[6]:screen-value = item.i-no
                                     eb.i-dscr[6]:screen-value = item.est-dscr
                                     eb.i-%[6]:screen-value    = string(k).
                when "7" then assign eb.i-ps[7]:screen-value   = string(counter)
                                     eb.i-code[7]:screen-value = item.i-no
                                     eb.i-dscr[7]:screen-value = item.est-dscr
                                     eb.i-%[7]:screen-value    = string(k).
                when "8" then assign eb.i-ps[8]:screen-value   = string(counter)
                                     eb.i-code[8]:screen-value = item.i-no
                                     eb.i-dscr[8]:screen-value = item.est-dscr
                                     eb.i-%[8]:screen-value    = string(k).
                when "9" then assign eb.i-ps[9]:screen-value   = string(counter)
                                     eb.i-code[9]:screen-value = item.i-no
                                     eb.i-dscr[9]:screen-value = item.est-dscr
                                     eb.i-%[9]:screen-value    = string(k).
                when "10" then assign eb.i-ps[10]:screen-value   = string(counter)
                                     eb.i-code[10]:screen-value = item.i-no
                                     eb.i-dscr[10]:screen-value = item.est-dscr
                                     eb.i-%[10]:screen-value    = string(k).             
             end case.
         end.
         else if (i > integer(eb.i-col:screen-value)) and
                 (i <= (integer(eb.i-col:screen-value) + 
                       integer(eb.i-coat:screen-value)))
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
         /*     assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = alt-item.i-no
                     bf-eb.i-dscr[i] = alt-item.est-dscr
                     bf-eb.i-%[i]    = 100.
           */
              case string(i) :
                when "1" then assign eb.i-ps[1]:screen-value   = string(counter)
                                     eb.i-code[1]:screen-value = alt-item.i-no
                                     eb.i-dscr[1]:screen-value = alt-item.est-dscr
                                     eb.i-%[1]:screen-value    = "100".
                when "2" then assign eb.i-ps[2]:screen-value   = string(counter)
                                     eb.i-code[2]:screen-value = alt-item.i-no
                                     eb.i-dscr[2]:screen-value = alt-item.est-dscr
                                     eb.i-%[2]:screen-value    = "100".
                when "3" then assign eb.i-ps[3]:screen-value   = string(counter)
                                     eb.i-code[3]:screen-value = alt-item.i-no
                                     eb.i-dscr[3]:screen-value = alt-item.est-dscr
                                     eb.i-%[3]:screen-value    = "100".
                when "4" then assign eb.i-ps[4]:screen-value   = string(counter)
                                     eb.i-code[4]:screen-value = alt-item.i-no
                                     eb.i-dscr[4]:screen-value = alt-item.est-dscr
                                     eb.i-%[4]:screen-value    = "100".
                when "5" then assign eb.i-ps[5]:screen-value   = string(counter)
                                     eb.i-code[5]:screen-value = alt-item.i-no
                                     eb.i-dscr[5]:screen-value = alt-item.est-dscr
                                     eb.i-%[5]:screen-value    = "100".
                when "6" then assign eb.i-ps[6]:screen-value   = string(counter)
                                     eb.i-code[6]:screen-value = alt-item.i-no
                                     eb.i-dscr[6]:screen-value = alt-item.est-dscr
                                     eb.i-%[6]:screen-value    = "100".
                when "7" then assign eb.i-ps[7]:screen-value   = string(counter)
                                     eb.i-code[7]:screen-value = alt-item.i-no
                                     eb.i-dscr[7]:screen-value = alt-item.est-dscr
                                     eb.i-%[7]:screen-value    = "100".
                when "8" then assign eb.i-ps[8]:screen-value   = string(counter)
                                     eb.i-code[8]:screen-value = alt-item.i-no
                                     eb.i-dscr[8]:screen-value = alt-item.est-dscr
                                     eb.i-%[8]:screen-value    = "100".
                when "9" then assign eb.i-ps[9]:screen-value   = string(counter)
                                     eb.i-code[9]:screen-value = alt-item.i-no
                                     eb.i-dscr[9]:screen-value = alt-item.est-dscr
                                     eb.i-%[9]:screen-value    = "100".
                when "10" then assign eb.i-ps[10]:screen-value   = string(counter)
                                     eb.i-code[10]:screen-value = alt-item.i-no
                                     eb.i-dscr[10]:screen-value = alt-item.est-dscr
                                     eb.i-%[10]:screen-value    = "100".             
             end.                   
         end.
         else if (i >  integer(eb.i-col:screen-value) + 
                       integer(eb.i-coat:screen-value) )
         then do:
        /*    assign bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  */
              case string(i) :
                   when "1" then assign eb.i-ps[1]:screen-value   = "0"
                                        eb.i-code[1]:screen-value = ""
                                        eb.i-dscr[1]:screen-value = ""
                                        eb.i-%[1]:screen-value    = "0".
                   when "2" then assign eb.i-ps[2]:screen-value   = "0"
                                        eb.i-code[2]:screen-value = ""
                                        eb.i-dscr[2]:screen-value = ""
                                        eb.i-%[2]:screen-value    = "0".
                   when "3" then assign eb.i-ps[3]:screen-value   = "0"
                                        eb.i-code[3]:screen-value = ""
                                        eb.i-dscr[3]:screen-value = ""
                                        eb.i-%[3]:screen-value    = "0".
                   when "4" then assign eb.i-ps[4]:screen-value   = "0"
                                        eb.i-code[4]:screen-value = ""
                                        eb.i-dscr[4]:screen-value = ""
                                        eb.i-%[4]:screen-value    = "0".
                   when "5" then assign eb.i-ps[5]:screen-value   = "0"
                                        eb.i-code[5]:screen-value = ""
                                        eb.i-dscr[5]:screen-value = ""
                                        eb.i-%[5]:screen-value    = "0".
                   when "6" then assign eb.i-ps[6]:screen-value   = "0"
                                        eb.i-code[6]:screen-value = ""
                                        eb.i-dscr[6]:screen-value = ""
                                        eb.i-%[6]:screen-value    = "0".
                   when "7" then assign eb.i-ps[7]:screen-value   = "0"
                                        eb.i-code[7]:screen-value = ""
                                        eb.i-dscr[7]:screen-value = ""
                                        eb.i-%[7]:screen-value    = "0".
                   when "8" then assign eb.i-ps[8]:screen-value   = "0"
                                        eb.i-code[8]:screen-value = ""
                                        eb.i-dscr[8]:screen-value = ""
                                        eb.i-%[8]:screen-value    = "0".
                   when "9" then assign eb.i-ps[9]:screen-value   = "0"
                                        eb.i-code[9]:screen-value = ""
                                        eb.i-dscr[9]:screen-value = ""
                                        eb.i-%[9]:screen-value    = "0".
                   when "10" then assign eb.i-ps[10]:screen-value   = "0"
                                        eb.i-code[10]:screen-value = ""
                                        eb.i-dscr[10]:screen-value = ""
                                        eb.i-%[10]:screen-value    = "0".

              end case.       
                     
         end.
         if i modulo j = 0 then counter = counter + 1.
         if counter > integer(eb.i-pass:screen-value) then counter = integer(eb.i-pass:screen-value).
      
      end.

   find bf-eb where recid(bf-eb) = recid(eb).
   assign bf-eb.i-ps[1] = int(eb.i-ps[1]:screen-value in frame {&frame-name})
          bf-eb.i-ps[2] = int(eb.i-ps[2]:screen-value in frame {&frame-name})
          bf-eb.i-ps[3] = int(eb.i-ps[3]:screen-value in frame {&frame-name})
          bf-eb.i-ps[4] = int(eb.i-ps[4]:screen-value in frame {&frame-name})
          bf-eb.i-ps[5] = int(eb.i-ps[5]:screen-value in frame {&frame-name})
          bf-eb.i-ps[6] = int(eb.i-ps[6]:screen-value in frame {&frame-name})
          bf-eb.i-ps[7] = int(eb.i-ps[7]:screen-value in frame {&frame-name})
          bf-eb.i-ps[8] = int(eb.i-ps[8]:screen-value in frame {&frame-name})
          bf-eb.i-ps[9] = int(eb.i-ps[9]:screen-value in frame {&frame-name})
          bf-eb.i-ps[10] = int(eb.i-ps[10]:screen-value in frame {&frame-name})
          bf-eb.i-code[1] = eb.i-code[1]:screen-value 
          bf-eb.i-code[2] = eb.i-code[2]:screen-value 
          bf-eb.i-code[3] = eb.i-code[3]:screen-value 
          bf-eb.i-code[4] = eb.i-code[4]:screen-value 
          bf-eb.i-code[5] = eb.i-code[5]:screen-value 
          bf-eb.i-code[6] = eb.i-code[6]:screen-value 
          bf-eb.i-code[7] = eb.i-code[7]:screen-value 
          bf-eb.i-code[8] = eb.i-code[8]:screen-value 
          bf-eb.i-code[9] = eb.i-code[9]:screen-value 
          bf-eb.i-code[10] = eb.i-code[10]:screen-value 
          bf-eb.i-dscr[1] = eb.i-dscr[1]:screen-value 
          bf-eb.i-dscr[2] = eb.i-dscr[2]:screen-value 
          bf-eb.i-dscr[3] = eb.i-dscr[3]:screen-value 
          bf-eb.i-dscr[4] = eb.i-dscr[4]:screen-value 
          bf-eb.i-dscr[5] = eb.i-dscr[5]:screen-value 
          bf-eb.i-dscr[6] = eb.i-dscr[6]:screen-value 
          bf-eb.i-dscr[7] = eb.i-dscr[7]:screen-value 
          bf-eb.i-dscr[8] = eb.i-dscr[8]:screen-value 
          bf-eb.i-dscr[9] = eb.i-dscr[9]:screen-value 
          bf-eb.i-dscr[10] = eb.i-dscr[10]:screen-value 
          bf-eb.i-%[1] = int(eb.i-%[1]:screen-value )
          bf-eb.i-%[2] = int(eb.i-%[2]:screen-value )
          bf-eb.i-%[3] = int(eb.i-%[3]:screen-value )
          bf-eb.i-%[4] = int(eb.i-%[4]:screen-value )
          bf-eb.i-%[5] = int(eb.i-%[5]:screen-value )
          bf-eb.i-%[6] = int(eb.i-%[6]:screen-value )
          bf-eb.i-%[7] = int(eb.i-%[7]:screen-value )
          bf-eb.i-%[8] = int(eb.i-%[8]:screen-value )
          bf-eb.i-%[9] = int(eb.i-%[9]:screen-value )
          bf-eb.i-%[10] = int(eb.i-%[10]:screen-value )
          .
   
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
  {src/adm/template/snd-list.i "eb"}

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

