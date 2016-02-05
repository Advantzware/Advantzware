&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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
/*{methods/defines/hndldefs.i} */
DEFINE {&NEW} SHARED VARIABLE Persistent-Handle AS HANDLE.
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.
/*{methods/prgsecur.i}*/

{custom/gcompany.i}
{custom/gloc.i}
/* style-maint preprocedure is for enable/disble variables */
&global-define style-maint Corr

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
&Scoped-define EXTERNAL-TABLES style flute
&Scoped-define FIRST-EXTERNAL-TABLE style


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR style, flute.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS style.dscr style.type style.design-no ~
style.royalty style.material[1] style.m-code[1] style.material[2] ~
style.m-code[2] style.material[3] style.m-code[3] style.material[4] ~
style.m-code[4] style.material[5] style.m-code[5] style.material[6] ~
style.m-code[6] style.material[7] style.m-code[7] 
&Scoped-define ENABLED-TABLES style
&Scoped-define FIRST-ENABLED-TABLE style
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-7 RECT-8 RECT-9 
&Scoped-Define DISPLAYED-FIELDS style.style style.dscr style.type ~
style.design-no style.royalty flute.code style.material[1] style.m-code[1] ~
style.m-dscr[1] style.material[2] style.m-code[2] style.m-dscr[2] ~
style.material[3] style.m-code[3] style.m-dscr[3] style.material[4] ~
style.m-code[4] style.m-dscr[4] style.material[5] style.m-code[5] ~
style.m-dscr[5] style.material[6] style.m-code[6] style.m-dscr[6] ~
style.material[7] style.m-code[7] style.m-dscr[7] 
&Scoped-define DISPLAYED-TABLES style flute
&Scoped-define FIRST-DISPLAYED-TABLE style
&Scoped-define SECOND-DISPLAYED-TABLE flute
&Scoped-Define DISPLAYED-OBJECTS ld-joint-tab ld-blank-width ld-glue-in ~
ld-glue-out ld-stitch-in ld-stitch-out ld-tape-score 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS style.style 
&Scoped-define ADM-ASSIGN-FIELDS style.m-dscr[1] style.m-dscr[2] ~
style.m-dscr[3] style.m-dscr[4] style.m-dscr[5] style.m-dscr[6] ~
style.m-dscr[7] 

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
DEFINE VARIABLE ld-blank-width AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Blank Width Score" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-glue-in AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Glue Tab In" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-glue-out AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Glue Tab Out" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-joint-tab AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Joint Tab Width" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-stitch-in AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Stitch Tab In" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-stitch-out AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Stitch Tab Out" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE ld-tape-score AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Tape Score Total" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     BGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 142 BY 11.19.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 7.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 7.62.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 50 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     style.style AT ROW 1.48 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FONT 4
     style.dscr AT ROW 1.48 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     style.type AT ROW 1.48 COL 69 COLON-ALIGNED HELP
          "'B'ox, 'D'ie Cut, 'F'oam, 'P'artition, 'R'oll, or 'W'ood"
          LABEL "Type" FORMAT "!"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     style.design-no AT ROW 1.48 COL 88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     style.royalty AT ROW 1.48 COL 108 COLON-ALIGNED
          LABEL "Markup$"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     flute.code AT ROW 1.48 COL 130 COLON-ALIGNED
          LABEL "Flute"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     ld-joint-tab AT ROW 4.57 COL 25 COLON-ALIGNED
     style.material[1] AT ROW 4.57 COL 62 COLON-ALIGNED
          LABEL "Board"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[1] AT ROW 4.57 COL 91 COLON-ALIGNED
          LABEL "1"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[1] AT ROW 4.57 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-blank-width AT ROW 5.52 COL 25 COLON-ALIGNED
     style.material[2] AT ROW 5.52 COL 62 COLON-ALIGNED
          LABEL "Ink"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[2] AT ROW 5.52 COL 91 COLON-ALIGNED
          LABEL "2"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[2] AT ROW 5.52 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-glue-in AT ROW 6.48 COL 25 COLON-ALIGNED
     style.material[3] AT ROW 6.48 COL 62 COLON-ALIGNED
          LABEL "Ink Cov %"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[3] AT ROW 6.48 COL 91 COLON-ALIGNED
          LABEL "3"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[3] AT ROW 6.48 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-glue-out AT ROW 7.43 COL 25 COLON-ALIGNED
     style.material[4] AT ROW 7.43 COL 62 COLON-ALIGNED
          LABEL "Adder"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[4] AT ROW 7.43 COL 91 COLON-ALIGNED
          LABEL "4"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[4] AT ROW 7.43 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-stitch-in AT ROW 8.38 COL 25 COLON-ALIGNED
     style.material[5] AT ROW 8.38 COL 62 COLON-ALIGNED
          LABEL "Label"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[5] AT ROW 8.38 COL 91 COLON-ALIGNED
          LABEL "5"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     style.m-dscr[5] AT ROW 8.38 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-stitch-out AT ROW 9.33 COL 25 COLON-ALIGNED
     style.material[6] AT ROW 9.33 COL 62 COLON-ALIGNED
          LABEL "Coating"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[6] AT ROW 9.33 COL 91 COLON-ALIGNED
          LABEL "6"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[6] AT ROW 9.33 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     ld-tape-score AT ROW 10.29 COL 25 COLON-ALIGNED
     style.material[7] AT ROW 10.29 COL 62 COLON-ALIGNED
          LABEL "Joint Glue"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     style.m-code[7] AT ROW 10.29 COL 91 COLON-ALIGNED
          LABEL "7"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FONT 4
     style.m-dscr[7] AT ROW 10.29 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     RECT-19 AT ROW 1 COL 1
     RECT-7 AT ROW 4.1 COL 3
     RECT-8 AT ROW 4.1 COL 45
     RECT-9 AT ROW 4.1 COL 87
     "DEFAULT MATERIAL CODES" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 3.86 COL 47
     "DEFAULT MACHINE ROUTING" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 3.86 COL 93
     "DEFAULT FLUTE DEMSNS" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 3.86 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.style,ASI.flute
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
         HEIGHT             = 22.33
         WIDTH              = 150.2.
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

/* SETTINGS FOR FILL-IN flute.code IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ld-blank-width IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-glue-in IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-glue-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-joint-tab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-stitch-in IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-stitch-out IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ld-tape-score IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style.m-code[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-code[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.m-dscr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.m-dscr[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN style.material[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[2] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[3] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[4] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[5] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[6] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.material[7] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.royalty IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN style.style IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN style.type IN FRAME F-Main
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
   def var char-val as cha no-undo.   
   def var ls-cur-val as cha no-undo.
   def var lv-foam as log no-undo.
   def var lv-icode as cha no-undo.
   def var lv-mat-type as cha no-undo.
   def var lv-rowid as rowid no-undo.


   case focus:name :
        when 'design-no' then do:
             run windows/l-boxdes.w (focus:screen-value, output char-val).
             if char-val <> "" then focus:screen-value = entry(1,char-val).    
        end.
        when 'code' then do:
           run windows/l-flute.w (style.company,output char-val).
           if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.   

        end.
        when 'material' then do:
             case focus:index :
                  when 1 then do:  /* board */
                         def var lv-ind like style.industry no-undo.
                         ls-cur-val = focus:screen-value.
                         if avail style then lv-ind = style.industry.
                         else lv-ind = "".  
                         if avail style and style.type:screen-value = "f" then  /* foam */
                            run windows/l-boardf.w (style.company,lv-ind,ls-cur-val,output char-val).
                         else run windows/l-board1.w (eb.company,lv-ind,focus:screen-value, output lv-rowid).
                         FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
                         IF AVAIL ITEM AND ITEM.i-no NE FOCUS:SCREEN-VALUE THEN
                           assign focus:screen-value in frame {&frame-name} = item.i-no
                           .
                        return no-apply.                  
                  end.
                  when 4 then do:  /* adder */
                    if avail style then assign lv-ind = style.industry
                                               lv-foam = style.type:screen-value = "F" .
                    else assign lv-ind = ""
                                lv-foam = no.
                    run windows/l-boarda.w (style.company,lv-ind, focus:screen-value, output char-val).
                    if char-val <> "" then 
                        assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                      .
                    return no-apply.

                  end.
                  /*=====????
                  when 2 or when 5 or when 6 or when 7 then do:
                     if avail style then assign lv-ind = style.industry
                                                lv-foam = style.type:screen-value = "F" .
                     else assign lv-ind = ""
                                 lv-foam = no
                                 .
                     lv-mat-type = if lv-foam then "1234" else "BPR".             
                     lv-icode = "B".    
                  /*   do while true:
                       message "Please enter item code ('R'eal, 'E'st or 'B'oth) " update lv-icode .
                       if index("REB", lv-icode) > 0 then leave. 
                     end.
                  */  
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.

                  end.
                  */
                  when 2 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-mat-type =  "I".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 5 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "WLF".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 6 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "V".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.
                  when 7 then do:
                     if avail style then assign lv-ind = style.industry.
                     else assign lv-ind = ""                     .
                     lv-ind = "".
                     lv-mat-type =  "GTS".
                     run windows/l-itmsty.w (style.company, lv-ind, lv-mat-type, focus:screen-value,lv-foam,lv-icode,
                                      output char-val).
                     if char-val <> "" then 
                         assign focus:screen-value in frame {&frame-name} = entry(1,char-val)
                         .
                     return no-apply.
                  end.

             end case.
        end.  /* material*/
        when 'm-code' then do:
             run windows/l-mach.w (style.company, gloc, focus:screen-value, output char-val).
             if char-val <> "" then 
                case focus:index :
                     when 1 then assign style.m-code[1]:screen-value in frame {&frame-name} = entry(1,char-val)
                                        style.m-dscr[1]:screen-value = entry(2,char-val).
                     when 2 then assign style.m-code[2]:screen-value = entry(1,char-val)
                                        style.m-dscr[2]:screen-value = entry(2,char-val).
                     when 3 then assign style.m-code[3]:screen-value = entry(1,char-val)
                                        style.m-dscr[3]:screen-value = entry(2,char-val).
                     when 4 then assign style.m-code[4]:screen-value = entry(1,char-val)
                                        style.m-dscr[4]:screen-value = entry(2,char-val).
                     when 5 then assign style.m-code[5]:screen-value = entry(1,char-val)
                                        style.m-dscr[5]:screen-value = entry(2,char-val).
                     when 6 then assign style.m-code[6]:screen-value = entry(1,char-val)
                                        style.m-dscr[6]:screen-value = entry(2,char-val).
                     when 7 then assign style.m-code[7]:screen-value = entry(1,char-val)
                                        style.m-dscr[7]:screen-value = entry(2,char-val).

                end case.
        end.  /* m-code */
   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.design-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.design-no V-table-Win
ON LEAVE OF style.design-no IN FRAME F-Main /* Design # */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first box-design-hdr where /*box-design-hdr.company = style.company */
                     box-design-hdr.design-no = int(self:screen-value))
     then do:
        message "Invalid Box Design Number. Try help." view-as alert-box.
        return no-apply.
     end.                  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-blank-width
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-blank-width V-table-Win
ON ENTRY OF ld-blank-width IN FRAME F-Main /* Blank Width Score */
DO:
   def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label, style.style:screen-value in frame {&frame-name},
                      flute.code, "2", output ld-total   )                  
                      .
  
   self:screen-value = string(ld-total).
   apply "entry" to ld-glue-in in frame {&frame-name}.
   return no-apply.
      
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-blank-width V-table-Win
ON LEAVE OF ld-blank-width IN FRAME F-Main /* Blank Width Score */
DO:
  /* apply "entry" to ld-glue-in in frame {&frame-name}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-glue-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-in V-table-Win
ON ENTRY OF ld-glue-in IN FRAME F-Main /* Glue Tab In */
DO:
     def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label,style.style:screen-value in frame {&frame-name},
                      flute.code, "3", output ld-total
                     )   .
                                     
   self:screen-value = string(ld-total).
   apply "entry" to ld-glue-out in frame {&frame-name}.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-in V-table-Win
ON LEAVE OF ld-glue-in IN FRAME F-Main /* Glue Tab In */
DO:
  /* apply "entry" to ld-glue-out in frame {&frame-name}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-glue-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-out V-table-Win
ON ENTRY OF ld-glue-out IN FRAME F-Main /* Glue Tab Out */
DO:
     def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label,style.style:screen-value in frame {&frame-name},
                      flute.code, "4", output ld-total
                     )                   .
                     
   self:screen-value = string(ld-total).
   apply "entry" to ld-stitch-in in frame {&frame-name}.
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-glue-out V-table-Win
ON LEAVE OF ld-glue-out IN FRAME F-Main /* Glue Tab Out */
DO:
  /*  apply "enter" to ld-stitch-in in frame {&frame-name}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-stitch-in
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-in V-table-Win
ON ENTRY OF ld-stitch-in IN FRAME F-Main /* Stitch Tab In */
DO:
     def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label,style.style:screen-value in frame {&frame-name},
                      flute.code, "5", output ld-total
                     )                   .
   self:screen-value = string(ld-total).
                     .
   apply "entry" to ld-stitch-out in frame {&frame-name}.
   return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-in V-table-Win
ON LEAVE OF ld-stitch-in IN FRAME F-Main /* Stitch Tab In */
DO:
  /*  apply "entry" to ld-stitch-out in frame {&frame-name}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-stitch-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-out V-table-Win
ON ENTRY OF ld-stitch-out IN FRAME F-Main /* Stitch Tab Out */
DO:
     def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label,style.style:screen-value in frame {&frame-name},
                      flute.code, "6", output ld-total
                     )                   .
   self:screen-value = string(ld-total).
                     .
  apply "entry" to ld-tape-score in frame {&frame-name}.
  return no-apply.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-stitch-out V-table-Win
ON LEAVE OF ld-stitch-out IN FRAME F-Main /* Stitch Tab Out */
DO:
  /*apply "entry" to ld-tape-score in frame {&frame-name}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-tape-score
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-tape-score V-table-Win
ON ENTRY OF ld-tape-score IN FRAME F-Main /* Tape Score Total */
DO:
    def var lv-flute as cha no-undo.
   def var ld-total as dec no-undo.
   run cec/stymatx.w (self:label, style.style:screen-value in frame {&frame-name},
                      flute.code, "7", output ld-total
                     )       .
                                 
   self:screen-value = string(ld-total).
   apply "entry" to style.material[1] in frame {&frame-name}    .
   return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[1] V-table-Win
ON LEAVE OF style.m-code[1] IN FRAME F-Main /* 1 */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[2] V-table-Win
ON LEAVE OF style.m-code[2] IN FRAME F-Main /* 2 */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[3] V-table-Win
ON LEAVE OF style.m-code[3] IN FRAME F-Main /* 3 */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[4] V-table-Win
ON LEAVE OF style.m-code[4] IN FRAME F-Main /* 4 */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[5] V-table-Win
ON LEAVE OF style.m-code[5] IN FRAME F-Main /* 5 */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[6] V-table-Win
ON LEAVE OF style.m-code[6] IN FRAME F-Main /* 6 */
DO:
      if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.m-code[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.m-code[7] V-table-Win
ON LEAVE OF style.m-code[7] IN FRAME F-Main /* 7 */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= self:screen-value )
    then do:
       message "Invalid Machine Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[1] V-table-Win
ON LEAVE OF style.material[1] IN FRAME F-Main /* Board */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first item where item.company = style.company and 
                  /*item.industry = style.industry and*/
                  item.i-no = self:screen-value )
    then do:
       message "Invalid Board Code. Try help please." view-as alert-box.
       return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[2] V-table-Win
ON LEAVE OF style.material[2] IN FRAME F-Main /* Ink */
DO:
    if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first item where item.company = style.company and 
             /*item.industry = style.industry and*/
             item.i-no = self:screen-value )
    then do:
       message "Invalid Ink Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[4] V-table-Win
ON LEAVE OF style.material[4] IN FRAME F-Main /* Adder */
DO:
   if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first item where item.company = style.company and 
                 /* item.industry = style.industry and */
                  item.i-no = self:screen-value )
    then do:
       message "Invalid Adder Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[5] V-table-Win
ON LEAVE OF style.material[5] IN FRAME F-Main /* Label */
DO:
   if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first item where item.company = style.company and 
                 /* item.industry = style.industry and */
                  item.i-no = self:screen-value )
    then do:
       message "Invalid Label Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[6] V-table-Win
ON LEAVE OF style.material[6] IN FRAME F-Main /* Coating */
DO:
   if lastkey <> -1 and self:screen-value <> "" and
       not can-find(first item where item.company = style.company and 
                 /* item.industry = style.industry and */
                  item.i-no = self:screen-value )
    then do:
       message "Invalid Coating Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.material[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.material[7] V-table-Win
ON LEAVE OF style.material[7] IN FRAME F-Main /* Joint Glue */
DO:
   if lastkey <> -1 and self:screen-value <> "" and
      not can-find(first item where item.company = style.company and 
                 /* item.industry = style.industry and */
                  item.i-no = self:screen-value )
    then do:
       message "Invalid Joint Glue Code. Try help please." view-as alert-box.
       return no-apply.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.style
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.style V-table-Win
ON LEAVE OF style.style IN FRAME F-Main /* Style No. */
DO:
    def buffer bf-style for style.
    if lastkey <> -1 and 
       can-find(first bf-style where bf-style.company = style.company and
                      bf-style.style = self:screen-value)
    then do:
         message "Style already exists. Try different style code." view-as alert-box.
         return no-apply.
    end.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style.type V-table-Win
ON LEAVE OF style.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{custom/getcmpny.i}
{custom/getloc.i}
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
  {src/adm/template/row-list.i "style"}
  {src/adm/template/row-list.i "flute"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "style"}
  {src/adm/template/row-find.i "flute"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-flute-dim V-table-Win 
PROCEDURE display-flute-dim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-code as int no-undo.
  
  find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = style.style
                        and reftable.loc = flute.code
                        and reftable.code = string(ip-code)
                        no-error.
  if not avail reftable then do:
    
     create reftable.
     assign reftable.reftable = "STYFLU" 
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = string(ip-code)
            . 
  end.                       
  case ip-code:
      when 1 then do:
             ld-joint-tab:screen-value in frame {&frame-name} = string(reftable.val[13]).
      end.
      when 2 then ld-blank-width:screen-value in frame {&frame-name} = string(reftable.val[13]).
      when 3 then ld-glue-in:screen-value = string(reftable.val[13]).
      when 4 then ld-glue-out:screen-value = string(reftable.val[13]).
      when 5 then ld-stitch-in:screen-value = string(reftable.val[13]).
      when 6 then ld-stitch-out:screen-value = string(reftable.val[13]).                 
      when 7 then ld-tape-score:screen-value = string(reftable.val[13]).
  end case.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-style-field V-table-Win 
PROCEDURE enable-style-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* called from methods/viewers/enable/style.i */
  
  enable ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dim-values V-table-Win 
PROCEDURE get-dim-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-style like style.style no-undo.
  def input param ip-flute as cha no-undo.
  def input param ip-code as cha no-undo.
  def output param op-total as dec no-undo.
  def var i as int no-undo.
  
  find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = ip-style
                        and reftable.loc = ip-flute
                        and reftable.code = ip-code
                        no-error.
  if  avail reftable then op-total = reftable.val[13].
  /*  same val[13] is sum of [1 for 12]
  do i = 1 to 12:
          op-total = op-total + reftable.val[i] .
  end.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-flute for flute.
  def buffer bf-reftable for reftable.
  def var i as int no-undo.
  def var ls-old-style as cha no-undo.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  ls-old-style = style.style.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  /* ======== create flute for the style ==========*/
  if adm-new-record then do:
     def var j as int no-undo.
     for each bf-flute no-lock:  /* used be reftable */
        do j = 1 to 7:
           if not can-find(first reftable where reftable.reftable = "STYFLU"
                               and reftable.company = style.style
                               and reftable.loc = bf-flute.code
                               and reftable.code = string(j) )
           then do:
                create reftable.
                assign reftable.reftable = "STYFLU"
                       reftable.company = style.style
                       reftable.loc = bf-flute.code
                       reftable.code = string(j)
                       .

                       
           end. 
           if not adm-adding-record then do i = 1 to 13:  /* copy */
              find first bf-reftable where bf-reftable.reftable = "STYFLU"
                                    and bf-reftable.company = ls-old-style
                                    and bf-reftable.loc = bf-flute.code
                                    and bf-reftable.code = string(j) 
                                    no-lock no-error.
               reftable.val[i] = if avail bf-reftable then bf-reftable.val[i] else 0.
           end.
        end.
     end.  
  end.
  
  
  
  find first reftable where reftable.reftable = "STYFLU" 
                         and reftable.company = style.style
                         and reftable.loc = flute.code
                         and reftable.code = "1"  /* joint tab */
                         no-error
                         .
  if not avail reftable then do:
     create reftable.
     assign reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "1".  
  
  end.
  reftable.val[13] = decimal(ld-joint-tab:screen-value in frame {&frame-name}).                                              

  find first reftable where reftable.reftable = "STYFLU" 
                        and reftable.company = style.style
                        and reftable.loc = flute.code
                        and reftable.code = "BOARD"
                        no-error.
  if not avail reftable then do:
     create reftable.
     assign reftable.reftable = "STYFLU"
            reftable.company = style.style
            reftable.loc = flute.code
            reftable.code = "BOARD".  
  end. 
  reftable.dscr = style.material[1]:screen-value.        
  
  /* ============= end ==============*/
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.

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
  assign style.company = gcompany
         style.industry = "2". /* corrugated style */

  if adm-adding-record then do:
         
     assign ld-joint-tab = 0
            ld-blank-width = 0
            ld-glue-in = 0
            ld-glue-out = 0 
            ld-stitch-in = 0
            ld-stitch-out = 0
          ld-tape-score = 0.
   
     display ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.

  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   
   if not avail style then return.
   
/* moved to next page
   do i = 1 to 7:
      run display-flute-dim (i).
   end.
 */  
   
   def var ld-total as dec no-undo.
   run get-dim-values (style.style,flute.code, "1", output ld-total).                                 
   ld-joint-tab = ld-total.
   run get-dim-values (    style.style,flute.code, "2", output ld-total).                                 
   ld-blank-width = (ld-total).
   run get-dim-values (    style.style,flute.code, "3", output ld-total).                                 
   ld-glue-in = (ld-total).
   run get-dim-values (    style.style,flute.code, "4", output ld-total).                                 
   ld-glue-out = (ld-total).
   run get-dim-values (    style.style,flute.code, "5", output ld-total).                                 
   ld-stitch-in = (ld-total).
   run get-dim-values (    style.style,flute.code, "6", output ld-total).                                 
   ld-stitch-out = (ld-total).
   run get-dim-values (    style.style,flute.code, "7", output ld-total).                                 
   ld-tape-score = (ld-total).

  display ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.

  disable ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.
          
  find first reftable where reftable.reftable = "STYFLU" and reftable.company = style.style
                        and reftable.loc = flute.code
                        and reftable.code = "BOARD"
                        no-lock no-error.
  style.material[1]:screen-value = if avail reftable then reftable.dscr else style.material[1].        

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
  run enable-style-field.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record V-table-Win 
PROCEDURE local-reset-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   enable ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var i as int no-undo.
  def var ls-m-value as cha no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  /* validation check */
  RUN valid-type NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  do with frame {&frame-name}:
     if not can-find(first box-design-hdr where /*box-design-hdr.company = style.company 
                       and*/  box-design-hdr.design-no = int(style.design-no:screen-value))
     then do:
        message "Invalid Box Design Number. Try help." view-as alert-box.
        apply "entry" to style.design-no .
        return no-apply.
     end.                  
     /* style.material validation */
     ls-m-value = "".   
     do i = 1 to 7:
        case i:
           when 1 then ls-m-value = style.material[1]:screen-value.
           when 2 then ls-m-value = style.material[2]:screen-value.
           when 3 then next.
           when 4 then ls-m-value = style.material[4]:screen-value.
           when 5 then ls-m-value = style.material[5]:screen-value.
           when 6 then ls-m-value = style.material[6]:screen-value.
           when 7 then ls-m-value = style.material[7]:screen-value.           
        end case.
        
        if not can-find(first item where item.company = style.company and 
                 /* item.industry = style.industry and */
                  item.i-no = ls-m-value )
           and ls-m-value <> ""       
        then do:
          message "Invalid Material Code. Try help please." ls-m-value view-as alert-box.
          case i:
             when 1 then apply "entry" to style.material[1] in frame {&frame-name}.
             when 2 then apply "entry" to style.material[2] in frame {&frame-name}.
             when 4 then apply "entry" to style.material[4] in frame {&frame-name}.
             when 5 then apply "entry" to style.material[5] in frame {&frame-name}.
             when 6 then apply "entry" to style.material[6] in frame {&frame-name}.
             when 7 then apply "entry" to style.material[7] in frame {&frame-name}.
          end.
          return no-apply.
        end.
    end.
    /* style.m-code validation */
    ls-m-value = "".
    do i = 1 to 7:
       case i:
          when 1 then ls-m-value = style.m-code[1]:screen-value.
          when 2 then ls-m-value = style.m-code[2]:screen-value.
          when 3 then ls-m-value = style.m-code[3]:screen-value.
          when 4 then ls-m-value = style.m-code[4]:screen-value.
          when 5 then ls-m-value = style.m-code[5]:screen-value.
          when 6 then ls-m-value = style.m-code[6]:screen-value.
          when 7 then ls-m-value = style.m-code[7]:screen-value.          
       end case.
       if ls-m-value <> "" and
       not can-find(first mach where mach.company = style.company and 
                  mach.m-code= ls-m-value )
       then do:
           message "Invalid Machine Code. Try help please." view-as alert-box.
           case i :
              when 1 then apply "entry" to style.m-code[1].
              when 2 then apply "entry" to style.m-code[2]. 
              when 3 then apply "entry" to style.m-code[3]. 
              when 4 then apply "entry" to style.m-code[4].
              when 5 then apply "entry" to style.m-code[5].              
              when 6 then apply "entry" to style.m-code[6].
              when 7 then apply "entry" to style.m-code[7].
           end case.
           return no-apply.
       end.
    end.
   end.   /* do with frame */
   
   
   /* end of validation */
   
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  disable ld-joint-tab ld-blank-width ld-glue-in ld-glue-out ld-stitch-in ld-stitch-out
          ld-tape-score with frame {&frame-name}.

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
  {src/adm/template/snd-list.i "style"}
  {src/adm/template/snd-list.i "flute"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF NOT CAN-DO("B,D,F,P,R,W",style.type:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid Type. Type must be 'B'ox, 'D'ie Cut, 'F'oam, 'P'artition, 'R'oll Partition, or 'W'ood..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

