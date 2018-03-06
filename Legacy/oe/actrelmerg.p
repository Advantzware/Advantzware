&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/actrel.i
    Purpose     : order entry - Create actual releases from planned 
                  release line  

    Syntax      :

    Description :

    Author(s)   : 01/98 JLF 
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* begin inserted from oe/actrel.p */
DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipcAction AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAMETER iocPrompt AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opr-oerelh-row AS ROWID NO-UNDO.

/* ***************************  Definitions  ************************** */

{sys/inc/VAR.i SHARED}

DEF SHARED VAR out-recid AS RECID NO-UNDO.


DEF SHARED VAR relh-recid AS RECID  NO-UNDO.
DEF SHARED VAR v-auto     AS LOG    NO-UNDO.

DEFINE VARIABLE choice          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-merge-prompt  AS LOGICAL   INIT ? NO-UNDO.
DEFINE VARIABLE v-email         AS LOGICAL   INIT YES NO-UNDO.
DEFINE VARIABLE v-nxt-r-no      AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE v-dlg-sel       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-cust-no       AS CHARACTER NO-UNDO.
DEFINE VARIABLE llAlreadyPosted AS LOGICAL   NO-UNDO.

DEFINE VARIABLE ll-rell-found    AS LOGICAL        NO-UNDO.
DEFINE VARIABLE ll-by-po         AS LOGICAL        NO-UNDO.
DEFINE VARIABLE l-rno            LIKE oe-rell.r-no NO-UNDO.
DEFINE VARIABLE lr-oe-relh-rowid AS ROWID          NO-UNDO.
DEFINE VARIABLE vcShiptoLevelRelmerge AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcRelMergeCalc   AS CHARACTER      NO-UNDO.
DEFINE VARIABLE b-oe-rel-row     AS ROWID          NO-UNDO.
DEFINE VARIABLE b2-oe-rell-row   AS ROWID          NO-UNDO.
DEFINE VARIABLE oe-relh-row      AS ROWID          NO-UNDO.
DEFINE VARIABLE reft-s-code LIKE oe-rel.s-code NO-UNDO.
DEFINE VARIABLE reft-dscr        AS CHARACTER       NO-UNDO.
DEFINE VARIABLE ll-ans           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMergeMessage    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessageBody     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMessageRelh     AS CHARACTER   NO-UNDO.
DEF BUFFER b-reft-findrelh FOR reftable.
DEF BUFFER b-reft-fob FOR reftable.
DEF BUFFER b2-oe-rell FOR oe-rell.
DEF BUFFER b2-reftable FOR reftable.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER s-code FOR reftable.
DEF BUFFER bf-rel FOR oe-rel.
DEF STREAM s1.
{oe/chkordl.i}

{oe/relemail.i}

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-get-dscr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-dscr Procedure 
FUNCTION get-dscr RETURNS CHARACTER
  ( INPUT ipr-oe-rel-r-no AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-fob-dscr2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fob-dscr2 Procedure 
FUNCTION get-fob-dscr2 RETURNS CHARACTER
  ( INPUT ipcRecKey AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-merge-prompt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-merge-prompt Procedure 
FUNCTION get-merge-prompt RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-get-shipto-level-relmerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-shipto-level-relmerge Procedure 
FUNCTION get-shipto-level-relmerge RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRelmergeCharValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRelmergeCharValue Procedure 
FUNCTION getRelmergeCharValue RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-run-from-bol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD run-from-bol Procedure 
FUNCTION run-from-bol RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 18.48
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE llReady    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lcFobDscr2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE llPromptByItem AS LOG NO-UNDO.
DEFINE BUFFER bf-notes FOR notes .
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-relh FOR oe-relh .
FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

opr-oerelh-row = ?. /* initial value */

DO TRANSACTION:
  {sys\inc\addxfer.i}
END.


IF ipcAction EQ "CREATE" THEN DO:
  
  RUN ask-to-proceed (OUTPUT choice, OUTPUT llPromptByItem).

  /* Setting these character values with the idea that we may need 'First One' */
  IF iocPrompt = "" THEN DO:      
      iocPrompt = (IF llPromptByItem THEN "ALWAYS" ELSE "NEVER").
  END.
  

  IF choice THEN
    RUN check-if-ready (OUTPUT llReady).
END.
ELSE
  ASSIGN choice = YES llReady = YES.

IF choice AND llReady THEN DO:
    v-cust-no = oe-rel.cust-no.

  IF addxfer-log THEN
    RUN get-s-code.

  /* Set v-merge-prompt value */
  v-merge-prompt = ?.

    RELEASE oe-relh.
    
    reft-s-code = oe-rel.s-code.
    reft-dscr   = oe-rel.fob-code.

    /* ************ Set 'by po' for the particular customer as relmerge value ** */
    ll-by-po = NO.
    vcShiptoLevelRelmerge = get-shipto-level-relmerge().
/*     vcRelMergeCalc = relmerge-chr.              */
/*     IF vcShiptoLevelRelmerge GT "" THEN         */
/*         vcRelMergeCalc = vcShiptoLevelRelmerge. */
    vcRelMergeCalc = DYNAMIC-FUNCTION('getRelmergeCharValue':U).
    
    IF vcShiptoLevelRelmerge BEGINS "SamePo#Only" THEN
      ll-by-po = YES.

    
    /* *********** Determine if matchine oe-rell found for given oe-rel ******* */ 
    /* *********** with same PO number, rel-date, cust-no               ******* */ 
    /* *********** Set l-rno and ll-rell-found if it is found by        ******* */
    /* *********** ord-no as well as rel-date, ship-id, cust-no         ******* */
    ll-rell-found = NO.
    /* To make sure not available initially */
    RELEASE oe-relh.
    RUN find-matching-oerell (OUTPUT ll-rell-found, OUTPUT l-rno, 
                              OUTPUT b-oe-rel-row, OUTPUT b2-oe-rell-row,
                              OUTPUT oe-relh-row).

    IF ll-rell-found THEN DO:
        /* wfk - 6/6/13 - found that these buffer not needed below, and making oe-relh */
        /* available here was causing problems with merging incorrectly                */
        /* all that is needed is ll-rell-found and l-rno                               */

        /* Finding in case needed below */
/*         FIND b-oe-rel   WHERE ROWID(b-oe-rel) = b-oe-rel-row NO-LOCK NO-ERROR.     */
/*         FIND b2-oe-rell WHERE ROWID(b2-oe-rell) = b2-oe-rell-row NO-LOCK NO-ERROR. */
/*         FIND oe-relh    WHERE ROWID(oe-relh) EQ oe-relh-row NO-LOCK NO-ERROR.      */
    END.
    
    /* ************ Search for oe-relh to match to oe-rel by rel-date, cust-no, ship-to ***** */
    /* ************ with exceptions for when relmerge value is sameOrderONly or samePoOnly ** */
    /* ************ In those cases, oe-rell must have been matched also. If it was, leave  ** */
    /* ************ the loop with oe-relh available                                      **** */
    lr-oe-relh-rowid = ?.

    /* Try matching first with the oe-relh that was just created */
    IF relh-recid NE ? THEN
      RUN find-matching-relh (INPUT YES, OUTPUT lr-oe-relh-rowid).
    IF lr-oe-relh-rowid EQ ? THEN
      RUN find-matching-relh (INPUT NO, OUTPUT lr-oe-relh-rowid).
    
    IF lr-oe-relh-rowid <> ?  THEN
        FIND oe-relh WHERE ROWID(oe-relh) = lr-oe-relh-rowid NO-ERROR.
  
  IF AVAIL oe-relh THEN DO:
    /* ******  If an oe-relh was found matching the given oe-rel        ****** */
    /* ******  then see if we have to prompt to merge into this existing ***** */
    
    /* ****** Determines if merge value is set per customer for samePO#Only ** */  
    IF ((NOT vcShiptoLevelRelmerge EQ "SamePo#Only")
       OR (ll-rell-found AND oe-relh.r-no = l-rno)) 
    THEN
      v-merge-prompt = get-merge-prompt().

/* wfk - relmerge-log is used to determine whether to merge only when not printed */
/*     IF v-merge-prompt = ? THEN         */
/*         v-merge-prompt = relmerge-log. */
  END. /* If Avail Oe-relh based on include file */
  
/* wfk - 6/7/13 - this is now handled by running find-matching-oerelh twice */
  /* if oe-relh exists more than one for same cust-no, ship-no,rel-date - pick first oe-relh - wrong*/
/*   IF relh-recid NE ? THEN                       */
/*       FIND oe-relh                              */
/*         WHERE RECID(oe-relh)  EQ relh-recid     */
/*           AND oe-relh.cust-no EQ v-cust-no      */
/*           AND oe-relh.ship-id EQ oe-rel.ship-id */
/*         NO-LOCK NO-ERROR.                       */
  IF AVAIL oe-relh THEN DO:
    RUN set-merge-msg (INPUT vcRelMergeCalc, OUTPUT cMergeMessage, 
                       OUTPUT cMessageBody, OUTPUT cMessageRelh).
  
   IF vcRelMergeCalc NE "SamePo#Only" AND vcRelMergeCalc BEGINS "SamePo#Only" THEN
       v-merge-prompt = get-merge-prompt().
   ELSE
       v-merge-prompt = IF iocPrompt = "ALWAYS" THEN TRUE ELSE FALSE.
 
   out-recid = RECID(oe-relh).
   IF run-from-bol() THEN
       v-merge-prompt = NO.

    /* v-merge-prompt is initially ? */
    IF (v-auto AND v-merge-prompt NE YES) 
               OR (v-merge-prompt EQ NO) THEN  
          v-dlg-sel = 1.
    ELSE DO:

          /* *** Ask the user - Based on v-merge-prompt value *************  */
     ll-ans = ?.
     IF v-merge-prompt THEN
       RUN ask-to-merge (OUTPUT ll-ans).

/*           IF ll-ans THEN v-dlg-sel = 1.                    */
/*                     ELSE IF NOT ll-ans THEN v-dlg-sel = 2. */
/*                     ELSE v-dlg-sel = 3.                    */
          CASE ll-ans:
              WHEN YES THEN v-dlg-sel = 1.
              WHEN NO  THEN v-dlg-sel = 2.
              OTHERWISE v-dlg-sel = 3.
          END CASE.

    END. /* Not v-auto, ask user whether to merge */
    
    IF iocPrompt = "FIRST" THEN DO:
        ASSIGN v-dlg-sel = 2 /* don't merge on first one */
               iocPrompt = "NEVER".
    END.
  END. /* If avail oe-relh */  
  
  IF ipcAction = "FINDRELH" THEN DO:
      IF AVAIL oe-relh THEN
        opr-oerelh-row = ROWID(oe-relh).
      RETURN.
  END.
  /* Answered 'Yes', using existing oe-relh for this oe-rel */

  /* Answered 'Cancel' */
  IF v-dlg-sel EQ 3 THEN RETURN.
  
  /* Answered 'NO', so a separate release is created */
  IF v-dlg-sel EQ 2 OR NOT AVAIL oe-relh THEN do: 
      RUN oe/cre-relh.p (RECID(oe-rel)).
      FIND FIRST bf-oe-ordl NO-LOCK
          WHERE bf-oe-ordl.company EQ oe-rel.company
            AND bf-oe-ordl.ord-no  EQ oe-rel.ord-no
            AND bf-oe-ordl.line    EQ oe-rel.line
          NO-ERROR.
      FIND bf-oe-relh WHERE RECID(bf-oe-relh) EQ relh-recid NO-LOCK.
      IF AVAIL bf-oe-ordl THEN 
        FOR EACH notes NO-LOCK WHERE notes.rec_key = bf-oe-ordl.rec_key 
                 AND notes.note_type <> "S" and notes.note_type <> "o"  :
             IF AVAILABLE bf-oe-relh THEN DO:
                 CREATE bf-notes .
                 BUFFER-COPY notes EXCEPT rec_key TO bf-notes .
                 ASSIGN bf-notes.rec_key = bf-oe-relh.rec_key .
             END.
        END.
  END.
  ELSE
  /* If merging, set the recid of the one that was found since it */
  /* may not be the last one                                      */
      IF AVAIL oe-relh THEN
          relh-recid = RECID(oe-relh).
  
  /* ***** If Answered 'YES', using the existing oe-relh implicitly ***** */
  IF v-email THEN  
    RUN create-tt-email.
  
  
  /* ******       Create Actual Release                             ***** */
  RUN oe/cre-rell.p (RECID(oe-rel)).
  IF iocPrompt = "FIRST" THEN DO:
      ASSIGN v-dlg-sel = 2 /* don't merge on first one */
             iocPrompt = "NEVER".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ask-to-merge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ask-to-merge Procedure 
PROCEDURE ask-to-merge :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opl-ans AS LOGICAL NO-UNDO.
      /* *** Ask the user - Based on v-merge-prompt value *************  */
      MESSAGE cMergeMessage SKIP(1)
          "     " +  cMessageRelh
          SKIP(1)
          "Matching the current release line: "
          SKIP(1)
          "     " + cMessageBody
          SKIP(1)
        "Choose YES to print multiple items on one Release/Pick ticket.,Choose NO to create a separate Release/Pick ticket for each item."
          VIEW-AS ALERT-BOX BUTTON YES-NO-CANCEL UPDATE ll-ans AS LOG.
      opl-ans = ll-ans.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ask-to-proceed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ask-to-proceed Procedure 
PROCEDURE ask-to-proceed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER choice AS LOG NO-UNDO.    
DEFINE OUTPUT PARAMETER opPromptItem AS LOG NO-UNDO.
DEF VAR ip-view-type AS CHAR NO-UNDO.
DEF VAR ip-parms     AS CHAR NO-UNDO.
DEF VAR op-values    AS CHAR NO-UNDO.
DEF VAR llFromAutoRun AS LOG NO-UNDO.
DEF VAR lcMergeMessage AS CHAR NO-UNDO.

/* This prompt is supressed if from autorun.p */
IF INDEX(PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE ""), "autorel") GT 0 THEN
  llFromAutoRun = TRUE.
ELSE
  llFromAutoRun = FALSE.

ASSIGN
    choice = YES
    opPromptItem = relmerge-log.
    
IF NOT v-auto AND NOT llFromAutoRun THEN DO:

    lcMergeMessage = "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) + 
                " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?" .

         
    ip-parms = 
          "type=literal,name=fi4,row=3,col=18,enable=false,width=58,scrval=" + lcMergeMessage + ",FORMAT=X(58)"
/*          + "|type=toggle,name=tg1,row=6,col=16,enable=true,scrval=no,label=test this label,width=3" */
/*          + "|type=literal,name=fi5,row=6,col=20,enable=false,scrval=Prompt to merge each item?,width=28,format=x(28)"  */
          + "|type=image,image=webspeed\images\question.gif,name=im1,row=2,col=5,enable=false" 
          + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=8".
    /*
    ip-parms = 
    "type=literal,name=fi1,row=3,col=15,enable=false,width=60,FORMAT=X(60),scrval=This will create an Actual release from this Planned release. " 
    + "|type=literal,name=fi4,row=4,col=15,enable=false,width=30,scrval=Would you like to continue?,FORMAT=X(30)"
    + "|type=toggle,name=tg1,row=6,col=16,enable=true,scrval=no,label=test this label"
    + "|type=literal,name=fi5,row=6,col=23,enable=false,scrval=Prompt to merge each item?,width=28,format=x(28)" 
    + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=2,enable=true " 
    + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30)".
   */
   RUN custom/d-prompt.w (INPUT "yes-no", ip-parms, "", OUTPUT op-values).
   
    /* Default */
    
    opPromptItem = relmerge-log.
    DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
        IF ENTRY(i, op-values) EQ "default" THEN
          choice = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR.
        IF ENTRY(i, op-values) EQ "tg1" THEN DO:
            opPromptItem = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR.          

        END.
    END.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-already-posted) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-already-posted Procedure 
PROCEDURE check-already-posted :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER opl-already-posted AS LOG NO-UNDO.

opl-already-posted = NO.

rel-block:
  /** Find the most recent actual release with the same #order, shipto,
  release date, not deleted, and not printed. **/
  REPEAT PRESELECT
    EACH oe-relh NO-LOCK
      WHERE oe-relh.company  EQ oe-rel.company
        /*             and oe-relh.ord-no   eq oe-rel.ord-no */
        AND oe-relh.rel-date EQ oe-rel.rel-date
        AND oe-relh.cust-no  EQ oe-rel.cust-no
        AND oe-relh.ship-id  EQ oe-rel.ship-id
        AND oe-relh.posted   EQ NO
        /*             and oe-relh.po-no    eq oe-rel.po-no */
        AND oe-relh.deleted  EQ NO
    USE-INDEX rel-date,
    
    EACH oe-rell NO-LOCK
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
        AND oe-rell.ord-no  EQ oe-rel.ord-no
        AND oe-rell.i-no    EQ oe-rel.i-no
        AND oe-rell.rel-no  EQ oe-rel.rel-no /* YSK added 01/20/03 - TASK 01170303*/
    :
    
    FIND NEXT oe-rell NO-ERROR.

    IF NOT AVAIL oe-rell THEN LEAVE rel-block.

    IF AVAIL oe-rell THEN DO:
      IF NOT v-auto /*or program-name(2) begins "oe/oe-relx." */ THEN DO:
        opl-already-posted = YES.
        LEAVE rel-block.
      END. /* Not V-auto */
    END. /* Avail Oe-rell */
  END. /* repeat preselect oe-relh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-check-if-ready) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-if-ready Procedure 
PROCEDURE check-if-ready :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oplReady AS LOG NO-UNDO.

  oplReady = YES.
  RUN check-already-posted (OUTPUT llAlreadyPosted).

  IF llAlreadyPosted AND NOT v-auto THEN DO:
    MESSAGE "This has been already been released. Can not release."
    VIEW-AS ALERT-BOX ERROR.
    /* Program may return here */
    oplReady = NO.
  END. /* Not V-auto */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-create-tt-email) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tt-email Procedure 
PROCEDURE create-tt-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE tt-email.
    ASSIGN
      tt-email.cust-no = oe-rel.cust-no
      tt-email.ord-no  = oe-rel.ord-no
      tt-email.i-no    = oe-rel.i-no
      tt-email.rel-qty = oe-rel.qty
      tt-email.rel-date = IF AVAIL oe-relh THEN oe-relh.rel-date
                          ELSE oe-rel.rel-date
      tt-email.po-no    = oe-rel.po-no.
    
    RELEASE tt-email.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-matching-oerell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-matching-oerell Procedure 
PROCEDURE find-matching-oerell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: wfk - seems to indicate a match on PO# and returns matching 
         oe-relh.r-no      
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER opl-rell-found   AS LOGICAL NO-UNDO.
 DEFINE OUTPUT PARAMETER opi-l-rno           AS INTEGER NO-UNDO.
 DEFINE OUTPUT PARAMETER opr-b-oe-rel-row    AS ROWID   NO-UNDO.
 DEFINE OUTPUT PARAMETER opr-b2-oe-rell-row  AS ROWID   NO-UNDO.
 DEFINE OUTPUT PARAMETER opr-oe-relh-row     AS ROWID   NO-UNDO.
 DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.

    DEF BUFFER bf-oe-ordl FOR oe-ordl.

        
 DEFINE VARIABLE ll-rell-found AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE l-rno AS INTEGER     NO-UNDO.

FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-rel.company
    AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
    AND bf-oe-ordl.line EQ oe-rel.line
    NO-LOCK NO-ERROR.
IF AVAIL bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.    


 ll-rell-found = NO.
 REL-MATCH:
 FOR EACH b-oe-rel WHERE b-oe-rel.company  EQ oe-rel.company
                    AND b-oe-rel.po-no    EQ oe-rel.po-no
                    AND b-oe-rel.r-no     NE oe-rel.r-no
                    AND b-oe-rel.rel-date EQ oe-rel.rel-date
                    AND b-oe-rel.cust-no  EQ oe-rel.cust-no
                    AND (IF INDEX(vcRelMergeCalc, "SameOrder") GT 0 THEN 
                            b-oe-rel.ord-no EQ oe-rel.ord-no ELSE TRUE)   
                  NO-LOCK,
    EACH b2-oe-rell WHERE b2-oe-rell.company EQ b-oe-rel.company
                      AND b2-oe-rell.ord-no  EQ b-oe-rel.ord-no
                      AND b2-oe-rell.i-no    EQ b-oe-rel.i-no
                       AND (IF vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO" THEN 
                            b2-oe-rell.loc EQ oe-rel.spare-char-1 ELSE TRUE)          
                    NO-LOCK,
    EACH oe-relh WHERE oe-relh.company  EQ oe-rel.company
                   AND oe-relh.rel-date EQ oe-rel.rel-date
                   AND oe-relh.cust-no  EQ v-cust-no
                   AND oe-relh.ship-id  EQ oe-rel.ship-id                    
                   AND oe-relh.posted   EQ NO
                   AND oe-relh.deleted  EQ NO
                   AND (oe-relh.printed EQ NO OR relmerge-log) 
                   AND oe-relh.r-no     EQ b2-oe-rell.r-no
                 NO-LOCK:
     FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ b-oe-rel.company
         AND bf-oe-ordl.ord-no EQ b-oe-rel.ord-no
         AND bf-oe-ordl.line EQ b-oe-rel.line
         NO-LOCK NO-ERROR.
     
     IF vcRelMergeCalc EQ "AllOrders&NotRunShip" AND AVAIL(bf-oe-ordl) THEN DO:
         /* Criteria for AllOrders&ShipFromWhse, Run & ship can't be merged with any other */
         IF  bf-oe-ordl.whsed = TRUE OR lRunShip EQ TRUE THEN 
           NEXT REL-MATCH.                  
     END.
               
    ASSIGN 
        ll-rell-found       = TRUE
        l-rno               = b2-oe-rell.r-no
        opr-b-oe-rel-row    = ROWID(b-oe-rel)
        opr-b2-oe-rell-row  = ROWID(b-oe-rel)
        opr-oe-relh-row     = ROWID(oe-relh).

    LEAVE.

 END.


 ASSIGN
    opl-rell-found = ll-rell-found
    opi-l-rno      = l-rno.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-find-matching-relh) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE find-matching-relh Procedure 
PROCEDURE find-matching-relh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  
        Search for oe-relh to match to oe-rel by rel-date, cust-no, ship-to 
        with exceptions for when relmerge value is sameOrderONly or samePoOnly
        In those cases, oe-rell must have been matched also. If it was, leave
        the loop with oe-relh available. If iplUsingNew, tries to match only
        with a newly created oe-relh via relh-recid set in cre-rell
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER iplUsingNew AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER oprRelhRowid AS ROWID NO-UNDO.
DEFINE VARIABLE lRunShip AS LOGICAL NO-UNDO.
DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf-oe-ordl FOR oe-ordl.
FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-rel.company
  AND bf-oe-ordl.ord-no EQ oe-rel.ord-no
  AND bf-oe-ordl.line EQ oe-rel.line
  NO-LOCK NO-ERROR.
 IF AVAIL bf-oe-ordl THEN lRunShip = bf-oe-ordl.whsed.
 

    lr-oe-relh-rowid = ?.
    oe-relh-loop:
    FOR EACH oe-relh
        WHERE  oe-relh.company EQ oe-rel.company
          AND oe-relh.rel-date EQ oe-rel.rel-date
          AND oe-relh.cust-no  EQ v-cust-no
          AND oe-relh.ship-id  EQ oe-rel.ship-id
        
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND (oe-relh.printed EQ NO OR relmerge-log) 
          AND (IF iplUsingNew THEN RECID(oe-relh) EQ relh-recid ELSE TRUE)

          /* **** Either the reftable matching relh is not found or the s-code matches *** */
          /* **** the one found                                     ********************** */
          AND (NOT reft-s-code GT "" OR
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.s-code EQ reft-s-code))

          /* ****** Either it's not 'SameOrderOnly', or the order number does match ****/
          AND ((vcRelMergeCalc NE "SameOrderOnly" OR ll-by-po) OR
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.ord-no EQ oe-rel.ord-no))

          /* ****** Either it's not '..ShipFromWhse', or the ShipFromWhse  does match ****/
          AND ((NOT vcRelMergeCalc EQ "AllOrders&ShipFromWhse") OR
               /* Criteria for AllOrders&ShipFromWhse */
               CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))

          /* ****** Either it's not 'SamePO#Only' for this customer or ll-rell-found * ***/
          AND ((NOT vcRelMergeCalc  BEGINS "SamePO#Only")                          
                OR  
                /* Criteria for SamePO#Only */
                (ll-rell-found AND oe-relh.r-no = l-rno))      

          /* ****** Either it's not '..SamePO#&ShipFromWhse', or the PO# & ShipFromWhse  does match ****/
          AND ((NOT vcRelMergeCalc EQ "SamePO#&ShipFromWhse") OR
               /* Criteria for Same ship-from */
               (CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
               AND /* Criteria for Same PO# */ 
                 (ll-rell-found AND oe-relh.r-no = l-rno))
     
      /* ****** Either it's not '..SameOrder&SameShipFrom', or the Order # & ShipFromWhse does match ****/
       AND ((NOT vcRelMergeCalc EQ "SameOrder&SameShipFrom") OR
               /* Criteria for Same ship-from */
               (CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
               AND /* Criteria for same order */
                     CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.ord-no EQ oe-rel.ord-no))
    
      /* ****** Either it's not '..SameOrder&SameShipFrom&SamePO', or the Order #, PO#, ShipFromWhse does match ****/
       AND ((NOT vcRelMergeCalc EQ "SameOrder&SameShipFrom&SamePO") OR
               /* Criteria for Same ship-from */
               (CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.loc EQ oe-rel.spare-char-1 /* Ship-from-whse */))
               AND /* Criteria for same order */
                     CAN-FIND(FIRST oe-rell
                        WHERE oe-rell.r-no   EQ oe-relh.r-no
                          AND oe-rell.ord-no EQ oe-rel.ord-no)
               AND /* Criteria for SamePO#Only */
                (ll-rell-found AND oe-relh.r-no = l-rno))

        USE-INDEX rel-date NO-LOCK
        BY oe-relh.printed
        BY oe-relh.r-no:


        /* ****** Either it's not '..AllOrders&NoRunShip', or the oeitem is not run & ship ****/
         IF vcRelMergeCalc EQ "AllOrders&NotRunShip" THEN DO:
            
            /* Criteria for AllOrders&ShipFromWhse */
            FIND FIRST  bf-oe-rell
                 WHERE bf-oe-rell.r-no   EQ oe-relh.r-no
                 NO-LOCK NO-ERROR.
          

            IF (AVAIL bf-oe-rell AND CAN-FIND(FIRST oe-ordl
            WHERE oe-ordl.company   EQ bf-oe-rell.company
            AND oe-ordl.ord-no EQ bf-oe-rell.ord-no
            AND oe-ordl.line EQ bf-oe-rell.line
            AND oe-ordl.whsed EQ YES)) OR lRunShip THEN 
              NEXT.
              
        END.  
        
        
        /* Check that the fob desc for this oe-relh matches that of oe-rel */
        IF reft-dscr GT "" THEN
        DO:
           FOR EACH b2-oe-rell FIELDS(rec_key) WHERE
               b2-oe-rell.r-no EQ oe-relh.r-no
               NO-LOCK:
               lcFobDscr2 = get-fob-dscr2(b2-oe-rell.rec_key).
               /* Find of fob description for this oe-rell */
               IF (lcFobDscr2 GT "" AND lcFobDscr2 NE reft-dscr) OR
                  (lcFobDscr2 EQ "" AND reft-dscr NE "") THEN
                  NEXT oe-relh-loop.
           END.
        END.
        lr-oe-relh-rowid = ROWID(oe-relh).
        
        LEAVE oe-relh-loop.

    END.
    
    oprRelhRowid = lr-oe-relh-rowid.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-cust-x) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-cust-x Procedure 
PROCEDURE get-cust-x :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST cust 
    WHERE cust.company EQ cocode 
      AND cust.active EQ 'X'
    NO-LOCK NO-ERROR.

  IF AVAIL cust THEN
  DO:
    IF CAN-FIND(FIRST shipto WHERE
      shipto.company EQ cocode AND
      shipto.cust-no EQ cust.cust-no AND
      shipto.ship-no EQ oe-rel.ship-no AND
      shipto.ship-id EQ oe-rel.ship-id) THEN
    v-cust-no = cust.cust-no.

    RELEASE cust.
  END. /* Avail Cust */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-s-code) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-s-code Procedure 
PROCEDURE get-s-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF oe-rel.s-code EQ 'T' THEN
          RUN get-cust-x.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-set-merge-msg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-merge-msg Procedure 
PROCEDURE set-merge-msg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       cMessageRelh is same as cMessageCore except for inclusion of i-no
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER ipcMergeType AS CHAR.
DEF OUTPUT PARAMETER cMessage AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cMessageCore AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cMessageRelh AS CHAR NO-UNDO.

CASE ipcMergeType:
    WHEN "" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date:"
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                  "/" + oe-rel.i-no.
    WHEN "AllOrders" OR WHEN "AllOrders&NotRunShip" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                  "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        .
    WHEN "SameOrderOnly" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/Order:"
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99") 
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") +
                                   "/" + oe-rel.i-no .
    WHEN "SamePo#Only" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no  +
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no.
    WHEN "SamePo#OnlyWithPrompt" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no +
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no.
    WHEN "SamePo#OnlyWithoutPrompt" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/PO#:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99") + "/" + 
                                   oe-rel.po-no +
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                          TRIM(oe-relh.ship-id) + "/" +
                          STRING(IF oe-relh.rel-date = ? THEN 
                              1/1/1980 ELSE 
                           oe-relh.rel-date,"99-99-99") + "/" + 
                           oe-relh.po-no.
    WHEN "AllOrders&ShipFromWhse" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/ShipFrom:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +
                                   oe-rel.spare-char-1 +
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   oe-rel.spare-char-1 .
    WHEN "SamePO#&ShipFromWhse" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/PO/ShipFrom:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 +
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 .
    WHEN "SameOrder&SameShipFrom" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/Order/ShipFrom:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +                                   
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.spare-char-1 + 
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.spare-char-1 .
    WHEN "SameOrder&SameShipFrom&SamePO" THEN ASSIGN 
        cMessage = "A previous release exists for Customer/Ship-To/Date/Order/PO/ShipFrom:"
        cMessageCore = TRIM(oe-rel.cust-no) + "/" +
                                  TRIM(oe-rel.ship-id) + "/" +
                                  STRING(IF oe-rel.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-rel.rel-date,"99-99-99")+ "/" +                                   
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 + 
                                   "/" + oe-rel.i-no
        cMessageRelh = "Rel#: " + string(oe-relh.release#) + " " + TRIM(oe-relh.cust-no) + "/" +
                                  TRIM(oe-relh.ship-id) + "/" +
                                  STRING(IF oe-relh.rel-date = ? THEN 
                                      1/1/1980 ELSE 
                                   oe-relh.rel-date,"99-99-99")+ "/" +
                                   STRING(oe-rel.ord-no) + "/" +
                                   oe-rel.po-no + "/" +
                                   oe-rel.spare-char-1 .
    

  END CASE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-get-dscr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-dscr Procedure 
FUNCTION get-dscr RETURNS CHARACTER
  ( INPUT ipr-oe-rel-r-no AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-fob-dscr2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-fob-dscr2 Procedure 
FUNCTION get-fob-dscr2 RETURNS CHARACTER
  ( INPUT ipcRecKey AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   /* Find of fob dscr for this oe-relh */
   
   IF b2-oe-rell.rec_key <> "" THEN 
     RETURN oe-rell.fob-code.
   ELSE
     RETURN "".     
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-get-merge-prompt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-merge-prompt Procedure 
FUNCTION get-merge-prompt RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Merge-prompt is initially ?, this uses the NK1 to determine if 
            will prompt the user to merge
------------------------------------------------------------------------------*/
        
  FIND FIRST sys-ctrl-shipto
    WHERE sys-ctrl-shipto.company        = oe-relh.company
      AND sys-ctrl-shipto.NAME           = "RelMerge"
      AND sys-ctrl-shipto.cust-vend      = YES
      AND sys-ctrl-shipto.char-fld  BEGINS "SamePo#Only"
      AND sys-ctrl-shipto.cust-vend-no   = oe-relh.cust-no
  NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl-shipto THEN DO:
  
    /* Set the v-merge-prompt value if sys-ctrl-shipto was found */
    IF INDEX(sys-ctrl-shipto.char-fld, "WithPrompt") > 0 THEN
        v-merge-prompt = YES.
    ELSE IF INDEX(sys-ctrl-shipto.char-fld, "WithoutPrompt") > 0 THEN
      v-merge-prompt = NO.
    ELSE
      v-merge-prompt = ?.
    RETURN v-merge-prompt.
  END.
  ELSE
    RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-get-shipto-level-relmerge) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-shipto-level-relmerge Procedure 
FUNCTION get-shipto-level-relmerge RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST sys-ctrl-shipto
    WHERE sys-ctrl-shipto.company = oe-rel.company
      AND sys-ctrl-shipto.NAME = "RelMerge"
      AND sys-ctrl-shipto.cust-vend = YES
      AND sys-ctrl-shipto.cust-vend-no = v-cust-no
    NO-LOCK NO-ERROR.

  IF AVAIL sys-ctrl-shipto THEN
      RETURN sys-ctrl-shipto.char-fld.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRelmergeCharValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRelmergeCharValue Procedure 
FUNCTION getRelmergeCharValue RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR vcShiptoLevelRelmerge AS CHAR NO-UNDO.
DEF VAR vcRelMergeCalc        AS CHAR NO-UNDO.

  vcShiptoLevelRelmerge = get-shipto-level-relmerge().
  vcRelMergeCalc = relmerge-chr.
  IF vcShiptoLevelRelmerge GT "" THEN
      vcRelMergeCalc = vcShiptoLevelRelmerge.
  RETURN vcRelMergeCalc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-run-from-bol) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION run-from-bol Procedure 
FUNCTION run-from-bol RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  If run from relbol (create bol button) then don't prompt per Joe
------------------------------------------------------------------------------*/
DEF VAR lvStr AS CHAR NO-UNDO.
lvStr =  (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "")
        + (IF PROGRAM-NAME(8) NE ? THEN "," + PROGRAM-NAME(8) ELSE "")
        + (IF PROGRAM-NAME(9) NE ? THEN "," + PROGRAM-NAME(9) ELSE "")
        + (IF PROGRAM-NAME(10) NE ? THEN "," + PROGRAM-NAME(10) ELSE "")
        + (IF PROGRAM-NAME(11) NE ? THEN "," + PROGRAM-NAME(11) ELSE "")
        + (IF PROGRAM-NAME(12) NE ? THEN "," + PROGRAM-NAME(12) ELSE "").

IF INDEX(lvStr, "bol") GT 0 THEN
    RETURN TRUE.
ELSE
    RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

