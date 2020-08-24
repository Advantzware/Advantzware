
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM v-term LIKE report.term-id NO-UNDO.
DEFINE INPUT  PARAMETER iplShowMessage AS LOGICAL NO-UNDO.

{oe/closchk.i}

DEF BUFFER b-fg-bin FOR fg-bin.
DEF BUFFER b-oe-boll FOR oe-boll.

DEF VAR li AS INT NO-UNDO.
DEF VAR v-tag2 AS CHAR NO-UNDO.
DEFINE VARIABLE riRowId AS ROWID NO-UNDO.

{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}   
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW} 
{fg/fgPostBatch.i} 

/* ************************  Function Prototypes ********************** */

FUNCTION fGetBOLTransferPost RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.
    

FOR EACH oe-boll WHERE ROWID(oe-boll) EQ ip-rowid,
    FIRST oe-bolh
    WHERE oe-bolh.b-no EQ oe-boll.b-no
      AND CAN-FIND(FIRST b-oe-boll
                   WHERE b-oe-boll.b-no EQ oe-bolh.b-no
                     AND b-oe-boll.qty  NE 0),
    FIRST cust NO-LOCK
    WHERE cust.company EQ oe-bolh.company
      AND cust.cust-no EQ oe-bolh.cust-no,
    FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ oe-boll.company,
    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-boll.company
      AND oe-ord.ord-no  EQ oe-boll.ord-no,
    FIRST oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ oe-boll.company
      AND oe-ordl.ord-no  EQ oe-boll.ord-no
      AND oe-ordl.line    EQ oe-boll.line
      AND oe-ordl.i-no    EQ oe-boll.i-no
    USE-INDEX ord-no,
    FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ oe-boll.company
      AND itemfg.i-no    EQ oe-boll.i-no :

  RUN oe/custxship.p (oe-bolh.company,
                      oe-bolh.cust-no,
                      oe-bolh.ship-id,
                      BUFFER shipto).

  IF oe-ord.type EQ "T" OR oe-boll.s-code EQ "T" THEN DO: /* Process in-house transfer */
    IF AVAIL shipto AND CAN-FIND(FIRST fg-bin
                                 WHERE fg-bin.company EQ shipto.company
                                   AND fg-bin.i-no    EQ ""
                                   AND fg-bin.loc     EQ shipto.loc
                                   AND fg-bin.loc-bin EQ shipto.loc-bin) THEN DO:

      FIND FIRST sys-ctrl WHERE
           sys-ctrl.company EQ oe-bolh.company AND
           sys-ctrl.NAME EQ "BOLPOST"
           NO-LOCK NO-ERROR.

      IF AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN
      DO:
         li = 0.
        
         FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
         IF AVAIL fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.
        
         FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
                 
         CREATE fg-rctd.
         ASSIGN
          fg-rctd.r-no      = li + 1
          fg-rctd.company   = oe-boll.company
          fg-rctd.bol-no    = oe-bolh.bol-no
          fg-rctd.rct-date  = oe-bolh.bol-date
          fg-rctd.trans-time = TIME
          fg-rctd.i-no      = oe-boll.i-no
          fg-rctd.rita-code = "T"
          fg-rctd.job-no    = oe-boll.job-no
          fg-rctd.job-no2   = oe-boll.job-no2
          fg-rctd.loc       = oe-boll.loc
          fg-rctd.loc-bin   = oe-boll.loc-bin
          fg-rctd.tag       = oe-boll.tag
          fg-rctd.cust-no   = oe-boll.cust-no
          fg-rctd.partial   = oe-boll.partial
          fg-rctd.cases     = oe-boll.cases
          fg-rctd.qty-case  = oe-boll.qty-case
          fg-rctd.t-qty     = oe-boll.qty
          fg-rctd.loc2      = shipto.loc
          fg-rctd.loc-bin2  = shipto.loc-bin
          fg-rctd.tag2      = fg-rctd.tag.
      END. /* IF AVAIL sys-ctrl */

      FIND FIRST fg-bin NO-LOCK      /* Make sure we have a bin to relieve */
          WHERE fg-bin.company  EQ oe-boll.company
            AND fg-bin.i-no     EQ oe-boll.i-no
            AND fg-bin.job-no   EQ oe-boll.job-no
            AND fg-bin.job-no2  EQ oe-boll.job-no2
            AND fg-bin.loc      EQ oe-boll.loc
            AND fg-bin.loc-bin  EQ oe-boll.loc-bin
            AND fg-bin.tag      EQ oe-boll.tag
            AND fg-bin.cust-no  EQ oe-boll.cust-no
          NO-ERROR.

      IF NOT AVAIL fg-bin THEN DO:
        CREATE fg-bin.
        ASSIGN
         fg-bin.company      = oe-boll.company
         fg-bin.i-no         = oe-boll.i-no
         fg-bin.job-no       = oe-boll.job-no
         fg-bin.job-no2      = oe-boll.job-no2
         fg-bin.loc          = oe-boll.loc
         fg-bin.loc-bin      = oe-boll.loc-bin
         fg-bin.tag          = oe-boll.tag
         fg-bin.cust-no      = oe-boll.cust-no
         fg-bin.case-count   = oe-boll.qty-case
         fg-bin.pur-uom      = itemfg.prod-uom
         fg-bin.std-tot-cost = itemfg.std-tot-cost
         fg-bin.std-mat-cost = itemfg.std-mat-cost
         fg-bin.std-lab-cost = itemfg.std-lab-cost
         fg-bin.std-var-cost = itemfg.std-var-cost
         fg-bin.std-fix-cost = itemfg.std-fix-cost.
      END. /* IF NOT AVAIL fg-bin */
             
      v-tag2 = oe-boll.tag.

      IF AVAIL fg-rctd AND
         fg-bin.qty GT oe-boll.qty AND
         fg-bin.tag NE ""            AND
         fg-bin.tag EQ oe-boll.tag  
         AND AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
         DO:
            RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT v-tag2).
            fg-rctd.tag2 = v-tag2.
         END.
      ELSE
         IF NOT AVAIL fg-rctd AND
            fg-bin.qty GT oe-boll.qty AND
            fg-bin.tag NE "" AND
            fg-bin.tag EQ oe-boll.tag  
            AND AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
            RUN fg/mkloadtg2.p (ROWID(oe-boll), shipto.loc, shipto.loc-bin, 0, INPUT-OUTPUT v-tag2).
            

      IF fg-bin.loc     NE shipto.loc     OR
         fg-bin.loc-bin NE shipto.loc-bin OR
         fg-bin.tag     NE v-tag2 OR
         v-tag2 EQ "" THEN DO:
        
         /*As part of 36989 - Block of code that pre-created a bin was removed*/
         /*This was considered unnecessary since the Posting program will create the bin when it processes the transfers*/
         /*The newly created bin was corrupting costs since it used the IF1 standard cost rather than bin costs*/
         IF AVAIL fg-rctd THEN
         DO:
            ASSIGN 
             fg-rctd.pur-uom  = fg-bin.pur-uom
             fg-rctd.cost-uom = fg-bin.pur-uom /*#29642 - Lack of this being filled in causing problems with ext-cost calc downstream (fg-bin.pur-uom is actually itemfg.prod-uom at time of create)*/
             fg-rctd.std-cost = fg-bin.std-tot-cost.
           
            IF fg-rctd.pur-uom EQ "EA" THEN
               fg-rctd.ext-cost = fg-rctd.std-cost.
            ELSE
               RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                                      fg-rctd.std-cost, OUTPUT fg-rctd.ext-cost).
           
            fg-rctd.ext-cost = fg-rctd.ext-cost * fg-rctd.t-qty.
            riRowId = ROWID(fg-rctd).
            RELEASE fg-rctd.
            
         END.
         RELEASE fg-bin.
         ASSIGN
           oe-bolh.posted = YES
           oe-boll.posted = YES.
      END.  /* IF fg-bin.loc     NE shipto.loc */

      ELSE DO:
         IF iplShowMessage THEN 
           MESSAGE "BOL # " + STRING(oe-bolh.bol-no) +  " cannot be posted as it contains a Transfer transaction" 
                   SKIP "that has the same 'From' and 'To' Warehouse/Bin." 
                   SKIP(2) "Please review your BOL and edit or delete as needed."
           VIEW-AS ALERT-BOX.
         IF AVAIL fg-rctd THEN
            DELETE fg-rctd.
      END. 
    END. /* IF AVAIL shipto AND CAN-FIND(FIRST fg-bin */

    /* to close order (do not close order for transfers) */
    IF oe-boll.s-code <> "T" THEN DO:
        CREATE w-ord.
        ASSIGN
         w-ord.ord-no = oe-ordl.ord-no
         w-ord.rec-id = RECID(oe-ord).
    END.
      
    RUN pAutoPostTransferTransaction(INPUT riRowId,INPUT oe-bolh.company).
   
  END. /*  IF oe-ord.type EQ "T" OR oe-boll.s-code EQ "T" */

  {oe/seq-bolh.i}
END.


PROCEDURE pAutoPostTransferTransaction:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid   AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE dtPostDate         AS DATE      INITIAL TODAY NO-UNDO.
    DEFINE VARIABLE cFgEmails          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFgEmails          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lFgEmails          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hInventoryProcs    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lActiveBin         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPromptForClose    AS LOGICAL   NO-UNDO INITIAL YES.   
    DEFINE VARIABLE lFGBOLTransferPost AS LOGICAL   NO-UNDO.
    
    lFGBOLTransferPost = fGetBOLTransferPost(ipcCompany). 
      
    IF lFGBOLTransferPost THEN
    DO: 
        FOR EACH fg-rctd NO-LOCK
            WHERE fg-rctd.company EQ ipcCompany 
            AND rowid(fg-rctd) EQ iprwRowid:
                                 
            CREATE w-fg-rctd.
            BUFFER-COPY fg-rctd TO w-fg-rctd
                ASSIGN 
                w-fg-rctd.row-id  = ROWID(fg-rctd)
                w-fg-rctd.has-rec = YES.                            
            ASSIGN
                dtPostDate = TODAY .
            RUN fg/fgpostBatch.p ( 
                INPUT dtPostDate, /* Post date      */
                INPUT NO,          /* tg-recalc-cost */
                INPUT "T",         /* Transfer       */
                INPUT lFgEmails,   /* Send fg emails */
                INPUT YES,         /* Create work-gl */
                INPUT lPromptForClose, /* Executes .w closing orders logic */
                INPUT TABLE w-fg-rctd BY-REFERENCE,
                INPUT TABLE tt-fgemail BY-REFERENCE,
                INPUT TABLE tt-email BY-REFERENCE,
                INPUT TABLE tt-inv BY-REFERENCE).              
        END.   /* for each  fg-rctd */                    
    END.  /* lFGBOLTransferPost*/        
    
END PROCEDURE.

/* ************************  Function Implementations ***************** */ 
FUNCTION fGetBOLTransferPost RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  return Nk1 value 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lReturnValue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRecordFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnChar AS LOGICAL NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany, "FGBOLTransferPost", "L", NO, NO, "", "", 
        OUTPUT cReturnChar, OUTPUT lRecordFound).    
        lReturnValue = LOGICAL(cReturnChar).
    
    RETURN lReturnValue.
    		
END FUNCTION.
