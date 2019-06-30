
/*------------------------------------------------------------------------
    File        : OeRelease.p
    Purpose     : Release

    Syntax      :

    Description : Return a Dataset of all Order Inquiry release

    Author(s)   : kuldeep gill
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OeRelease.i}
{sys/inc/var.i new shared }
 

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRowid      AS RECID  NO-UNDO.
DEFINE INPUT PARAMETER Asi      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER AShipTo      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER AVia      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ASqty      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ApoNo      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER AlotNo      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER ADate      AS DATE  NO-UNDO.
DEFINE INPUT PARAMETER sellPrice      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER HeaderDueDate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER HeaderLastShipDate AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER LineItemDueDate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER LineItemLastShipDate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RelAllDt             AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER AllPo AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ShipAllDate     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER Afrtpay      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Afob         AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRel.
DEFINE OUTPUT PARAMETER cError      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vMailto     AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vSubject    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vBody       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vMailFrom   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER vInternalUser AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lv-stat AS CHAR.
DEFINE VAR ll-ans1 AS LOG.
DEF VAR ls-to-list AS cha NO-UNDO.


   DEF VAR retcode AS INT NO-UNDO.
   DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
   
def var lv-rel-recid as recid no-undo.
DEF VAR lv-cust-x LIKE cust.cust-no NO-UNDO.
def new shared var out-recid as recid no-undo.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR relstat AS CHARACTER.
DEFINE VAR relout AS CHARACTER.
def new shared var relh-recid as recid no-undo.
def new shared var v-auto as log no-undo.
DEFINE NEW SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEF NEW SHARED VAR a-user AS CHAR NO-UNDO.
def NEW shared var ARowid as recid no-undo.

def var ll-ans as log no-undo.
DEF {1} NEW SHARED WORKFILE w-ordl FIELD w-rowid AS ROWID
                               FIELD w-ok AS LOG
                               FIELD w-auto AS LOG.

DEF {1} NEW SHARED WORKFILE w-ord FIELD w-ord-no LIKE oe-ordl.ord-no.

/* oe/relemail.i*/

DEF {1} NEW SHARED TEMP-TABLE tt-email
    FIELD ord-no AS INT
    FIELD i-no AS CHAR
    FIELD rel-qty AS DEC
    FIELD rel-date AS DATE
    FIELD po-no AS CHAR
    FIELD cust-no AS cha
    INDEX tt-cust IS PRIMARY ord-no.


DEF VAR ll-transfer AS LOG NO-UNDO.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER lot-no FOR reftable.
DEF BUFFER s-code FOR reftable.
DEF BUFFER bf-rel FOR oe-rel.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER bf-ordl FOR oe-ordl.
DEF VAR choice AS LOG NO-UNDO INITIAL YES.
DEF BUFFER cust-po-mand FOR reftable.
DEF VAR ld-date as DATE NO-UNDO.
def var li-ship-no as int no-undo.  /* if ship-to is changed */
DEF BUFFER ref-sell-price FOR reftable.
DEF BUFFER buff_oe-relh FOR oe-relh.
DEFINE VAR prmA      AS CHARACTER  NO-UNDO.
DEF VAR v-cust-no AS CHAR NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.

DEFINE NEW SHARED BUFFER xoe-ordl FOR oe-ordl.
DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.

 /** create bol  */
 DEF VAR addxfer-log AS LOG NO-UNDO.
def var v-all-items as log no-undo.
def var v-first as log no-undo.
DEF VAR lv-save-recid AS RECID NO-UNDO.
DEF VAR v-invoice AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-chkflg AS LOG INIT NO NO-UNDO.

/*****************************/


IF Asi = ? THEN ASSIGN Asi = "".
IF AShipTo = ? THEN ASSIGN AShipTo = "".
IF AVia = ? THEN ASSIGN AVia = "".
IF ASqty = ? THEN ASSIGN ASqty = "".
IF ApoNo = ? THEN ASSIGN ApoNo = "".
IF AlotNo = ? THEN ASSIGN AlotNo = "".
IF Afrtpay = ? THEN ASSIGN Afrtpay = "".
IF Afob = ? THEN ASSIGN Afob = "".

IF prmUser = ? THEN ASSIGN prmUser = "".
IF sellPrice = ? THEN ASSIGN sellPrice = "".
IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum   = ? THEN ASSIGN prmItemNum   = "".
IF prmAction = ""  THEN ASSIGN prmAction = "Select".
IF  HeaderDueDate = ? THEN assign HeaderDueDate = "".
IF  HeaderLastShipDate = ? THEN assign HeaderLastShipDate = "".
IF  LineItemDueDate = ? THEN assign LineItemDueDate = "".
IF  LineItemLastShipDate = ? THEN assign LineItemLastShipDate = "".
IF  RelAllDt = ? THEN assign RelAllDt = "".
IF  ShipAllDate = ? THEN assign ShipAllDate = "".
IF  AllPo = ? THEN assign AllPo = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN relh-recid = ?
       g_company = prmComp
       cocode    = prmComp
       a-user    = prmUser
         .

{sys/inc/addrelse.i}
{sys/inc/oereordr.i}

IF prmAction = "updateRelease" THEN DO:
    
    FIND oe-rel WHERE RECID(oe-rel) = vRowid. 

    
    FIND FIRST oe-ordl where
         oe-ordl.company EQ oe-rel.company AND
         oe-ordl.ord-no = int(prmOrderNum) AND
         oe-ordl.LINE = int(prmItemNum)
         NO-LOCK .
    
   /* IF int(Asi) < 0  THEN DO:
        RETURN.
    END. */

    
    
  /*  IF ADate <> "" THEN DO:
        ld-date = DATE(INT(SUBSTR(ADate,1,2)),
                   INT(SUBSTR(ADate,4,2)),
                   INT(SUBSTR(ADate,7,4))) NO-ERROR.
  
    END.*/
    ASSIGN ld-date = ADate.
    
   IF ApoNo <> ""  THEN DO:
        FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-rel.company
          AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-ERROR.
        IF AVAIL oe-ord THEN
            FIND FIRST cust NO-LOCK WHERE cust.company EQ oe-ord.company
                                      AND cust.cust-no EQ oe-ord.cust-no
                                      AND CAN-FIND(FIRST cust-po-mand
                                                   WHERE cust-po-mand.reftable EQ "cust.po-mand"
                                                   AND cust-po-mand.company  EQ cust.company
                                                   AND cust-po-mand.loc      EQ ""
                                                   AND cust-po-mand.code     EQ cust.cust-no
                                                   AND cust-po-mand.val[1]   EQ 1) NO-ERROR.
            IF AVAIL cust AND ApoNo = "" THEN DO:
                ASSIGN     cError =  "PO# is mandatory for this Customer...".
                
                RETURN .

            END.
    END. /*IF ApoNo <> ""  THEN DO:*/
    IF AShipTo  <> "" THEN do:
        FIND FIRST oe-ord WHERE oe-ord.company EQ oe-rel.company 
                            AND oe-ord.ord-no  EQ oe-rel.ord-no
                             NO-LOCK.
        RUN oe/custxship.p (oe-rel.company,
                            oe-ord.cust-no, AShipTo,
                            BUFFER shipto).
        IF AVAIL shipto THEN li-ship-no = shipto.ship-no.
        ELSE DO:
            ASSIGN     cError = "Invalid ship Id ".
            
            RETURN .
        END.
    END. /*IF AShipTo  <> "" THEN do:*/
    IF ld-date NE oe-rel.rel-date  AND
              CAN-FIND(FIRST bf-rel
                       WHERE bf-rel.company  EQ oe-rel.company
                         AND bf-rel.ord-no   EQ oe-rel.ord-no
                         AND bf-rel.link-no  EQ 0
                         AND bf-rel.rel-date EQ oe-rel.rel-date
                         AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN DO:
              /*
               MESSAGE "Update all other Scheduled Releases for this order with a" SKIP
                "release date of " + STRING(ld-date,"99/99/9999") + " to " +
                STRING(oe-rel.rel-date,"99/99/9999")
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-ans.*/
            IF RelAllDt = "yes" THEN  DO:      

            FOR EACH bf-rel WHERE bf-rel.company  EQ oe-rel.company
                              AND bf-rel.ord-no   EQ oe-rel.ord-no
                              AND bf-rel.link-no  EQ 0
                              AND bf-rel.rel-date EQ oe-rel.rel-date
                               AND (ROWID(bf-rel)   NE ROWID(oe-rel))
                           EXCLUSIVE-LOCK:
                  RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
                  IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.rel-date = ld-date.
            END. 
            END.
                  
          END.

      IF ApoNo NE oe-rel.po-no                AND
         CAN-FIND(FIRST bf-rel
                  WHERE bf-rel.company  EQ oe-rel.company
                    AND bf-rel.ord-no   EQ oe-rel.ord-no
                    AND bf-rel.link-no  EQ 0
                    AND ROWID(bf-rel)   NE ROWID(oe-rel)) THEN DO:
             /*MESSAGE "Change item PO Number on all items? "
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.*/
        IF AllPo = "yes" THEN DO:
            FOR EACH  bf-rel WHERE bf-rel.company EQ oe-rel.company
                AND bf-rel.ord-no  EQ oe-rel.ord-no
                AND bf-rel.link-no EQ 0
                 EXCLUSIVE-LOCK:
            RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
            IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.po-no = ApoNo. 
            
            END.
        END.

        /*  ll-ans = NO.
          MESSAGE "All ship dates?"
              VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
          IF NOT ll-ans THEN DO:
            ld-date = oe-rel.rel-date.
            MESSAGE "Which ship date do you wish to update? " UPDATE ld-date.
          END.   */  
            IF ShipAllDate = "yes" THEN DO:
                FOR EACH  bf-rel WHERE bf-rel.company EQ oe-rel.company
                AND bf-rel.ord-no  EQ oe-rel.ord-no
                AND bf-rel.link-no EQ 0
                AND bf-rel.rel-date EQ ld-date EXCLUSIVE-LOCK:
            RUN oe/rel-stat.p (ROWID(bf-rel), OUTPUT lv-stat).
            IF INDEX("SLI",lv-stat) GT 0 THEN bf-rel.po-no = ApoNo. 
            
          END.
            END.
            
          
        
      END.

  
  find first bf-rel where recid(bf-rel) = recid(oe-rel)
                      EXCLUSIVE-LOCK no-error.
  if avail bf-rel then do:
      ASSIGN 
          bf-rel.ship-id  = AShipTo
          bf-rel.po-no    = ApoNo
          bf-rel.tot-qty  = DECIMAL(ASqty)
          bf-rel.carrier  = AVia

          .
     
      FIND FIRST lot-no WHERE lot-no.reftable EQ "oe-rel.lot-no"
                              AND lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
                               EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL lot-no THEN DO:
          CREATE lot-no.
          ASSIGN
              lot-no.reftable = "oe-rel.lot-no"
              lot-no.company  = STRING(oe-rel.r-no,"9999999999").
          
      END.
      ASSIGN lot-no.CODE = AlotNo
             lot-no.code2 = Afrtpay
             lot-no.dscr = Afob.
      
      RELEASE lot-no.

      
     FIND FIRST s-code WHERE s-code.reftable EQ "oe-rel.s-code"
                          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
                           NO-ERROR.
      IF NOT AVAIL s-code THEN DO:
          CREATE s-code.
          ASSIGN
              s-code.reftable = "oe-rel.s-code"
              s-code.company  = STRING(oe-rel.r-no,"9999999999").
          
      END.
       s-code.code = Asi.
       IF oe-ordl.is-a-component AND CAN-DO("B,I",s-code.code) THEN s-code.code = "S".
        
     
      if (li-ship-no <> 0 and li-ship-no <> bf-rel.ship-no)  then do:
          find oe-ord where oe-ord.company = bf-rel.company 
                        and oe-ord.ord-no = bf-rel.ord-no no-lock .
          RUN oe/custxship.p (bf-rel.company,
                              oe-ord.cust-no,
                              bf-rel.ship-id,
                              BUFFER shipto).
          if avail shipto then do:
              assign bf-rel.ship-no = shipto.ship-no
                  bf-rel.ship-addr[1] = shipto.ship-addr[1]
                  bf-rel.ship-addr[2] = shipto.ship-addr[2]
                  bf-rel.ship-city = shipto.ship-city
                  bf-rel.ship-state = shipto.ship-state
                  bf-rel.ship-zip = shipto.ship-zip
                  bf-rel.ship-i[1] = shipto.notes[1]
                  bf-rel.ship-i[2] = shipto.notes[2]
                  bf-rel.ship-i[3] = shipto.notes[3]
                  bf-rel.ship-i[4] = shipto.notes[4].
          END.
      end. 
      
      IF  oe-rel.rel-date NE ld-date THEN DO:
        
        {oe/rel-stat.i lv-stat}
            
            IF INDEX("ABW",lv-stat) GT 0 THEN DO:
                IF AVAIL oe-relh THEN DO TRANSACTION:
                    FIND CURRENT oe-relh.
                    oe-relh.rel-date = ld-date.
                    FIND CURRENT oe-relh NO-LOCK.  
                    
                END.
                IF AVAIL oe-relh THEN 
                    FOR EACH oe-rell WHERE oe-rell.company EQ oe-relh.company
                                       AND oe-rell.r-no    EQ oe-relh.r-no
                                      USE-INDEX r-no NO-LOCK,
                    FIRST oe-ord OF oe-rell:
                    IF HeaderDueDate = "yes" THEN oe-ord.due-date  = oe-relh.rel-date.
                    IF HeaderLastShipDate = "yes" THEN oe-ord.last-date = oe-relh.rel-date.
                    FOR EACH oe-ordl OF oe-ord WHERE oe-ordl.i-no EQ oe-rell.i-no
                                                 AND oe-ordl.line EQ oe-rell.line:
                        
                        IF LineItemDueDate = "yes" THEN do:
                            
                            oe-ordl.req-date  = oe-relh.rel-date.
                                FIND FIRST fg-set WHERE  fg-set.company = oe-relh.company
                                                    AND fg-set.part-no = oe-ordl.i-no NO-LOCK NO-ERROR.
                                IF AVAIL fg-set THEN DO:
                                    FIND FIRST bf-ordl OF oe-ord WHERE bf-ordl.i-no = fg-set.set-no NO-ERROR.
                                    IF AVAIL bf-ordl THEN ASSIGN bf-ordl.req-date = oe-relh.rel-date
                                         .
                                END.
                        END. 
                        IF LineItemLastShipDate = "yes" THEN do:
                            
                            oe-ordl.prom-date = oe-relh.rel-date.
                            FIND FIRST fg-set WHERE  fg-set.company = oe-relh.company
                                                AND fg-set.part-no = oe-ordl.i-no NO-LOCK NO-ERROR.
                            IF AVAIL fg-set THEN DO:
                                FIND FIRST bf-ordl OF oe-ord WHERE bf-ordl.i-no = fg-set.set-no NO-ERROR.
                                IF AVAIL bf-ordl THEN bf-ordl.prom-date = oe-relh.rel-date.
                            END.
                        END.
                    END.
                    END.
                    
            END.
            ELSE
                IF INDEX("SLI",lv-stat) GT 0 THEN DO:
                    
                    IF AVAIL oe-ordl AND oe-ordl.req-date GT ld-date THEN DO:
                        
                         IF LineItemDueDate = "yes" THEN do:
                            DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
                                FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).
                                b-oe-ordl.req-date = ld-date.
                                FIND CURRENT b-oe-ordl NO-LOCK NO-ERROR.
                                
                        END.
                    END.
                    FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
                    IF AVAIL oe-ord AND oe-ord.due-date GT ld-date THEN DO:
                        
                        IF HeaderDueDate = "yes" THEN DO:
                            FIND CURRENT oe-ord NO-ERROR.
                            oe-ord.due-date = ld-date.
                            FIND CURRENT oe-ord NO-LOCK NO-ERROR.
                            
                        END.
                    END.
                     ASSIGN bf-rel.rel-date = ld-date.
                END.

      END.
      ELSE DO:
          ASSIGN bf-rel.rel-date = ld-date.
          
      END.
   
      
      
     RELEASE bf-rel.
  END. /*if avail bf-rel then do:*/


    ASSIGN prmAction = "Select"
        prmA = "Sel".
    
END. /*IF prmAction = "updateRelease" THEN DO:*/
IF prmAction = "unpostActual"  THEN DO:

    FIND oe-rel WHERE RECID(oe-rel) = vRowid. 
    IF AVAIL oe-rel AND oe-rel.link-no EQ 0 THEN DO:
        ASSIGN cError = "Cannot unpost planned releases, posted only...".
            
        RETURN.
    END.
    FIND FIRST oe-ord
        WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
        NO-LOCK NO-ERROR.
    relstat = "".
            FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
            {oe/rel-stat.i relstat}


    FIND FIRST oe-rell NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          NO-ERROR.
      
    IF AVAIL oe-rell AND relstat EQ "P" THEN
        FIND FIRST oe-boll WHERE oe-boll.company  EQ oe-rell.company
                             AND oe-boll.ord-no   EQ oe-rell.ord-no
                             AND oe-boll.line     EQ oe-rell.line
                             AND oe-boll.i-no     EQ oe-rell.i-no
                             AND oe-boll.r-no     EQ oe-rell.r-no
                             AND oe-boll.rel-no   EQ oe-rell.rel-no
                             AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                             NO-LOCK NO-ERROR.
    IF INDEX("CZ",relstat) NE 0 THEN 
        MESSAGE "Cannot unpost, this release has been Invoiced..."
        VIEW-AS ALERT-BOX ERROR.
    ELSE
        IF AVAIL oe-boll THEN
           
            MESSAGE "Sorry, first you must delete BOL: " +
          TRIM(STRING(oe-boll.bol-no,">>>>>>>>>>")) + "..."
            VIEW-AS ALERT-BOX ERROR.
            
        
        ELSE DO:
            IF NOT choice THEN 
                MESSAGE "Warning, this will erase the actual release flag."
                "This will cause the item to appear as unreleased!"
                SKIP "Do you want to continue? "
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice . 
            IF choice THEN DO:
                /* Added to remove release qty from order line total release */
                FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel).
                FIND b-oe-ordl WHERE bf-oe-rel.company EQ b-oe-ordl.company 
                                 AND bf-oe-rel.ord-no  EQ b-oe-ordl.ord-no
                                 AND bf-oe-rel.i-no    EQ b-oe-ordl.i-no  
                                 AND bf-oe-rel.line    EQ b-oe-ordl.line .
                
                b-oe-ordl.t-rel-qty = b-oe-ordl.t-rel-qty - oe-rel.qty.

                FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.
                IF AVAIL oe-relh THEN DO:
                    FOR EACH oe-rell WHERE oe-rell.company EQ oe-rel.company
                                       AND oe-rell.r-no    EQ oe-rel.link-no
                                       AND oe-rell.i-no    EQ oe-rel.i-no
                                       AND oe-rell.line    EQ oe-rel.line
                                       AND oe-rell.link-no EQ oe-rel.r-no:
                        DELETE oe-rell.
                    END.
                    FIND FIRST oe-rell WHERE oe-rell.company EQ oe-relh.company
                                         AND oe-rell.r-no    EQ oe-relh.r-no
                                         USE-INDEX r-no NO-LOCK NO-ERROR.
                    IF NOT AVAIL oe-rell THEN DO:
                        oe-relh.posted = NO.
                        DELETE oe-relh.
                    END.
                END.
                bf-oe-rel.link-no = 0.
                FIND CURRENT b-oe-ordl NO-LOCK.
            END.  /* choice */
        END.  /* else */
        
 ASSIGN prmAction = "Select"
     prmA = "Sel".

END. /*IF prmAction = "unpostActual"  THEN DO:*/
IF prmAction = "delete" THEN DO:
  
  FIND oe-rel WHERE RECID(oe-rel) = vRowid. 
  IF NOT AVAIL oe-rel THEN DO:
      ASSIGN cError = "Manually added actual releases may not be deleted...".
             
      RETURN.
  END.

  FIND bf-rel WHERE RECID(bf-rel) = RECID(oe-rel). 
   
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK.
  relstat = "".
            FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
            {oe/rel-stat.i relstat}

  

  IF INDEX("CPZ",relstat) > 0 THEN DO:
      ASSIGN cError = "Posted releases may not be deleted, must delete BOL first!".
             
      RETURN.
  END.
  ELSE IF INDEX("ABW",relstat) > 0 THEN DO:
      ASSIGN cError =  "Actual or Backordered release should not be deleted Delete anyway?".
              
      IF ll-ans1 AND AVAIL oe-relh THEN DO:
         bf-rel.link-no = 0 .
         
         FOR EACH oe-rell WHERE oe-rell.company = bf-rel.company
                            AND oe-rell.r-no = oe-relh.r-no
                            AND oe-rell.i-no = bf-rel.i-no
                          USE-INDEX r-no:
             RUN oe/relldel1.p  (RECID(oe-rell)).
             DELETE oe-rell.
         END.
         FIND FIRST oe-rell WHERE oe-rell.company = bf-rel.company
                              AND oe-rell.r-no = oe-relh.r-no
                            USE-INDEX r-no NO-LOCK NO-ERROR.
         IF NOT AVAIL oe-rell THEN DO:
            FIND CURRENT oe-relh NO-ERROR.
            IF AVAIL oe-relh THEN DO: 
               DISABLE TRIGGERS FOR LOAD OF oe-relh.
               DELETE oe-relh.
            END.
         END.
      END.
      ELSE RETURN.
  END.
  FIND FIRST notes WHERE notes.rec_key EQ oe-rel.rec_key NO-ERROR.
  IF AVAIL notes THEN DELETE notes.
  IF AVAILABLE oe-rel THEN DO:
      DELETE oe-rel.
  END.
  
   ASSIGN prmAction = "Select"
       prmA = "Sel".
END.  /*IF prmAction = "delete" THEN DO:*/
    /*
IF prmAction = "Select" THEN DO:
    MESSAGE "prm" prmAction prmOrderNum prmItemNum.
    
FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.i-no = prmItemNum NO-LOCK :
          
      FOR EACH oe-rel WHERE oe-rel.company = oe-ordl.company 
                        AND oe-rel.ord-no = oe-ordl.ord-no 
                        AND oe-rel.i-no = oe-ordl.i-no
                        AND oe-rel.line    EQ oe-ordl.line
                         NO-LOCK:
          relstat = "".
          FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
          {oi/rel-stat.i relstat}
              create ttRel.
              assign 
                  ttRel.Part = oe-ordl.part-no
                  ttRel.ord-no = string(oe-ordl.ord-no) 
                  ttRel.line = string(oe-ordl.line)
                  ttRel.price= oe-ordl.t-price
                  ttRel.Vstatus = relstat
                  ttRel.vRowid  = RECID(oe-rel) 
                  .

                                  
               FIND FIRST oe-relh  where oe-relh.company  eq oe-rel.company
                                  and oe-relh.ord-no   eq oe-rel.ord-no  
                                  and oe-relh.rel-no   eq oe-rel.rel-no
                                  and oe-relh.b-ord-no eq oe-rel.b-ord-no
                                  and oe-relh.cust-no  eq oe-rel.cust-no
                                  use-index order NO-LOCK NO-ERROR.
             
               IF NOT AVAILABLE oe-relh THEN DO:
                   IF (TODAY + (5 * 365)) < oe-rel.rel-date THEN DO:
                       assign ttRel.Vdate = "Scheduled".
                   END.
                   ELSE DO:
                       assign ttRel.Vdate = string(oe-rel.rel-date).
                   END.
               END.
               ELSE DO:
                  IF (TODAY + (5 * 365)) < oe-relh.rel-date THEN DO:
                      assign ttRel.Vdate = "Scheduled".
                  END.
                  ELSE DO:
                      assign ttRel.Vdate = string(oe-relh.rel-date).
                   END.
                   
               END.
               FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.
               IF oe-relh.printed = TRUE THEN DO:
                       ASSIGN
                           ttRel.print         = "Y".
               END.
               ELSE DO:
                       ASSIGN
                           ttRel.print          = "N".
               END. 

               ll-transfer = CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-ordl.company
                           AND oe-ord.ord-no  EQ oe-ordl.ord-no
                           AND oe-ord.type    EQ "T").
               
MESSAGE "kulu5".
               FIND FIRST s-code WHERE s-code.reftable EQ "oe-rel.s-code"
                                   AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
                                    NO-LOCK NO-ERROR.
               ttRel.SI = IF ll-transfer            THEN "T"
                          ELSE
                              IF oe-ordl.is-a-component AND
                                  (NOT AVAIL s-code OR
                                   s-code.code NE "T")   THEN "S"
                               ELSE
                                   IF AVAIL s-code           THEN s-code.code
                                   ELSE
                                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                           ELSE "B".


             
              
               FIND FIRST ref-lot-no WHERE ref-lot-no.reftable EQ "oe-rel.lot-no" 
                                       AND ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
                                        NO-LOCK NO-ERROR.
               IF AVAIL ref-lot-no THEN DO:
                   ttRel.lot-no = ref-lot-no.CODE.
                   
               END.
                 FIND FIRST ref-sell-price WHERE ref-sell-price.reftable EQ "oe-rel.sell-price" 
                                           AND ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")
                                            NO-LOCK NO-ERROR.
               IF AVAIL ref-sell-price THEN DO:
                   ASSIGN
                   ttRel.sell-price = ref-sell-price.val[1].
                   
               END. 
                              assign
                                 
                                  ttRel.qty = oe-rel.qty
                                  ttRel.po-no = oe-rel.po-no
                                  ttRel.ShipTo = oe-rel.ship-id
                                  ttRel.addr = STRING(oe-rel.ship-addr[1] + " " + oe-rel.ship-addr[2])
                                  ttRel.city = oe-rel.ship-city
                                  ttRel.SQty = oe-rel.tot-qty
                                  ttRel.state = oe-rel.ship-state
                                  ttRel.Via = oe-rel.carrier
                                  .
                               
                                 
                
          END. 
 END.     
END. /* prmAction = select*/
*/
/*RELEASE*/
IF prmAction = "Releaseitem" THEN DO:

    DEFINE VARIABLE fil_id AS RECID.
    FIND FIRST oe-rel WHERE RECID(oe-rel) = vRowid  NO-LOCK NO-ERROR.


    
    FIND FIRST oe-ctrl WHERE oe-ctrl.company = oe-rel.company NO-LOCK NO-ERROR.
    ASSIGN ARowid = vRowid.
    /*{sys/inc/addrelse.i}*/
        FIND FIRST oe-ordl WHERE
        oe-ordl.company EQ oe-rel.company AND
        oe-ordl.ord-no = INT(prmOrderNum) AND
        oe-ordl.LINE = int(prmItemNum) 
        NO-LOCK NO-ERROR.
    
    FIND FIRST xoe-ord WHERE xoe-ord.company = oe-ordl.company 
                         AND xoe-ord.ord-no = oe-ordl.ord-no NO-LOCK.
RUN check-release NO-ERROR.

RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

if index("ABCPZW", lv-stat) > 0 then do:
    /*  message entry(index("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice")
    "release entries can not be modified"
    view-as alert-box error.*/
    return .
end. /**if index("ABCPZ", lv-stat) > 0 then do:*/
v-auto = NO.
fil_id = recid(oe-ordl).  

run oe/actrel.p (recid(oe-rel)).

/*ASSIGN ttRel.qty = oe-rel.qty.*/
 /*Email logic*/
v-prgmname = "actrel.".
    FOR EACH tt-email,
       FIRST cust WHERE
       cust.company = oe-rel.company AND
       cust.cust-no = tt-email.cust-no /*AND
       cust.active = "E"*/
       NO-LOCK
       BREAK BY tt-email.ord-no:       

       IF FIRST-OF(tt-email.ord-no) THEN
          vBody = "Order Number " +  STRING(tt-email.ord-no)
                      + " has been released." + CHR(10).

       vBody = vBody  + "<br>"
                   + "Item: " + STRING(tt-email.i-no,"X(15)") + "&nbsp;&nbsp;"
                   + " Qty: " + STRING(tt-email.rel-qty,"->>,>>>,>>9") + "<br>"  
                   + " Date: " + STRING(tt-email.rel-date,"99/99/99") + "&nbsp;&nbsp;" 
                   + " PO#: "  + STRING(tt-email.po-no,"X(15)") . 
       
              IF LAST-OF(tt-email.ord-no) THEN do:                  
       
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}
               
           IF ls-to-list NE '' THEN DO:
             ASSIGN vMailto =  ls-to-list
                    vSubject = "Release Generated".
           END.    

               FIND FIRST users WHERE  users.USER_id = prmUser  NO-LOCK NO-ERROR.
                 IF AVAIL users THEN DO:
                         ASSIGN                              
                             vInternalUser = STRING(users.internal-user) .
                 END.

                 IF vMailto = "" THEN
                ASSIGN 
                    vMailto = vMailto +  users.image_filename.

                 IF vMailto <>  users.image_filename AND vMailto <> "" THEN
                ASSIGN 
                    vMailto = vMailto + "," +  users.image_filename.


       END. /* last-of(tt-email.cust-no) */       
   END.
   


PROCEDURE mail EXTERNAL "xpMail.dll" :
   DEF INPUT PARAM mailTo AS CHAR.
   DEF INPUT PARAM mailsubject AS CHAR.
   DEF INPUT PARAM mailText AS CHAR.
   DEF INPUT PARAM mailFiles AS CHAR.
   DEF INPUT PARAM mailDialog AS LONG.
   DEF OUTPUT PARAM retCode AS LONG.
END.

  

  RELEASE xoe-ord.
  RELEASE xoe-ordl.

RUN notify-source.


ASSIGN prmAction = "Select"
       prmA = "Sel".


END.   /*prmAction = "Releaseitem"*/
/**********************************************************************************************************/

IF prmAction = "SelectRow" THEN DO:

FOR EACH oe-ordl where
    oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = int(prmOrderNum) AND
    oe-ordl.LINE = int(prmItemNum) NO-LOCK :
    
    FIND oe-rel WHERE RECID(oe-rel) = vRowid NO-LOCK.
    RUN create-report-record (ROWID(oe-rel), NO).
          
 END.     
END. /* prmAction = select*/



IF prmAction = "ValidateBolinv" THEN DO:

ASSIGN ARowid = vRowid.
FIND oe-rel WHERE RECID(oe-rel) EQ ARowid NO-LOCK NO-ERROR.
FIND bf-rel WHERE RECID(bf-rel) = RECID(oe-rel). 

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ oe-rel.company
      AND sys-ctrl.name    EQ "ADDXFER"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = oe-rel.company
   sys-ctrl.name    = "ADDXFER"
   sys-ctrl.module  = "OU1"
   sys-ctrl.descrip = "When creating actual transfer releases, inhouse customer?"
   sys-ctrl.int-fld = 0.
end.
    assign
        addxfer-log = sys-ctrl.log-fld.

/*{sys/inc/addxfer.i}*/


FIND oe-rel WHERE RECID(oe-rel) = vRowid.
find xoe-ord where xoe-ord.company = g_company and
    xoe-ord.ord-no = oe-rel.ord-no no-lock.

find first oe-ctrl where oe-ctrl.company = xoe-ord.company no-lock .

FIND FIRST oe-ordl WHERE oe-ordl.company EQ oe-rel.company AND
                         oe-ordl.ord-no = int(prmOrderNum) AND
                         oe-ordl.LINE = int(prmItemNum) NO-LOCK .

/*{sys/inc/addrelse.i}*/

choice = no.

IF lv-msg EQ "" AND xoe-ord.stat eq "H" THEN
    lv-msg = "customers on Credit Hold".
IF lv-msg EQ "" AND xoe-ord.stat EQ "W" THEN
    lv-msg = "unapproved web orders".
IF lv-msg EQ "" AND NOT xoe-ord.opened THEN
    lv-msg = "closed orders".
IF lv-msg EQ "" AND TRIM(oe-ordl.job-no) NE ""
                AND CAN-FIND(FIRST job
                             WHERE job.company EQ oe-ordl.company
                               AND job.job-no  EQ oe-ordl.job-no
                               AND job.job-no2 EQ oe-ordl.job-no2
                               AND job.stat    EQ "H") THEN
    lv-msg = "jobs on hold".


IF lv-msg NE "" THEN DO:

 cError = lv-msg.
  RETURN.
END. /*IF lv-msg NE "" THEN DO:*/ 


/* gdm - 02020902 */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name EQ "RELCREDT" NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
    
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ oe-ordl.company
          AND cust.cust-no EQ oe-ordl.cust-no NO-ERROR.
    IF AVAIL cust THEN RUN oe/CRcheck.p (INPUT ROWID(cust),
                                         INPUT YES,
                                         OUTPUT v-chkflg).
    IF v-chkflg THEN DO:

        ASSIGN cError = "Can't create BOL, there are unpaid invoices. Please create actual release first".
        RETURN.    
    END.
END.

ASSIGN
lv-save-recid = RECID(oe-rel).
v-first = YES.


SESSION:SET-WAIT-STATE("general").

    FIND FIRST xoe-ordl OF oe-rel NO-LOCK NO-ERROR.

    {oe/rel-stat.i lv-stat x}.  

    IF INDEX("CPZ",lv-stat) GT 0 THEN DO:
      cError = ENTRY(INDEX("ABCPZ",lv-stat),"Actual,Backorder,Completed,Posted,Invoice") +
              " release entries can not be released.". 
      RETURN.
    END.

    if v-first then do on endkey undo, retry:
         assign  v-first = no
                 choice  = yes.
        /*ASSIGN cError = "Create BOL for Release Date-" + trim(string(bf-rel.rel-date)) +
                " and ShipID-" +  trim(bf-rel.ship-id) + " ?".*/
               
    end.

END.  /* end of validate create bol */






IF prmAction = "Bolinvcreate" THEN DO:

ASSIGN ARowid = vRowid.

FIND oe-rel WHERE RECID(oe-rel) EQ ARowid NO-LOCK NO-ERROR.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ oe-rel.company
      AND sys-ctrl.name    EQ "ADDXFER"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = oe-rel.company
   sys-ctrl.name    = "ADDXFER"
   sys-ctrl.module  = "OU1"
   sys-ctrl.descrip = "When creating actual transfer releases, inhouse customer?"
   sys-ctrl.int-fld = 0.
end.
assign
 addxfer-log = sys-ctrl.log-fld.
/*{sys/inc/addxfer.i}*/

FIND oe-rel WHERE RECID(oe-rel) = vRowid.
FIND bf-rel WHERE RECID(bf-rel) = RECID(oe-rel). 

find xoe-ord where xoe-ord.company = g_company and
                   xoe-ord.ord-no = oe-rel.ord-no no-lock.

find first oe-ctrl where oe-ctrl.company = xoe-ord.company no-lock .

FIND FIRST oe-ordl WHERE oe-ordl.company EQ oe-rel.company AND
                         oe-ordl.ord-no = int(prmOrderNum) AND
                         oe-ordl.LINE = int(prmItemNum) NO-LOCK .

/*{sys/inc/addrelse.i}*/

choice = no.

/* gdm - 02020902 */

ASSIGN
lv-save-recid = RECID(oe-rel).
v-first = YES.


SESSION:SET-WAIT-STATE("general").

    FIND FIRST xoe-ordl OF oe-rel NO-LOCK NO-ERROR.
 
    if v-first then do on endkey undo, retry:
         assign  v-first = no
                 choice  = yes.
        /*ASSIGN cError = "Create BOL for Release Date-" + trim(string(bf-rel.rel-date)) +
                " and ShipID-" +  trim(bf-rel.ship-id) + " ?".*/
               
    end.

    if choice then do:
        out-recid = recid(oe-rel).
        IF NOT AVAIL oe-rell THEN DO:
       
            v-cust-no = oe-rel.cust-no.
           
            IF addxfer-log THEN
            DO:
               FIND FIRST s-code WHERE
                    s-code.reftable EQ "oe-rel.s-code" AND
                    s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
                    NO-LOCK NO-ERROR.
              
               IF AVAIL s-code THEN
               DO:
                  IF s-code.CODE EQ 'T' AND lv-cust-x NE "" AND
                    CAN-FIND(FIRST shipto WHERE
                    shipto.company EQ cocode AND
                    shipto.cust-no EQ lv-cust-x AND
                    shipto.ship-no EQ oe-rel.ship-no AND
                    shipto.ship-id EQ oe-rel.ship-id) THEN
                    v-cust-no = lv-cust-x.
                  
                  RELEASE s-code.
               END.
            END.
            
            {oe/findrelh.i oe-rel v-cust-no}
            IF AVAIL oe-relh THEN
            FIND LAST oe-rell
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no NO-LOCK NO-ERROR.
        END.
            IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
            ELSE do:
                ASSIGN
                v-auto = YES
                out-recid = recid(oe-rel).
                RUN oe/relbol.p (RECID(xoe-ordl)).
                v-auto = NO.               
            END.
            
            run oe/do-bol.p(INPUT NOT v-invoice).
            
            
            /*FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
            IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
            FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.    */         
    END.
        
  RELEASE xoe-ord.
  RELEASE xoe-ordl.
  

/*RUN notify-source.*/
  ASSIGN prmAction = "Select"
     prmA = "Sel".

SESSION:SET-WAIT-STATE("").

END.



IF prmAction = "Select" THEN DO:


/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-nxt-r-no LIKE oe-rel.r-no NO-UNDO.
DEF VAR lv-qty AS INT NO-UNDO.

DEF VAR lv-s-code LIKE oe-rell.s-code EXTENT 2 NO-UNDO.

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rell-exc FOR oe-rell.


FIND FIRST oe-ordl where
     oe-ordl.company EQ prmComp AND
     oe-ordl.ord-no = int(prmOrderNum) AND
     oe-ordl.LINE = int(prmItemNum) NO-LOCK .
  ll-transfer = CAN-FIND(FIRST oe-ord
                         WHERE oe-ord.company EQ oe-ordl.company
                           AND oe-ord.ord-no  EQ oe-ordl.ord-no
                           AND oe-ord.type    EQ "T").

 /* RUN delete-phantoms.*/

  FOR EACH ttRel:
    DELETE ttRel.
  END.
   
  /*RUN oe/cleanrel.p (ROWID(oe-ordl)).*/
  
  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item
      BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no
      TRANSACTION:
       
    IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN
      RUN create-report-record (ROWID(oe-rel), NO).
    
  END.

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company  EQ oe-ordl.company
        AND oe-boll.ord-no   EQ oe-ordl.ord-no
        AND oe-boll.i-no     EQ oe-ordl.i-no
        AND oe-boll.line     EQ oe-ordl.line
      USE-INDEX ord-no,

      FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
      
      FIRST oe-rell NO-LOCK
      WHERE oe-rell.company  EQ oe-boll.company
        AND oe-rell.ord-no   EQ oe-boll.ord-no
        AND oe-rell.line     EQ oe-boll.line
        AND oe-rell.i-no     EQ oe-boll.i-no
        AND oe-rell.r-no     EQ oe-boll.r-no
        AND oe-rell.rel-no   EQ oe-boll.rel-no
        AND oe-rell.b-ord-no EQ oe-boll.b-ord-no
        AND oe-rell.po-no    EQ oe-boll.po-no
      USE-INDEX ord-no,

      FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-boll.r-no
      
      BREAK BY oe-boll.r-no
            BY oe-boll.rel-no
            BY oe-boll.b-ord-no
            BY oe-boll.po-no

      TRANSACTION:

    IF FIRST-OF(oe-boll.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-boll.qty.

    IF LAST-OF(oe-boll.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE oe-rel.
      IF oe-rell.link-no NE 0 THEN
      FIND oe-rel NO-LOCK
          WHERE oe-rel.r-no EQ oe-rell.link-no
          USE-INDEX seq-no NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.link-no  EQ oe-rell.r-no
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX link NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-rell.company
            AND oe-rel.ord-no   EQ oe-rell.ord-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-no   EQ oe-rell.rel-no
            AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
            AND oe-rel.po-no    EQ oe-rell.po-no
          USE-INDEX ord-item NO-ERROR.

      IF AVAIL oe-rel THEN
      FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

      IF AVAIL oe-rel THEN DO:
        FIND CURRENT oe-rel.
        ASSIGN
         oe-rel.link-no  = oe-rell.r-no
         oe-rel.rel-no   = oe-rell.rel-no
         oe-rel.b-ord-no = oe-rell.b-ord-no
         oe-rel.po-no    = oe-rell.po-no
         oe-rel.qty      = lv-qty.

        FOR EACH b-oe-rell NO-LOCK
            WHERE b-oe-rell.company  EQ oe-rel.company
              AND b-oe-rell.r-no     EQ oe-rel.link-no
              AND b-oe-rell.ord-no   EQ oe-rel.ord-no
              AND b-oe-rell.i-no     EQ oe-rel.i-no
              AND b-oe-rell.line     EQ oe-rel.line
              AND b-oe-rell.rel-no   EQ oe-rel.rel-no
              AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND b-oe-rell.po-no    EQ oe-rel.po-no
            USE-INDEX r-no:
          FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
              EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
        END.
        
      END.

      ELSE DO:
        FIND FIRST oe-rel NO-LOCK USE-INDEX seq-no NO-ERROR.
        v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.

        CREATE oe-rel.
        ASSIGN
         oe-rel.company   = oe-relh.company
         oe-rel.r-no      = v-nxt-r-no
         oe-rel.link-no   = oe-rell.r-no
         oe-rel.cust-no   = oe-relh.cust-no
         oe-rel.ord-no    = oe-rell.ord-no
         oe-rel.i-no      = oe-rell.i-no
         oe-rel.line      = oe-rell.line
         oe-rel.rel-no    = oe-rell.rel-no
         oe-rel.b-ord-no  = oe-rell.b-ord-no
         oe-rel.rel-date  = oe-relh.rel-date
         oe-rel.carrier   = oe-relh.carrier
         oe-rel.ship-no   = oe-relh.ship-no
         oe-rel.ship-id   = oe-relh.ship-id
         oe-rel.ship-i[1] = oe-relh.ship-i[1]
         oe-rel.ship-i[2] = oe-relh.ship-i[2]
         oe-rel.ship-i[3] = oe-relh.ship-i[3]
         oe-rel.ship-i[4] = oe-relh.ship-i[4]
         oe-rel.po-no     = oe-boll.po-no
         oe-rel.qty       = lv-qty.
          
        RUN oe/custxship.p (oe-rel.company,
                            oe-rel.cust-no,
                            oe-rel.ship-id,
                            BUFFER shipto).

        if avail shipto then
          assign
           oe-rel.ship-addr[1] = shipto.ship-addr[1]
           oe-rel.ship-addr[2] = shipto.ship-addr[2]
           oe-rel.ship-city    = shipto.ship-city
           oe-rel.ship-state   = shipto.ship-state
           oe-rel.ship-zip     = shipto.ship-zip.
        RUN create-report-record (ROWID(oe-rel), NO).
       
      END.
    END.
  END.

  FOR EACH oe-rell
      WHERE oe-rell.company  EQ oe-ordl.company
        AND oe-rell.ord-no   EQ oe-ordl.ord-no
        AND oe-rell.i-no     EQ oe-ordl.i-no
        AND oe-rell.line     EQ oe-ordl.line
        AND NOT CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ oe-rell.company
                           AND oe-boll.r-no     EQ oe-rell.r-no
                           AND oe-boll.ord-no   EQ oe-rell.ord-no
                           AND oe-boll.i-no     EQ oe-rell.i-no
                           AND oe-boll.line     EQ oe-rell.line
                           AND oe-boll.rel-no   EQ oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ oe-rell.po-no
                         USE-INDEX ord-no)
      USE-INDEX ord-no NO-LOCK,

      FIRST oe-relh NO-LOCK
      WHERE oe-relh.r-no    EQ oe-rell.r-no
        AND oe-relh.posted EQ NO 
      
      BREAK BY oe-rell.r-no
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no

      TRANSACTION:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.
   
    IF LAST-OF(oe-rell.po-no) AND lv-qty NE 0 THEN DO:
      RELEASE b-oe-rell.
      IF oe-relh.posted THEN
      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ oe-rell.company
            AND b-oe-rell.r-no    EQ oe-rell.r-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            AND CAN-FIND(FIRST oe-boll
                         WHERE oe-boll.company  EQ b-oe-rell.company
                           AND oe-boll.ord-no   EQ b-oe-rell.ord-no
                           AND oe-boll.i-no     EQ b-oe-rell.i-no
                           AND oe-boll.line     EQ b-oe-rell.line
                           AND oe-boll.r-no     EQ b-oe-rell.r-no
                           AND oe-boll.rel-no   EQ b-oe-rell.rel-no
                           AND oe-boll.b-ord-no EQ b-oe-rell.b-ord-no
                           AND oe-boll.po-no    EQ b-oe-rell.po-no
                         USE-INDEX ord-no)
          USE-INDEX r-no NO-LOCK:

        LEAVE.
      END.

      IF NOT AVAIL b-oe-rell THEN DO:
        RELEASE oe-rel.
        IF oe-rell.link-no NE 0 AND oe-relh.posted THEN
        FIND oe-rel NO-LOCK
            WHERE oe-rel.r-no EQ oe-rell.link-no
            USE-INDEX seq-no NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.link-no  EQ oe-rell.r-no
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX link NO-ERROR.
        IF NOT AVAIL oe-rel THEN
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-rell.company
              AND oe-rel.ord-no   EQ oe-rell.ord-no
              AND oe-rel.i-no     EQ oe-rell.i-no
              AND oe-rel.line     EQ oe-rell.line
              AND oe-rel.rel-no   EQ oe-rell.rel-no
              AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
              AND oe-rel.po-no    EQ oe-rell.po-no
            USE-INDEX ord-item NO-ERROR.

        IF NOT AVAIL oe-rel THEN DO:
          FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.
          v-nxt-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.
           
          CREATE oe-rel.
          ASSIGN
           oe-rel.company   = oe-relh.company
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.link-no   = IF oe-relh.posted THEN oe-rell.r-no ELSE 0
           oe-rel.cust-no   = oe-relh.cust-no
           oe-rel.ord-no    = oe-rell.ord-no
           oe-rel.i-no      = oe-rell.i-no
           oe-rel.line      = oe-rell.line
           oe-rel.rel-no    = oe-rell.rel-no
           oe-rel.b-ord-no  = oe-rell.b-ord-no
           oe-rel.rel-date  = oe-relh.rel-date
           oe-rel.carrier   = oe-relh.carrier
           oe-rel.ship-no   = oe-relh.ship-no
           oe-rel.ship-id   = oe-relh.ship-id
           oe-rel.ship-i[1] = oe-relh.ship-i[1]
           oe-rel.ship-i[2] = oe-relh.ship-i[2]
           oe-rel.ship-i[3] = oe-relh.ship-i[3]
           oe-rel.ship-i[4] = oe-relh.ship-i[4]
           oe-rel.po-no     = oe-rell.po-no
           oe-rel.qty       = lv-qty.
          
          RUN oe/custxship.p (oe-rel.company,
                              oe-rel.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).

          if avail shipto then
            assign
             oe-rel.ship-addr[1] = shipto.ship-addr[1]
             oe-rel.ship-addr[2] = shipto.ship-addr[2]
             oe-rel.ship-city    = shipto.ship-city
             oe-rel.ship-state   = shipto.ship-state
             oe-rel.ship-zip     = shipto.ship-zip.

          RUN create-report-record (ROWID(oe-rel), NO).
          
        END.

        ELSE DO:
          FIND CURRENT oe-rel EXCLUSIVE NO-ERROR NO-WAIT.

          IF AVAIL oe-rel THEN DO:
            IF oe-relh.posted THEN DO:
              ASSIGN
               oe-rel.link-no  = oe-rell.r-no
               oe-rel.rel-no   = oe-rell.rel-no
               oe-rel.b-ord-no = oe-rell.b-ord-no
               oe-rel.po-no    = oe-rell.po-no
               oe-rel.qty      = lv-qty.
               
              FOR EACH b-oe-rell NO-LOCK
                  WHERE b-oe-rell.company  EQ oe-rel.company
                    AND b-oe-rell.r-no     EQ oe-rel.link-no
                    AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                    AND b-oe-rell.i-no     EQ oe-rel.i-no
                    AND b-oe-rell.line     EQ oe-rel.line
                    AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                    AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND b-oe-rell.po-no    EQ oe-rel.po-no
                  USE-INDEX r-no:
                FIND b-oe-rell-exc WHERE ROWID(b-oe-rell-exc) EQ ROWID(b-oe-rell)
                    EXCLUSIVE NO-ERROR NO-WAIT.
                IF AVAIL b-oe-rell-exc THEN b-oe-rell-exc.link-no = oe-rel.r-no.
              END.
              
            END.

            ELSE DO:
              IF oe-rel.link-no NE 0 THEN oe-rel.link-no = 0.
              FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
              IF AVAIL tt-report THEN tt-report.qty = lv-qty.

              
            END.
          END.
        END.
      END.
    END.
  END.

  /*FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no EQ 0
      USE-INDEX ord-item
      TRANSACTION:

    FIND FIRST s-code
        WHERE s-code.reftable EQ "oe-rel.s-code"
          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    lv-s-code[1] = IF AVAIL s-code THEN s-code.code ELSE "B".

    FOR EACH b-oe-rel
        WHERE b-oe-rel.company  EQ oe-rel.company
          AND b-oe-rel.ord-no   EQ oe-rel.ord-no
          AND b-oe-rel.i-no     EQ oe-rel.i-no
          AND b-oe-rel.line     EQ oe-rel.line
          AND b-oe-rel.po-no    EQ oe-rel.po-no
          AND b-oe-rel.ship-id  EQ oe-rel.ship-id
          AND b-oe-rel.rel-date EQ oe-rel.rel-date
          AND b-oe-rel.carrier  EQ oe-rel.carrier
          AND b-oe-rel.qty      EQ oe-rel.qty
          AND b-oe-rel.link-no  EQ 0
          AND ROWID(b-oe-rel)   NE ROWID(oe-rel)
        USE-INDEX ord-item:

      FIND FIRST s-code
          WHERE s-code.reftable EQ "oe-rel.s-code"
            AND s-code.company  EQ STRING(b-oe-rel.r-no,"9999999999")
          NO-LOCK NO-ERROR.
          
      lv-s-code[2] = IF AVAIL s-code THEN s-code.code ELSE "B".

      IF lv-s-code[1] EQ lv-s-code[2] THEN DELETE b-oe-rel.
    END.
  END.*/
/*
  FOR EACH oe-rel
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
      USE-INDEX ord-item:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
    IF INDEX("SIL",lv-stat) GT 0 OR 
       (INDEX("CZ",lv-stat) LE 0 AND oe-rel.qty EQ 0) THEN
      oe-rel.qty = oe-rel.tot-qty.
     
  END.
*/

IF NOT AVAILABLE ttRel THEN DO:
    IF prmAction = "Select" AND prmA <> "Sel" THEN DO:
    ASSIGN prmAction = "Addrelease1" .
    END.
    
END.


  RELEASE oe-rel.
  RELEASE b-oe-rell.
  RELEASE oe-rell.
  RELEASE oe-boll.
  


END.

PROCEDURE create-report-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
      
  

  FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL oe-rel THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company EQ oe-rel.company 
        AND oe-ord.ord-no  EQ oe-rel.ord-no
      NO-LOCK NO-ERROR.

  IF AVAIL oe-ord THEN DO:
       FIND FIRST tt-report
        WHERE tt-report.rec-id EQ RECID(oe-rel)
        NO-ERROR.

    IF NOT AVAIL tt-report THEN CREATE tt-report.


        {oe/rel-stat.i relstat}

    RELEASE inv-line.
    IF relstat EQ "Z" AND AVAIL oe-boll THEN
    FIND FIRST inv-line
        WHERE inv-line.company EQ oe-boll.company
          AND inv-line.b-no    EQ oe-boll.b-no
          AND inv-line.ord-no  EQ oe-boll.ord-no
          AND inv-line.i-no    EQ oe-boll.i-no
          AND inv-line.po-no   NE ""
        NO-LOCK NO-ERROR.

    RUN create-report-record-1 (ip-phantom,
                                IF AVAIL oe-relh THEN oe-relh.rel-date
                                                 ELSE oe-rel.rel-date).
                                                      
  END.

END PROCEDURE.

FUNCTION get-rel-stat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  IF NOT AVAIL oe-rel THEN
  FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.

  RUN oe/rel-stat.p (IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?, OUTPUT lv-stat).
 

  RETURN lv-stat.
  
END FUNCTION.

FUNCTION get-rel-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :

  IF NOT AVAIL oe-rel THEN DO:
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      FIND FIRST tt-report WHERE tt-report.rec-id EQ RECID(oe-rel) NO-ERROR.
  END.


 RETURN IF /*(NOT oereleas-log AND INDEX("AB",get-rel-stat()) GT 0) OR*/
            INDEX("SIL",get-rel-stat()) GT 0                       THEN 0
         ELSE
         IF AVAIL oe-rel THEN oe-rel.qty
         ELSE INT(oe-rel.qty).

END FUNCTION.



PROCEDURE create-report-record-1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-phantom AS LOG NO-UNDO.
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.
  DEFINE VAR vPrint AS LOG.
  
 

    relstat = "".
          FIND FIRST oe-ord OF oe-rel NO-LOCK NO-ERROR.
          {oe/rel-stat.i relstat}
              create ttRel.
              assign 
                  ttRel.Part = oe-ordl.part-no
                  ttRel.ord-no = string(oe-ordl.ord-no) 
                  ttRel.line = string(oe-ordl.line)
                  ttRel.price= oe-ordl.t-price
                  ttRel.Vstatus = relstat
                  ttRel.vRowid  = RECID(oe-rel) 
                  .
              IF prmAction = "AddRelease1" THEN DO:
                  ASSIGN ttRel.aRowid  = RECID(oe-rel).
              END.
            
              ASSIGN
                  lv-rel-recid  = RECID(oe-rel) .
              
              
    ASSIGN
      
    ttRel.Vdate  = ip-date
    ttRel.po-no   = oe-rel.po-no
     vPrint = (AVAIL oe-relh AND oe-relh.printed) OR
                         INDEX("PCZ",relstat) GT 0.
    
    IF vPrint = TRUE THEN DO:
        ASSIGN
            ttRel.print         = "Y".
    END.
    ELSE DO:
        ASSIGN
            ttRel.print          = "N".
    END. 

    FIND FIRST s-code
        WHERE s-code.reftable EQ "oe-rel.s-code"
          AND s-code.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-LOCK NO-ERROR.
    ttRel.SI = IF ll-transfer            THEN "T"
                       ELSE
                       IF oe-ordl.is-a-component AND
                          (NOT AVAIL s-code OR
                           s-code.code NE "T")   THEN "S"
                       ELSE
                       IF AVAIL s-code           THEN s-code.code
                       ELSE
                       IF AVAIL oe-rell          THEN oe-rell.s-code
                                                 ELSE "B".

    FIND FIRST ref-lot-no WHERE
         ref-lot-no.reftable EQ "oe-rel.lot-no" AND
         ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-lot-no THEN
    DO:
       ASSIGN ttRel.lot-no = ref-lot-no.CODE
              ttrel.frtpay = ref-lot-no.code2
              ttrel.fob = ref-lot-no.dscr .
       
       RELEASE ref-lot-no.
       
    END.

    FIND FIRST ref-sell-price WHERE
         ref-sell-price.reftable EQ "oe-rel.sell-price" AND
         ref-sell-price.company  EQ STRING(oe-rel.r-no,"9999999999")
         NO-LOCK NO-ERROR.

    IF AVAIL ref-sell-price THEN
    DO:
       ttRel.sell-price = ref-sell-price.val[1].
       RELEASE ref-sell-price.
    END.
    
  assign
    /* ttRel.qty = oe-rel.qty*/
     ttRel.ShipTo = oe-rel.ship-id
     ttRel.addr = STRING(oe-rel.ship-addr[1] + " " + oe-rel.ship-addr[2])
     ttRel.city = oe-rel.ship-city
     ttRel.SQty = oe-rel.tot-qty
     ttRel.state = oe-rel.ship-state
     ttRel.Via = oe-rel.carrier 
     ttRel.Qty = get-rel-qty()
     .
   
  
END PROCEDURE.


/***************************************************************************/
PROCEDURE check-release:
DEF VAR lv-msg AS CHAR NO-UNDO.
IF lv-msg EQ "" AND xoe-ord.stat eq "H" THEN
    lv-msg = "customers on Credit Hold".
IF lv-msg EQ "" AND xoe-ord.stat EQ "W" THEN
    lv-msg = "unapproved web orders".
IF lv-msg EQ "" AND NOT xoe-ord.opened THEN
    lv-msg = "closed orders".
IF lv-msg EQ "" AND TRIM(oe-ordl.job-no) NE ""
                AND CAN-FIND(FIRST job
                             WHERE job.company EQ oe-ordl.company
                               AND job.job-no  EQ oe-ordl.job-no
                               AND job.job-no2 EQ oe-ordl.job-no2
                               AND job.stat    EQ "H") THEN
    lv-msg = "jobs on hold".


IF lv-msg NE "" THEN DO:

 /* MESSAGE "Can't release items for " +
          TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.*/
END. /*IF lv-msg NE "" THEN DO:*/ 

END PROCEDURE.

/***********************************************************************/
PROCEDURE notify-source:
    DEF VAR char-hdl AS cha NO-UNDO.
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    IF AVAIL oe-ordl THEN DO:
        lv-rowid = IF AVAIL oe-rel THEN ROWID(oe-rel) ELSE ?.
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN DO:
            /*      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"bolrel-source",OUTPUT char-hdl).*/
            IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
                RUN reposit-item IN WIDGET-HANDLE(char-hdl) (RECID(oe-ord), RECID(oe-ordl)).
        END.  /*if avail oe-ord*/
        /*RUN reopen-query.
        IF lv-rowid NE ? THEN RUN repo-query (lv-rowid).*/
    END.   /*IF AVAIL oe-ordl THEN DO:*/
    
    RUN enable-ticket.
END PROCEDURE.

/***********************************************************************************************************************************/

PROCEDURE enable-ticket :
    DEFINE VARIABLE relStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
    out-recid = ?.
    IF AVAIL oe-rel THEN DO:
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-rel.company 
             AND oe-ord.ord-no  EQ oe-rel.ord-no
             NO-ERROR.
        {oe/rel-stat.i relStatus}
            /*{methods/run_link.i "container-source" "relTicketEnabled" "(CAN-DO('A,B',relStatus))"}*/
            IF NOT CAN-DO('A,B,W',relStatus) THEN RETURN.
            /*FIND FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-rel.link-no NO-ERROR.*/
            IF AVAIL oe-relh THEN DO:
                IF AVAIL oe-rell THEN out-recid = RECID(oe-rell).
                /*RUN oe/setUserPrint.p (oe-rel.company,'oe-relh_.',
                                       'begin_cust-no,end_cust-no,begin_relnum,end_relnum,begin_ord-no,end_ord-no,tb_printed,tb_posted',
                                       oe-relh.cust-no + ',' + oe-relh.cust-no + ',' +
                                       STRING(oe-relh.release#) + ',' + STRING(oe-relh.release#) + ',' +
                                       STRING(oe-rel.ord-no) + ',' + STRING(oe-rel.ord-no) + ',' +
                                       STRING(oe-relh.printed) + ',' + STRING(oe-relh.posted)).*/
            END.
    END.  /*iF AVAIL oe-rel THEN DO:*/

END PROCEDURE.
/*******************************************************************************************************************************/
PROCEDURE relticket-printed :
  FIND oe-rell WHERE RECID(oe-rell) EQ out-recid NO-LOCK NO-ERROR.
  IF AVAIL oe-rell AND oe-rell.link-no NE 0 THEN
  FIND oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

  RUN notify-source.

END PROCEDURE.
/************************************************************************************************************************/
/*******************************************************************/
/**Addrelease1*/


IF prmAction = "Addrelease1" THEN DO:

 def var v-qty-sum as int no-undo.
  
  def var v-lst-rel as date INIT TODAY no-undo.
  def var v-pct-chg as dec no-undo.
  def var v-ship-id like oe-rel.ship-id no-undo.
  def var v-carrier like oe-rel.carrier no-undo.
  def var v-num-shipto as int no-undo.
  def var v-qty-mod as log no-undo.
  
  
 CREATE oe-rel.
  
  assign v-qty-sum  = 0.

  FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND
       oe-ordl.ord-no = INT(prmOrderNum) AND
       oe-ordl.LINE = int(prmItemNum)
       NO-LOCK.

  if avail oe-ordl then do:
      find FIRST bf-rel  use-index seq-no  no-error.
      
      assign
          v-nxt-r-no = (if avail bf-rel then bf-rel.r-no ELSE 0 ) + 1.
       
      cocode = oe-ordl.company.
      

      DO TRANSACTION:
         
          {sys/inc/oeship.i}
             
              {sys/inc/oereleas.i}
               
              {sys/ref/relpost.i}
                 
             /* {sys/inc/addxfer.i}*/
                 
     END.
  
      lv-cust-x = "".
      FOR EACH cust NO-LOCK
          WHERE cust.company EQ oe-ordl.company
          AND cust.active  EQ "X":
          lv-cust-x = cust.cust-no.
      END.
     FIND FIRST oe-ord OF oe-ordl NO-LOCK.
     for each bf-rel where bf-rel.company = oe-ord.company
                       and bf-rel.ord-no = oe-ord.ord-no
                       and bf-rel.i-no = oe-ordl.i-no 
                       and bf-rel.LINE = oe-ordl.LINE
                       NO-LOCK:
        
         FIND FIRST s-code
             WHERE s-code.reftable EQ "oe-rel.s-code"
               AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
             NO-LOCK NO-ERROR.
         IF NOT AVAIL s-code OR CAN-DO("B,S",s-code.code) THEN
           v-qty-sum = v-qty-sum + bf-rel.qty. 
     end.
     
    /* if v-qty-sum GE oe-ordl.qty + (oe-ordl.qty * (oe-ordl.over-pct / 100)) then
        message "Total Planned release quantity will exceed the Or" +
                        "der quantity + the Underrun %."
                view-as alert-box warning.
     */   
     find first sys-ctrl where sys-ctrl.company eq oe-ordl.company
                          and sys-ctrl.name    eq "OECARIER"
               no-lock no-error.
     if not avail sys-ctrl then do:
       create sys-ctrl.
       assign sys-ctrl.company  = oe-ordl.company
             sys-ctrl.name     = "OECARIER"
             sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
             sys-ctrl.char-fld = "ShipTo".       
      /* do while true:
          message "Default Shipping Carrier from Header or Shipto?" update sys-ctrl.char-fld.
          if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "ShipTo" then leave. 
       end.*/
     end.

    

     IF oeship-cha EQ "OEShipto" THEN DO:
       FIND FIRST shipto NO-LOCK
           WHERE shipto.company EQ oe-ord.company
             AND shipto.cust-no EQ oe-ord.cust-no
             AND shipto.ship-id EQ v-ship-id
           NO-ERROR.
       IF v-carrier EQ "" THEN v-carrier = oe-ord.carrier.
     END.

     ELSE
     IF oe-ordl.est-no NE "" THEN
     FOR EACH eb NO-LOCK
         WHERE eb.company EQ oe-ordl.company
           AND eb.est-no  EQ oe-ordl.est-no
           AND eb.cust-no EQ oe-ord.cust-no
           AND eb.form-no NE 0,
         FIRST shipto OF eb NO-LOCK
         BREAK BY eb.stock-no DESC:
       IF LAST(eb.stock-no)           OR
          eb.stock-no EQ oe-ordl.i-no THEN LEAVE.
     END.

    
    
    IF NOT AVAIL shipto THEN
    FOR EACH shipto
        WHERE shipto.company  EQ oe-ordl.company
           AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND
                                     tt-report.s-code EQ "T" THEN lv-cust-x
                                                             ELSE oe-ord.cust-no)
        NO-LOCK
        BREAK BY shipto.ship-no DESC:
      IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.
    END.

    IF v-carrier EQ "" AND AVAIL shipto THEN v-carrier = shipto.carrier.

    assign oe-rel.company   = oe-ordl.company
           oe-rel.loc       = oe-ord.loc
           oe-rel.ord-no    = oe-ordl.ord-no
           oe-rel.i-no      = oe-ordl.i-no
           oe-rel.cust-no   = oe-ord.cust-no
           oe-rel.po-no     = if oe-ordl.po-no ne "" then oe-ordl.po-no 
                                                     else oe-ord.po-no
           oe-rel.qty       = oe-ordl.qty - v-qty-sum
           oe-rel.line      = oe-ordl.line
           oe-rel.s-comm[1] = oe-ord.s-comm[1]
           oe-rel.s-comm[2] = oe-ord.s-comm[2]
           oe-rel.s-comm[3] = oe-ord.s-comm[3]
           oe-rel.s-name[1] = oe-ord.sname[1]
           oe-rel.s-name[2] = oe-ord.sname[2]
           oe-rel.s-name[3] = oe-ord.sname[3]
           oe-rel.s-pct[1]  = oe-ord.s-pct[1]
           oe-rel.s-pct[2]  = oe-ord.s-pct[2]
           oe-rel.s-pct[3]  = oe-ord.s-pct[3]
           oe-rel.sman[1]   = oe-ord.sman[1]
           oe-rel.sman[2]   = oe-ord.sman[2]
           oe-rel.sman[3]   = oe-ord.sman[3]
           oe-rel.sold-no   = oe-ord.sold-no
           oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" and avail shipto then shipto.carrier
                              else v-carrier
           oe-rel.r-no      = v-nxt-r-no
           oe-rel.rel-date  = if oereleas-cha eq "LastShip" then oe-ord.last-date
                                                            else oe-ordl.req-date.

          if oe-rel.qty lt 0 then oe-rel.qty = 0.

    oe-rel.tot-qty = oe-rel.qty.

    if oe-rel.rel-date le v-lst-rel then oe-rel.rel-date = v-lst-rel + 1.


    if avail shipto then
       assign oe-rel.ship-addr[1] = shipto.ship-addr[1]
              oe-rel.ship-city    = shipto.ship-city
              oe-rel.ship-state   = shipto.ship-state
              oe-rel.ship-zip     = shipto.ship-zip
              oe-rel.ship-no      = shipto.ship-no
              oe-rel.ship-id      = shipto.ship-id
              oe-rel.ship-i[1]    = shipto.notes[1]
              oe-rel.ship-i[2]    = shipto.notes[2]
              oe-rel.ship-i[3]    = shipto.notes[3]
              oe-rel.ship-i[4]    = shipto.notes[4].
    
    else assign oe-rel.ship-no   = oe-ord.sold-no
                oe-rel.ship-id   = oe-ord.sold-id
                oe-rel.ship-i[1] = oe-ord.ship-i[1]
                oe-rel.ship-i[2] = oe-ord.ship-i[2]
                oe-rel.ship-i[3] = oe-ord.ship-i[3]
                oe-rel.ship-i[4] = oe-ord.ship-i[4].
    IF NOT AVAIL shipto THEN DO:
        
          RUN oe/custxship.p (oe-rel.company,
                              oe-ord.cust-no,
                              oe-rel.ship-id,
                              BUFFER shipto).
          if avail shipto then do:
              assign 
                  oe-rel.ship-addr[1] = shipto.ship-addr[1]
                  oe-rel.ship-addr[2] = shipto.ship-addr[2]
                  oe-rel.ship-city = shipto.ship-city
                  oe-rel.ship-state = shipto.ship-state
                  oe-rel.ship-zip = shipto.ship-zip
                  .
          END.
    END.

    RUN create-report-record-1 (NO, oe-rel.rel-date).
  end.

  
END.


