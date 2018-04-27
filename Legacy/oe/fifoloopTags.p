/*File: oe\fifoloopcsc.p */

DEFINE INPUT PARAMETER iprTableRow AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iplNoAssignTagNum AS LOG NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplNoRecFound AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER ophTToe-rell AS HANDLE NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE SHARED VARIABLE out-recid       AS RECID   NO-UNDO.

DEFINE        VARIABLE iRelQtyToAssign LIKE oe-rell.qty NO-UNDO.
DEFINE        VARIABLE iRelQtyTotal    AS INTEGER NO-UNDO.
DEFINE        VARIABLE iFifoLoopCount  AS INTEGER NO-UNDO.
DEFINE        VARIABLE iFifoLoopCount2 AS INTEGER NO-UNDO.
DEFINE        VARIABLE iOnBOL          AS INTEGER NO-UNDO.
DEFINE        VARIABLE iAvailable      AS INTEGER NO-UNDO.
DEFINE        VARIABLE cINo            LIKE oe-rell.i-no NO-UNDO.
DEFINE        VARIABLE cPoNo           LIKE oe-rell.po-no NO-UNDO.
DEFINE        VARIABLE iOrdNo          LIKE oe-rell.ord-no NO-UNDO.
DEFINE        VARIABLE iReleaseLineNum LIKE oe-rell.line NO-UNDO.
DEFINE        VARIABLE iRNo            LIKE oe-rell.link-no NO-UNDO.
DEFINE        VARIABLE lUseCSCIndex    AS LOGICAL     NO-UNDO.
DEFINE        VARIABLE cRunMode        AS CHARACTER NO-UNDO. 
DEFINE        VARIABLE lFgBinFound     AS LOGICAL NO-UNDO.
DEFINE        VARIABLE lUseLogs        AS LOGICAL NO-UNDO.
DEFINE        VARIABLE cDebugLog       AS CHARACTER NO-UNDO.
DEFINE BUFFER b-oe-ord   FOR oe-ord.
DEFINE BUFFER b-reftable FOR reftable.

DEFINE TEMP-TABLE ttoe-rell LIKE oe-rell.

DO TRANSACTION:
    {sys/inc/relmerge.i}
    {sys/inc/addrelse.i}
END.

FUNCTION fDebugLog RETURNS CHARACTER 
  (INPUT ipcMessage AS CHARACTER ) FORWARD.

cDebugLog = "logs/" + STRING(TODAY , "99999999") + STRING(TIME) + "fifo.txt".
lUseLogs = FALSE.
IF SEARCH("logs/fifo.txt") NE ?  THEN
   lUseLogs = TRUE.
DEFINE STREAM sDebug.
IF lUseLogs THEN DO:
    OUTPUT STREAM sDEbug TO VALUE(cDebugLog) append.
    OUTPUT TO VALUE(REPLACE(cDebugLog, "txt", "errs")) append.
END.

/** If Shipping From Bill Of Lading Then Set Ship Code = B
    Or If Shipping From Finiished Goods Then Set Ship Code = I **/
FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode  NO-ERROR.

/* Out-recid assumed to be coming in from calling program */
FIND oe-relh NO-LOCK WHERE RECID(oe-relh) EQ out-recid  NO-ERROR.

FIND FIRST oe-rel NO-LOCK WHERE ROWID(oe-rel) EQ iprTableRow  NO-ERROR.

IF NOT AVAILABLE oe-rel THEN
    FIND FIRST inv-line WHERE ROWID(inv-line) EQ iprTableRow NO-LOCK NO-ERROR.

IF NOT AVAILABLE inv-line AND NOT AVAILABLE oe-rel THEN 
    FIND oe-boll NO-LOCK WHERE ROWID(oe-boll) EQ iprTableRow
        NO-ERROR.
/*MESSAGE "fifo avail oe-boll"  AVAIL(oe-boll)
  VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF AVAILABLE oe-rel THEN
    ASSIGN
        cINo            = oe-rel.i-no
        cPoNo           = oe-rel.po-no
        iOrdNo          = oe-rel.ord-no
        iReleaseLineNum = oe-rel.line
        iRNo            = oe-rel.r-no
        iRelQtyToAssign = oe-rel.tot-qty
        cRunMode        = "oe-rel".
ELSE
    IF AVAILABLE inv-line THEN 
        ASSIGN
            cINo            = inv-line.i-no
            cPoNo           = inv-line.po-no
            iOrdNo          = 0
            iReleaseLineNum = inv-line.line
            iRNo            = 0
            iRelQtyToAssign = inv-line.ship-qty
            cRunMode        = "inv-line".
    ELSE
        IF AVAILABLE oe-boll THEN 
            ASSIGN
                cINo            = oe-boll.i-no
                cPoNo           = oe-boll.po-no
                iOrdNo          = oe-boll.ord-no
                iReleaseLineNum = oe-boll.line
                iRNo            = oe-boll.r-no
                iRelQtyToAssign = oe-boll.qty
                cRunMode        = "oe-boll".       



RUN pDetermineIfCSC.


ASSIGN
    oplNoRecFound = YES
    iRelQtyTotal  = 0
    .

/* === rel-no logic moved to line (oe-rell) ========*/
DEFINE BUFFER bf-rell FOR oe-rell .
DEFINE VARIABLE iNextRelNo AS INTEGER NO-UNDO.
FOR EACH bf-rell NO-LOCK
    WHERE bf-rell.company EQ cocode
    AND bf-rell.ord-no  EQ oe-rel.ord-no  
    BY bf-rell.rel-no DESCENDING:
    
    iNextRelNo =  bf-rell.rel-no.
    LEAVE.  
END.
iNextRelNo = iNextRelNo + 1.
/*========== */

FIND FIRST oe-ordl
    WHERE oe-ordl.company EQ cocode
    AND oe-ordl.ord-no  EQ iOrdNo
    AND oe-ordl.i-no    EQ cINo
    AND oe-ordl.line    EQ iReleaseLineNum
    NO-LOCK NO-ERROR.

IF AVAILABLE oe-ordl THEN
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

FIND FIRST itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ cINo
    NO-LOCK NO-ERROR.



IF NOT iplNoAssignTagNum AND AVAILABLE oe-relh AND (AVAILABLE oe-rel OR AVAILABLE inv-line OR AVAILABLE oe-boll) THEN
DO: 

    IF lUseCSCIndex THEN 
    DO:
        fDebugLog("fifo CSC Index Option").

        fifo-loop-csc:
        DO iFifoLoopCount = 1 TO 2.
            FOR EACH fg-bin
                WHERE fg-bin.company    EQ cocode
                AND fg-bin.i-no       EQ cINo
                AND (fg-bin.cust-no   EQ "" /*OR fg-bin.cust-no EQ oe-relh.cust-no*/)
                AND (ipcLocationList EQ "" OR LOOKUP(fg-bin.loc, ipcLocationList) GT 0)
                AND (NOT AVAILABLE oe-ordl                          OR
                
                ((itemfg.cust-no EQ oe-ord.cust-no OR
                  CAN-FIND(FIRST cust
                      WHERE cust.company EQ itemfg.company
                          AND cust.cust-no EQ itemfg.cust-no
                          AND cust.active  EQ "X")) AND
                      (oe-ordl.job-no EQ ""             OR
                      relmerge-int   EQ 1))             OR    
                      
                      (oe-ordl.job-no  NE ""                AND
                      fg-bin.job-no   EQ oe-ordl.job-no     AND
                      fg-bin.job-no2  EQ oe-ordl.job-no2))
                  
                  AND ((fg-bin.qty      GT 0 AND iFifoLoopCount EQ 2) OR
                        (fg-bin.qty     GE iRelQtyToAssign AND iFifoLoopCount EQ 1))
                  USE-INDEX i-no,

                FIRST fg-rcpth NO-LOCK
                WHERE fg-rcpth.company EQ cocode
                    AND fg-rcpth.i-no    EQ fg-bin.i-no
                    AND fg-rcpth.job-no  EQ fg-bin.job-no
                    AND fg-rcpth.job-no2 EQ fg-bin.job-no2
                    
                    AND CAN-FIND(FIRST fg-rdtlh 
                                    WHERE fg-rdtlh.r-no    EQ fg-rcpth.r-no
                                      AND fg-rdtlh.loc     EQ fg-bin.loc
                                      AND fg-rdtlh.loc-bin EQ fg-bin.loc-bin
                                      AND fg-rdtlh.tag     EQ fg-bin.tag
                                      AND fg-rdtlh.cust-no EQ fg-bin.cust-no
                                      USE-INDEX rm-rdtl)
                    BY fg-rcpth.trans-date
                    BY fg-rcpth.r-no
                    BY fg-bin.job-no
                    BY fg-bin.job-no2
                    BY fg-bin.qty:
                     
                RUN pCreateTempOeRell (INPUT ROWID(fg-bin), ROWID(fg-rcpth)).


        
                RUN pCreateTempOeRell (INPUT ROWID(fg-bin), INPUT ROWID(fg-rcpth)).
 
                IF iRelQtyToAssign LE 0 THEN LEAVE fifo-loop-csc.
            END. /* end for each fg-bin */
        END. /* do iFifoLoopCount */
    END. /* IF Using CSC: No Index */
    ELSE 
    DO:
        fDebugLog("fifo - before loop " + " qty to asign " + STRING(iRelQtyToAssign) + " item " + cIno).        
        DEFINE VARIABLE iLastToAssign AS INTEGER.
        iLastToAssign = 0.
        /* Using i-no index */
        fifo-loop:
        REPEAT:
/*            DEFINE VARIABLE iLastToAssign AS INTEGER.*/
/*            iLastToAssign = 0.                       */
            loop-count:
            DO iFifoLoopCount = 1 TO 2.
                lFgBinFound = FALSE.
                FOR EACH fg-bin
                    WHERE fg-bin.company    EQ cocode
                        AND fg-bin.i-no       EQ cINo
                        AND (fg-bin.cust-no   EQ "" /*OR fg-bin.cust-no EQ oe-relh.cust-no*/)
                        AND (ipcLocationList EQ "" OR LOOKUP(fg-bin.loc, ipcLocationList) GT 0) 
                        AND (NOT AVAILABLE oe-ordl                          OR
                            ((itemfg.cust-no EQ oe-ord.cust-no OR
                            
                                CAN-FIND(FIRST cust
                                                  WHERE cust.company EQ itemfg.company
                                                    AND cust.cust-no EQ itemfg.cust-no
                                                    AND cust.active  EQ "X")) 
                                AND  (oe-ordl.job-no EQ ""       OR
                                       relmerge-int   EQ 1))     OR   
                             
                                (oe-ordl.job-no  NE ""                AND
                                fg-bin.job-no   EQ oe-ordl.job-no     AND
                                fg-bin.job-no2  EQ oe-ordl.job-no2))
    
                        AND ((fg-bin.qty      GT 0 AND iFifoLoopCount EQ 2) OR
                            (fg-bin.qty      GE iRelQtyToAssign AND iFifoLoopCount EQ 1))

                        AND NOT CAN-FIND( FIRST ttoe-rell
                                              WHERE ttoe-rell.company          EQ            fg-bin.company                                                                                         
                                                  AND ttoe-rell.loc            EQ            fg-bin.loc     
                                                  AND ttoe-rell.loc-bin        EQ            fg-bin.loc-bin 
                                                  AND ttoe-rell.tag            EQ            fg-bin.tag     
                                                  AND ttoe-rell.job-no         EQ            fg-bin.job-no
                                                  AND ttoe-rell.job-no2        EQ            fg-bin.job-no2
/*                                                   and ttoe-rell.cust-no        eq            fg-bin.cust-no */
/*                                                   and ttoe-rell.i-no           eq            fg-bin.i-no    */
/*                                                   and ttoe-rell.po-no          eq            fg-bin.po-no   */
/*                                                   and ttoe-rell.ord-no         eq            fg-bin.ord-no  */
                                          ) 
                        USE-INDEX i-no,
        
                        FIRST fg-rcpth NO-LOCK
                          WHERE fg-rcpth.company EQ cocode
                            AND fg-rcpth.i-no    EQ fg-bin.i-no
                            AND fg-rcpth.job-no  EQ fg-bin.job-no
                            AND fg-rcpth.job-no2 EQ fg-bin.job-no2
                        
                        AND CAN-FIND(FIRST fg-rdtlh 
                                        WHERE fg-rdtlh.r-no    EQ fg-rcpth.r-no
                                          AND fg-rdtlh.loc     EQ fg-bin.loc
                                          AND fg-rdtlh.loc-bin EQ fg-bin.loc-bin
                                          AND fg-rdtlh.tag     EQ fg-bin.tag
                                          AND fg-rdtlh.cust-no EQ fg-bin.cust-no
                                          USE-INDEX rm-rdtl)
                        USE-INDEX i-no 
        
                        BY fg-rcpth.trans-date
                        BY fg-rcpth.r-no
                        BY fg-bin.job-no
                        BY fg-bin.job-no2
                        BY fg-bin.qty:



                    lFgBinFound = TRUE.

                    iOnBOL = 0.
                    FOR  EACH oe-boll FIELDS(qty job-no job-no2) NO-LOCK
                      WHERE oe-boll.company EQ fg-bin.company                      
                        AND oe-boll.i-no    EQ fg-bin.i-no
                        AND oe-boll.loc     EQ fg-bin.loc
                        AND oe-boll.loc-bin EQ fg-bin.loc-bin
                        AND oe-boll.tag     EQ fg-bin.tag
                        AND oe-boll.cust-no EQ fg-bin.cust-no
                      USE-INDEX i-no,
                      FIRST oe-bolh FIELDS(company b-no) NO-LOCK
                        WHERE oe-bolh.b-no EQ oe-boll.b-no
                          AND oe-bolh.posted  EQ NO:
            
                       IF fg-bin.job-no NE "" AND
                         NOT(oe-boll.job-no EQ fg-bin.job-no AND
                            oe-boll.job-no2 EQ fg-bin.job-no2) THEN
                         NEXT.
                     
                       iOnBOL = iOnBOL + oe-boll.qty.
                    END.
                    iAvailable = fg-bin.qty - iOnBOL.
                    
                    IF NOT ((iAvailable      GT 0 AND iFifoLoopCount EQ 2) OR
                            (iAvailable      GE iRelQtyToAssign AND iFifoLoopCount EQ 1)) THEN DO:
                        fDebugLog("run skiptag for qty problem " + fg-bin.tag + " avail " + string(iavailable) + " irelqtytoass " + string(iRelQtytoassign)).                                
                            NEXT.
                    END.
                    fDebugLog("run pcreatetempoerell " + fg-bin.tag).
                    RUN pCreateTempOeRell (INPUT ROWID(fg-bin), ROWID(fg-rcpth)).
    

            
                     /* Record was found, so leave the loop */
                     LEAVE loop-count.
                END. /* end for each fg-bin */
                 fDebugLog("next of loop " + string(iFifoLoopCount)).
            END. /* loop-count: do iFifoLoopCount 1 to 2 */

            IF iRelQtyToAssign LE 0 OR iRelQtyToAssign EQ iLastToAssign OR lFgBinFound = FALSE THEN LEAVE fifo-loop.
              
            /* Prevents Repeatedly finding the same tag to no effect */
            iLastToAssign = iRelQtyToAssign.
              
        END. /* fifo loop repeat */
    END. /* Not using CSC Index */ 
END. /* If assigning tag #'s */


fDebugLog("run pcreateOeREll " + cRunMode).
/* Based on temp-tables created, create the actual records */
IF cRunMode NE "oe-boll" THEN
  RUN pCreateOeRell.
ELSE 
  RUN pCreateDynamicTT.

OUTPUT STREAM sDebug CLOSE.


/* ************************  Function Prototypes ********************** */

FUNCTION fDebugLog RETURNS CHARACTER 
	( INPUT ipcMessage AS CHARACTER  ) FORWARD.

PROCEDURE pCreateDynamicTT:
 
        DEFINE VARIABLE httoe-rell AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBufTToe-rell AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBufPassedTToe-rell AS HANDLE NO-UNDO.
      /*  DEFINE VARIABLE hQueryTToe-rell AS HANDLE NO-UNDO. */    
      hBufPassedTToe-rell = BUFFER ttOe-Rell:HANDLE.

     /* Get database table handles */
    /* hBufTToe-rell = BUFFER ttOe-Rell:HANDLE. */
    
    /* Create an empty, undefined TEMP-TABLE */
    CREATE TEMP-TABLE hTToe-rell.
    
    /* Main output parameter for program */
    ophTToe-rell = hTToe-rell. 
    
    /* Give it oe-rell tables fields & indexes */
    hTToe-rell:CREATE-LIKE(hBufPassedTToe-rell).

    /* No more fields will be added */
    hTToe-rell:TEMP-TABLE-PREPARE("ttoe-rell").
    
    /* Get the buffer handle for the temp-table */
    hBufTToe-rell = hTToe-rell:DEFAULT-BUFFER-HANDLE.

    FOR EACH ttOe-Rell:
          
          fDebugLog("creating hbufftt " + ttOe-Rell.tag).
         /* Populate the temp-table from oe-rell */   
          hBufTToe-rell:BUFFER-CREATE.
          hBufTToe-rell:BUFFER-COPY(hBufPassedTToe-rell).
                  
    END. /* each ttoe-Rell */
    
END PROCEDURE.

PROCEDURE pDetermineIfCSC:
    FIND FIRST sys-ctrl WHERE
        sys-ctrl.company EQ cocode AND
        sys-ctrl.NAME EQ "POPRINT"
        NO-LOCK NO-ERROR.

    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "CSC-GA" THEN
        lUseCSCIndex = YES.

    IF lUseCSCIndex THEN
    DO:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ cocode AND
            sys-ctrl.NAME EQ "QUOPRINT"
            NO-LOCK NO-ERROR.

        IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "CSC-Excel" THEN
            lUseCSCIndex = YES.
        ELSE
            lUseCSCIndex = NO.
    END.

END PROCEDURE.

PROCEDURE pCreateTempOeRell:
    DEFINE INPUT  PARAMETER iprFgBin AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER iprFgRcpth AS ROWID NO-UNDO.
    FIND FIRST fg-bin NO-LOCK 
        WHERE ROWID(fg-bin) EQ iprFgBin
        NO-ERROR.
    FIND FIRST fg-rcpth NO-LOCK 
        WHERE ROWID(fg-rcpth) EQ iprFgRcpth
        NO-ERROR.
 
     fDebugLog("create ttOe-rell " + fg-bin.tag).
    /* Create temp-table record to either create an oe-rell or to be returned to calling program */
    CREATE ttoe-rell.
    ASSIGN 
        out-recid          = RECID(ttoe-rell)
        ttoe-rell.company  = cocode
        ttoe-rell.r-no     = oe-relh.r-no
        ttoe-rell.rel-no   = iNextRelNo
        ttoe-rell.loc      = fg-bin.loc
        ttoe-rell.loc-bin  = fg-bin.loc-bin
        ttoe-rell.tag      = IF iplNoAssignTagNum THEN "" ELSE fg-bin.tag
        ttoe-rell.job-no   = fg-bin.job-no
        ttoe-rell.job-no2  = fg-bin.job-no2
        ttoe-rell.cust-no  = fg-bin.cust-no
        ttoe-rell.i-no     = cINo
        ttoe-rell.po-no    = cPoNo
        ttoe-rell.ord-no   = iOrdNo
        ttoe-rell.line     = iReleaseLineNum
        ttoe-rell.printed  = NO
        ttoe-rell.posted   = NO
        ttoe-rell.deleted  = NO
        /** Set link to the planned releases **/
        ttoe-rell.link-no  = iRNo
        ttoe-rell.s-code   = IF AVAILABLE oe-rel AND oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                       IF fg-bin.cust-no GT ""                THEN "S"
                                                              ELSE
                       IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" 
                                                              ELSE "I"
        ttoe-rell.qty-case = IF fg-bin.case-count GT 0 THEN fg-bin.case-count
                        ELSE
                        IF AVAILABLE itemfg           AND
                           itemfg.case-count GT 0 THEN itemfg.case-count
                        ELSE
                        IF AVAILABLE oe-ordl          AND
                           oe-ordl.cas-cnt   GT 0 THEN oe-ordl.cas-cnt
                        ELSE 1
        .


    IF fg-bin.qty LE iRelQtyToAssign THEN
        ASSIGN
            ttoe-rell.qty     = fg-bin.qty
            ttoe-rell.partial = fg-bin.partial-count
            .

    ELSE 
        ASSIGN
            ttoe-rell.qty     = iRelQtyToAssign
            ttoe-rell.partial = 0
            .

    ASSIGN
        ttoe-rell.cases   = TRUNC((ttoe-rell.qty - ttoe-rell.partial) /
                              ttoe-rell.qty-case,0)
        ttoe-rell.partial = ttoe-rell.qty - (ttoe-rell.cases * ttoe-rell.qty-case)
        .

    ASSIGN
        oplNoRecFound   = NO
        iRelQtyToAssign = iRelQtyToAssign - ttoe-rell.qty
        iRelQtyTotal    = iRelQtyTotal + ttoe-rell.qty
        .
END PROCEDURE.

PROCEDURE pCreateOeRell:
    DEFINE VARIABLE rLastOeRell AS ROWID NO-UNDO.
    
    rlastOeRell = ?.
    FOR EACH ttOe-Rell:
        CREATE oe-rell.
        BUFFER-COPY ttoe-rell TO oe-rell.
        DELETE ttoe-rell.
        rLastOeRell = ROWID(oe-rell).
    END.
    
    IF rLastOeREll NE ? THEN 
      FIND oe-rell EXCLUSIVE-LOCK WHERE ROWID(oe-rell) EQ rLastOeREll NO-ERROR.
    
    fDebugLog("pCreateOerell avail oe-rell? " + STRING(avail(oe-rell)) ).
    IF NOT AVAILABLE oe-rell THEN 
      RETURN.
    

    FIND FIRST b-reftable NO-LOCK
        WHERE b-reftable.reftable EQ "oe-rel.lot-no"
        AND b-reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
        NO-ERROR.
    
   IF AVAIL b-reftable THEN DO:
        ASSIGN
             oe-rell.lot-no     = b-reftable.code
             oe-rell.frt-pay    = b-reftable.code2
             oe-rell.fob-code   = b-reftable.dscr.
        RELEASE b-reftable. 
    END.
  ELSE ASSIGN 
         oe-rell.lot-no  = oe-rel.lot-no
         oe-rell.frt-pay = oe-rel.frt-pay
         oe-rell.fob-code = oe-rel.fob-code.
   
    
   
      ASSIGN
       oe-rell.sell-price = oe-rel.sell-price
       oe-rell.zeroPrice = oe-rel.zeroPrice.
    
    IF iRelQtyToAssign GT 0 AND AVAILABLE oe-rel THEN 
    DO:
        IF AVAILABLE oe-rell THEN 
        DO:
            oe-rell.qty = ttoe-rell.qty + iRelQtyToAssign.
    
            ASSIGN
                oe-rell.cases   = trunc((oe-rell.qty - oe-rell.partial) /
                              oe-rell.qty-case,0)
                oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case).
        END.
    END.
    RELEASE oe-rell.
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fDebugLog RETURNS CHARACTER 
	(INPUT ipcMessage AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    IF lUseLogs THEN DO:
        OUTPUT STREAM sDebug CLOSE. OUTPUT STREAM sDebug TO VALUE(cDebugLog) append.
        PUT STREAM sDebug UNFORMATTED ipcMessage SKIP.
    END.
    
    RETURN result.
		
END FUNCTION.
