/* getPrice.i - shared by oe/impord.w & cXML/monitor.w */

PROCEDURE getPrice:
    DEFINE INPUT PARAMETER ipriOeOrdl AS ROWID NO-UNDO.

    DEFINE VARIABLE lv-rowid         AS ROWID     NO-UNDO.
    DEFINE VARIABLE lv-save-xoe-ordl AS ROWID     NO-UNDO.  
    DEFINE VARIABLE cEstNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPrice           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cPriceUOM        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQuoteNo         LIKE quotehd.q-no NO-UNDO.
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    FIND bf-oe-ordl 
        WHERE ROWID(bf-oe-ordl) EQ ipriOeOrdl 
        NO-ERROR.  
    IF NOT AVAILABLE bf-oe-ordl THEN RETURN NO-APPLY. 
    ELSE 
    DO: 
        IF AVAILABLE bf-oe-ordl THEN 
        DO:
            IF NOT AVAILABLE xoe-ord THEN
                FIND FIRST xoe-ord NO-LOCK  
                    WHERE xoe-ord.company EQ g_company
                    AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                    NO-ERROR.

            ASSIGN
                save_id   = RECID(bf-oe-ordl)
                lv-rowid  = ROWID(bf-oe-ordl)
                v-i-item  = bf-oe-ordl.i-no
                v-i-qty   = INT(bf-oe-ordl.qty)
                v-qty-mod = YES. /* new record, so will have been modified */

            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ v-i-item
                NO-ERROR.
        
            IF AVAILABLE itemfg THEN 
            DO:
                IF bf-oe-ordl.q-no <> 0 THEN 
                DO:
                    FOR EACH quotehd NO-LOCK  
                        WHERE quotehd.company EQ cocode
                        AND quotehd.loc     EQ locode
                        AND quotehd.q-no EQ  bf-oe-ordl.q-no
                        ,
                        EACH quoteitm OF quotehd NO-LOCK 
                        WHERE quoteitm.part-no EQ bf-oe-ordl.part-no 
                        OR (quoteitm.i-no EQ bf-oe-ordl.i-no AND bf-oe-ordl.i-no NE "")
                        ,
                        EACH quoteqty OF quoteitm NO-LOCK 
                        WHERE quoteqty.qty EQ bf-oe-ordl.qty:           
                
                        ASSIGN 
                            dPrice    = quoteqty.price
                            cPriceUOM = quoteqty.uom
                            .
                        LEAVE.
                    END.       
                END.
                IF dPrice EQ 0 AND bf-oe-ordl.est-no <> "" THEN 
                DO:        
                    cEstNo = FILL(" ",8 - LENGTH(TRIM(bf-oe-ordl.est-no))) +
                        TRIM(bf-oe-ordl.est-no).
                    FIND FIRST xest NO-LOCK  
                        WHERE xest.company EQ cocode 
                        AND xest.est-no EQ cEstNo 
                        NO-ERROR.
                    ASSIGN 
                        dPrice    = bf-oe-ordl.price
                        cPriceUOM = bf-oe-ordl.pr-uom
                        lv-qty    = bf-oe-ordl.qty
                        cItemID   = bf-oe-ordl.i-no.    

                    RUN oe/getqprdt.p (RECID(xest), bf-oe-ordl.part-no,
                        cItemID,
                        INPUT-OUTPUT dPrice,
                        INPUT-OUTPUT cPriceUOM,
                        OUTPUT iQuoteNo,
                        INPUT-OUTPUT lv-qty).                             
               
                END.
                IF dPrice EQ 0 THEN 
                    RUN oe/oe-price.p.
                ELSE 
                    ASSIGN 
                        bf-oe-ordl.price  = dPrice
                        bf-oe-ordl.pr-uom = cPriceUOM
                        .                  
            END. /* est-no <> "" */
        END.
        RUN Conv_CalcTotalPrice(bf-oe-ordl.company, 
                        bf-oe-ordl.i-no,
                        bf-oe-ordl.qty,
                        bf-oe-ordl.price,
                        bf-oe-ordl.pr-uom,
                        bf-oe-ordl.disc,
                        bf-oe-ordl.cas-cnt,    
                        OUTPUT bf-oe-ordl.t-price).
        //{oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl}
    END. /*ELSE bf-oe-ordl is available */
END PROCEDURE.
