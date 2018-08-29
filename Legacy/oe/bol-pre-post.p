
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER v-term LIKE report.term-id NO-UNDO.

{oe/closchk.i}

DEFINE BUFFER b-fg-bin  FOR fg-bin.
DEFINE BUFFER b-oe-boll FOR oe-boll.

DEFINE VARIABLE li     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-tag2 AS CHARACTER NO-UNDO.

FOR EACH oe-boll NO-LOCK WHERE ROWID(oe-boll) EQ ip-rowid,
    FIRST oe-bolh NO-LOCK WHERE 
    oe-bolh.b-no EQ oe-boll.b-no AND 
    CAN-FIND(FIRST b-oe-boll WHERE 
    b-oe-boll.b-no EQ oe-bolh.b-no AND 
    b-oe-boll.qty  NE 0),
    FIRST cust NO-LOCK WHERE 
    cust.company EQ oe-bolh.company AND 
    cust.cust-no EQ oe-bolh.cust-no,
    FIRST oe-ctrl NO-LOCK WHERE 
    oe-ctrl.company EQ oe-boll.company,
    FIRST oe-ord NO-LOCK  WHERE 
    oe-ord.company EQ oe-boll.company AND 
    oe-ord.ord-no  EQ oe-boll.ord-no,
    FIRST oe-ordl NO-LOCK WHERE 
    oe-ordl.company EQ oe-boll.company AND 
    oe-ordl.ord-no  EQ oe-boll.ord-no AND 
    oe-ordl.line    EQ oe-boll.line AND 
    oe-ordl.i-no    EQ oe-boll.i-no
    USE-INDEX ord-no,
    FIRST itemfg NO-LOCK WHERE 
    itemfg.company EQ oe-boll.company AND 
    itemfg.i-no    EQ oe-boll.i-no:

    RUN oe/custxship.p (oe-bolh.company,
        oe-bolh.cust-no,
        oe-bolh.ship-id,
        BUFFER shipto).

    IF oe-ord.type EQ "T" 
        OR oe-boll.s-code EQ "T" THEN 
    DO: /* Process in-house transfer */
        IF AVAILABLE shipto 
            AND CAN-FIND(FIRST fg-bin WHERE 
            fg-bin.company EQ shipto.company AND 
            fg-bin.i-no    EQ "" AND 
            fg-bin.loc     EQ shipto.loc AND 
            fg-bin.loc-bin EQ shipto.loc-bin) THEN 
        DO:

            FIND FIRST sys-ctrl WHERE
                sys-ctrl.company EQ oe-bolh.company AND
                sys-ctrl.NAME EQ "BOLPOST"
                NO-LOCK NO-ERROR.

            IF AVAILABLE sys-ctrl 
                AND sys-ctrl.int-fld EQ 0 THEN 
            DO:
                li = 0.
        
                FIND LAST fg-rctd NO-LOCK 
                    USE-INDEX fg-rctd 
                    NO-ERROR.
                IF AVAILABLE fg-rctd 
                    AND fg-rctd.r-no GT li THEN ASSIGN 
                        li = fg-rctd.r-no.
        
                FIND LAST fg-rcpth NO-LOCK 
                    USE-INDEX r-no 
                    NO-ERROR.
                IF AVAILABLE fg-rcpth 
                    AND fg-rcpth.r-no GT li THEN ASSIGN 
                        li = fg-rcpth.r-no.
                 
                CREATE fg-rctd.
                ASSIGN
                    fg-rctd.r-no       = li + 1
                    fg-rctd.company    = oe-boll.company
                    fg-rctd.bol-no     = oe-bolh.bol-no
                    fg-rctd.rct-date   = oe-bolh.bol-date
                    fg-rctd.trans-time = TIME
                    fg-rctd.i-no       = oe-boll.i-no
                    fg-rctd.rita-code  = "T"
                    fg-rctd.job-no     = oe-boll.job-no
                    fg-rctd.job-no2    = oe-boll.job-no2
                    fg-rctd.loc        = oe-boll.loc
                    fg-rctd.loc-bin    = oe-boll.loc-bin
                    fg-rctd.tag        = oe-boll.tag
                    fg-rctd.cust-no    = oe-boll.cust-no
                    fg-rctd.partial    = oe-boll.partial
                    fg-rctd.cases      = oe-boll.cases
                    fg-rctd.qty-case   = oe-boll.qty-case
                    fg-rctd.t-qty      = oe-boll.qty
                    fg-rctd.loc2       = shipto.loc
                    fg-rctd.loc-bin2   = shipto.loc-bin
                    fg-rctd.tag2       = fg-rctd.tag.
            END. /* IF AVAIL sys-ctrl */

            FIND FIRST fg-bin NO-LOCK WHERE  /* Make sure we have a bin to relieve */
                fg-bin.company  EQ oe-boll.company AND 
                fg-bin.i-no     EQ oe-boll.i-no AND 
                fg-bin.job-no   EQ oe-boll.job-no AND 
                fg-bin.job-no2  EQ oe-boll.job-no2 AND 
                fg-bin.loc      EQ oe-boll.loc AND 
                fg-bin.loc-bin  EQ oe-boll.loc-bin AND 
                fg-bin.tag      EQ oe-boll.tag AND 
                fg-bin.cust-no  EQ oe-boll.cust-no
                NO-ERROR.

            IF NOT AVAILABLE fg-bin THEN 
            DO:
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
             
            ASSIGN 
                v-tag2 = oe-boll.tag.

            IF AVAILABLE fg-rctd 
                AND fg-bin.qty GT oe-boll.qty 
                AND fg-bin.tag NE "" 
                AND fg-bin.tag EQ oe-boll.tag  
                AND AVAILABLE sys-ctrl 
                AND sys-ctrl.int-fld EQ 0 THEN 
            DO: /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
                RUN fg/mkloadtg.p (ROWID(fg-rctd), 
                    0, 
                    INPUT-OUTPUT v-tag2).
                ASSIGN 
                    fg-rctd.tag2 = v-tag2.
            END.
            ELSE IF NOT AVAILABLE fg-rctd 
                    AND fg-bin.qty GT oe-boll.qty 
                    AND fg-bin.tag NE "" 
                    AND fg-bin.tag EQ oe-boll.tag  
                    AND AVAILABLE sys-ctrl 
                    AND sys-ctrl.int-fld EQ 0 THEN /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
                    RUN fg/mkloadtg2.p (ROWID(oe-boll), 
                        shipto.loc, 
                        shipto.loc-bin, 
                        0, 
                        INPUT-OUTPUT v-tag2).
            
            IF fg-bin.loc NE shipto.loc
                OR fg-bin.loc-bin NE shipto.loc-bin 
                OR fg-bin.tag NE v-tag2 
                OR v-tag2 EQ "" THEN 
            DO:
                RELEASE fg-bin.
                FIND FIRST b-fg-bin NO-LOCK WHERE /* Make sure we have a bin to receive */
                    b-fg-bin.company EQ oe-boll.company AND 
                    b-fg-bin.i-no    EQ oe-boll.i-no AND 
                    trim(b-fg-bin.job-no) EQ trim(oe-boll.job-no) AND 
                    b-fg-bin.job-no2 EQ oe-boll.job-no2 AND 
                    b-fg-bin.loc     EQ trim(shipto.loc) AND 
                    b-fg-bin.loc-bin EQ trim(shipto.loc-bin) AND 
                    b-fg-bin.tag     EQ v-tag2 AND 
                    b-fg-bin.cust-no EQ oe-boll.cust-no
                    NO-ERROR.
        
                IF NOT AVAILABLE b-fg-bin THEN 
                DO:
                    CREATE b-fg-bin.
                    ASSIGN
                        b-fg-bin.company      = oe-boll.company
                        b-fg-bin.i-no         = oe-boll.i-no
                        b-fg-bin.job-no       = oe-boll.job-no
                        b-fg-bin.job-no2      = oe-boll.job-no2
                        b-fg-bin.loc          = shipto.loc
                        b-fg-bin.loc-bin      = shipto.loc-bin
                        b-fg-bin.tag          = v-tag2
                        b-fg-bin.cust-no      = oe-boll.cust-no
                        b-fg-bin.case-count   = oe-boll.qty-case
                        b-fg-bin.pur-uom      = /*fg-bin.pur-uom */      itemfg.prod-uom    
                        b-fg-bin.std-tot-cost = /*fg-bin.std-tot-cost*/  itemfg.std-tot-cost
                        b-fg-bin.std-mat-cost = /*fg-bin.std-mat-cost*/  itemfg.std-mat-cost
                        b-fg-bin.std-lab-cost = /*fg-bin.std-lab-cost*/  itemfg.std-lab-cost
                        b-fg-bin.std-var-cost = /*fg-bin.std-var-cost*/  itemfg.std-var-cost
                        b-fg-bin.std-fix-cost = /*fg-bin.std-fix-cost*/  itemfg.std-fix-cost   .
                END.
           
                IF AVAILABLE fg-rctd THEN 
                DO:
                    ASSIGN 
                        fg-rctd.cost-uom = itemfg.prod-uom /* 29642 - IU2 Transfers with incorrect cost - MYT - 08/29/18 */
                        fg-rctd.pur-uom  = itemfg.pur-uom
                        fg-rctd.std-cost = b-fg-bin.std-tot-cost.
           
                    IF fg-rctd.pur-uom EQ "EA" THEN ASSIGN 
                            fg-rctd.ext-cost = fg-rctd.std-cost.
                    ELSE
                        RUN sys/ref/convcuom.p (fg-rctd.pur-uom, 
                            "EA", 
                            0, 
                            0, 
                            0, 
                            0,
                            fg-rctd.std-cost, 
                            OUTPUT fg-rctd.ext-cost).
           
                    ASSIGN 
                        fg-rctd.ext-cost = fg-rctd.ext-cost * fg-rctd.t-qty.

                    RELEASE fg-rctd.
                END.
        
                ASSIGN
                    oe-bolh.posted = YES
                    oe-boll.posted = YES.
            END.  /* IF fg-bin.loc     NE shipto.loc */
            ELSE IF AVAILABLE fg-rctd THEN
                    DELETE fg-rctd.
        END. /* IF AVAIL shipto AND CAN-FIND(FIRST fg-bin */

        /* to close order (do not close order for transfers) */
        IF oe-boll.s-code <> "T" THEN 
        DO:
            CREATE w-ord.
            ASSIGN
                w-ord.ord-no = oe-ordl.ord-no
                w-ord.rec-id = RECID(oe-ord).
        END.
    END. /*  IF oe-ord.type EQ "T" OR oe-boll.s-code EQ "T" */

    {oe/seq-bolh.i}
END.
