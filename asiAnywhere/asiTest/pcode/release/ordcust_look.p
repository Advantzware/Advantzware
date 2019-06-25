/*------------------------------------------------------------------------
    File        : ordcust_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderCustLookup NO-UNDO 
    FIELD ordno          AS INT
    FIELD estno          AS CHAR
    FIELD jobno          AS CHAR
    FIELD jobno2         AS INT
    FIELD ino            AS CHAR
    FIELD partno         AS CHAR
    FIELD qty            AS DEC
    FIELD invqty         AS INT
    FIELD shpqty         AS INT
    FIELD proqty         AS INT
    FIELD onhbal         AS INT

    FIELD loc            AS CHAR
    FIELD rno            AS INT
    FIELD pono           AS CHAR
    FIELD lin            AS INT
    FIELD linkno         AS INT
    FIELD scod           AS CHAR
    FIELD qtycas         AS INT
    FIELD relno          AS INT
    FIELD cases          AS INT
    FIELD partial        AS INT

    FIELD extra           AS CHAR
    .

DEFINE DATASET dsOrderCustLookup FOR ttOrderCustLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcust      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderCustLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmcust      = ? THEN ASSIGN prmcust      = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction = "Select" then do:
    FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp 
        AND ASI.oe-ordl.opened eq yes 
        AND ASI.oe-ordl.cust-no eq prmcust
        use-index opened NO-LOCK:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

        create ttOrderCustLookup.
        assign
            ttOrderCustLookup.ordno      = oe-ordl.ord-no
            ttOrderCustLookup.estno      = oe-ordl.est-no
            ttOrderCustLookup.jobno      = oe-ordl.job-no
            ttOrderCustLookup.jobno2     = oe-ordl.job-no2 
            ttOrderCustLookup.ino        = oe-ordl.i-no
            ttOrderCustLookup.partno     = oe-ordl.part-no
            ttOrderCustLookup.qty        = oe-ordl.qty
            ttOrderCustLookup.invqty     = oe-ordl.inv-qty 
            ttOrderCustLookup.shpqty     = oe-ordl.ship-qty
             .

        IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN do:
            FOR EACH fg-act
                WHERE fg-act.company EQ prmComp
                AND fg-act.job-no  EQ oe-ordl.job-no
                AND fg-act.job-no2 EQ oe-ordl.job-no2
                AND fg-act.i-no    EQ oe-ordl.i-no
                NO-LOCK:
                ttOrderCustLookup.proqty = ttOrderCustLookup.proqty + fg-act.qty.
            END.

            FOR EACH fg-bin
                WHERE fg-bin.company EQ prmComp
                AND fg-bin.job-no  EQ oe-ordl.job-no
                AND fg-bin.job-no2 EQ oe-ordl.job-no2
                AND fg-bin.i-no    EQ oe-ordl.i-no
                NO-LOCK:
                ttOrderCustLookup.onhbal = ttOrderCustLookup.onhbal + fg-bin.qty.
            END.
        END.

        IF AVAIL oe-ord THEN DO:
            FIND FIRST oe-relh WHERE oe-relh.company EQ prmComp NO-LOCK NO-ERROR.

         /*   FIND FIRST oe-rell WHERE oe-rell.company EQ oe-relh.company
                AND oe-rell.r-no EQ oe-relh.r-no
                AND oe-rell.s-code NE ""  NO-LOCK NO-ERROR.

            find first oe-rel where oe-rel.company = oe-ord.company
                and oe-rel.ord-no = oe-ord.ord-no
                and oe-rel.ship-id = oe-relh.ship-id
                and oe-rel.rel-date = oe-relh.rel-date
                and oe-rel.i-no = oe-ordl.i-no
                and oe-rel.link-no = 0 NO-LOCK no-error.

            FIND FIRST itemfg WHERE itemfg.company EQ oe-rell.company
                AND itemfg.i-no    EQ oe-rell.i-no NO-LOCK NO-ERROR.

            ASSIGN
                ttOrderCustLookup.loc       = IF AVAIL oe-rel THEN oe-rel.spare-char-1 ELSE "MAIN"
                ttOrderCustLookup.pono      = IF AVAIL oe-rel THEN oe-rel.po-no ELSE oe-ord.po-no
                ttOrderCustLookup.lin       = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
                ttOrderCustLookup.linkno    = if avail oe-rel then oe-rel.r-no else 0
                ttOrderCustLookup.scod      = IF AVAIL oe-rell THEN oe-rell.s-code ELSE "B"
                ttOrderCustLookup.qtycas    = if avail oe-ordl and
                                              oe-ordl.cas-cnt gt 0 then oe-ordl.cas-cnt else
                                              if avail itemfg AND itemfg.case-count gt 0
                                               then itemfg.case-count else 1
                ttOrderCustLookup.relno     = IF AVAIL oe-rell THEN oe-rell.rel-no ELSE 0
                ttOrderCustLookup.cases     = IF AVAIL oe-rell THEN (TRUNC((oe-rell.qty - oe-rell.partial) / oe-rell.qty-case,0)) ELSE 0
                ttOrderCustLookup.partial   = IF AVAIL oe-rell THEN (oe-rell.qty - (oe-rell.cases * oe-rell.qty-case) ) ELSE 0
                ttOrderCustLookup.rno       = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0. */

            DEF BUFFER b-oe-rell FOR oe-rell.
            DEF BUFFER b-oe-rell-new FOR oe-rell.

            FIND FIRST b-oe-rell NO-LOCK
                WHERE b-oe-rell.r-no   EQ oe-relh.r-no
                AND b-oe-rell.s-code NE ""
                AND ROWID(b-oe-rell) NE ROWID(oe-rell)
                NO-ERROR.

            find first oe-rel where oe-rel.company = oe-ord.company
                and oe-rel.ord-no = oe-ord.ord-no
                and oe-rel.ship-id = oe-relh.ship-id
                and oe-rel.rel-date = oe-relh.rel-date
                and oe-rel.i-no = oe-ordl.i-no
                and oe-rel.link-no = 0
                no-error.

            create b-oe-rell-new.
            assign b-oe-rell-new.company = oe-relh.company
                b-oe-rell-new.loc = IF AVAIL oe-rel THEN oe-rel.spare-char-1
                                    ELSE "MAIN"
                b-oe-rell-new.r-no = oe-relh.r-no
                b-oe-rell-new.po-no = IF AVAIL oe-rel THEN oe-rel.po-no
                                       ELSE
                                       IF oe-ordl.po-no NE "" THEN oe-ordl.po-no
                                       ELSE oe-ord.po-no 

                b-oe-rell-new.line = oe-ordl.line
                b-oe-rell-new.qty = if avail oe-rel then oe-rel.qty else oe-ordl.qty
                b-oe-rell-new.i-no = oe-ordl.i-no
                b-oe-rell-new.link-no = if avail oe-rel then oe-rel.r-no else 0
                b-oe-rell-new.s-code = IF AVAIL b-oe-rell THEN b-oe-rell.s-code ELSE "B"
                b-oe-rell-new.qty-case = oe-ordl.cas-cnt
                b-oe-rell-new.ord-no = oe-ord.ord-no
                b-oe-rell-new.job-no = oe-ordl.job-no
                b-oe-rell-new.job-no2 = oe-ordl.job-no2.

            DEF BUFFER bf-rell FOR oe-rell .
            DEF VAR li-nxt-rel-no AS INT NO-UNDO.
                for each bf-rell where bf-rell.company eq prmComp
                    and bf-rell.ord-no  eq b-oe-rell-new.ord-no no-lock 
                    by bf-rell.rel-no desc:
                    li-nxt-rel-no =  bf-rell.rel-no.
                    leave.  
                end.

                ASSIGN
                    li-nxt-rel-no = li-nxt-rel-no + 1
                    b-oe-rell-new.rel-no = li-nxt-rel-no.

                if avail oe-rel then oe-rel.rel-no = b-oe-rell-new.rel-no. 
                
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ b-oe-rell-new.company
                    AND itemfg.i-no    EQ b-oe-rell-new.i-no
                    NO-ERROR.
                
                ASSIGN
                    b-oe-rell-new.qty-case = if avail oe-ordl and
                                             oe-ordl.cas-cnt gt 0 then oe-ordl.cas-cnt
                                             else
                                             if avail itemfg           and
                                                itemfg.case-count gt 0 then itemfg.case-count
                                             else 1
                    b-oe-rell-new.cases   = TRUNC((b-oe-rell-new.qty - b-oe-rell-new.partial) /
                                                  b-oe-rell-new.qty-case,0)
                    b-oe-rell-new.partial = b-oe-rell-new.qty - (b-oe-rell-new.cases * b-oe-rell-new.qty-case).

                ASSIGN                            
                    ttOrderCustLookup.loc       = b-oe-rell-new.loc
                    ttOrderCustLookup.pono      = b-oe-rell-new.po-no
                    ttOrderCustLookup.lin       = b-oe-rell-new.line
                    ttOrderCustLookup.linkno    = b-oe-rell-new.link-no
                    ttOrderCustLookup.scod      = b-oe-rell-new.s-code
                    ttOrderCustLookup.qtycas    = b-oe-rell-new.qty-case
                                                  
                                                  
                                                  
                    ttOrderCustLookup.relno     = b-oe-rell-new.rel-no
                    ttOrderCustLookup.cases     = b-oe-rell-new.cases
                    ttOrderCustLookup.partial   = b-oe-rell-new.partial
                    ttOrderCustLookup.rno       = b-oe-rell-new.r-no .
        END.
    END.  /*FOR EACH oe-ordl*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmField = "order"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH oe-ordl WHERE oe-ordl.company eq prmComp
                  AND oe-ordl.ord-no = int(prmText)
                  AND ASI.oe-ordl.opened eq yes 
                  AND ASI.oe-ordl.cust-no eq prmcust
                  use-index opened NO-LOCK:
                  FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

                  create ttOrderCustLookup.
                  assign
                        ttOrderCustLookup.ordno      = oe-ordl.ord-no
                        ttOrderCustLookup.estno      = oe-ordl.est-no
                        ttOrderCustLookup.jobno      = oe-ordl.job-no
                        ttOrderCustLookup.jobno2     = oe-ordl.job-no2 
                        ttOrderCustLookup.ino        = oe-ordl.i-no
                        ttOrderCustLookup.partno     = oe-ordl.part-no
                        ttOrderCustLookup.qty        = oe-ordl.qty
                        ttOrderCustLookup.invqty     = oe-ordl.inv-qty 
                        ttOrderCustLookup.shpqty     = oe-ordl.ship-qty
                         .

                        IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN do:
                            FOR EACH fg-act
                                WHERE fg-act.company EQ prmComp
                                AND fg-act.job-no  EQ oe-ordl.job-no
                                AND fg-act.job-no2 EQ oe-ordl.job-no2
                                AND fg-act.i-no    EQ oe-ordl.i-no
                                NO-LOCK:
                                ttOrderCustLookup.proqty = ttOrderCustLookup.proqty + fg-act.qty.
                            END.
                        
                            FOR EACH fg-bin
                                WHERE fg-bin.company EQ prmComp
                                AND fg-bin.job-no  EQ oe-ordl.job-no
                                AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                AND fg-bin.i-no    EQ oe-ordl.i-no
                                NO-LOCK:
                                ttOrderCustLookup.onhbal = ttOrderCustLookup.onhbal + fg-bin.qty.
                            END.
                        END.

                        IF AVAIL oe-ord THEN DO:
                            FIND FIRST oe-relh WHERE oe-relh.company EQ prmComp NO-LOCK NO-ERROR.
                            
                            FIND FIRST oe-rell WHERE oe-rell.company EQ oe-relh.company
                                AND oe-rell.r-no EQ oe-relh.r-no
                                AND oe-rell.s-code NE ""  NO-LOCK NO-ERROR.
                            
                            find first oe-rel where oe-rel.company = oe-ord.company
                                and oe-rel.ord-no = oe-ord.ord-no
                                and oe-rel.ship-id = oe-relh.ship-id
                                and oe-rel.rel-date = oe-relh.rel-date
                                and oe-rel.i-no = oe-ordl.i-no
                                and oe-rel.link-no = 0 NO-LOCK no-error.
                            
                            FIND FIRST itemfg WHERE itemfg.company EQ oe-rell.company
                                AND itemfg.i-no    EQ oe-rell.i-no NO-LOCK NO-ERROR.
                            
                            ASSIGN
                                ttOrderCustLookup.loc       = IF AVAIL oe-rel THEN oe-rel.spare-char-1 ELSE "MAIN"
                                ttOrderCustLookup.pono      = IF AVAIL oe-rel THEN oe-rel.po-no ELSE oe-ord.po-no
                                ttOrderCustLookup.lin       = IF AVAIL oe-ordl THEN oe-ordl.LINE ELSE 0
                                ttOrderCustLookup.linkno    = if avail oe-rel then oe-rel.r-no else 0
                                ttOrderCustLookup.scod      = IF AVAIL oe-rell THEN oe-rell.s-code ELSE "B"
                                ttOrderCustLookup.qtycas    = if avail oe-ordl and
                                                              oe-ordl.cas-cnt gt 0 then oe-ordl.cas-cnt else
                                                              if avail itemfg AND itemfg.case-count gt 0
                                                               then itemfg.case-count else 1
                                ttOrderCustLookup.relno     = IF AVAIL oe-rell THEN oe-rell.rel-no ELSE 0
                                ttOrderCustLookup.cases     = IF AVAIL oe-rell THEN (TRUNC((oe-rell.qty - oe-rell.partial) / oe-rell.qty-case,0)) ELSE 0
                                ttOrderCustLookup.partial   = IF AVAIL oe-rell THEN (oe-rell.qty - (oe-rell.cases * oe-rell.qty-case) ) ELSE 0
                                ttOrderCustLookup.rno       = IF AVAIL oe-relh THEN oe-relh.r-no ELSE 0.
                        END.
              END. /*FOR EACH oe-ordl*/
         END. 
     END .  /* if prmField = state  */
END.  /* IF prmAction = search then do: */
