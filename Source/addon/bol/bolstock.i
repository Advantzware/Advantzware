/* addon/bol/bolstock.i if order line has no job do as stock box*/ 
FIND first oe-rell NO-LOCK WHERE oe-rell.company   EQ cocode
                    AND oe-rell.r-no      EQ oe-relh.r-no
                    AND oe-rell.i-no = loadtag.i-no
                    USE-INDEX r-no NO-ERROR.
IF AVAIL oe-rell THEN DO:
   FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company = cocode      
                                AND oe-ordl.ord-no = oe-rell.ord-no
                                AND oe-ordl.i-no = oe-rell.i-no NO-ERROR.
   IF AVAIL oe-ordl AND oe-ordl.job-no = "" THEN v-ord-no = oe-rell.ord-no.
END.
