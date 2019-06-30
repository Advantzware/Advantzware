/* addon/bol/bolpoord.i*/
FIND FIRST oe-ordl WHERE oe-ordl.company = cocode 
                     AND oe-ordl.ord-no = v-ord-no
                     AND oe-ordl.i-no = loadtag.i-no NO-LOCK NO-ERROR.
lv-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
