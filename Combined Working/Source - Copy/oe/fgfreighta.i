 IF v-FGFreightClass AND {1}.dest-code GT "" AND (itemfg.frt-class = "" OR NEW(itemfg)) THEN DO:
   FIND FIRST carr-mtx WHERE carr-mtx.company = cocode
                         AND carr-mtx.loc     = locode
                         AND carr-mtx.carrier = {1}.carrier
                         AND carr-mtx.del-zone = {1}.dest-code
                       NO-LOCK NO-ERROR.
   itemfg.frt-class = {1}.dest-code.
   IF AVAIL carr-mtx THEN
     itemfg.frt-class-dscr = carr-mtx.del-dscr.
 END.
