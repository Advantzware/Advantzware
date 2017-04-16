
DEF INPUT        PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT        PARAM ip-qty   LIKE loadtag.qty NO-UNDO.
DEF INPUT-OUTPUT PARAM io-tag   LIKE fg-rctd.tag NO-UNDO.

DEF VAR li-tag-no AS INT NO-UNDO.

FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL fg-rctd THEN DO:
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ fg-rctd.company
        AND itemfg.i-no    EQ fg-rctd.i-no
      NO-ERROR.

  li-tag-no = 0.

  FOR EACH loadtag
      WHERE loadtag.company   EQ fg-rctd.company
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    BEGINS STRING(CAPS(fg-rctd.i-no),"x(15)")      
      NO-LOCK
      BY loadtag.tag-no DESC:
    li-tag-no = INT(SUBSTR(loadtag.tag-no,16,5)).
    LEAVE.
  END. /* repeat*/

  CREATE loadtag.
  ASSIGN
   loadtag.company      = fg-rctd.company
   loadtag.tag-no       = STRING(CAPS(fg-rctd.i-no),"x(15)") +
                          STRING(li-tag-no + 1,"99999")
   loadtag.item-type    = NO /*FGitem*/
   loadtag.job-no       = fg-rctd.job-no
   loadtag.job-no2      = fg-rctd.job-no2
   loadtag.loc          = IF fg-rctd.rita-code EQ "T" THEN fg-rctd.loc2 ELSE fg-rctd.loc
   loadtag.loc-bin      = IF fg-rctd.rita-code EQ "T" THEN fg-rctd.loc-bin2 ELSE fg-rctd.loc-bin
   loadtag.i-no         = CAPS(fg-rctd.i-no)
   loadtag.i-name       = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
   loadtag.qty          = ip-qty
   loadtag.qty-case     = fg-rctd.qty-case
   loadtag.case-bundle  = fg-rctd.cases
   loadtag.partial      = fg-rctd.partial
   loadtag.pallet-count = loadtag.qty-case * loadtag.case-bundle + loadtag.partial
   loadtag.tot-cases    = fg-rctd.cases
   loadtag.tag-date     = TODAY
   loadtag.tag-time     = TIME
   io-tag               = loadtag.tag-no.
END.
