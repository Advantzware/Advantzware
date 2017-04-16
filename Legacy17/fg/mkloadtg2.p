DEF INPUT    PARAM ip-rowid AS ROWID.
DEF INPUT    PARAM ip-loc AS CHAR NO-UNDO.
DEF INPUT    PARAM ip-loc-bin AS CHAR NO-UNDO.
DEF INPUT    PARAM ip-qty   LIKE loadtag.qty NO-UNDO.
DEF INPUT-OUTPUT PARAM io-tag   LIKE fg-rctd.tag NO-UNDO.

DEF VAR li-tag-no AS INT NO-UNDO.

FIND oe-boll WHERE
     ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-boll THEN
DO:
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
      NO-ERROR.

  li-tag-no = 0.

  FOR EACH loadtag
      WHERE loadtag.company   EQ oe-boll.company
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    BEGINS STRING(CAPS(oe-boll.i-no),"x(15)")      
      NO-LOCK
      BY loadtag.tag-no DESC:
    li-tag-no = INT(SUBSTR(loadtag.tag-no,16,5)).
    LEAVE.
  END. /* repeat*/
  
  CREATE loadtag.
  ASSIGN
   loadtag.company      = oe-boll.company
   loadtag.tag-no       = STRING(CAPS(oe-boll.i-no),"x(15)") +
                          STRING(li-tag-no + 1,"99999")
   loadtag.item-type    = NO /*FGitem*/
   loadtag.job-no       = oe-boll.job-no
   loadtag.job-no2      = oe-boll.job-no2
   loadtag.loc          = ip-loc
   loadtag.loc-bin      = ip-loc-bin
   loadtag.i-no         = CAPS(oe-boll.i-no)
   loadtag.i-name       = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
   loadtag.qty          = ip-qty
   loadtag.qty-case     = oe-boll.qty-case
   loadtag.case-bundle  = oe-boll.cases
   loadtag.partial      = oe-boll.partial
   loadtag.pallet-count = loadtag.qty-case * loadtag.case-bundle + loadtag.partial
   loadtag.tot-cases    = oe-boll.cases
   loadtag.tag-date     = TODAY
   loadtag.tag-time     = TIME
   io-tag               = loadtag.tag-no.
END.

