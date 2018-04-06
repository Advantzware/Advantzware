
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF ef.

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est OF ef NO-LOCK NO-ERROR.

IF AVAIL est THEN
FOR EACH ef OF est:
  ASSIGN
   ef.leaf      = ""
   ef.leaf-dscr = ""
   ef.leaf-snum = 0
   ef.leaf-bnum = 0
   ef.leaf-w    = 0
   ef.leaf-l    = 0
   li           = 0.

  FOR EACH est-flm
      WHERE est-flm.company EQ ef.company
        AND est-flm.est-no  EQ ef.est-no
        AND est-flm.snum    EQ ef.form-no
      NO-LOCK

      BY est-flm.line:

    IF est-flm.i-no NE "" THEN DO:
      li = li + 1.

      IF li LE EXTENT(ef.leaf) THEN
        ASSIGN
         ef.leaf[li]      = est-flm.i-no
         ef.leaf-dscr[li] = est-flm.dscr
         ef.leaf-snum[li] = est-flm.snum
         ef.leaf-bnum[li] = est-flm.bnum
         ef.leaf-w[li]    = est-flm.wid
         ef.leaf-l[li]    = est-flm.len.
    END.
  END.
END.
