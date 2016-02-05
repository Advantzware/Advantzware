DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
    
DEF VAR li AS INT NO-UNDO.
DEF VAR li-line LIKE est-flm.line NO-UNDO.


FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:
  FOR EACH est-flm
      WHERE est-flm.company EQ ef.company
        AND est-flm.est-no  EQ ef.est-no
        AND est-flm.snum    EQ ef.form-no:
    DELETE est-flm.
  END.

  DO li = 1 TO EXTENT(ef.leaf):
    IF ef.leaf[li] NE "" THEN DO:
      li-line = 1.
      FOR EACH est-flm
          WHERE est-flm.company EQ ef.company
            AND est-flm.est-no  EQ ef.est-no
          NO-LOCK
          BY est-flm.line DESC:
        li-line = est-flm.line + 1.
        LEAVE.
      END.

      CREATE est-flm.
      ASSIGN
       est-flm.company = ef.company
       est-flm.est-no  = ef.est-no
       est-flm.line    = li-line
       est-flm.snum    = ef.form-no
       est-flm.i-no    = ef.leaf[li]
       est-flm.dscr    = ef.leaf-dscr[li]
       est-flm.bnum    = ef.leaf-bnum[li]
       est-flm.wid     = ef.leaf-w[li]
       est-flm.len     = ef.leaf-l[li].
    END.
  END.
END.
