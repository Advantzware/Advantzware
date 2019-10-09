DEF VAR ld AS DEC NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH est WHERE e-num GT 0 AND est-type LE 4:
  FOR EACH ef OF est WHERE ef.xgrain NE "N" AND ef.xgrain NE "B":
    ASSIGN
     ld         = ef.nsh-wid
     ef.nsh-wid = ef.nsh-len
     ef.nsh-len = ld.

    ASSIGN
     ld         = ef.lsh-wid
     ef.lsh-wid = ef.lsh-len
     ef.lsh-len = ld.

    ASSIGN
     ld        = ef.trim-w
     ef.trim-w = ef.trim-l
     ef.trim-l = ld.

    FOR EACH eb OF ef:
      ASSIGN
       ld         = eb.num-wid
       eb.num-wid = eb.num-len
       eb.num-len = ld.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").
