
DEF VAR li AS INT NO-UNDO.
    
    
FOR EACH ef BY company BY est-no BY form-no:
  DISPLAY company
          est-no FORMAT "x(8)"
          form-no.

  DO li = 1 TO EXTENT(spec-qty):
    RUN custom/extradec.p (10000, spec-qty[li], OUTPUT spec-qty[li]).
  END.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Utility Completed..." VIEW-AS ALERT-BOX.
