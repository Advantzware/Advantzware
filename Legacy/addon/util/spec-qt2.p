/* addon/util/spec-qt2.p  sub-program. running from addon/util/spec-qty.p*/
DEF VAR li AS INT NO-UNDO.

FOR EACH rfqitem BY company BY rfq-no BY form-no:
  DISPLAY company
          rfq-no
          form-no.

  DO li = 1 TO EXTENT(rfqitem.spec-qty):
    RUN custom/extradec.p (10000, rfqitem.spec-qty[li], OUTPUT rfqitem.spec-qty[li]).
  END.
END.

HIDE ALL NO-PAUSE.
