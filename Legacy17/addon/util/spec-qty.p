   
/* CONNECT -pf VALUE(".\addon\rfq.pf") NO-ERROR. */
IF CONNECTED("rfq") THEN do:
    
    RUN addon/util/spec-qt2.p.
/* moved to addon/util/spec-qt2.p
FOR EACH rfqitem BY company BY rfq-no BY form-no:
  DISPLAY company
          rfq-no
          form-no.

  DO li = 1 TO EXTENT(spec-qty):
    RUN custom/extradec.p (10000, spec-qty[li], OUTPUT spec-qty[li]).
  END.
END.

HIDE ALL NO-PAUSE.
*/

    MESSAGE "Utility Completed..." VIEW-AS ALERT-BOX.
/*    DISCONNECT rfq. */
END.
