/* delete al estimate */

MESSAGE "Are you sure you want to delete all estimates" 
      VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:


FOR EACH est:
    DELETE est.
END.

FOR EACH est-qty:
      DELETE est-qty.
END.
FOR EACH ef:
    DELETE ef.
END.
FOR EACH eb:
    DELETE eb.
END.

FOR EACH est-op:
    DELETE est-op.
END.

FOR EACH est-prep:
    DELETE est-prep.
END.
FOR EACH est-inst:
    DELETE est-inst.
END.


END.
