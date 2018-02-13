
  DEF PARAM BUFFER b-itemfg FOR itemfg.


  FOR EACH itemfg-ink
      WHERE itemfg-ink.company EQ b-itemfg.company
        AND itemfg-ink.i-no    EQ b-itemfg.i-no:
    DELETE itemfg-ink.
  END.

  IF b-itemfg.i-no NE "" THEN DO:
     FOR EACH e-itemfg
         WHERE e-itemfg.company EQ b-itemfg.company
           AND e-itemfg.i-no    EQ b-itemfg.i-no:
       DELETE e-itemfg.
     END.
    
     FOR EACH e-itemfg-vend
         WHERE e-itemfg-vend.company EQ b-itemfg.company
           AND e-itemfg-vend.i-no    EQ b-itemfg.i-no:
       DELETE e-itemfg-vend.
     END.
    
     

     IF CAN-FIND(FIRST asi._file WHERE asi._file._FILE-NAME = "item-comm") THEN
        RUN fg\delitemcomm.p(INPUT b-itemfg.company,
                             INPUT b-itemfg.i-no).
  END.

  FOR EACH reftable
      WHERE reftable.reftable EQ "itemfg.exempt-disc"
        AND reftable.company  EQ b-itemfg.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ b-itemfg.i-no:
      DELETE reftable.
  END.

  IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part") THEN
    RUN custom/delcpart.p (b-itemfg.company,
                           b-itemfg.i-no).
