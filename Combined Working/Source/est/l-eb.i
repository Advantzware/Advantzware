  
RUN enable_UI.

APPLY "value-changed" TO rd-sort.

&scoped-define key-phrase {&key-phrase1}
/*  very slow
IF ip-cur-val NE "" THEN DO:
  {custom/lookpos5.i &lookup-file="eb" &lookup-field="{&fld-name-{1}}" 
      &where-phrase = "AND ((asi.eb.est-type GE 1 AND asi.eb.est-type LE 4 AND ip-est-type GE 1 AND ip-est-type LE 4) 
                            OR (asi.eb.est-type > 4 AND ip-est-type > 4))
                       AND ROWID(eb) NE ip-rowid"   
      &USE-INDEX={&USE-INDEX}}
END.

*/
