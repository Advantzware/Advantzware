  
RUN enable_UI.

APPLY "value-changed" TO rd-sort.

&scoped-define key-phrase {&key-phrase1}

IF ip-cur-val NE "" THEN DO:
  {custom/lookpos5.i &lookup-file="eb" &lookup-field="{&fld-name-{1}}" 
      &where-phrase = "AND (asi.eb.est-type GT 4 AND ip-est-type GT 4) 
                           AND ROWID(eb) NE ip-rowid"   
      &USE-INDEX={&USE-INDEX}}
END.

