/* est/copyscor.i */
find first reftable where reftable.reftable = "STYFLU" 
                         and reftable.company = from-style.style
                         and reftable.loc = flute.code
                         and reftable.code = "{1}"  /* Blank width */
                         no-lock no-error
                         .
IF AVAIL reftable THEN DO:
   FIND first to-reftable where to-reftable.reftable = "STYFLU" 
                         and to-reftable.company = to-style.style
                         and to-reftable.loc = flute.code
                         and to-reftable.code = "{1}"  
                         no-error.
   IF NOT AVAIL to-reftable THEN do:
      CREATE to-reftable.
      ASSIGN to-reftable.company = to-style.style.
   END.
   BUFFER-COPY reftable EXCEPT reftable.company TO to-reftable.

   find first bf-reftable where bf-reftable.reftable = "STYFLU" 
                         and bf-reftable.company = from-style.style
                         and bf-reftable.loc = flute.code
                         and bf-reftable.code = "{1}"  
                         and bf-reftable.code2 = "1"
                         no-lock no-error.
   IF AVAIL bf-reftable THEN DO:
      find first to2-reftable where to2-reftable.reftable = "STYFLU" 
                         and to2-reftable.company = to-style.style
                         and to2-reftable.loc = flute.code
                         and to2-reftable.code = "{1}"  
                         and to2-reftable.code2 = "1"
                         no-error.
      IF NOT AVAIL to2-reftable THEN do:
         CREATE to2-reftable.
         ASSIGN to2-reftable.company = to-style.style.
      END.
      BUFFER-COPY bf-reftable EXCEPT bf-reftable.company TO to2-reftable.
   END.
   /* Score Type */
   find first bf-reftable where bf-reftable.reftable = "STYSCORE" 
                         and bf-reftable.company = from-style.style
                         and bf-reftable.loc = flute.code
                         and bf-reftable.code = "{1}"  
                         NO-LOCK no-error.
   IF AVAIL bf-reftable THEN DO:
      find first to2-reftable where to2-reftable.reftable = "STYSCORE" 
                         and to2-reftable.company = to-style.style
                         and to2-reftable.loc = flute.code
                         and to2-reftable.code = "{1}"  
                         no-error.
      IF NOT AVAIL to2-reftable THEN do:
         CREATE to2-reftable.
         ASSIGN to2-reftable.company = to-style.style.
      END.
      BUFFER-COPY bf-reftable EXCEPT bf-reftable.company TO to2-reftable.
   END.
END.
