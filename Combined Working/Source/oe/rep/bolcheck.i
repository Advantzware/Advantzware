
&SCOPED-DEFINE bol-check                            ~
       IF oe-bolh.bol-no   GE fbol  AND             ~
          oe-bolh.bol-no   LE tbol  AND             ~
          oe-bolh.bol-date GE fdate AND             ~
          oe-bolh.bol-date LE tdate THEN DO:        ~
         RELEASE inv-line.                          ~
         RELEASE oe-bolh.                           ~
         LEAVE.                                     ~
       END.                                         ~
       ELSE NEXT {2}.


RELEASE oe-bolh.
  
IF {1}.bol-no EQ 0 THEN
FOR EACH inv-line NO-LOCK WHERE inv-line.r-no EQ {1}.r-no,
    FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ inv-line.b-no:
        
  {&bol-check}
END.

ELSE
FOR EACH oe-bolh NO-LOCK
    WHERE oe-bolh.company EQ cocode
      AND oe-bolh.bol-no  EQ {1}.bol-no
    BREAK BY oe-bolh.b-no:
        
  {&bol-check}
END.
   
