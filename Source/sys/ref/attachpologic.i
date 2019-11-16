/*sys/ref/attachpologic.i*/

DEF BUFFER bf2-po-ord FOR po-ord.
DEF BUFFER bf2-po-ordl FOR po-ordl.

FIND FIRST bf2-po-ord WHERE bf2-po-ord.rec_key = ip-rec_key NO-LOCK NO-ERROR.
IF AVAIL bf2-po-ord THEN
   ASSIGN
      v-po-no = STRING(bf2-po-ord.po-no)
      v-rec-key-list = bf2-po-ord.rec_key.
ELSE
DO:
   FIND FIRST bf2-po-ordl WHERE bf2-po-ordl.rec_key = ip-rec_key NO-LOCK NO-ERROR.

   IF AVAIL bf2-po-ordl THEN
   DO:
      FIND FIRST bf2-po-ord WHERE
           bf2-po-ord.company EQ bf2-po-ordl.company AND
           bf2-po-ord.po-no EQ bf2-po-ordl.po-no
           NO-LOCK NO-ERROR.

      IF AVAIL bf2-po-ord THEN
         ASSIGN
            v-po-no = STRING(bf2-po-ord.po-no)
            v-rec-key-list = bf2-po-ord.rec_key.
   END.
      
END.
