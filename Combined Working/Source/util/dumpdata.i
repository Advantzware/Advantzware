/* util/dumpdata.i */

OUTPUT TO VALUE (v-dumpfile + "{2}").
v-heading = "".
FOR EACH {1}:
    IF v-first THEN DO:
       FIND FIRST _file WHERE _file-name = "{1}" no-lock.
       FOR EACH _field OF _file NO-LOCK BY _field._order:
           v-heading = v-heading +
               (IF _field._label = ? THEN _field._field-name ELSE _field._label) + v-delimiter.
       END.
       PUT UNFORMATTED v-heading SKIP.
       
       v-first = NO.
    END.
    EXPORT DELIMITER "~t" {1}.
END.



