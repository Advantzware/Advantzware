DEF VAR v-f AS cha NO-UNDO.
DEF VAR v-t AS cha NO-UNDO.

UPDATE "Change Path form Image Files :" v-t WITH NO-LABEL.

FOR EACH box-design-hdr.
    IF box-design-hdr.box-image <> "" THEN 
        SUBSTRING(box-design-hdr.box-image,1,1) = v-t.
    DISP box-design-hdr.box-image FORM "x(40)".

END.
