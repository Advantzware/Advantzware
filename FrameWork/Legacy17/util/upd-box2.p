/* util/upd-box.p */

DEF VAR v-f AS cha NO-UNDO.
DEF VAR v-t AS cha FORM "x(60)" LABEL "2D" NO-UNDO.
DEF VAR v-3d AS cha FORM "x(60)" LABEL "3D" NO-UNDO.
DEF VAR v-image-file AS cha NO-UNDO.
DEF VAR v-image-file-3d AS cha NO-UNDO.

REPEAT:
   UPDATE  "2D Image:" v-t skip
           "3D Image:" v-3d WITH NO-LABEL title " Change Folder Name for Image Files ".
           .
   IF substring(v-t,LENGTH(v-t),1) <> "\" AND
      substring(v-t,LENGTH(v-t),1) <> "/" THEN do:
      MESSAGE "Enter back slash(\) at the end." VIEW-AS ALERT-BOX ERROR.
       UNDO, RETRY.
   END.
   ELSE IF substring(v-3d,LENGTH(v-3d),1) <> "\" AND
      substring(v-3d,LENGTH(v-3d),1) <> "/" THEN do:
      MESSAGE "Enter back slash(\) at the end." VIEW-AS ALERT-BOX ERROR.
       UNDO, RETRY.
   END.
   ELSE LEAVE.
END.
MESSAGE "Are you ready to update?" VIEW-AS ALERT-BOX QUESTION
    BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN
FOR EACH box-design-hdr.
    IF box-design-hdr.box-image <> "" THEN DO: 
       IF R-INDEX(box-design-hdr.box-image,"/") <> 0 THEN
          v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"/") + 1,LENGTH(box-design-hdr.box-image)).
       else IF R-INDEX(box-design-hdr.box-image,"\") <> 0 THEN
          v-image-file = SUBSTRING(box-design-hdr.box-image,R-INDEX(box-design-hdr.box-image,"\") + 1,LENGTH(box-design-hdr.box-image)).

       ASSIGN box-design-hdr.box-image = v-t + v-image-file.
    END.
    
    IF box-design-hdr.box-3d-image <> "" THEN DO:
       IF R-INDEX(box-design-hdr.box-3d-image,"/") <> 0 THEN
          v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"/") + 1,LENGTH(box-design-hdr.box-3d-image)).
       else IF R-INDEX(box-design-hdr.box-3d-image,"\") <> 0 THEN
          v-image-file = SUBSTRING(box-design-hdr.box-3d-image,R-INDEX(box-design-hdr.box-3d-image,"\") + 1,LENGTH(box-design-hdr.box-3d-image)).

        ASSIGN box-design-hdr.box-3d-image = v-3d + v-image-file.
    END.

    DISP box-design-hdr.box-image FORM "x(40)".
    PAUSE 0.
END.
