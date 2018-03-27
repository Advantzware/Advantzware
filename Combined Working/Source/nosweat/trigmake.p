/* trigmake.p*/

DEFINE INPUT PARAMETER compile-trig AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER trig-exists AS LOGICAL NO-UNDO.

FOR EACH dictdb._file WHERE NOT dictdb._file._file-name BEGINS "_" 
                        AND NOT dictdb._file._file-name BEGINS "SYS" NO-LOCK:
  IF NOT CAN-FIND(FIRST dictdb._field OF dictdb._file
     WHERE dictdb._field._field-name = "rec_key") OR
     CAN-DO("mfvalues,notes,rec_key",dictdb._file._file-name) THEN
  NEXT.
  RUN Create_Trigger.
  RUN Delete_Trigger.
  RUN Write_Trigger.
END.

PROCEDURE Create_Trigger:
  IF NOT trig-exists THEN
  IF SEARCH("create.trg/" + dictdb._file._file-name + ".p") NE ? THEN
  RETURN.
  OUTPUT TO VALUE("create.trg/" + dictdb._file._file-name + ".p").
  PUT UNFORMATTED
    "~&Scoped-define TABLENAME " dictdb._file._file-name SKIP(1)
    "TRIGGER PROCEDURE FOR CREATE OF ~{&TABLENAME}." SKIP(1)
    "~{methods/triggers/create.i}" SKIP.
  OUTPUT CLOSE.
  IF compile-trig THEN
  COMPILE VALUE("create.trg/" + dictdb._file._file-name + ".p") SAVE.
  RUN File_Trigger ("CREATE").
END PROCEDURE.

PROCEDURE Delete_Trigger:
  IF NOT trig-exists THEN
  IF SEARCH("delete.trg/" + dictdb._file._file-name + ".p") NE ? THEN
  RETURN.
  OUTPUT TO VALUE("delete.trg/" + dictdb._file._file-name + ".p").
  PUT UNFORMATTED
    "~&Scoped-define ACTION DELETE" SKIP
    "~&Scoped-define DBNAME " LDBNAME("dictdb") SKIP
    "~&Scoped-define TABLENAME " dictdb._file._file-name SKIP(1)
    "TRIGGER PROCEDURE FOR DELETE OF ~{&TABLENAME}." SKIP(1)
    "~{methods/triggers/delete.i}" SKIP.
  IF SEARCH("methods/delete.trg/" + dictdb._file._file-name + ".i") NE ? THEN
  PUT UNFORMATTED "~{methods/delete.trg/~{&TABLENAME}.i}" SKIP.
  OUTPUT CLOSE.
  IF compile-trig THEN
  COMPILE VALUE("delete.trg/" + dictdb._file._file-name + ".p") SAVE.
  RUN File_Trigger ("DELETE").
END PROCEDURE.

PROCEDURE Write_Trigger:
  IF NOT trig-exists THEN
  IF SEARCH("write.trg/" + dictdb._file._file-name + ".p") NE ? THEN
  RETURN.
  OUTPUT TO VALUE("write.trg/" + dictdb._file._file-name + ".p").
  PUT UNFORMATTED
    "~&Scoped-define ACTION UPDATE" SKIP
    "~&Scoped-define DBNAME " LDBNAME("dictdb") SKIP
    "~&Scoped-define TABLENAME " dictdb._file._file-name SKIP(1)
    "TRIGGER PROCEDURE FOR WRITE OF ~{&TABLENAME} OLD BUFFER old-~{&TABLENAME}." SKIP(1)
    "~{methods/triggers/write.i}" SKIP.
  OUTPUT CLOSE.
  IF compile-trig THEN
  COMPILE VALUE("write.trg/" + dictdb._file._file-name + ".p") SAVE.
  RUN File_Trigger ("WRITE").
END PROCEDURE.

PROCEDURE File_Trigger:
  DEFINE INPUT PARAMETER trigtype AS CHARACTER NO-UNDO.

  FIND dictdb._file-trig
      WHERE dictdb._file-trig._file-recid = RECID(dictdb._file) AND
            dictdb._file-trig._event = trigtype 
      EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE dictdb._file-trig THEN
  RUN Create_File_Trigger (trigtype).
  ELSE
  IF dictdb._file-trig._proc-name NE
     trigtype + ".trg/" + dictdb._file._file-name + ".p" THEN
  DO:
    DELETE dictdb._file-trig.
    RUN Create_File_Trigger (trigtype).
  END.
  
END PROCEDURE.

PROCEDURE Create_File_Trigger:
  DEFINE INPUT PARAMETER trigtype AS CHARACTER NO-UNDO.

  CREATE dictdb._file-trig.
  ASSIGN
    dictdb._file-trig._file-recid = RECID(dictdb._file)
    dictdb._file-trig._event = trigtype
    dictdb._file-trig._override = yes
    dictdb._file-trig._proc-name =
        LC(trigtype) + ".trg/" + dictdb._file._file-name + ".p"
    dictdb._file-trig._trig-crc = ?.

END PROCEDURE.
