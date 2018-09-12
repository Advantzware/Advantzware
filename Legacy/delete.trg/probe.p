&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME probe

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR li AS INT NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.
DEF VAR viDirCount AS INT NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.

{methods/triggers/delete.i}

find first sys-ctrl where
    sys-ctrl.company eq {&TABLENAME}.company AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

  if not avail sys-ctrl then DO TRANSACTION:
        create sys-ctrl.
        assign sys-ctrl.company = {&TABLENAME}.company
               sys-ctrl.name    = "CEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
        
  end.

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

DO viDirCount = 1 TO 3:

   IF viDirCount EQ 2 THEN tmp-dir = "users\".
   ELSE IF viDirCount EQ 3 THEN tmp-dir = ".\".

   v-probe-fmt = IF {&TABLENAME}.LINE LT 100 THEN "99" ELSE "999".

   IF OPSYS EQ "unix" THEN DO:
      UNIX SILENT rm VALUE(tmp-dir + TRIM({&TABLENAME}.est-no) + "-*.*" + STRING({&TABLENAME}.LINE,v-probe-fmt)).
      UNIX SILENT rm VALUE(tmp-dir + TRIM({&TABLENAME}.est-no) +   ".*" + STRING({&TABLENAME}.LINE,v-probe-fmt)).
   END.
   ELSE DO:
       if search((tmp-dir + TRIM({&TABLENAME}.est-no) + "-*.*" + STRING({&TABLENAME}.LINE,v-probe-fmt))) <> ? then   
         DOS SILENT DEL VALUE(tmp-dir + TRIM({&TABLENAME}.est-no) + "-*.*" + STRING({&TABLENAME}.LINE,v-probe-fmt)).
     if search((tmp-dir + TRIM({&TABLENAME}.est-no) +   ".*" + STRING({&TABLENAME}.LINE,v-probe-fmt))) <> ? then   
        DOS SILENT DEL VALUE(tmp-dir + TRIM({&TABLENAME}.est-no) +   ".*" + STRING({&TABLENAME}.LINE,v-probe-fmt)).
   END.
END.

FOR EACH probeit
    WHERE probeit.company EQ {&TABLENAME}.company
      AND probeit.est-no  EQ {&TABLENAME}.est-no
      AND probeit.line    EQ {&TABLENAME}.line
    USE-INDEX est-no:
  DELETE probeit.
END.

FOR EACH est-summ
    WHERE est-summ.company EQ {&TABLENAME}.company
      AND est-summ.est-no  EQ {&TABLENAME}.est-no
      AND est-summ.e-num   EQ {&TABLENAME}.line
    USE-INDEX est-qty:
  DELETE est-summ.
END.

FOR EACH reftable {est/probreft.i reftable {&TABLENAME}}:
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "probe-ref"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {&TABLENAME}.est-no
      AND reftable.code2    EQ STRING({&TABLENAME}.line,"9999999999"):
  DELETE reftable.
END.

FOR EACH reftable
    WHERE reftable.reftable EQ "probe-board"
      AND reftable.company  EQ {&TABLENAME}.company
      AND reftable.loc      EQ ""
      AND reftable.code     EQ {&TABLENAME}.est-no
      AND reftable.code2    EQ STRING({&TABLENAME}.line,"9999999999"):
  DELETE reftable.
END.
