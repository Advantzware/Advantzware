/* helpdesk/dumpdata1000.p  */
DEFINE VARIABLE ipcPFPatch AS CHARACTER NO-UNDO.
DEFINE VARIABLE ipcOutputPath AS CHARACTER NO-UNDO.

DEF STREAM sNotes.
DEF VAR vcCommand     AS CHAR NO-UNDO.

DEFINE VARIABLE cPrgrmsPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmxrefPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupsPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmailCodPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReftablePath AS CHARACTER NO-UNDO.

ASSIGN
cPrgrmsPath = ipcOutputPath + "\" + "prgrms.d"
cPrgmxrefPath = ipcOutputPath + "\" + "prgmxref.d"
cLookupsPath = ipcOutputPath + "\" + "lookups.d"
cEmailCodPath = ipcOutputPath + "\" + "emailcod.d"
cReftablePath = ipcOutputPath + "\" + "reftable.d".


vcCommand = 'ATTRIB -R ' + cPrgrmsPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cPrgrmsPath).
FOR EACH nosweat-asi.prgrms NO-LOCK:
    EXPORT nosweat-asi.prgrms.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cPrgmxrefPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cPrgmxrefPath).
FOR EACH nosweat-asi.prgmxref NO-LOCK:
    EXPORT nosweat-asi.prgmxref.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cLookupsPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cLookupsPath).
FOR EACH nosweat-asi.lookups NO-LOCK:
    EXPORT nosweat-asi.lookups.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cEmailcodPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cEmailcodpath).
FOR EACH emailcod NO-LOCK:
    EXPORT emailcod.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cReftablePath.
OS-COMMAND SILENT VALUE (vcCommand).

vcCommand = 'ATTRIB -R ' + cNotePath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cReftablePath).
OUTPUT STREAM sNotes TO VALUE(cNotePath).
FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ 'Utilities':
    EXPORT reftable.
    FOR EACH nosweat-asi.notes NO-LOCK WHERE nosweat-asi.notes.rec_key EQ reftable.rec_key:
      EXPORT STREAM sNotes nosweat-asi.notes.
    END.
END.
OUTPUT STREAM sNotes CLOSE.
OUTPUT CLOSE.
/*
RUN copy-proc(INPUT "p:\asi10test\rco1010\prgrms.d",
              INPUT "p:\asi10test\patch\rco1010\prgrms.d").


RUN copy-proc(INPUT "p:\asi10test\rco1010\prgmxref.d",
              INPUT "p:\asi10test\patch\rco1010\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\lookups.d",
              INPUT "p:\asi10test\patch\rco1010\lookups.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\emailcod.d",
              INPUT "p:\asi10test\patch\rco1010\emailcod.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\reftable.d",
              INPUT "p:\asi10test\patch\rco1010\reftable.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\notes.d",
              INPUT "p:\asi10test\patch\rco1010\notes.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\prgrms.d",
              INPUT "p:\asi10ship\rco1010\prgrms.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\prgmxref.d",
              INPUT "p:\asi10ship\rco1010\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\lookups.d",
              INPUT "p:\asi10ship\rco1010\lookups.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\emailcod.d",
              INPUT "p:\asi10ship\rco1010\emailcod.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\reftable.d",
              INPUT "p:\asi10ship\rco1010\reftable.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\notes.d",
              INPUT "p:\asi10ship\rco1010\notes.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\prgrms.d",
              INPUT "p:\asi10ship\patch\rco1010\prgrms.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\prgmxref.d",
              INPUT "p:\asi10ship\patch\rco1010\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\lookups.d",
              INPUT "p:\asi10ship\patch\rco1010\lookups.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\emailcod.d",
              INPUT "p:\asi10ship\patch\rco1010\emailcod.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\reftable.d",
              INPUT "p:\asi10ship\patch\rco1010\reftable.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\notes.d",
              INPUT "p:\asi10ship\patch\rco1010\notes.d").

/* addon */
vcCommand = 'ATTRIB -R ' + "p:\asi10test\rco1010\addon\prgrms.d".
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO p:\asi10test\rco1010\addon\prgrms.d.
FOR EACH nosweat-addon.prgrms NO-LOCK:
    EXPORT nosweat-addon.prgrms.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + "p:\asi10test\rco1010\addon\prgmxref.d".
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO p:\asi10test\rco1010\addon\prgmxref.d.
FOR EACH nosweat-addon.prgmxref NO-LOCK:
    EXPORT nosweat-addon.prgmxref.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + "p:\asi10test\rco1010\addon\lookups.d".
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO p:\asi10test\rco1010\addon\lookups.d.
FOR EACH nosweat-addon.lookups NO-LOCK:
    EXPORT nosweat-addon.lookups.
END.
OUTPUT CLOSE.

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgrms.d",
              INPUT "p:\asi10test\patch\rco1010\addon\prgrms.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgmxref.d",
              INPUT "p:\asi10test\patch\rco1010\addon\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\lookups.d",
              INPUT "p:\asi10test\patch\rco1010\addon\lookups.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgrms.d",
              INPUT "p:\asi10ship\rco1010\addon\prgrms.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgmxref.d",
              INPUT "p:\asi10ship\rco1010\addon\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\lookups.d",
              INPUT "p:\asi10ship\rco1010\addon\lookups.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgrms.d",
              INPUT "p:\asi10ship\patch\rco1010\addon\prgrms.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\prgmxref.d",
              INPUT "p:\asi10ship\patch\rco1010\addon\prgmxref.d").

RUN copy-proc(INPUT "p:\asi10test\rco1010\addon\lookups.d",
              INPUT "p:\asi10ship\patch\rco1010\addon\lookups.d").

/* copy menu files*/
RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.lst",
              INPUT "p:\asi10test\rco1010\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.fol",
              INPUT "p:\asi10test\rco1010\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.cor",
              INPUT "p:\asi10test\rco1010\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.lst",
              INPUT "p:\asi10test\patch\rco1010\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.fol",
              INPUT "p:\asi10test\patch\rco1010\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.cor",
              INPUT "p:\asi10test\patch\rco1010\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.lst",
              INPUT "p:\asi10ship\rco1010\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.fol",
              INPUT "p:\asi10ship\rco1010\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.cor",
              INPUT "p:\asi10ship\rco1010\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.lst",
              INPUT "p:\asi10ship\patch\rco1010\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.fol",
              INPUT "p:\asi10ship\patch\rco1010\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\menu.cor",
              INPUT "p:\asi10ship\patch\rco1010\menu.cor").

/* addon menu */
RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.lst",
              INPUT "p:\asi10test\rco1010\addon\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.fol",
              INPUT "p:\asi10test\rco1010\addon\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.cor",
              INPUT "p:\asi10test\rco1010\addon\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.lst",
              INPUT "p:\asi10test\patch\rco1010\addon\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.fol",
              INPUT "p:\asi10test\patch\rco1010\addon\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.cor",
              INPUT "p:\asi10test\patch\rco1010\addon\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.lst",
              INPUT "p:\asi10ship\rco1010\addon\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.fol",
              INPUT "p:\asi10ship\rco1010\addon\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.cor",
              INPUT "p:\asi10ship\rco1010\addon\menu.cor").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.lst",
              INPUT "p:\asi10ship\patch\rco1010\addon\menu.lst").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.fol",
              INPUT "p:\asi10ship\patch\rco1010\addon\menu.fol").

RUN copy-proc(INPUT "p:\asi_gui10\pco1010\addon\menu.cor",
              INPUT "p:\asi10ship\patch\rco1010\addon\menu.cor").
*/

/******************************************************************************************/
PROCEDURE copy-proc:

   DEFINE INPUT PARAMETER ip-copy-from AS CHAR FORMAT "X(100)" NO-UNDO.
   DEFINE INPUT PARAMETER ip-copy-to   AS CHAR FORMAT "X(100)" NO-UNDO.

   vcCommand = 'ATTRIB -R ' + ip-copy-from.
   OS-COMMAND SILENT VALUE (vcCommand).

   vcCommand = 'ATTRIB -R ' + ip-copy-to.
   OS-COMMAND SILENT VALUE (vcCommand).
               
   OS-COPY VALUE(ip-copy-from) VALUE (ip-copy-to).

   /*vcCommand = 'ATTRIB +R ' + ip-copy-from.
   OS-COMMAND SILENT VALUE (vcCommand). */

END PROCEDURE.
