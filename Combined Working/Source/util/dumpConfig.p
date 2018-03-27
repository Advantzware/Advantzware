


DEFINE INPUT PARAMETER ipcOutputPath AS CHARACTER NO-UNDO.

DEF STREAM sNotes.
DEF    VAR      vcCommand     AS CHAR      NO-UNDO.

DEFINE VARIABLE cPrgrmsPath   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmxrefPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLookupsPath  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmailCodPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReftablePath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNotePath     AS CHARACTER NO-UNDO.

ASSIGN
    cPrgrmsPath   = ipcOutputPath + "\" + "prgrms.d"
    cPrgmxrefPath = ipcOutputPath + "\" + "prgmxref.d"
    cLookupsPath  = ipcOutputPath + "\" + "lookups.d"
    cEmailCodPath = ipcOutputPath + "\" + "emailcod.d"
    cReftablePath = ipcOutputPath + "\" + "reftable.d".
cNotePath = ipcOutputPath + "\" + "notes.d".

vcCommand = 'ATTRIB -R ' + cPrgrmsPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cPrgrmsPath).
FOR EACH asinos.prgrms NO-LOCK:
    EXPORT asinos.prgrms.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cPrgmxrefPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cPrgmxrefPath).
FOR EACH asinos.prgmxref NO-LOCK:
    EXPORT asinos.prgmxref.
END.
OUTPUT CLOSE.

vcCommand = 'ATTRIB -R ' + cLookupsPath.
OS-COMMAND SILENT VALUE (vcCommand).

OUTPUT TO VALUE(cLookupsPath).
FOR EACH asinos.lookups NO-LOCK:
    EXPORT asinos.lookups.
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
    FOR EACH asinos.notes NO-LOCK WHERE asinos.notes.rec_key EQ reftable.rec_key:
        EXPORT STREAM sNotes asinos.notes.
    END.
END.
OUTPUT STREAM sNotes CLOSE.
OUTPUT CLOSE.


ASSIGN
    cPrgrmsPath   = ipcOutputPath + "\addon\" + "prgrms.d"
    cPrgmxrefPath = ipcOutputPath + "\addon\" + "prgmxref.d"
    cLookupsPath  = ipcOutputPath + "\addon\" + "lookups.d"
    cEmailCodPath = ipcOutputPath + "\addon\" + "emailcod.d"
    cReftablePath = ipcOutputPath + "\addon\" + "reftable.d"
    cNotePath = ipcOutputPath + "\addon\" + "notes.d"
    .

FILE-INFO:FILE-NAME = ipcOutputPath + "\addon".  
DEF VAR cAddonDir AS CHAR .
cAddonDir = FILE-INFO:FULL-PATHNAME.
MESSAGE "caddondir" caddondir
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* addon */
IF cAddonDir NE ? AND cAddonDir NE "" THEN 
DO:
    vcCommand = 'ATTRIB -R ' + cPrgrmsPath.
    OS-COMMAND SILENT VALUE (vcCommand).

    OUTPUT TO VALUE(cPrgrmsPath).
    FOR EACH nosweat.prgrms NO-LOCK:
        EXPORT nosweat.prgrms.
    END.
    OUTPUT CLOSE.

    vcCommand = 'ATTRIB -R ' + cPrgmxrefPath.
    OS-COMMAND SILENT VALUE (vcCommand).

    OUTPUT TO VALUE(cPrgmxrefPath).
    FOR EACH nosweat.prgmxref NO-LOCK:
        EXPORT nosweat.prgmxref.
    END.
    OUTPUT CLOSE.

    vcCommand = 'ATTRIB -R ' + cLookupsPath.
    OS-COMMAND SILENT VALUE (vcCommand).

    OUTPUT TO VALUE(cLookupsPath).
    FOR EACH nosweat.lookups NO-LOCK:
        EXPORT nosweat.lookups.
    END.
    OUTPUT CLOSE.
END.

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
