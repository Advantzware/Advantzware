/* _scriptsDat.p */

{_base.i}

DEF VAR datFile AS CHAR NO-UNDO.
DEF VAR cFile   AS CHAR NO-UNDO.
DEF VAR idx     AS INT  NO-UNDO.

DEFINE STREAM sScriptsDat.

INPUT STREAM sScriptsDat FROM {&datLocation}\scriptsDat{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sScriptsDat datFile.
    PUT UNFORMATTED datFile SKIP.
    INPUT FROM VALUE("{&datLocation}\" + datFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        OS-COPY VALUE("{&base}" + cFile) VALUE("{&v17}" + cFile).
    END.
    INPUT CLOSE.
END.
INPUT STREAM sScriptsDat CLOSE.
