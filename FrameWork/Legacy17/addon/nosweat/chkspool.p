/* chkspool.p */

{methods/defines/hndldefs.i}

DEFINE VARIABLE search-dir AS CHARACTER INITIAL "spoolrpt" NO-UNDO.
DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(26)" NO-UNDO.
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE spoolfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE ctitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE cname AS CHARACTER NO-UNDO.
DEFINE VARIABLE printerhndl AS INTEGER NO-UNDO.
DEFINE VARIABLE cdate AS DATE NO-UNDO.
DEFINE VARIABLE ctime AS CHARACTER NO-UNDO.
DEFINE VARIABLE ampm AS CHARACTER NO-UNDO.
DEFINE VARIABLE runspool AS LOGICAL NO-UNDO.
DEFINE VARIABLE spooltime AS INTEGER NO-UNDO.

IF SESSION:BATCH-MODE THEN
  IF SEARCH("splchkup") NE ? THEN
  DO:
    {methods/nowait.i}.
    RETURN.
  END.
  ELSE
  DO:
    OUTPUT TO splchkup.
    OUTPUT CLOSE.
  END.

REPEAT:
  INPUT FROM OS-DIR(search-dir) NO-ECHO.
  REPEAT:
    SET file-name ^ attr-list.
    IF attr-list NE "f" OR
       INDEX(file-name,".spl") = 0 THEN
    NEXT.
    cdummy = cdummy + file-name + ",".
  END.
  INPUT CLOSE.
  DO i = 1 TO NUM-ENTRIES(cdummy) - 1:
    spoolfile = search-dir + "/" + ENTRY(i,cdummy).
    INPUT FROM VALUE(spoolfile) NO-ECHO.
    IMPORT ctitle ^ cname printerhndl.
    IMPORT ^.
    IMPORT ^.
    IMPORT cdate.
    IMPORT ctime.
    IMPORT ampm.
    INPUT CLOSE.
    ASSIGN
      spooltime =
         (INTEGER(SUBSTR(ctime,1,2)) + IF ampm = "pm" THEN 12 ELSE 0) * 3600 +
          INTEGER(SUBSTR(ctime,4,2)) * 60
      runspool = IF TODAY GE cdate AND TIME GE spooltime THEN yes ELSE no.
    IF NOT runspool OR
      (SEARCH("lstlogic/" + cname + "p") = ? AND
       SEARCH("lstlogic/" + cname + "r") = ?) THEN
    NEXT.
    RUN VALUE("lstlogic/" + cname + "p") (REPLACE(spoolfile,".spl",".rpt")).
    OS-DELETE VALUE(spoolfile).
    SESSION:PRINTER-CONTROL-HANDLE = printerhndl.
    OUTPUT TO PRINTER.
    INPUT FROM VALUE(REPLACE(spoolfile,".spl",".rpt")) NO-ECHO.
    REPEAT:
      IMPORT UNFORMATTED cdummy.
      IF cdummy NE "" THEN
      PUT UNFORMATTED cdummy SKIP.
      ELSE
      PUT UNFORMATTED SKIP(1).
    END.
    INPUT CLOSE.
    OUTPUT CLOSE.
  END.
  IF SESSION:BATCH-MODE THEN
  /* started as a batch process */
  DO:
    /* check if shutdown issued */
    IF SEARCH("splchkdn") NE ? THEN
    DO:
      OS-DELETE splchkup.
      OS-DELETE splchkdn.
      LEAVE.
    END.
    PAUSE 5 NO-MESSAGE.
  END.
  ELSE
  /* not a batch process, leave after one pass */
  LEAVE.
  cdummy = "".
END. /* REPEAT */
{methods/nowait.i}
