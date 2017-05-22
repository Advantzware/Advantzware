/* sbHTML.p */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/filterVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i NEW}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

DEFINE VARIABLE beginDate AS DATE NO-UNDO.
DEFINE VARIABLE beginTime AS INTEGER NO-UNDO.
DEFINE VARIABLE stopDate AS DATE NO-UNDO.
DEFINE VARIABLE stopTime AS INTEGER NO-UNDO.
DEFINE VARIABLE startDate AS DATE NO-UNDO.
DEFINE VARIABLE startTime AS INTEGER NO-UNDO.
DEFINE VARIABLE endDate AS DATE NO-UNDO.
DEFINE VARIABLE endTime AS INTEGER NO-UNDO.
DEFINE VARIABLE colSpan AS INTEGER NO-UNDO.
DEFINE VARIABLE jobColSpan AS INTEGER NO-UNDO.
DEFINE VARIABLE tdJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE htmlFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE d AS DATE NO-UNDO.

DEFINE TEMP-TABLE bTtblResource LIKE ttblResource.

FUNCTION timeSpan RETURNS INTEGER
  (ipStartDate AS DATE,ipStartTime AS INTEGER,ipEndDate AS DATE,ipEndTime AS INTEGER) :
  {{&includes}/timeSpan.i}
END FUNCTION.

IF TRUE THEN DO:
  MESSAGE 'This Module No Longer Functions'
    VIEW-AS ALERT-BOX TITLE 'SB HTML'.
  RETURN.
END.

SESSION:SET-WAIT-STATE('General').
RUN getConfiguration.
RUN getResources.
htmlFile = '{&data}\' + ID + '\sb.html'.
OUTPUT TO VALUE(htmlFile).
RUN htmlHeader.
RUN calcColSpan (OUTPUT beginDate,OUTPUT beginTime,OUTPUT stopDate,OUTPUT stopTime,OUTPUT colSpan).
RUN sbJobs.
RUN htmlFooter.
OUTPUT CLOSE.
OS-COMMAND NO-WAIT VALUE(SEARCH(htmlFile)).
SESSION:SET-WAIT-STATE('').

PROCEDURE sbJobs:
  PUT UNFORMATTED
    '  <tr>' SKIP
    '    <td bgcolor="#CCFFCC" align="center">Date</td>' SKIP.
  DO d = beginDate TO stopDate:
    PUT UNFORMATTED
      '    <td bgcolor="#CCFFCC">[</td>' SKIP
      '    <td bgcolor="#CCFFCC" colspan="60">' SUBSTR(STRING(d),1,5) '</td>' SKIP.
    DO i = 1 TO 23:
      PUT UNFORMATTED
        '    <td bgcolor="#CCFFCC">~&nbsp</td>' SKIP
        '    <td bgcolor="#CCFFCC" colspan="60">~&nbsp</td>' SKIP
        '    <td bgcolor="#CCFFCC">~&nbsp</td>' SKIP.
    END.
    PUT UNFORMATTED '    <td bgcolor="#CCFFCC">]</td>' SKIP.
  END.
  PUT UNFORMATTED '  </tr>' SKIP.
  PUT UNFORMATTED
    '  <tr>' SKIP
    '    <td bgcolor="#CCCCCC" align="center">Resource</td>' SKIP.
  DO d = beginDate TO stopDate:
    PUT UNFORMATTED 
      '    <td bgcolor="#CCCCCC">[</td>' SKIP
      '    <td nowrap bgcolor="#CCCCCC" colspan="60" align="center">12 am</td>' SKIP
      '    <td bgcolor="#CCCCCC">]</td>' SKIP.
    DO i = 1 TO 12:
      PUT UNFORMATTED
        '    <td bgcolor="#CCCCCC">[</td>' SKIP
        '    <td nowrap bgcolor="#CCCCCC" colspan="60" align="center">' i ' am</td>' SKIP
        '    <td bgcolor="#CCCCCC">]</td>' SKIP.
    END.
    DO i = 1 TO 11:
      PUT UNFORMATTED
        '    <td bgcolor="#CCCCCC">[</td>' SKIP
        '    <td nowrap bgcolor="#CCCCCC" colspan="60" align="center">' i ' pm</td>' SKIP
        '    <td bgcolor="#CCCCCC">]</td>' SKIP.
    END.
  END.
  PUT UNFORMATTED '  </tr>' SKIP.
  FOR EACH ttblResource NO-LOCK,
      EACH ttblJob NO-LOCK WHERE ttblJob.resource EQ ttblResource.resource
      BREAK BY ttblResource.order
            BY ttblResource.sortOrder
            BY ttblResource.resource:
      IF FIRST-OF(ttblResource.resource) THEN
      DO:
        ASSIGN
          startDate = beginDate
          startTime = beginTime.
        PUT UNFORMATTED
          '  <tr>' SKIP
          '    <td>' SKIP
          '      <table border="1" cellspacing="0" cellpadding="0" width="100%">' SKIP
          '        <tr>' SKIP
          '          <td nowrap bgcolor="yellow" align="center">' SKIP
          '            <b>' ttblJob.resource '</b>' SKIP
          '          </td>' SKIP
          '        </tr>' SKIP
          '      </table>' SKIP
          '    </td>' SKIP.
      END.
      /* time not used */
      jobColSpan = timeSpan(startDate,startTime,ttblJob.startDate,ttblJob.startTime) / 60.
      IF jobColSpan NE 0 THEN
      DO:
        PUT UNFORMATTED   '    <td bgcolor="#000000"><font color="#FFFFFF">[</font></td>' SKIP.
        DO i = 1 TO TRUNC(jobColSpan / 60,0):
          PUT UNFORMATTED '    <td bgcolor="#000000" colspan="60">~&nbsp</td>' SKIP.
        END.
        PUT UNFORMATTED '    <td bgcolor="#000000" colspan="' jobColSpan MOD 60 '">~&nbsp</td>' SKIP.
        PUT UNFORMATTED '    <td bgcolor="#000000"><font color="#FFFFFF">]</font></td>' SKIP.
      END.
      /* job cell */
      ASSIGN
        tdJob = '<u>' + ttblJob.job + '</u>'
        jobColSpan = timeSpan(ttblJob.startDate,ttblJob.startTime,
                              ttblJob.endDate,ttblJob.endTime) / 60.
      PUT UNFORMATTED   '    <td>[</td>' SKIP.
      DO i = 1 TO TRUNC(jobColSpan / 60,0):
        PUT UNFORMATTED '    <td nowrap colspan="60">' tdJob '</td>' SKIP.
        tdJob = '~&nbsp'.
      END.
      PUT UNFORMATTED
        '    <td nowrap colspan="' jobColSpan MOD 60 '">' SKIP
        '      ' tdJob.
      /*
      DO i = 1 TO NUM-ENTRIES(ttblJob.jobDescription):
        PUT UNFORMATTED '<br>' SKIP '      ' ENTRY(i,ttblJob.jobDescription).
      END.
      */
      PUT UNFORMATTED SKIP '    </td>' SKIP '    <td>]</td>' SKIP.
      ASSIGN
        startDate = ttblJob.endDate
        startTime = ttblJob.endTime.
      IF LAST-OF(ttblResource.resource) THEN
      PUT UNFORMATTED '  <tr><td height="2">~&nbsp</td></tr>' SKIP.
  END. /* each ttblresource */
END PROCEDURE.

PROCEDURE calcColSpan:
  DEFINE OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndTime AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opTimeSpan AS INTEGER NO-UNDO.

  FIND FIRST ttblJob NO-LOCK USE-INDEX startDateTime.
  ASSIGN
    opStartDate = ttblJob.startDate
    opStartTime = 0.
  FIND LAST ttblJob NO-LOCK USE-INDEX startDateTime.
  ASSIGN
    opEndDate = ttblJob.endDate
    opEndTime = 84600.
  opTimeSpan = timeSpan(opStartDate,opStartTime,opEndDate,opEndTime) / 60.
END PROCEDURE.

PROCEDURE getConfiguration:
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version asOfTime.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.

PROCEDURE getResources:
  CREATE bTtblResource.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resources.dat')) NO-ECHO.
  REPEAT:
    IMPORT bTtblResource.
    IF CAN-FIND(ttblResource WHERE ttblResource.resource EQ bTtblResource.resource) THEN
    NEXT.
    CREATE ttblResource.
    BUFFER-COPY bTtblResource TO ttblResource.
  END. /* repeat */
END PROCEDURE.

PROCEDURE htmlHeader:
  PUT UNFORMATTED
    '<html>' SKIP
    '<head>' SKIP
    '<title>Scheduler (' ID ') ' version ' Pro (beta version)</title>' SKIP
    '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
    '<meta http-equiv="Refresh" content="120">' SKIP
    '</head>' SKIP
    '<a name="Top">' SKIP
    '<table border="0" cellspacing="0" cellpadding="0" width="100%">' SKIP.
END PROCEDURE.

PROCEDURE htmlFooter:
  PUT UNFORMATTED
    '</table>' SKIP
    '<br>' SKIP
    '<fieldset>' SKIP
    '<div align="left"><a href="#Top">Top</a>' SKIP
    '<div align="right">~&copy; Ron Stark (The Stark Group, Inc.), All Rights Reserved</div>' SKIP
    '</fieldset>' SKIP
    '</html>' SKIP.
END PROCEDURE.
