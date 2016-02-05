/* startUp.p - used to create custom startUp code */

DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opCompany AS CHARACTER NO-UNDO.

/* custom code (security check, etc) here, PROGRAM-NAME(2) is
   name of calling program (sbPro.p, sbView.p or sbBasic.p */

DEFINE VARIABLE sbName AS CHARACTER NO-UNDO.

{schedule/scopDir.i}

sbName = IF INDEX(PROGRAM-NAME(2),'sbPro.') NE 0 THEN 'sbPro.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbView.') NE 0 THEN 'sbView.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbBasic.') NE 0 THEN 'sbBasic.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbHTML.') NE 0 THEN 'sbHTML.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbReport.') NE 0 THEN 'sbReport.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbStatus.') NE 0 THEN 'sbStatus.'
    ELSE ''.

{methods/defines/hndldefs.i}
{custom/prgsecur.i &vprgmname=sbName}

ASSIGN
  opContinue = NOT access-close /* output YES or NO */
  opCompany = g_company.

IF opContinue THEN
RUN copyDataFiles. /* move data files to all/folding/corrugated */

PROCEDURE copyDataFiles:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE dirList AS CHARACTER NO-UNDO INITIAL 'ALL,Corrugated,Fleetwood,Folding'.
  DEFINE VARIABLE datList AS CHARACTER NO-UNDO INITIAL
    'capacity,columns,config,customValues,downtimes,jobNotes,Pending,priorityList,resourceList,resources,startUp'.
  
  IF SEARCH('{&data}/ASI/config.dat') EQ ? THEN RETURN.
  DO i = 1 TO NUM-ENTRIES(datList):
    DO j = 1 TO NUM-ENTRIES(dirList):
      OS-COPY VALUE('{&data}/ASI/' + ENTRY(i,datList) + '.dat')
              VALUE('{&data}/ASI/' + ENTRY(j,dirList) + '/' + ENTRY(i,datList) + '.dat').
    END.
    OS-DELETE VALUE('{&data}/ASI/' + ENTRY(i,datList) + '.dat').
  END.
  DO j = 1 TO NUM-ENTRIES(dirList):
    OS-COPY VALUE('{&scenarios}/ASI/Actual.dat')
            VALUE('{&scenarios}/ASI/' + ENTRY(j,dirList) + '/Actual.dat').
  END.
  OS-DELETE VALUE('{&scenarios}/ASI/Actual.dat').
END PROCEDURE.
