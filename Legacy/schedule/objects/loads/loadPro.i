/* loadPro.i */

DEFINE INPUT PARAMETER ipContainerHandle AS HANDLE NO-UNDO.

{schedule/scopDir.i}
{{&includes}/defBoard.i}

{{&includes}/sharedVars.i}
DEFINE VARIABLE custVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE debugCount AS INTEGER NO-UNDO.
DEFINE VARIABLE holdFlag AS CHARACTER NO-UNDO.
DEFINE VARIABLE matType AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.

{{&viewers}/includes/sharedVars.i NEW}

DEFINE STREAM sCapacity.
DEFINE STREAM sJobNotes.
DEFINE STREAM sPending.
DEFINE STREAM sResource.
DEFINE STREAM sScenario.

DEFINE TEMP-TABLE bBrowseColumn LIKE browseColumn.

DEFINE TEMP-TABLE colCheck NO-UNDO
  FIELD fld1 AS CHARACTER
  FIELD fld2 AS CHARACTER
  FIELD fld3 AS CHARACTER
  FIELD fld4 AS CHARACTER
  FIELD found AS LOGICAL
    INDEX colCheck1 fld1 fld2
    INDEX colCheck2 fld3 fld4.

DEFINE BUFFER bColCheck FOR colCheck.

DEFINE TEMP-TABLE statusCheckOffs NO-UNDO
  FIELD statusCheckOffs AS CHARACTER
  FIELD materialType AS CHARACTER
    INDEX statusCheckOffs statusCheckOffs.

/* moved to loadProEnd.i */
/* RUN updateColumns. /* add newly added columns incase they are missing */ */

&IF '{&Board}' NE 'View' &THEN
RUN spCreateAuditHdr (
    "LOG",     /* type  */
    "ASI",     /* db    */
    "sbPro.", /* table */
    ID,        /* key   */
    OUTPUT iAuditID
    ).
RUN spCreateAuditDtl (
    iAuditID,    /* audit id     */
    "LoadBegin", /* field        */
    0,           /* extent       */
    STRING(TODAY,"99.99.9999") + " @ " + STRING(TIME,"hh:mm:ss"), /* before value */
    "",          /* after value  */
    NO           /* index field  */
    ).

OUTPUT STREAM sCapacity TO VALUE(SEARCH('{&data}/' + ID + '/capacity.dat')).
OUTPUT STREAM sJobNotes TO VALUE(SEARCH('{&data}/' + ID + '/jobNotes.dat')).
OUTPUT STREAM sPending  TO VALUE(SEARCH('{&data}/' + ID + '/Pending.dat')).
OUTPUT STREAM sResource TO VALUE(SEARCH('{&data}/' + ID + '/resources.dat')).
OUTPUT STREAM sScenario TO VALUE(SEARCH('{&scenarios}/' + ID + '/Actual.dat')).
&ENDIF

{{&loads}/commonLoad.i}

INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/customValues.dat')).
IMPORT customValueList.
INPUT CLOSE.

INPUT FROM VALUE(SEARCH('{&data}/statusCheckOffs.dat')).
REPEAT:
  IMPORT DELIMITER '~t' custVal matType.
  IF matType EQ ? THEN NEXT.
  CREATE statusCheckOffs.
  ASSIGN
    statusCheckOffs.statusCheckOffs = custVal
    statusCheckOffs.materialType = matType.
END. /* repeat */
INPUT CLOSE.
