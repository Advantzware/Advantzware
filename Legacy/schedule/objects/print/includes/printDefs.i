/* printDefs.i - additional parameters effect procedure print in board.w,
                 proc testPrint in viewers/dynColumns.w,
                 search for all calls to fieldFilter.w */

DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFormat AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipTitle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipExcel AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipShowParametersPage AS LOGICAL NO-UNDO.

{{&includes}/defBoard.i}
{{&includes}/filterVars.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&viewers}/includes/sharedVars.i}
{{&includes}/rptTables.i}

DEFINE VARIABLE noteRowID AS ROWID NO-UNDO.
DEFINE VARIABLE runTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

&GLOBAL-DEFINE jobTable ttblJob
&GLOBAL-DEFINE jobStartDate startDate
&GLOBAL-DEFINE jobEndDate endDate
&GLOBAL-DEFINE asiPrintNames dieCutting,finishing,printing,sheeting
