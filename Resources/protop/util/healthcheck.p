/* healthcheck.p
 *
 * ProTop 3 healthcheck routine
 *
 * mbpro /dbPath/dbName -p util/healthcheck.p -param "friendlyName|startupDataCollectorList" > tmp/healthcheck.err 2>&1
 *
 */

define new global shared variable dbgMode as integer no-undo.

/* dbgMode = 1	minimal
 * dbgMode = 3	errors
 * dbgMode = 4	errors + success
 * dbgMode = 5	verbose socket communications details (overkill)
 */

dbgMode = 3.

define variable ptInitDC     as character   no-undo.			/* which data collectors should we start with?		*/

assign                                                                  /* a lazy american must have written this code...       */
  session:date-format    = "ymd"
  session:numeric-format = "american"
.


/*** core stats for a healthcheck
 ***
 ***/

ptInitDC =
  "DBId,"             +
  "Dashboard,"        +
  "Configuration,"    +
  "TableActivity,"    +
  "IndexActivity,"    +
  "LatchActivity,"    +
  "ResourceWaits,"    +
  "UserIOActivity,"   +
  "SequenceActivity," +
  "StorageAreas,"     +
  "OSInfo,"           +
  "Checkpoints,"      +
  "df,"               +
  "netstat"
.

{ssg/sausage07.i}

return.

/* the end */
