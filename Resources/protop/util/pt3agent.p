/* pt3agent.p
 *
 * ProTop 3 server side agent
 *
 * mpro /dbPath/dbName -p util/pt3agent -param "friendlyName|startupDataCollectorList"
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

/*** everything -- for reference or to tweak below
 ***
 ***/

/*
ptInitDC =								/* ptdefs/tt_protop.xml					*/
  "DBId," +
  "Dashboard," +
  "Configuration," +
  "OSInfo," +
/*
  "ReplAgent," +
  "TenantInfo," +
 */

  "Blocked," +
  "Blocker," +
  "ActiveTRX," +

  "TableActivity," +
  "IndexActivity," +
  "LatchActivity," +
  "ResourceWaits," +
/*
  "RemoteServerActivity," +
 */
  "UserIOActivity," +
  "SequenceActivity," +

  "StorageAreas," +
  "FileIOActivity," +
  "BigBGuesstimator," +
  "Checkpoints," +
  "TXE," +
  "Who," +
  "df," +
  "netstat"
.
 */

/*** core stats for the web portal
 ***
 ***/

ptInitDC =
  "DBId,"           +
  "Dashboard,"      +
  "Configuration,"  +
  "TableActivity,"  +
  "IndexActivity,"  +
  "LatchActivity,"  +
  "ResourceWaits,"  +
  "UserIOActivity," +
  "StorageAreas"
.

{ssg/sausage07.i}

return.

/* the end */
