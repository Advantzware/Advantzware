/* lib/aion.p
 *
 * is after-imaging enabled?
 *
 */

define output parameter aiEnabled as logical no-undo.

find dictdb._Logging no-lock.

if _Logging-AIBegin begins "-" then 				         /* _Logging-AIJournal is apparently broken :( 		*/
  aiEnabled = no.
 else
  aiEnabled = yes.

return.
