/* lib/tt_dblist.i
 *
 */

define temp-table tt_dbList no-undo
  field xvalid        as logical
  field friendlyName  as character format "x(20)"
  field dbPath        as character format "x(60)" /* "x(128)" view-as fill-in size 60 by 1 */
  field serverName    as character format "x(20)" /* "x(128)" view-as fill-in size 20 by 1 */
  field dlcPath       as character format "x(20)" /* "x(128)" view-as fill-in size 20 by 1 */
  field monitorDB     as logical                  label "Mon?"
  field resrcType     as character format "x(10)" label "Type"
  field monitorPID    as character format "x(10)" label "PID"
  field monitorStat   as character format "x(20)" label "Status"
  field statusInfo    as character format "x(50)" label "Status Info"
&IF DEFINED( OE10 ) &THEN
  field dbAlert      as datetime-tz
&ELSE
  field dbAlert      as integer
&ENDIF
  index friendlyName-idx is primary unique friendlyName
.

