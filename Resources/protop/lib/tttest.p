/* lib/tttest.p
 *
 */

{lib/ttdebug.i}

&IF DEFINED( DEBUGTT ) &THEN
Progress.Database.TempTableInfo:ArchiveTableStatistics = true.
Progress.Database.TempTableInfo:ArchiveIndexStatistics = true.
&ENDIF

run lib/ttinfo.p persistent.

define temp-table tt_unused	/* counted in "peak" but not instantiated -- so no details are visible */
  field x as integer
.

define temp-table tt_inuse
  field y as integer
.

create tt_inuse.
y = 1.

create tt_inuse.
y = 2.

publish "protop_showTT".
