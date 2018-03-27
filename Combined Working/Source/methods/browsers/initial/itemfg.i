/* itemfg.i */

{custom/getcmpny.i}
{sys/inc/fgbrowse.i}
useColors = sys-ctrl.char-fld.

&IF DEFINED(noGetCellColumns) EQ 0 &THEN
RUN getCellColumns.
&ENDIF
