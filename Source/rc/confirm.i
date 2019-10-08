/* 02.21.91 by hs:
1.  hide frame f-confirm afer update.

*/
define var confirm as logical format "yes/no" initial "no"
    label "Are you sure?".
BELL.
update confirm with frame f-confirm CENTER overlay side-labels
    title "{1}" color value(c_wrn).

hide frame f-confirm.
