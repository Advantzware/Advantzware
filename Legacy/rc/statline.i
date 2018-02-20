def var fill_date as char format 'x(08)' label "SysDate" no-undo.
def var fill_time as char format 'x(08)' label "@"       no-undo.
FORM
    ws_co       label "Co#"
    ws_dept     label "Dept"
    ws_app      label "App"
    ws_per      label "Per"
    ws_userid   label "UserID"
     space(3)
    fill_date  no-label /* to 71 */
    fill_time  format 'x(07)' no-label /* to 80 */
    "" at 80
WITH FRAME f-stat ROW 1 COLUMN 1 NO-BOX SIDE-LABELS COLOR value(c_pop)
    no-hide.

{rc/statdisp.i}
