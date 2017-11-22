def var month_list as char no-undo initial '123456789OND3'.
def input  param p_date as date no-undo.
def output param p_ext  as char no-undo.
p_ext =
    '.'
    + substring(month_list,month(p_date),1)
    + string(day(p_date),'99').
