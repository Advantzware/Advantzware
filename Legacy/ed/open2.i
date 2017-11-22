DEF STREAM s-tran.
DEF STREAM s-line.
def var export_all as logical no-undo.
run rc/regkey.p (input "dirsep", output dirsep).
run rc/regkey.p (input "export_all", output ws_char).
if ws_char begins "Y" then export_all = true.

if export_all then do:

def var edi_outbound_path as char no-undo.
run rc/regkey.p (input "edi_outbound_path", output edi_outbound_path).
OUTPUT STREAM s-tran TO value(edi_outbound_path + dirsep + "{1}")
    {3}.
OUTPUT STREAM s-line TO value(edi_outbound_path + dirsep + "{2}")
    {3}.
end.
