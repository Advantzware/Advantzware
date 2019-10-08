/*
05.10.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Merged into rc/loginv.i.

06.23.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Changed default page_len back from 63 to 60.  Was overflowing on
    customer laser printers set for 60.
06.09.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added support for export stream.
2.  Inserted variables from std headings.  Putting them here allows them
to be initialized or tested between inclusion of hdg-xxx.i and getprint.i.

def {&shared} var printfid AS char FORMAT 'x(50)' NO-UNDO.
def {&shared} var printdest AS char NO-UNDO.
def {&shared} var printstring as char format 'x(78)' NO-UNDO.

def {&shared} var print-width AS int INITIAL 80 NO-UNDO.
def {&shared} var dos_command AS char FORMAT 'x(60)' NO-UNDO.
def {&shared} var page_len AS int NO-UNDO INITIAL 60.

def {&shared} var hdg_name as char format 'x(30)' NO-UNDO.
def {&shared} var hdg_text as char format 'X(30)' NO-UNDO.
def {&shared} var hdg_rpt_code as char format 'x(18)' /* 9704 CAH was 15 */
    initial '{1}' no-undo.
def {&shared} var hdg_desc as char format 'x(30)'
    initial '{2}' NO-UNDO.
def {&shared} var hdg_perdate as date NO-UNDO.
define new shared var hdg_widerpt as logical initial false NO-UNDO.

def stream s-export.
def {&shared} var export_opt as logical no-undo
    format 'Y/N' label 'Create Export File?'.
def {&shared} var export_capable as logical no-undo initial false.
def {&shared} var export_fid as char no-undo initial 'export.txt'
    format 'x(30)' label 'Export Filename'.
def {&shared} var export_open as logical no-undo initial false.
def {&shared} var export_action as char no-undo initial 'R' format '!'.


*/
