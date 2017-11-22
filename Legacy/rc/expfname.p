/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\rc\expfname.
**       By:
** Descript:
12.31.98 by CAH on \\ricky\rv8 Log#0000:
1.  if opsys = "WIN" then returns .CSV as file extention instead of MDD.  This
makes it easier to import the file into a spreadsheet or other program.
**
*****************************************************************************
\***************************************************************************/
DEF OUTPUT PARAM export_fid AS char NO-UNDO.
DEF var ws_path     AS char NO-UNDO.
DEF var ws_dirsep   AS char NO-UNDO.
DEF var ws_fid      AS char NO-UNDO.
DEF var ws_fid_ext  AS char NO-UNDO.
DEF var ws_int AS int NO-UNDO.
DEF var i1 AS int NO-UNDO.
DEF var i2 AS int NO-UNDO.
if connected("rpro") then do:
  RUN rc/regkey.p ("public_export_dir", OUTPUT ws_path).
  RUN rc/regkey.p ("dirsep", OUTPUT ws_dirsep).
end.
else assign 
    ws_path = "." 
    ws_dirsep = if opsys = "unix" then "/" else "~\".
/* 9812 CAH */
IF OPSYS BEGINS "win" THEN
ws_fid_ext = ".CSV".
ELSE
RUN rc/dt2fext.p (TODAY, OUTPUT ws_fid_ext).
ws_fid = (IF PROGRAM-NAME(2) = ? THEN "export.txt"
ELSE
PROGRAM-NAME(2)).
i1 = INDEX(ws_fid, ws_dirsep) + 1.
IF i1 <= 1 THEN
i1 = INDEX(ws_fid, '/') + 1.
i2 = R-INDEX(ws_fid, ".").
ws_int = i2 - i1.
ws_fid =
IF ws_int > 0 THEN
substring(ws_fid,i1,ws_int) ELSE
ws_fid.
export_fid =
(IF ws_path <> ? AND ws_dirsep <> ?
  THEN
ws_path + ws_dirsep ELSE
'')
+ ws_fid + ws_fid_ext.
