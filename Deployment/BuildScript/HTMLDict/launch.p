define variable gcProgramDir as character no-undo.

if '{&uib_is_running}' <> '' then
  gcProgramDir = 'd:\data\progress\datadigger\'.
else do:
  file-info:file-name = this-procedure:file-name.
  gcProgramDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
end.

/* Add program dir to propath (if not already in) */
if search('htmldict.i') = ? then
  propath = gcProgramDir + ',' + propath.

run HtmlDict.w.
