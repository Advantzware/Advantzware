define variable iCurCol as integer no-undo.
define variable iCurRow as integer no-undo.
define variable chWorkSheet as com-handle no-undo.
define variable iCurrentColor as integer no-undo init 1.
define variable cColors as character no-undo init "8,6,4".

function setWorkSheet returns logical (h as com-handle):
  chWorkSheet = h.
end function.

function setCol returns logical (i as integer):
  iCurCol = i.
end function.

function setRow returns logical (i as integer):
  iCurRow = i.
end function.

function moveNextCol returns logical ():
  iCurCol = iCurCol + 1.
end function.

function moveNextRow returns logical ():
  iCurRow = iCurRow + 1.
  iCurCol = 1.
end function.

function getCol returns character ():
  return chr(64 + iCurCol).
  /*26*/
end function.

function putText returns logical (txt as character):
  ASSIGN 
    chWorkSheet:Range(getCol() + string(iCurRow)):VALUE = txt
  .
end function.

function putTextNext returns logical (txt as character):
  putText(txt).
  moveNextCol().
end function.

function ColorizeRow returns logical (range as character):
  define variable chRange as com-handle no-undo.
  chRange = chWorkSheet:Range(entry(1, range, ":") + string(iCurRow) + ":" + 
                              entry(2, range, ":") + string(iCurRow)).
  chRange:Interior:ColorIndex = integer(entry(iCurrentColor, cColors)).
  chRange:Interior:Pattern = 1.
  chRange:Interior:PatternColorIndex = -4105.

end function.

function ColorMoveNext returns logical:
  iCurrentColor = iCurrentColor + 1.
  if iCurrentColor > num-entries(cColors) then
    iCurrentColor = 1.
end function.
