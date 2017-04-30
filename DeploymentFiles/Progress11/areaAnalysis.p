def var cDb as char.
def var iFree as int.

cDb = session:param.

output to value(cDb + "Areas.txt").
FOR EACH _AreaStatus:
iFree = _AreaStatus-TotBlocks - _AreaStatus-Hiwater .
export delimiter "," "Database" "Area Name" "Total Blocks" "High Watermark" "Free".
    export delimiter "," cDb _AreaStatus-AreaName
    _AreaStatus-TotBlocks  
    _AreaStatus-Hiwater 
    iFree.
END.
output close.
