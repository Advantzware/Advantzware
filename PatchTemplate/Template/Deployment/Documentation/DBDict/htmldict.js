
function writeToggleBegin()
{
 document.write("<a href='javascript:toggleSort(\"SortName\",\"SortOrder\")' class='thlink'>");
}

function writeToggleEnd()
{
 document.write("</a>");
}

function toggleSort(_NameId, _OrderId)
{
 var thisName = document.getElementById( _NameId );
 var thisOrder = document.getElementById( _OrderId );
 if (thisOrder.style.display == "none")
 {
  thisOrder.style.display = "";
  thisName.style.display = "none";
 }
 else
 {
  thisOrder.style.display = "none";
  thisName.style.display = "";
 }
}