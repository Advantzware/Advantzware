function writeStyleChooser()
{
 document.write("Style:"); 
 document.write("<select class='stylecombo' onchange='var tmpVal=this.selectedIndex;eval(this.options[tmpVal].value);this.options[tmpVal].selected=true;'>"); 
 document.write("  <option value=''> </option>");
 document.write("  <option value='changeStyle(\"blue\");'>blue.css</option>");
 document.write("  <option value='changeStyle(\"default\");'>default.css</option>");
 document.write("  <option value='changeStyle(\"green\");'>green.css</option>");
 document.write("  <option value='changeStyle(\"oldstyle\");'>oldstyle.css</option>");
 document.write("  <option value='changeStyle(\"red\");'>red.css</option>");
}