<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="true" AutoEventWireup="true" Inherits="fold_Inks" Title="Inks Estimate" Codebehind="fold_inks.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<asp:ScriptManager ID="ScriptManager1" runat="server"> </asp:ScriptManager>
<script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd(vmessage) {
        var retVal = makeMsgBox("Confirmation", vmessage, 48, 4, 256, 4096);
        if (retVal == 6) {
            var NewWindow = window.open("corr_vendor_cost.aspx", "VendorCost", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

    }
</script>

<script language="javascript" type="text/javascript">

    function getSelect(val) {
        gid = "ctl00_ContentPlaceHolder1_FormView_Fold_" + val;
        
        var selectval = document.getElementById(gid);
        selectval.select();
    }

//window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vColorTextBox"))
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vColorTextBox").disabled!=true)
        {
            var inks = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vColorTextBox");
            inks.focus();
            inks.select();
        }
        else
        {
            var packcode = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vPackCodeTextBox");
            packcode.focus();
            packcode.select();
        }
    }
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.value=obj;
}

function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.value="";
  Datelook();
}

function Platelook(){
 var NewWindow = window.open("Plate_lookup.aspx","DieLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function plateLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vPlateNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vPlateNumTextBox.focus();
  }

  var tval = "";

  function getval(val) {
      tval = val;
  }

  function validate(evt) {
      if (evt.value == "")
          evt.value = "0";

      if (isNaN(evt.value)) {
          evt.focus();
          alert("Enter Numeric Value");
          evt.value = tval;
          return;
      }
      if (evt.id == "ctl00_ContentPlaceHolder1_FormView_Fold_vUnit15TextBox") {
          setfocus();
      }
  }

  /*function check(obj) {
      if (obj.value == "" || obj.value == "0") {
          var inkname = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vColorTextBox").value;
          alert("Incompatible Press Type for Ink: Ink, Please re-enter");
          obj.value = tval;
          return;
      } 
  }
  */
 
 function Codelook()
  { 
  
  var NewWindow = window.open("CodeLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

  var cval = "";
  function Code2look(val) {
      cval = val;
  var NewWindow = window.open("Code2Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code2LookUp(ReturnObj1,ReturnObj2,ReturnObj3) {
    if (cval == 1) {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode1TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr1TextBox.value = ReturnObj2;
        if (ReturnObj3 == "I")
            document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer1TextBox.value = "20";
        else
            document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer1TextBox.value = "100";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr1TextBox.focus();
    }
    else if (cval == 2) {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode2TextBox.value = ReturnObj1;
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr2TextBox.value = ReturnObj2;
        if (ReturnObj3 == "I")
            document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer2TextBox.value = "20";
        else
            document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer2TextBox.value = "100";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr2TextBox.focus();
    }
}
function Code3look()
  { 
  var NewWindow = window.open("Code3Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code3LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr3TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer3TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer3TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr3TextBox.focus();
  
}
function Code4look()
  { 
  var NewWindow = window.open("Code4Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code4LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode4TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr4TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer4TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer4TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr4TextBox.focus();
}
function Code5look()
  { 
  var NewWindow = window.open("Code5Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code5LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode5TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr5TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer5TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer5TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr5TextBox.focus();
}
function Code6look()
  {
  var NewWindow = window.open("Code6Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code6LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode6TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr6TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer6TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer6TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr6TextBox.focus();
}
function Code7look()
  { 
  var NewWindow = window.open("Code7Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code7LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode7TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr7TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer7TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer7TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr7TextBox.focus();
}
function Code8look()
  { 
  var NewWindow = window.open("Code8Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code8LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode8TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr8TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer8TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer8TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr8TextBox.focus();
}
function Code9look()
  { 
  var NewWindow = window.open("Code9Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code9LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode9TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr9TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer9TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer9TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr9TextBox.focus();
}
function Code10look()
  { 
  var NewWindow = window.open("Code10Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code10LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode10TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr10TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer10TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer10TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr10TextBox.focus();
}
function Code11look()
  { 
  var NewWindow = window.open("Code11Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code11LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode11TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr11TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer11TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer11TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr11TextBox.focus();
}
function Code12look()
  { 
  var NewWindow = window.open("Code12Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code12LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode12TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr12TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer12TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer12TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr12TextBox.focus();
}
function Code13look()
  { 
  var NewWindow = window.open("Code13Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code13LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode13TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr13TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer13TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer13TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr13TextBox.focus();
}
function Code14look()
  { 
  var NewWindow = window.open("Code14Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code14LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode14TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr14TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer14TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer14TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr14TextBox.focus();
}
function Code15look()
  { 
  var NewWindow = window.open("Code15Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code15LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode15TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr15TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer15TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer15TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr15TextBox.focus();
}
function carrierlook()
{
var NewWindow = window.open("Carrier_lookup.aspx","CarrierLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(ReturnObj1,ReturnObj2) 
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCarrierTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCarrDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCarrDscrTextBox.focus();
}
function adhesiveLook(){ 
  var looktype = "C";
  var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vEstNumTextBox").value;
  var NewWindow = window.open("adhesive_lookup.aspx?look2="+ looktype +",?lookest="+est+"","AdhesiveLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdhesiveLookup1(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPackCodeTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnitLenTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnitWidTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnitDepTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vBundlTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vBoxCodeTextBox.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vWtPackTextBox.value = ReturnObj9;  
  
  var layer = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLayerTextBox");
  var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCountTextBox");
  count.value = ReturnObj7 * ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPackCodeTextBox.focus();
  
  
}
function unitlook(){ 
    var typelook = "D";
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vEstNumTextBox").value;
    
  var NewWindow = window.open("unit_corlookup.aspx?looktype="+typelook+",?esttype="+est+"","UnitLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function UnitLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnitTextBox.value = ReturnObj1;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLengthTextBox.value = ReturnObj2;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vWidthTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vHeightTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnitTextBox.focus();
}

function zonelook(){ 
    var zonelook = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCarrierTextBox").value;    
  var NewWindow = window.open("zone_lookup.aspx?zone="+ zonelook +"","ZoneLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function zoneLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelZonTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelZonTextBox.focus();
}

function counttype()
{
    var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vBoxCodeTextBox");
    var palet = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vBundlTextBox");
    var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCountTextBox");
    var pack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWtPackTextBox");
    var total;
    total= code.value * palet.value;
    count.value=total; 
    pack.value = 0;  
    
}

function wtpack()
{
    var pack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWtPackTextBox");
    var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCountTextBox");
    var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vBoxCodeTextBox");
    if(code.value == 0)
    {
    return;
    }
    else  if(pack.value >= 1)
    {
    alert("Boxes/Code must be Zero");    
    code.value=0;
    }    
}

function wtcode()
{
    var pack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWtPackTextBox");
    var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vBoxCodeTextBox");
    if (code.value == 0)
    {
    return;
    }
    else if(code.value >= 1)
    pack.value = "0";
}

function pscal() {
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode1TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr1TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer1TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit1TextBox.value = "0";

    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode2TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr2TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer2TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit2TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode3TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr3TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer3TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit3TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode4TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr4TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer4TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit4TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode5TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr5TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer5TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit5TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode6TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr6TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer6TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit6TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode7TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr7TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer7TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit7TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode8TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr8TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer8TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit8TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode9TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr9TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer9TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit9TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode10TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr10TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer10TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit10TextBox.value = "0";
    }

    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode11TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr11TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer11TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit11TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode12TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr12TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer12TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit12TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode13TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr13TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer13TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit13TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode14TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr14TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer14TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit14TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode15TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDscr15TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPer15TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vUnit15TextBox.value = "0";
    }
}


function assignval()
{

if(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode1TextBox.value!="" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs1TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs1TextBox.value = 1;
    
if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode2TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs2TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs2TextBox.value = 1;
    
if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode3TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs3TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs3TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode4TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs4TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs4TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode5TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs5TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs5TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode6TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs6TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs6TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode7TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs7TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs7TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode8TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs8TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs8TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode9TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs9TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs9TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode10TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs10TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs10TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode11TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs11TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs11TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode12TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs12TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs12TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode13TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs13TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs13TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode14TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs14TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs14TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vCode15TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs15TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vPs15TextBox.value = 1;

}
/*function color()
{
    var col = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vColorTextBox");
    var pass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vPassesTextBox");
    if (col.value == 0) {
        pass.value = 0;
        pass.select();    
    }
    else if (col.value >= 1 && pass.value >= 1) {
    pass.select();
    }
    else {
        pass.value = 1;
        pass.select();       
    }    
}
*/
/*function coat()
{
    var coat = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCoatTextBox");
    var pass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCoatPassTextBox");
    if (coat.value == 0) {
        pass.value = 0;
        pass.select();
    } else if (coat.value >= 1 && pass.value >= 1) {
    pass.select();
    }
    else {
        pass.value = 1;
        pass.select();
    }
}
*/
  function lenback()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLengthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLengthTextBox").value = frontback;
    }
   }
   function widback()
    {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWidthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWidthTextBox").value = frontback;
    }
    }
   function heiback()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vHeightTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vHeightTextBox").value = frontback;
    }
    }
    function unitlen()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitLenTextBox").value = frontback;
    }
    }
    function unitwid()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitWidTextBox").value = frontback;
    }
    }
    function unitdep()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitDepTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vUnitDepTextBox").value = frontback;
    }
    }

function Leaflook1(){ 
    var typelook = "5";
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vEstNumTextBox").value;
    var NewWindow = window.open("leaf_lookup.aspx?look1="+typelook+",?leaftype="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup1(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4,ReturnObj5, ReturnObj6,ReturnObj7){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerPadTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerLenTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerWidTextBox.value = ReturnObj5;
 // document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerDepTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerQtyTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vLayerPadTextBox.focus();
  
}

function Leaflook2(){
    var typelook = "6"; 
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vEstNumTextBox").value;
    var NewWindow = window.open("leaf_lookup2.aspx?look2="+typelook+",?leaf2type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup2(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4,ReturnObj5, ReturnObj6,ReturnObj7){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelLenTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelWidTextBox.value = ReturnObj5;
 // document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelDepTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelQtyTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Fold_vDelTextBox.focus();
}

  function layerlen()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLayerLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLayerLenTextBox").value = frontback;
    }
    }
    function layerwid()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLayerWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vLayerWidTextBox").value = frontback;
    }
    }
  function divlen()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vDelLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vDelLenTextBox").value = frontback;
    }
    }
 function divwin()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vDelWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vDelWidTextBox").value = frontback;
    }
    }
function valcost()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCostTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCostTextBox").value = frontback;
    }
    }
 function valcost2()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCost2TextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vCost2TextBox").value = frontback;
    }
    }
function valfright()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vFreifgtTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vFreifgtTextBox").value = frontback;
    }
    }
 function valfright2()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vFreOutTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vFreOutTextBox").value = frontback;
    }
    }
  function valweper()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWeiPerTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vWeiPerTextBox").value = frontback;
    }
}

var sideval = "";
function getsideval(obj) {
    sideval = obj.value;
}
function checksideval(obj) {
    if (obj.value != "" && (!(obj.value == "F" || obj.value == "B"))) {
        obj.focus();
        alert("Invalid Side Value.  Valid Values are F and B.");
        obj.value = sideval;        
    }
}
function jobbuttonconfirm() {
    var order = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Fold_vOrderLabel");

    if (parseInt(order.innerHTML) > 0) {
        if (confirm("Recalculate Job Standards for job# " + order.innerHTML)) {
            return true;
        }
        else {
            return false;
        }
    }
    else {
        alert("Job Standards are not available");
        return false;
    }

}

</script>

    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />    
    
    <div>
    <asp:FormView ID="FormView_Fold"  runat="server" DataSourceID="CorrugatedFoldDataSource" OnDataBound="FormView_Fold_DataBound" >
        <EditItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <fieldset align="top" style="  border-color:Black">
            
            
                
                
            <table class="shade">
            <tr>
            <td colspan="2"><b>Estimate:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vEstNumTextBox" Width="100px" BackColor="Turquoise" ReadOnly="true" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vEstNum") %>'></asp:TextBox>
            <b>EstDate:</b>&nbsp;&nbsp;
            <asp:Label ID="vEstDateTextBox" Width="100px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'> </asp:Label>
            <b>Frm:</b>&nbsp;&nbsp;
            <asp:Label ID="vFormNoTextBox" Width="40px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vFormNo") %>'>  </asp:Label>
            <b>of</b>&nbsp;&nbsp;
            <asp:Label ID="vFormQtyTextBox" Width="40px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vFormQty") %>'> </asp:Label>
            <b>Black:</b>&nbsp;&nbsp;
            <asp:Label ID="vBlankNoTextBox" Width="40px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vBlankNo") %>'> </asp:Label>
            <b>of</b>&nbsp;&nbsp;
            <asp:Label ID="vBlankQtyTextBox" Width="40px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vBlankQty") %>'></asp:Label>
            <b>CustPart:</b>&nbsp;&nbsp;
            <asp:Label ID="vCustPartTextBox" Width="120px" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vCustPart") %>'>  </asp:Label>
            </td>    </tr>                        
            <tr><td valign="top">
            <fieldset style="height:460px" >
            <asp:UpdatePanel ID="UpdatePanel1" runat="server">
             <ContentTemplate>
              <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="UpdatePanel1"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div>
            <table>
            <tr><td nowrap colspan="4">
            <b>Press Type</b>&nbsp;&nbsp;
            <b>Inks:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vColorTextBox" Width="30px" AutoPostBack="true" OnTextChanged="colortextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);" runat="server" MaxLength="2"  Text='<%# Bind("vColor") %>'> </asp:TextBox>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vPassesTextBox" Width="30px" AutoPostBack="true" OnTextChanged="passestextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);" MaxLength="2"  runat="server" Text='<%# Bind("vPasses") %>'></asp:TextBox>
            <b>Coating:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vCoatTextBox" Width="30px" AutoPostBack="true" OnTextChanged="coattextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);" MaxLength="2"  runat="server" Text='<%# Bind("vCoat") %>'> </asp:TextBox>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vCoatPassTextBox" Width="30px" AutoPostBack="true" OnTextChanged="coatpasstextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);"  MaxLength="2"  runat="server" Text='<%# Bind("vCoatPass") %>'></asp:TextBox>
            </td></tr>
            <tr><td colspan="4">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Flexo:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vDscrTextBox" Width="250px" runat="server" Text='<%# Bind("vDscr") %>'></asp:TextBox>
            </td></tr>
            <tr>
                <td><b>Ps</b></td>
                <td><b>Code</b></td>
                <td>Description</td>
                <td><b>%</b></td>
                <td><b>Unit</b></td>
                <td><b>S</b></td>
            </tr>
            <tr>
                <td><asp:TextBox ID="vPs1TextBox" Width="30px" MaxLength="2" onfocus="getval(this.value);" onblur="validate(this);"  runat="server" Text='<%# Bind("vPs1") %>'> </asp:TextBox></td>
                <td><asp:TextBox ID="vCode1TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode1") %>'> </asp:TextBox>
                    <a href="#" tabindex="1" onClick="Code2look(1); return false" ><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                <td><asp:TextBox ID="vDscr1TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr1") %>'></asp:TextBox></td>
                <td><asp:TextBox ID="vPer1TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" runat="server" Text='<%# Bind("vPer1") %>'></asp:TextBox></td>
                <td><asp:TextBox ID="vUnit1TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit1") %>'> </asp:TextBox></td>
                <td><asp:TextBox ID="vSide1TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide1") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs2TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" runat="server" Text='<%# Bind("vPs2") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vCode2TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode2") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code2look(2); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr2TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr2") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer2TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer2") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit2TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit2") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide2TextBox" runat="server" Width="15px" onfocus="getsideval(this);" onblur="checksideval(this);" MaxLength="1" Text='<%# Bind("vSide2") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs3TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs3") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vCode3TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode3") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code3look(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr3TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr3") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer3TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer3") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vUnit3TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();"  MaxLength="3" Text='<%# Bind("vUnit3") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide3TextBox" runat="server" Width="15px" onfocus="getsideval(this);" onblur="checksideval(this);" MaxLength="1" Text='<%# Bind("vSide3") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs4TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs4") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode4TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode4") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code4look(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr4TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr4") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer4TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer4") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit4TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit4") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide4TextBox" runat="server" Width="15px" onfocus="getsideval(this);" onblur="checksideval(this);" MaxLength="1" Text='<%# Bind("vSide4") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs5TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs5") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode5TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode5") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code5look(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr5TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr5") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer5TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer5") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit5TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit5") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide5TextBox" runat="server" Width="15px" onfocus="getsideval(this);" onblur="checksideval(this);" MaxLength="1" Text='<%# Bind("vSide5") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs6TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs6") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode6TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode6") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code6look(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr6TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr6") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer6TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer6") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vUnit6TextBox" runat="server" onfocus="getval(this.value);" onblur="validate(this);" Width="40px" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit6") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide6TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide6") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs7TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs7") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode7TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode7") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code7look(); return false" ><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr7TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr7") %>'>  </asp:TextBox></td>
            <td><asp:TextBox ID="vPer7TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer7") %>'>  </asp:TextBox></td>
            <td><asp:TextBox ID="vUnit7TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit7") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide7TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide7") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs8TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs8") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode8TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode8") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code8look(); return false" ><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr8TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr8") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer8TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer8") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vUnit8TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit8") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide8TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide8") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs9TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs9") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode9TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode9") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code9look(); return false" ><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr9TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr9") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer9TextBox" Width="30px" onkeyup="assignval()" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="3" Text='<%# Bind("vPer9") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vUnit9TextBox" runat="server" Width="40px" onkeyup="assignval()" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="3" Text='<%# Bind("vUnit9") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide9TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide9") %>'> </asp:TextBox></td>
            </tr>
            <tr><td><asp:TextBox ID="vPs10TextBox" Width="30px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs10") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode10TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode10") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code10look(); return false" ><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr10TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr10") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer10TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer11") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit10TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit10") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide10TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide10") %>'> </asp:TextBox></td>            
            </tr> 
            <tr><td><asp:TextBox ID="vPs11TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs11") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode11TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode11") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code11look(); return false" ><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr11TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr11") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer11TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer11") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit11TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit11") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide11TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide11") %>'> </asp:TextBox></td>
            </tr>   
            <tr><td><asp:TextBox ID="vPs12TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs12") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode12TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode12") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code12look(); return false" ><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr12TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr12") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer12TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer12") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit12TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit12") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide12TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide12") %>'> </asp:TextBox></td>
            </tr>   
            <tr><td><asp:TextBox ID="vPs13TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs13") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode13TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode13") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code13look(); return false" ><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr13TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr13") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer13TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer13") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit13TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit13") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide13TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide13") %>'> </asp:TextBox></td>
            </tr>   
            <tr><td><asp:TextBox ID="vPs14TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs14") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode14TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode14") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code14look(); return false" ><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr14TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr14") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer14TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer14") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit14TextBox" runat="server" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vUnit14") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide14TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide14") %>'> </asp:TextBox></td>
            </tr>   
            <tr><td><asp:TextBox ID="vPs15TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs15") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode15TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode15") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code15look(); return false" ><asp:Image ID="Image19" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr15TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr15") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer15TextBox" Width="30px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer15") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vUnit15TextBox" onfocus="getval(this.value);" onblur="validate(this);" runat="server" Width="40px"  MaxLength="3" Text='<%# Bind("vUnit15") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vSide15TextBox" runat="server" onfocus="getsideval(this);" onblur="checksideval(this);" Width="15px" MaxLength="1" Text='<%# Bind("vSide15") %>'> </asp:TextBox></td>
            </tr>              
            </table>
            </ContentTemplate>
            </asp:UpdatePanel>
            </fieldset>
            </td>
            <td valign="top">
            <fieldset style="height:180px" >
            <table>            
            <tr><td></td><td></td>
            <td><b>Length</b></td>
            <td><b>Width</b></td>
            <td><b>Depth</b></td>
            <td><b>Qty</b></td></tr>
            <tr><td align="right"  style="padding-top:5px"><b>Packing Code:</b></td>
            <td><asp:TextBox ID="vPackCodeTextBox" width="100px" runat="server" Text='<%# Bind("vPackCode") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="adhesiveLook(); return false" ><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>           
            <td><asp:TextBox ID="vUnitLenTextBox" width="50px" onkeyup="unitlen()" MaxLength="7" runat="server" Text='<%# Bind("vUnitLen","{0:#0.0000}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vUnitLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>
            <td><asp:TextBox ID="vUnitWidTextBox" width="50px" onkeyup="unitwid()" MaxLength="7" runat="server" Text='<%# Bind("vUnitWid","{0:#0.0000}") %>'>  </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vUnitWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>
            <td><asp:TextBox ID="vUnitDepTextBox" width="50px" onkeyup="unitdep()" MaxLength="7" runat="server" Text='<%# Bind("vUnitDep","{0:#0.0000}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vUnitDepTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td><td>
            <asp:Label ID="vPackQtyTextBox" Width="50px" runat="server"  Text='<%# Bind("vPackQty") %>'></asp:label>
            </td> </tr>
            <tr>
            <td align="right" style="padding-top:5px"><b>Layer Pad:</b></td>
            <td><asp:TextBox ID="vLayerPadTextBox" Width="100px" runat="server" Text='<%# Bind("vLayerPad") %>'>  </asp:TextBox>
            <a href="#" tabindex="1" onClick="Leaflook1(); return false" ><asp:Image ID="Image20" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>            
            <td><asp:TextBox ID="vLayerLenTextBox" Width="50px" onkeyup="layerlen()" MaxLength="7" runat="server" Text='<%# Bind("vLayerLen","{0:#0.0000}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vLayerLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>            
            <td><asp:TextBox ID="vLayerWidTextBox" Width="50px" MaxLength="7" onkeyup="layerwid()" runat="server" Text='<%# Bind("vLayerWid","{0:#0.0000}") %>'> </asp:TextBox> 
            <asp:CompareValidator ID="CompareValidator20" runat="server" ControlToValidate="vLayerWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>           
            <td><asp:label ID="vLayerDepTextBox" Width="50px" MaxLength="7" runat="server" Text='<%# Bind("vLayerDep","{0:#0.0000}") %>'></asp:label></td>
            <td><asp:TextBox ID="vLayerQtyTextBox" Width="50px" runat="server" MaxLength="3" onkeypress="return validate(event)" Text='<%# Bind("vLayerQty") %>'> </asp:TextBox></td>
            </tr>
            <tr><td align="right" style="padding-top:5px"><b>Divider:</b></td>
            <td><asp:TextBox ID="vDelTextBox" Width="100px" runat="server" Text='<%# Bind("vDel") %>'> </asp:TextBox> 
            <a href="#" tabindex="1" onClick="Leaflook2(); return false" ><asp:Image ID="Image21" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>           
            <td><asp:TextBox ID="vDelLenTextBox" Width="50px" MaxLength="7" onkeyup="divlen()" runat="server" Text='<%# Bind("vDelLen","{0:#0.0000}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vDelLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>           
            <td><asp:TextBox ID="vDelWidTextBox" Width="50px" MaxLength="7" onkeyup="divwin()" runat="server" Text='<%# Bind("vDelWid","{0:#0.0000}") %>'> </asp:TextBox> 
            <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vDelWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>           
            <td><asp:label ID="vDelDepTextBox" Width="50px" MaxLength="7" runat="server" Text='<%# Bind("vDelDep","{0:#0.0000}") %>'> </asp:label></td>
            <td><asp:TextBox ID="vDelQtyTextBox" Width="50px" runat="server" MaxLength="3" onkeypress="return validate(event)" Text='<%# Bind("vDelQty") %>'></asp:TextBox></td>
            </tr> 
            <tr><td align="right" style="padding-top:5px"><b>Cost/Ea:</b></td>
            <td><asp:TextBox ID="vCostTextBox" width="100px" onkeyup="valcost()" MaxLength="5" runat="server" Text='<%# Bind("vCost","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td> <td align="right" style="padding-top:5px"><b>Boxes/Code:</b></td>
            <td colspan="3"><asp:TextBox ID="vBoxCodeTextBox" width="100px" onkeyup="counttype()" onblur="counttype()"  runat="server" Text='<%# Bind("vBoxCode") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vBoxCodeTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>           
            </tr>            
            <tr><td align="right" style="padding-top:5px"><b>Cases/Pall:</b></td>
            <td><asp:TextBox ID="vBundlTextBox" width="100px" runat="server" onkeyup="counttype()" Text='<%# Bind("vPallet") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vBundlTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td nowrap align="right" style="padding-top:5px"><b>Weight/Unit:</b></td>
            <td colspan="3"><asp:TextBox ID="vWtPackTextBox" width="100px" onblur="wtpack()" runat="server" Text='<%# Bind("vWtUnit","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vWtPackTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr>
                <td colspan="2"></td>
                <td align="right" style="padding-top:5px"><b>Pack Note:</b></td>
                <td colspan="3"><asp:TextBox ID="vNoteTextBox" Width="100px" runat="server" Text='<%# Bind("vNote") %>'></asp:TextBox></td>
            </tr>
            
            </table></fieldset>
            <fieldset style="height:120px">
            <table>
            <tr><td nowrap align="right" style="padding-top:5px"><b>Pallet#:</b></td>
            <td colspan="3" nowrap><asp:TextBox ID="vUnitTextBox" width="100px" runat="server" Text='<%# Bind("vUnit") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="unitlook(); return false" ><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            
            </tr>
            <tr><td align="right" style="padding-top:5px"><b>Cost/ea:</b></td>
            <td><asp:TextBox ID="vCost2TextBox" width="100px" onkeyup="valcost2()" MaxLength="5" runat="server" Text='<%# Bind("vCost2","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vCost2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-top:5px"><b>Length:</b></td>
            <td colspan="3"><asp:TextBox ID="vLengthTextBox" width="100px" onkeyup="lenback()" MaxLength="7" runat="server" Text='<%# Bind("vLength","{0:#0.0000}") %>'>  </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vLengthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Count:</b></td>
            <td><asp:TextBox ID="vCountTextBox" width="100px" runat="server"  Text='<%# Bind("vCount") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vCountTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td><td align="right" style="padding-top:5px"><b>Width:</b></td>
            <td colspan="3"><asp:TextBox ID="vWidthTextBox" width="100px" onkeyup="widback()" MaxLength="7" runat="server" Text='<%# Bind("vWidth","{0:#0.0000}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td> </tr>
            <tr><td align="right" style="padding-top:5px"><b># of Layers:</b></td>
            <td><asp:TextBox ID="vLayerTextBox" width="100px"  runat="server" Text='<%# Bind("vLayer") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vLayerTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td><td align="right" style="padding-top:5px"><b>Depth:</b></td>
            <td colspan="3"><asp:TextBox ID="vHeightTextBox" width="100px" onkeyup="heiback()" MaxLength="7" runat="server" Text='<%# Bind("vHeight","{0:#0.0000}") %>'></asp:TextBox>            
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vHeightTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            </tr></table></fieldset>
            <fieldset style="height:145px"><table>
            <tr><td align="right" style="padding-top:5px"><b>Freight Charge:</b></td>
            <td colspan="4">
            <asp:RadioButtonList ID="RadioButtonList1"   RepeatLayout="Flow" CellSpacing="1" RepeatColumns="4" SelectedValue='<%# Bind("vFrCharge") %>' datavaluefield='<%# Bind("vFrCharge") %>' runat="server">
                <asp:ListItem Value="P" Text="Prepaid"></asp:ListItem>
                <asp:ListItem Value="C" Text="Collect"></asp:ListItem>
                <asp:ListItem Value="B" Text="Bill"></asp:ListItem>
                <asp:ListItem Value="T" Text="Third Party"></asp:ListItem>
                </asp:RadioButtonList>
            </td></tr>
            <tr><td nowrap align="right" style="padding-top:5px"><b>Weight per M:</b></td>
            <td><asp:TextBox ID="vWeiPerTextBox" width="100px" onkeyup="valweper()" MaxLength="6" runat="server" Text='<%# Bind("vWeiPer","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vWeiPerTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>    </tr>
            <tr><td nowrap align="right" style="padding-top:5px"><b>Carrier:</b></td>
            <td nowrap><asp:TextBox ID="vCarrierTextBox" width="100px" runat="server" Text='<%# Bind("vCarrier") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="carrierlook(); return false" ><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vCarrierTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Carrier must be Enter"></asp:RequiredFieldValidator></td>
            <td colspan="4"><asp:TextBox ID="vCarrDscrTextBox" width="160px" runat="server" Text='<%# Bind("vCarrDscr") %>'> </asp:TextBox></td></tr>
            <tr><td nowrap align="right" style="padding-top:5px"><b>Delivery Zone:</b></td>
            <td nowrap colspan="3"><asp:TextBox ID="vDelZonTextBox" width="100px" runat="server" Text='<%# Bind("vDelZon") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="zonelook(); return false" ><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="vDelZonTextBox" SetFocusOnError="true" Display="dynamic" runat="server" ErrorMessage="DelZon must be Enter"></asp:RequiredFieldValidator>
            </td>        </tr>
            <tr><td nowrap align="right" style="padding-top:5px"><b>Freight Out/CWT:</b></td>
            <td nowrap><asp:TextBox ID="vFreifgtTextBox" width="100px" onkeyup="valfright()" MaxLength="6" runat="server" Text='<%# Bind("vFreifgt","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vFreifgtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td nowrap align="right" style="padding-top:5px"><b>Freight Out/M:</b></td>
            <td colspan="3"><asp:TextBox ID="vFreOutTextBox" onblur="setfocus()" width="100px" onkeyup="valfright2()" MaxLength="6" runat="server" Text='<%# Bind("vFreOut","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vFreOutTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td>  <asp:Label ID="vstockLabel" Visible="false" runat="server" Text='<%# Bind("vStock") %>'></asp:Label></td></tr>
            </table>            
           </fieldset>
            </td></tr>
 
            
                        
            <tr><td>
            <asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClientClick="pscal()" CausesValidation="True" OnClick="Save_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button></td></tr></table>
            
            </fieldset>
            </asp:Panel>
        </EditItemTemplate>
        
        <ItemTemplate>
            <fieldset align="top" style="  border-color:Black">
            <table class="shade" >
            <tr><td nowrap colspan="2" ><b>Estimate:</b>&nbsp;&nbsp;
            <asp:Label ID="vEstNumLabel" Width="80px" BackColor="turquoise" runat="server" Text='<%# Bind("vEstNum") %>'></asp:Label>
            <b>EstDate:</b>&nbsp;&nbsp;
            <asp:Label ID="vEstDateLabel" Width="80px" BackColor="turquoise" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label>
            <b>Frm:</b>&nbsp;&nbsp;
            <asp:Label ID="vFormNoLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vFormNo") %>'></asp:Label>
            <b>of</b>&nbsp;&nbsp;
            <asp:Label ID="vFormQtyLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
            <b>Blank:</b>&nbsp;&nbsp;
            <asp:Label ID="vBlankNoLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vBlankNo") %>'></asp:Label>
            <b>of</b>&nbsp;&nbsp;
            <asp:Label ID="vBlankQtyLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vBlankQty") %>'></asp:Label>
            <b>CustPart:</b>&nbsp;&nbsp;
            <asp:Label ID="vCustPartLabel" Width="200px" BackColor="turquoise" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></td>
            <td style="display:none"><asp:Label ID="vOrderLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("order") %>'></asp:Label></td>
            </tr>
            <tr><td  nowrap="noWrap" valign="top">
            <fieldset align="top" style="height:325px" >
            <table >
            <tr><td nowrap colspan="5" >
            <b>Press Type</b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <b>Inks:</b>&nbsp;&nbsp;
            <asp:Label ID="vColorLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vColor") %>'></asp:Label>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:Label ID="vPassesLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vPasses") %>'></asp:Label>
            <b>Coating:</b>&nbsp;&nbsp;
            <asp:Label ID="vCoatLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vCoat") %>'></asp:Label>
            <b>Passes</b>&nbsp;&nbsp;
            <asp:Label ID="vCoatPassLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vCoatPass") %>'></asp:Label>
            </td>  </tr>
            <tr><td colspan="5">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b>Flexo:</b>&nbsp;&nbsp;
            <asp:Label ID="vDscrLabel" Width="260px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr") %>'></asp:Label></td></tr>
            <tr>
                <td><b>Ps</b></td>
                <td><b>Code</b></td>
                <td><b>Description</b></td>
                <td><b>%</b></td>
                <td><b>Unit</b></td>
                <td><b>S</b></td>
            </tr>
            <tr><td><asp:Label ID="vPs1Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs1") %>'></asp:Label></td>
            <td><asp:Label ID="vCode1Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode1") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr1Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr1") %>'></asp:Label></td>
            <td><asp:Label ID="vPer1Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer1") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit1Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit1") %>'></asp:Label></td>
            <td><asp:Label ID="vSide1Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide1") %>'></asp:Label></td>
            </tr>            
            <tr><td><asp:Label ID="vPs2Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs2") %>'></asp:Label></td>
            <td><asp:Label ID="vCode2Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode2") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr2Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr2") %>'></asp:Label></td>
            <td><asp:Label ID="vPer2Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer2") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit2Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit2") %>'></asp:Label></td>
            <td><asp:Label ID="vSide2Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide2") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs3Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs3") %>'></asp:Label></td>
            <td><asp:Label ID="vCode3Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode3") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr3Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr3") %>'></asp:Label></td>
            <td><asp:Label ID="vPer3Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer3") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit3Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit3") %>'></asp:Label></td>
            <td><asp:Label ID="vSide3Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide3") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs4Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs4") %>'></asp:Label></td>
            <td><asp:Label ID="vCode4Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode4") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr4Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr4") %>'></asp:Label></td>
            <td><asp:Label ID="vPer4Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer4") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit4Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit4") %>'></asp:Label></td>
            <td><asp:Label ID="vSide4Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide4") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs5Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs5") %>'></asp:Label></td>
            <td><asp:Label ID="vCode5Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode5") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr5Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr5") %>'></asp:Label></td>
            <td><asp:Label ID="vPer5Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer5") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit5Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit5") %>'></asp:Label></td>
            <td><asp:Label ID="vSide5Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide5") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs6Label" Width="40px" BackColor="turquoise"   runat="server" Text='<%# Bind("vPs6") %>'></asp:Label></td>
            <td><asp:Label ID="vCode6Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode6") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr6Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr6") %>'></asp:Label></td>
            <td><asp:Label ID="vPer6Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer6") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit6Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit6") %>'></asp:Label></td>
            <td><asp:Label ID="vSide6Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide6") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs7Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs7") %>'></asp:Label></td>
            <td><asp:Label ID="vCode7Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode7") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr7Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr7") %>'></asp:Label></td>
            <td><asp:Label ID="vPer7Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer7") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit7Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit7") %>'></asp:Label></td>
            <td><asp:Label ID="vSide7Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide7") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs8Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs8") %>'></asp:Label></td>
            <td><asp:Label ID="vCode8Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode8") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr8Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr8") %>'></asp:Label></td>
            <td><asp:Label ID="vPer8Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer8") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit8Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit8") %>'></asp:Label></td>
            <td><asp:Label ID="vSide8Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide8") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs9Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs9") %>'></asp:Label></td>
            <td><asp:Label ID="vCode9Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode9") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr9Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr9") %>'></asp:Label></td>
            <td><asp:Label ID="vPer9Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer9") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit9Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit9") %>'></asp:Label></td>
            <td><asp:Label ID="vSide9Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide9") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="vPs10Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs10") %>'></asp:Label></td>
            <td><asp:Label ID="vCode10Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode10") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr10Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr10") %>'></asp:Label></td>
            <td><asp:Label ID="vPer10Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer10") %>'></asp:Label></td>
            <td><asp:Label ID="vUnit10Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit10") %>'></asp:Label></td>
            <td><asp:Label ID="vSide10Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide10") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="Label1" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs11") %>'></asp:Label></td>
            <td><asp:Label ID="Label2" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode11") %>'></asp:Label></td>
            <td><asp:Label ID="Label3" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr11") %>'></asp:Label></td>
            <td><asp:Label ID="Label4" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer11") %>'></asp:Label></td>
            <td><asp:Label ID="Label5" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit11") %>'></asp:Label></td>
            <td><asp:Label ID="vSide11Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide11") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="Label6" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs12") %>'></asp:Label></td>
            <td><asp:Label ID="Label7" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode12") %>'></asp:Label></td>
            <td><asp:Label ID="Label8" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr12") %>'></asp:Label></td>
            <td><asp:Label ID="Label9" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer12") %>'></asp:Label></td>
            <td><asp:Label ID="Label10" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit12") %>'></asp:Label></td>
            <td><asp:Label ID="vSide12Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide12") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="Label11" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs13") %>'></asp:Label></td>
            <td><asp:Label ID="Label12" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode13") %>'></asp:Label></td>
            <td><asp:Label ID="Label13" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr13") %>'></asp:Label></td>
            <td><asp:Label ID="Label14" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer13") %>'></asp:Label></td>
            <td><asp:Label ID="Label15" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit13") %>'></asp:Label></td>
            <td><asp:Label ID="vSide13Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide13") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="Label16" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs14") %>'></asp:Label></td>
            <td><asp:Label ID="Label17" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode14") %>'></asp:Label></td>
            <td><asp:Label ID="Label18" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr14") %>'></asp:Label></td>
            <td><asp:Label ID="Label19" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer14") %>'></asp:Label></td>
            <td><asp:Label ID="Label20" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit14") %>'></asp:Label></td>
            <td><asp:Label ID="vSide14Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide14") %>'></asp:Label></td>
            </tr>
            <tr><td><asp:Label ID="Label21" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs15") %>'></asp:Label></td>
            <td><asp:Label ID="Label22" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode15") %>'></asp:Label></td>
            <td><asp:Label ID="Label23" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr15") %>'></asp:Label></td>
            <td><asp:Label ID="Label24" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer15") %>'></asp:Label></td>
            <td><asp:Label ID="Label25" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit15") %>'></asp:Label></td>
            <td><asp:Label ID="vSide15Label" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("vSide15") %>'></asp:Label></td>
            </tr>
            </table>
            </fieldset>
            </td>
            <td valign="top" >
            <fieldset style="height:125px" >
            <table >
            <tr><td></td><td></td>
            <td><b>Length</b></td>
            <td><b>Width</b></td>
            <td><b>Depth</b></td>
            <td><b>Qty</b></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Packing Code:</b></td>
            <td><asp:Label ID="vPackCodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vPackCode") %>'></asp:Label></td>            
            <td><asp:Label ID="vUnitLenLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitLen","{0:#0.0000}") %>'></asp:Label></td>
            <td><asp:Label ID="vUnitWidLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitWid","{0:#0.0000}") %>'></asp:Label></td>
            <td><asp:Label ID="vUnitDepLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitDep","{0:#0.0000}") %>'></asp:Label></td>
            <td><asp:Label ID="vPackQtyLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vPackQty") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Layer Pad:</b></td>
            <td><asp:Label ID="vLayerPadLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayerPad") %>'></asp:Label></td>           
            <td><asp:Label ID="vLayerLenLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayerLen","{0:#0.0000}") %>'></asp:Label> </td>           
            <td><asp:Label ID="vLayerWidLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayerWid","{0:#0.0000}") %>'></asp:Label> </td>          
            <td><asp:Label ID="vLayerDepLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayerDep","{0:#0.0000}") %>'></asp:Label></td>
            <td><asp:Label ID="vLayerQtyLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayerQty") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Divider:</b></td>
            <td><asp:Label ID="vDelLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vDel") %>'></asp:Label></td>         
            <td><asp:Label ID="vDelLenLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vDelLen","{0:#0.0000}") %>'></asp:Label> </td>           
            <td><asp:Label ID="vDelWidLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vDelWid","{0:#0.0000}") %>'></asp:Label></td>           
            <td><asp:Label ID="vDelDepLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vDelDep","{0:#0.0000}") %>'></asp:Label></td>
            <td><asp:Label ID="vDelQtyLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vDelQty") %>'></asp:Label></td>
            </tr>                        
            <tr><td align="right" style="padding-right:5px"><b>Cost/Ea:</b></td>
            <td><asp:Label ID="vCostLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCost","{0:#0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Boxes/Code:</b></td>
            <td colspan="3"><asp:Label ID="vBoxCodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vBoxCode") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Cases/Pall:</b></td>
            <td><asp:Label ID="vBundlLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vPallet") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Weight/Unit:</b></td>
            <td colspan="3"><asp:Label ID="vWtPackLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vWtUnit","{0:##0.00}") %>'></asp:Label></td>
            </tr>
            <tr>
            <td align="right" colspan="3" style="padding-right:5px"><b>Pack Note: </b></td>
            <td><asp:Label ID="vNoteLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vNote") %>'></asp:Label></td>
            </tr>
            </table></fieldset>
            <fieldset style="height:85px"><table>
            <tr><td align="right" style="padding-right:5px"><b>Pallet#:</b></td>
            <td colspan="3"><asp:Label ID="vUnitLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit") %>'></asp:Label></td>            
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Cost/ea:</b></td>
            <td><asp:Label ID="vCost2Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCost2") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:Label ID="vLengthLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vLength","{0:#0.0000}") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"> <b>Count:</b></td>
            <td><asp:Label ID="vCountLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCount") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:Label ID="vWidthLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vWidth","{0:#0.0000}") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b># of Layers:</b></td>
            <td><asp:Label ID="vLayerLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayer") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Depth:</b></td>
            <td><asp:Label ID="vHeightLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vHeight","{0:#0.0000}") %>'></asp:Label></td>
            </tr></table></fieldset>
            <fieldset style="height:100px"><table>
            <tr><td align="right" style="padding-right:5px"><b>Freigth Charge:</b></td>
            <td colspan="2">
                <asp:RadioButtonList ID="RadioButtonList1" Enabled="false"  RepeatLayout="Flow" CellSpacing="1" RepeatColumns="4" SelectedValue='<%# Bind("vFrCharge") %>' datavaluefield='<%# Bind("vFrCharge") %>' runat="server">
                <asp:ListItem Value="P" Text="Prepaid"></asp:ListItem>
                <asp:ListItem Value="C" Text="Collect"></asp:ListItem>
                <asp:ListItem Value="B" Text="Bill"></asp:ListItem>
                <asp:ListItem Value="T" Text="Third Party"></asp:ListItem>
                </asp:RadioButtonList></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Weight per M:</b></td>
            <td><asp:Label ID="vWeiPerLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vWeiPer","{0:##0.00}") %>'></asp:Label></td>
            <td></td><td></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Carrier:</b></td>
            <td><asp:Label ID="vCarrierLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCarrier") %>'></asp:Label></td>
            
            <td colspan="2"><asp:Label ID="vCarrDscrLabel" Width="200px" BackColor="turquoise" runat="server" Text='<%# Bind("vCarrDscr") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Delivery Zone:</b></td>
            <td><asp:Label ID="vDelZonLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vDelZon") %>'></asp:Label></td>
            <td></td>
            <td></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Freight Out/CWT:</b></td>
            <td><asp:Label ID="vFreifgtLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vFreifgt","{0:##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Freight Out/M:</b></td>
            <td><asp:Label ID="vFreOutLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vFreOut","{0:##0.00}") %>'></asp:Label></td></tr>
            </table>
            </fieldset>
            </td></tr>
                       
            <tr><td >
            <asp:Button ID="UpdateInkButton" runat="server" CssClass="button" OnClick="updateFoldbutton_Click" CausesValidation="True" CommandName="edit"
                Text="Update Ink">                
            </asp:Button>
            <asp:Button ID="OverWriteCutton" runat="server" CssClass="button" OnClick="overwrite_Click" CausesValidation="True" CommandName="edit"
                Text="Override Unit">                
            </asp:Button> 
            <asp:Button ID="jobButton" runat="server" CausesValidation="false" CssClass="button" OnClick="Job_Button_Click" OnClientClick=" return jobbuttonconfirm()"  Text="Job Stds" >  </asp:Button>
            </td></tr>
            </table>
            </fieldset>
        </ItemTemplate>
        
    </asp:FormView>
    </div>
    <asp:ObjectDataSource ID="CorrugatedFoldDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="FoldInks" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmColor" Type="Int32" />
            <asp:Parameter Name="prmPass" Type="Int32" />
            <asp:Parameter Name="prmCoat" Type="Int32" />
            <asp:Parameter Name="prmCoatPass" Type="Int32" />
            <asp:Parameter Name="prmDscr" Type="String" />
            <asp:Parameter Name="prmPs1" Type="Int32" />
            <asp:Parameter Name="prmPs2" Type="Int32" />
            <asp:Parameter Name="prmPs3" Type="Int32" />
            <asp:Parameter Name="prmPs4" Type="Int32" />
            <asp:Parameter Name="prmPs5" Type="Int32" />
            <asp:Parameter Name="prmPs6" Type="Int32" />
            <asp:Parameter Name="prmPs7" Type="Int32" />
            <asp:Parameter Name="prmPs8" Type="Int32" />
            <asp:Parameter Name="prmPs9" Type="Int32" />
            <asp:Parameter Name="prmPs10" Type="Int32" />
            <asp:Parameter Name="prmPs11" Type="Int32" />
            <asp:Parameter Name="prmPs12" Type="Int32" />
            <asp:Parameter Name="prmPs13" Type="Int32" />
            <asp:Parameter Name="prmPs14" Type="Int32" />
            <asp:Parameter Name="prmPs15" Type="Int32" />
            <asp:Parameter Name="prmCode1" Type="String" />
            <asp:Parameter Name="prmCode2" Type="String" />
            <asp:Parameter Name="prmCode3" Type="String" />
            <asp:Parameter Name="prmCode4" Type="String" />
            <asp:Parameter Name="prmCode5" Type="String" />
            <asp:Parameter Name="prmCode6" Type="String" />
            <asp:Parameter Name="prmCode7" Type="String" />
            <asp:Parameter Name="prmCode8" Type="String" />
            <asp:Parameter Name="prmCode9" Type="String" />
            <asp:Parameter Name="prmCode10" Type="String" />
            <asp:Parameter Name="prmCode11" Type="String" />
            <asp:Parameter Name="prmCode12" Type="String" />
            <asp:Parameter Name="prmCode13" Type="String" />
            <asp:Parameter Name="prmCode14" Type="String" />
            <asp:Parameter Name="prmCode15" Type="String" />
            <asp:Parameter Name="prmDscr1" Type="String" />
            <asp:Parameter Name="prmDscr2" Type="String" />
            <asp:Parameter Name="prmDscr3" Type="String" />
            <asp:Parameter Name="prmDscr4" Type="String" />
            <asp:Parameter Name="prmDscr5" Type="String" />
            <asp:Parameter Name="prmDscr6" Type="String" />
            <asp:Parameter Name="prmDscr7" Type="String" />
            <asp:Parameter Name="prmDscr8" Type="String" />
            <asp:Parameter Name="prmDscr9" Type="String" />
            <asp:Parameter Name="prmDscr10" Type="String" />
            <asp:Parameter Name="prmDscr11" Type="String" />
            <asp:Parameter Name="prmDscr12" Type="String" />
            <asp:Parameter Name="prmDscr13" Type="String" />
            <asp:Parameter Name="prmDscr14" Type="String" />
            <asp:Parameter Name="prmDscr15" Type="String" />
            <asp:Parameter Name="prmPer1" Type="Int32" />
            <asp:Parameter Name="prmPer2" Type="Int32" />
            <asp:Parameter Name="prmPer3" Type="Int32" />
            <asp:Parameter Name="prmPer4" Type="Int32" />
            <asp:Parameter Name="prmPer5" Type="Int32" />
            <asp:Parameter Name="prmPer6" Type="Int32" />
            <asp:Parameter Name="prmPer7" Type="Int32" />
            <asp:Parameter Name="prmPer8" Type="Int32" />
            <asp:Parameter Name="prmPer9" Type="Int32" />
            <asp:Parameter Name="prmPer10" Type="Int32" />
            <asp:Parameter Name="prmPer11" Type="Int32" />
            <asp:Parameter Name="prmPer12" Type="Int32" />
            <asp:Parameter Name="prmPer13" Type="Int32" />
            <asp:Parameter Name="prmPer14" Type="Int32" />
            <asp:Parameter Name="prmPer15" Type="Int32" />
            <asp:Parameter Name="prmUnit1" Type="Decimal" />
            <asp:Parameter Name="prmUnit2" Type="Decimal" />
            <asp:Parameter Name="prmUnit3" Type="Decimal" />
            <asp:Parameter Name="prmUnit4" Type="Decimal" />
            <asp:Parameter Name="prmUnit5" Type="Decimal" />
            <asp:Parameter Name="prmUnit6" Type="Decimal" />
            <asp:Parameter Name="prmUnit7" Type="Decimal" />
            <asp:Parameter Name="prmUnit8" Type="Decimal" />
            <asp:Parameter Name="prmUnit9" Type="Decimal" />
            <asp:Parameter Name="prmUnit10" Type="Decimal" />
            <asp:Parameter Name="prmUnit11" Type="Decimal" />
            <asp:Parameter Name="prmUnit12" Type="Decimal" />
            <asp:Parameter Name="prmUnit13" Type="Decimal" />
            <asp:Parameter Name="prmUnit14" Type="Decimal" />
            <asp:Parameter Name="prmUnit15" Type="Decimal" />
            <asp:Parameter Name="prmPackCode" Type="String" />
            <asp:Parameter Name="prmUnitLen" Type="Decimal" />
            <asp:Parameter Name="prmUnitWid" Type="Decimal" />
            <asp:Parameter Name="prmUnitDep" Type="Decimal" />
            <asp:Parameter Name="prmLayerPad" Type="String" />
            <asp:Parameter Name="prmLayerLen" Type="Decimal" />
            <asp:Parameter Name="prmLayerWid" Type="Decimal" />
            <asp:Parameter Name="prmLayerDep" Type="Decimal" />
            <asp:Parameter Name="prmDivider" Type="String" />
            <asp:Parameter Name="prmDividerLen" Type="Decimal" />
            <asp:Parameter Name="prmDividerWid" Type="Decimal" />
            <asp:Parameter Name="prmDividerDep" Type="Decimal" />
            <asp:Parameter Name="prmCostEa" Type="Decimal" />
            <asp:Parameter Name="prmBoxCode" Type="Int32" />
            <asp:Parameter Name="prmPallet" Type="Int32" />
            <asp:Parameter Name="prmWTUnit" Type="Decimal" />
            <asp:Parameter Name="prmPackQty" Type="Decimal" />
            <asp:Parameter Name="prmLayerQty" Type="Decimal" />
            <asp:Parameter Name="prmDivQty" Type="Decimal" />
            <asp:Parameter Name="prmUnit" Type="String" />
            <asp:Parameter Name="prmCost2" Type="Decimal" />
            <asp:Parameter Name="prmCount" Type="Int32" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmHeight" Type="Decimal" />
            <asp:Parameter Name="prmLayer" Type="Int32" />
            <asp:Parameter Name="prmNote" Type="String" />
            <asp:Parameter Name="prmFrCharge" Type="String" />
            <asp:Parameter Name="prmWeightPer" Type="Decimal" />
            <asp:Parameter Name="prmCarrier" Type="String" />
            <asp:Parameter Name="prmCarrDscr" Type="String" />
            <asp:Parameter Name="prmDelZon" Type="String" />
            <asp:Parameter Name="prmFreight" Type="Decimal" />
            <asp:Parameter Name="prmFreight2" Type="Decimal" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_folding_blankno" Type="int32" />
            
            <asp:Parameter Name="prmSide1" Type="String" />
            <asp:Parameter Name="prmSide2" Type="String" />
            <asp:Parameter Name="prmSide3" Type="String" />
            <asp:Parameter Name="prmSide4" Type="String" />
            <asp:Parameter Name="prmSide5" Type="String" />
            <asp:Parameter Name="prmSide6" Type="String" />
            <asp:Parameter Name="prmSide7" Type="String" />
            <asp:Parameter Name="prmSide8" Type="String" />
            <asp:Parameter Name="prmSide9" Type="String" />
            <asp:Parameter Name="prmSide10" Type="String" />
            <asp:Parameter Name="prmSide11" Type="String" />
            <asp:Parameter Name="prmSide12" Type="String" />
            <asp:Parameter Name="prmSide13" Type="String" />
            <asp:Parameter Name="prmSide14" Type="String" />
            <asp:Parameter Name="prmSide15" Type="String" />
            
        </SelectParameters>
    </asp:ObjectDataSource>



</asp:Content>

