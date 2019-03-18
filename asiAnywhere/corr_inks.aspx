<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master" Debug="true" AutoEventWireup="true" Inherits="corr_Inks" Title="Inks Estimate" Codebehind="corr_inks.aspx.cs" %>
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
        gid = "ctl00_ContentPlaceHolder1_FormView_Ink_" + val;

        var selectval = document.getElementById(gid);
        selectval.select();
    }

//window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vColorTextBox"))
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vColorTextBox").disabled!=true)
        {
            var color=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vColorTextBox");
            color.focus();
            color.select();
        }
        else
        {
            var packcode = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vPackCodeTextBox");
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
      if (evt.id == "ctl00_ContentPlaceHolder1_FormView_Ink_vPer10TextBox") {
          setfocus();
      }   
}
 
 
 function Codelook()
  { 
  
  var NewWindow = window.open("CodeLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

//function CodeLookUp(ReturnObj1,ReturnObj2)
//{ 
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode1TextBox.value = ReturnObj1;
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr1TextBox.value=ReturnObj2;
//
  //}
  var cval = "";
  function Code2look(val) {
      cval = val;    
    var NewWindow = window.open("Code2Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

  function Code2LookUp(ReturnObj1, ReturnObj2, ReturnObj3) {
      if (cval == 1) {
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode1TextBox.value = ReturnObj1;
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr1TextBox.value = ReturnObj2;
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode1TextBox.focus();
          if (ReturnObj3 == "I")
              document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer1TextBox.value = "20";
          else
              document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer1TextBox.value = "100";
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs1TextBox.value = "1";
      }  
      else if (cval == 2) {
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode2TextBox.value = ReturnObj1;
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr2TextBox.value = ReturnObj2;
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode2TextBox.focus();
          if (ReturnObj3 == "I")
              document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer2TextBox.value = "20";
          else
              document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer2TextBox.value = "100";
          document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs2TextBox.value = "1";
      }
}
function Code3look()
  { 
  var NewWindow = window.open("Code3Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code3LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr3TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode3TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer3TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer3TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs3TextBox.value = "1";
  
}
function Code4look()
  { 
  var NewWindow = window.open("Code4Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code4LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode4TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr4TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode4TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer4TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer4TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs4TextBox.value = "1";
}
function Code5look()
  { 
  var NewWindow = window.open("Code5Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code5LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode5TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr5TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode5TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer5TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer5TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs5TextBox.value = "1";
}
function Code6look()
  {
  var NewWindow = window.open("Code6Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code6LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode6TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr6TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode6TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer6TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer6TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs6TextBox.value = "1";
}
function Code7look()
  { 
  var NewWindow = window.open("Code7Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code7LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode7TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr7TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode7TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer7TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer7TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs7TextBox.value = "1";
}
function Code8look()
  { 
  var NewWindow = window.open("Code8Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code8LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode8TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr8TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode8TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer8TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer8TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs8TextBox.value = "1";
}
function Code9look()
  { 
  var NewWindow = window.open("Code9Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code9LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode9TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr9TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr9TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer9TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer9TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs9TextBox.value = "1";
}
function Code10look()
  { 
  var NewWindow = window.open("Code10Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code10LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode10TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr10TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode10TextBox.focus();
  if(ReturnObj3=="I")
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer10TextBox.value="20";
  else
      document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer10TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs10TextBox.value = "1";
}
function carrierlook()
{
var NewWindow = window.open("Carrier_lookup.aspx","CarrierLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrierlookup(ReturnObj1,ReturnObj2) 
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCarrierTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCarrDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCarrierTextBox.focus();
}
function adhesiveLook(){ 
  var looktype = "C";
   var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vEstNumTextBox").value;
  var NewWindow = window.open("adhesive_lookup.aspx?look2="+ looktype +",?lookest="+est+"","AdhesiveLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdhesiveLookup1(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9) { 
 
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPackCodeTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vUnitLenTextBox.value = ReturnObj4;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vUnitWidTextBox.value = ReturnObj5;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vUnitDepTextBox.value = ReturnObj6;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vBundlTextBox.value = ReturnObj7;
    document.forms[0].ctl00$ContentPlaceHolder1$vBundlHiddenField.value = ReturnObj7;    
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vBoxCodeTextBox.value = ReturnObj8;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vWtPackTextBox.value = ReturnObj9;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPackCodeTextBox.focus();
    
    var stack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vStackTextBox");
    var layer = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vLayerTextBox");
    var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCountTextBox");
  
    layer.value= parseInt(ReturnObj7 / stack.value);
    count.value = ReturnObj7 * ReturnObj8;

    document.forms[0].ctl00$ContentPlaceHolder1$vLayerHiddenField.value = parseInt(ReturnObj7 / stack.value);
    document.forms[0].ctl00$ContentPlaceHolder1$vCountHiddenField.value = ReturnObj7 * ReturnObj8;  
}
function unitlook(){ 
    var typelook = "D";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vEstNumTextBox").value;
  var NewWindow = window.open("unit_corlookup.aspx?looktype="+ typelook +",?esttype="+est+"","UnitLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function UnitLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vUnitTextBox.value = ReturnObj1;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vLengthTextBox.value = ReturnObj2;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vWidthTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vHeightTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vUnitTextBox.focus();

  document.forms[0].ctl00$ContentPlaceHolder1$vHeightHiddenField.value  = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$vWidthHiddenField.value   = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$vLengthHiddenField.value  = ReturnObj2;
}

function zonelook(){ 
    var zonelook = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCarrierTextBox").value;
  var NewWindow = window.open("zone_lookup.aspx?zone="+ zonelook +"","ZoneLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function zoneLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDelZonTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDelZonTextBox.focus();  
}

function counttype() {
    if (document.getElementById("ctl00_ContentPlaceHolder1_HiddenField_unit_cal").value != 1) {       
        var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBoxCodeTextBox");
        var palet = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBundlTextBox");
        var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCountTextBox");
        var layer = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vLayerTextBox");
        var stack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vStackTextBox");
        var total;
        total = code.value * palet.value;
        count.value = total;
        layer.value = Math.ceil(palet.value / stack.value);
    }
    
}
function Palet()
{
    if (document.getElementById("ctl00_ContentPlaceHolder1_HiddenField_unit_cal").value != 1) {       
        var stack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vStackTextBox");
        var layer = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vLayerTextBox");
        var pallet = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBundlTextBox");
        var total;
        total = stack.value * layer.value;
        pallet.value = total;
        var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBoxCodeTextBox");
        //var palet = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBundlTextBox");
        var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCountTextBox");
        var total2;
        total2 = code.value * pallet.value;
        count.value = total2;
    }
    
}
function wtpack()
{
    var pack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWtPackTextBox");
    var count = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCountTextBox");
    var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBoxCodeTextBox");
   if( parseFloat(code.value) == 0)
   {  
   return;
   }
   else 
    if(parseFloat(pack.value) >= 1)
    {
    alert("Count must be Zero");
    count.value=0;
    code.value=0;
    }
    
}
function wtcode()
{
    var pack = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWtPackTextBox");
    var code = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vBoxCodeTextBox");
   
    if (parseFloat(code.value) == 0) {
        return;
    }
    else if (parseFloat(code.value) >= 1) {
        pack.value = "0";
    }
}

function pccal() {
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode1TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs1TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr1TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer1TextBox.value = "0";        
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode2TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs2TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr2TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer2TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode3TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs3TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr3TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer3TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode4TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs4TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr4TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer4TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode5TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs5TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr5TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer5TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode6TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs6TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr6TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer6TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode7TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs7TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr7TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer7TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode8TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs8TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr8TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer8TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode9TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs9TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr9TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer9TextBox.value = "0";
    }
    if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode10TextBox.value == "") {
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs10TextBox.value = "0";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vDscr10TextBox.value = "";
        document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPer10TextBox.value = "0";
    }
}


function assignval() {
    
if(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode1TextBox.value!="" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs1TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs1TextBox.value = 1;
    
if(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode2TextBox.value!="" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs2TextBox.value) < 1)
document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs2TextBox.value=1;

if(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode3TextBox.value!="" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs3TextBox.value)< 1)
document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs3TextBox.value=1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode4TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs4TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs4TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode5TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs5TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs5TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode6TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs6TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs6TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode7TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs7TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs7TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode8TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs8TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs8TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode9TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs9TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs9TextBox.value = 1;

if (document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vCode10TextBox.value != "" && parseFloat(document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs10TextBox.value) < 1)
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Ink_vPs10TextBox.value = 1;


}
/*function color()
{
    var col = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vColorTextBox");
    var pass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vPassesTextBox");
    if (col.value <= 0) {
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
    var coat = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCoatTextBox");
    var pass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCoatPassTextBox");
    if (coat.value <= 0) {
        pass.value = 0;
        pass.select();
    }
    else if (coat.value >= 1 && pass.value >= 1) {

    }
    else {
        pass.value = 1;
        pass.select();
    }
}
*/
  function lenback()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vLengthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vLengthTextBox").value = frontback;
    }
   }
   function widback()
    {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWidthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWidthTextBox").value = frontback;
    }
    }
   function heiback()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vHeightTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vHeightTextBox").value = frontback;
    }
    }
    function unitlen()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitLenTextBox").value = frontback;
    }
    }
    function unitwid()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitWidTextBox").value = frontback;
    }
    }
    function unitdep()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitDepTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vUnitDepTextBox").value = frontback;
    }
    }
function valcost()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCostTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCostTextBox").value = frontback;
    }
    }
 function valcost2()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCost2TextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vCost2TextBox").value = frontback;
    }
    }
function valfright()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vFreifgtTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vFreifgtTextBox").value = frontback;
    }
    }
 function valfright2()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vFreOutTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vFreOutTextBox").value = frontback;
    }
    }
function valweper()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWeiPerTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vWeiPerTextBox").value = frontback;
    }
    }


    function copybutton_click() {
        var NewWindow = window.open("copy_inks.aspx", "MaterialWindow", "width=60,height=130,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function jobbuttonconfirm() {
        var order = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Ink_vOrderLabel");

        if (parseInt(order.innerHTML) > 0) {
            if (confirm("Recalculate Job Standards for job# " + order.innerHTML)) {
                return true;
            }
            else {
                return false;
            }
        }
        else {
            alert("Order not available on the Estimate");
            return false;
        }

    }


</script>

    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />    
    
    <asp:HiddenField ID="vBundlHiddenField" runat="server" /> 
    <asp:HiddenField ID="vLengthHiddenField" runat="server" /> 
    <asp:HiddenField ID="vCost2HiddenField" runat="server" /> 
    <asp:HiddenField ID="vWidthHiddenField" runat="server" /> 
    <asp:HiddenField ID="vCountHiddenField" runat="server" /> 
    <asp:HiddenField ID="vHeightHiddenField" runat="server" /> 
    <asp:HiddenField ID="vLayerHiddenField" runat="server" /> 
    <asp:HiddenField ID="vStackHiddenField" runat="server" /> 
    <asp:HiddenField ID="vStcodeHiddenField" runat="server" /> 
    <asp:HiddenField ID="HiddenField_unit_cal" runat="server" /> 
    
    <asp:FormView ID="FormView_Ink"  runat="server" DataSourceID="CorrugatedInkDataSource" OnDataBound="FormView_Inks_DataBound" >
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <fieldset align="top" style="  border-color:Black">
            <table class="shade">
            <tr>
            <td colspan="2"><b>Estimate:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vEstNumTextBox" Width="100px" ReadOnly="true" BackColor="Turquoise" BorderColor="white" BorderWidth="1px" BorderStyle="solid" runat="server" Text='<%# Bind("vEstNum") %>'></asp:TextBox>
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
            <fieldset style="height:405px" >
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
            <b>Colors:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vColorTextBox" Width="40px" AutoPostBack="true" OnTextChanged="colortextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);"  runat="server" MaxLength="2"  Text='<%# Bind("vColor") %>'> </asp:TextBox>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vPassesTextBox" Width="40px" AutoPostBack="true" OnTextChanged="passestextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);" MaxLength="2"  runat="server" Text='<%# Bind("vPasses") %>'></asp:TextBox>
            <b>Coating:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vCoatTextBox" Width="40px" AutoPostBack="true" OnTextChanged="coattextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);"  MaxLength="2"  runat="server" Text='<%# Bind("vCoat") %>'> </asp:TextBox>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:TextBox ID="vCoatPassTextBox" Width="40px" AutoPostBack="true" OnTextChanged="coatpasstextchanged" onfocus="getval(this.value);this.select();" onblur="validate(this);" MaxLength="2"  runat="server" Text='<%# Bind("vCoatPass") %>'></asp:TextBox>
            </td></tr>
            <tr><td colspan="4">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:TextBox ID="vDscrTextBox" Width="250px" runat="server" Text='<%# Bind("vDscr") %>'></asp:TextBox>
            </td></tr>
            <tr><td><b>Ps</b></td>
            <td><b>Code</b></td>
            <td>Description</td>
            <td>%</td></tr>
            <tr><td><asp:TextBox ID="vPs1TextBox" Width="40px" MaxLength="2" onfocus="getval(this.value);" onblur="validate(this);"  runat="server" Text='<%# Bind("vPs1") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vCode1TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode1") %>'> </asp:TextBox>
                <%--<a href="#" tabindex="1" onClick="Codelook(); return false" ><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                <a href="#" tabindex="1" onClick="Code2look(1); return false" ><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td><asp:TextBox ID="vDscr1TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr1") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer1TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();"  MaxLength="3" runat="server" Text='<%# Bind("vPer1") %>'></asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs2TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);"  MaxLength="2" runat="server" Text='<%# Bind("vPs2") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vCode2TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode2") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code2look(2); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr2TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr2") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer2TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer2") %>'></asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs3TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs3") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vCode3TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode3") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code3look(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr3TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr3") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer3TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();"  MaxLength="3" Text='<%# Bind("vPer3") %>'> </asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs4TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs4") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode4TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode4") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code4look(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr4TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr4") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer4TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer4") %>'></asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs5TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs5") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode5TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode5") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code5look(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr5TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr5") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer5TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer5") %>'></asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs6TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs6") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode6TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode6") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="Code6look(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr6TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr6") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer6TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer6") %>'> </asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs7TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs7") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode7TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode7") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code7look(); return false" ><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr7TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr7") %>'>  </asp:TextBox></td>
            <td><asp:TextBox ID="vPer7TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer7") %>'>  </asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs8TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs8") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode8TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode8") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code8look(); return false" ><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr8TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr8") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer8TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" MaxLength="3" Text='<%# Bind("vPer8") %>'> </asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs9TextBox" Width="40px" runat="server" onfocus="getval(this.value);" onblur="validate(this);" MaxLength="2" Text='<%# Bind("vPs9") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode9TextBox" Width="100px" runat="server" onkeyup="assignval()" Text='<%# Bind("vCode9") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code9look(); return false" ><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr9TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr9") %>'> </asp:TextBox></td>
            <td><asp:TextBox ID="vPer9TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer9") %>'> </asp:TextBox></td></tr>
            
            <tr><td><asp:TextBox ID="vPs10TextBox" Width="40px" onfocus="getval(this.value);" onblur="validate(this);" runat="server"  MaxLength="2" Text='<%# Bind("vPs10") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vCode10TextBox" Width="100px" onkeyup="assignval()" runat="server" Text='<%# Bind("vCode10") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="Code10look(); return false" ><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><asp:TextBox ID="vDscr10TextBox" runat="server" onkeyup="assignval()" Text='<%# Bind("vDscr10") %>'></asp:TextBox></td>
            <td><asp:TextBox ID="vPer10TextBox" onfocus="getval(this.value);" onblur="validate(this);" Width="40px" onkeyup="assignval();" runat="server"  MaxLength="3" Text='<%# Bind("vPer10") %>'></asp:TextBox></td></tr>            
            </table>
            
            </ContentTemplate>
            </asp:UpdatePanel>
            
            </fieldset>
            </td>
            <td>
            <fieldset style="height:100px" >
            <table>
            <tr><td align="right"  style="padding-right:5px;width:92px"><b>Packing Code:</b></td>
            <td><asp:TextBox ID="vPackCodeTextBox" width="100px" runat="server" Text='<%# Bind("vPackCode") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onClick="adhesiveLook(); return false" ><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-top:5px"><b>Unit length:</b></td>
            <td><asp:TextBox ID="vUnitLenTextBox" width="100px" onkeyup="unitlen()" MaxLength="5" runat="server" Text='<%# Bind("vUnitLen","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vUnitLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Cost/Ea:</b></td>
            <td><asp:TextBox ID="vCostTextBox" width="100px" runat="server" onkeyup="valcost()" MaxLength="5" Text='<%# Bind("vCost","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-top:5px"><b>Unit Width:</b></td>
            <td><asp:TextBox ID="vUnitWidTextBox" width="100px" onkeyup="unitwid()" MaxLength="5" runat="server" Text='<%# Bind("vUnitWid","{0:#0.00}") %>'>  </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vUnitWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Boxes/Code:</b></td>
            <td><asp:TextBox ID="vBoxCodeTextBox" width="100px" onkeyup="counttype()" onblur="wtcode()" runat="server" Text='<%# Bind("vBoxCode") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vBoxCodeTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-top:5px"><b>Unit Depth:</b></td>
            <td><asp:TextBox ID="vUnitDepTextBox" width="100px" onkeyup="unitdep()" MaxLength="5" runat="server" Text='<%# Bind("vUnitDep","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vUnitDepTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Bundles/Pallet:</b></td>
            <td><asp:TextBox ID="vBundlTextBox" width="100px" runat="server" onkeyup="counttype()"  Text='<%# Bind("vBundl") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vBundlTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-top:5px"><b>Wt/Pack Code:</b></td>
            <td><asp:TextBox ID="vWtPackTextBox" width="100px" onblur="wtpack()" runat="server" Text='<%# Bind("vWtPack","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vWtPackTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr></table></fieldset>
            <fieldset style="height:150px">
            <table>
            <tr><td align="right" style="padding-right:5px;width:92px"><b>Pallet#:</b></td>
            <td><asp:TextBox ID="vUnitTextBox" width="100px" runat="server" Text='<%# Bind("vUnit") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="unitlook(); return false" ><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td style="width:81px" align="right" style="padding-top:5px"><b>Length:</b></td>
            <td><asp:TextBox ID="vLengthTextBox" width="100px" onkeyup="lenback()" MaxLength="6" runat="server" Text='<%# Bind("vLength","{0:##0.00}") %>'>  </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vLengthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Cost/ea:</b></td>
            <td><asp:TextBox ID="vCost2TextBox" width="100px" runat="server" onkeyup="valcost2()" MaxLength="5" Text='<%# Bind("vCost2","{0:#0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vCost2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-top:5px"><b>Width:</b></td>
            <td><asp:TextBox ID="vWidthTextBox" width="100px" onkeyup="widback()" MaxLength="6" runat="server" Text='<%# Bind("vWidth","{0:##0.00}") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Count:</b></td>
            <td><asp:TextBox ID="vCountTextBox" onblur="counttype()" width="100px" runat="server"  Text='<%# Bind("vCount") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vCountTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-top:5px"><b>Height:</b></td>
            <td><asp:TextBox ID="vHeightTextBox" width="100px" onkeyup="heiback()" MaxLength="6" runat="server" Text='<%# Bind("vHeight","{0:##0.00}") %>'></asp:TextBox>
            
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vUnitLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td></tr>
            <tr><td align="right" style="padding-top:5px"><b># of Layers:</b></td>
            <td><asp:TextBox ID="vLayerTextBox" width="100px" onkeyup="Palet()" onblur="Palet()" runat="server" Text='<%# Bind("vLayer") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vLayerTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>
            <td colspan="2" rowspan="3" align="center">
            <asp:Label Visible="false" ID="labelImagpath" runat="server" Text='<%# Bind("vImages") %>' ></asp:Label>
            <asp:Image ID="ImageMap"  Height="60px" Width="100px"  runat="server">
                </asp:Image>
            </td>            </tr>
            <tr><td align="right" style="padding-top:5px"><b># of Stacks:</b></td>
            <td><asp:TextBox ID="vStackTextBox" width="100px" onkeyup="Palet()" onblur="Palet()" runat="server" Text='<%# Bind("vStack") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vStackTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>           
            <tr><td align="right" style="padding-top:5px"><b>Stack Code:</b></td>
            <td><asp:TextBox ID="vStCodeTextBox" width="100px" runat="server" Text='<%# Bind("vStCode") %>'> </asp:TextBox>
            <%--<a href="#" onClick="carrierlook(); return false" ><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%></td></tr>
             </table></fieldset>
            <fieldset style="height:135px">
            <table>
            <tr><td align="right" style="padding-top:5px"><b>Freight Charge:</b></td>
            <td colspan="3">
            <asp:RadioButtonList ID="RadioButtonList1"   RepeatLayout="Flow" CellSpacing="1" RepeatColumns="5" SelectedValue='<%# Bind("vFrCharge") %>' datavaluefield='<%# Bind("vFrCharge") %>' runat="server">
                <asp:ListItem Value="P" Text="Prepaid"></asp:ListItem>
                <asp:ListItem Value="C" Text="Collect"></asp:ListItem>
                <asp:ListItem Value="B" Text="Bill"></asp:ListItem>
                <asp:ListItem Value="T" Text="Third Party"></asp:ListItem>               
                </asp:RadioButtonList>
            </td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Weight per M:</b></td>
            <td><asp:TextBox ID="vWeiPerTextBox" width="100px" onkeyup="valweper()" MaxLength="6" runat="server" Text='<%# Bind("vWeiPer","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vWeiPerTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td>    </tr>
            <tr><td align="right" style="padding-top:5px"><b>Carrier:</b></td>
            <td><asp:TextBox ID="vCarrierTextBox" width="100px" runat="server" Text='<%# Bind("vCarrier") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="carrierlook(); return false" ><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td colspan="2"><asp:TextBox ID="vCarrDscrTextBox" width="160px" runat="server" Text='<%# Bind("vCarrDscr") %>'> </asp:TextBox></td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Delivery Zone:</b></td>
            <td><asp:TextBox ID="vDelZonTextBox" width="100px" runat="server" Text='<%# Bind("vDelZon") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="zonelook(); return false" ><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td></td><td></td></tr>
            <tr><td align="right" style="padding-top:5px"><b>Freight Out/CWT:</b></td>
            <td><asp:TextBox ID="vFreifgtTextBox" width="100px" onkeyup="valfright()" MaxLength="6" runat="server" Text='<%# Bind("vFreifgt","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vFreifgtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-top:5px"><b>Freight Out/M:</b></td>
            <td><asp:TextBox ID="vFreOutTextBox" onblur="setfocus()" width="100px" onkeyup="valfright2()" MaxLength="6" runat="server" Text='<%# Bind("vFreOut","{0:##0.00}") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vFreOutTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
            </td></tr>
            
            </table>
            </fieldset>
            </td></tr>
                        
            <tr><td>
            <asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClientClick="pccal()" CausesValidation="True" OnClick="Save_Click"
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
            <td style="display:none"><asp:Label ID="vOrderLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("Order") %>'></asp:Label></td>
            </tr>
            <tr><td  nowrap="noWrap" valign="top">
            <fieldset align="top" style="height:300px" >
            <table >
            <tr><td nowrap colspan="4" >
            <b>Color:</b>&nbsp;&nbsp;
            <asp:Label ID="vColorLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vColor") %>'></asp:Label>
            <b>Passes:</b>&nbsp;&nbsp;
            <asp:Label ID="vPassesLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vPasses") %>'></asp:Label>
            <b>Coating:</b>&nbsp;&nbsp;
            <asp:Label ID="vCoatLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vCoat") %>'></asp:Label>
            <b>Passes</b>&nbsp;&nbsp;
            <asp:Label ID="vCoatPassLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vCoatPass") %>'></asp:Label>
            </td>  </tr>
            <tr><td colspan="4">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Label ID="vDscrLabel" Width="260px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr") %>'></asp:Label></td></tr>
            <tr><td><b>Ps</b></td>
            <td><b>Code</b></td>
            <td><b>Description</b></td>
            <td><b>%</b></td></tr>
            <tr><td><asp:Label ID="vPs1Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs1") %>'></asp:Label></td>
            <td><asp:Label ID="vCode1Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode1") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr1Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr1") %>'></asp:Label></td>
            <td><asp:Label ID="vPer1Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer1") %>'></asp:Label></td></tr>            
            <tr><td><asp:Label ID="vPs2Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs2") %>'></asp:Label></td>
            <td><asp:Label ID="vCode2Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode2") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr2Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr2") %>'></asp:Label></td>
            <td><asp:Label ID="vPer2Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer2") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs3Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs3") %>'></asp:Label></td>
            <td><asp:Label ID="vCode3Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode3") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr3Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr3") %>'></asp:Label></td>
            <td><asp:Label ID="vPer3Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer3") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs4Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs4") %>'></asp:Label></td>
            <td><asp:Label ID="vCode4Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode4") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr4Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr4") %>'></asp:Label></td>
            <td><asp:Label ID="vPer4Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer4") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs5Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs5") %>'></asp:Label></td>
            <td><asp:Label ID="vCode5Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode5") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr5Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr5") %>'></asp:Label></td>
            <td><asp:Label ID="vPer5Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer5") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs6Label" Width="40px" BackColor="turquoise"   runat="server" Text='<%# Bind("vPs6") %>'></asp:Label></td>
            <td><asp:Label ID="vCode6Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode6") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr6Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr6") %>'></asp:Label></td>
            <td><asp:Label ID="vPer6Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer6") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs7Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs7") %>'></asp:Label></td>
            <td><asp:Label ID="vCode7Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode7") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr7Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr7") %>'></asp:Label></td>
            <td><asp:Label ID="vPer7Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer7") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs8Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs8") %>'></asp:Label></td>
            <td><asp:Label ID="vCode8Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode8") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr8Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr8") %>'></asp:Label></td>
            <td><asp:Label ID="vPer8Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer8") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs9Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs9") %>'></asp:Label></td>
            <td><asp:Label ID="vCode9Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode9") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr9Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr9") %>'></asp:Label></td>
            <td><asp:Label ID="vPer9Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer9") %>'></asp:Label></td></tr>
            <tr><td><asp:Label ID="vPs10Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPs10") %>'></asp:Label></td>
            <td><asp:Label ID="vCode10Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCode10") %>'></asp:Label></td>
            <td><asp:Label ID="vDscr10Label" Width="150px" BackColor="turquoise" runat="server" Text='<%# Bind("vDscr10") %>'></asp:Label></td>
            <td><asp:Label ID="vPer10Label" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("vPer10") %>'></asp:Label></td></tr>
            </table>
            </fieldset>
            </td>
            <td  >
            
            <fieldset style="height:70px">
            <table >
            <tr><td align="right" style="padding-right:5px;width:92px"><b>Packing Code:</b></td>
            <td><asp:Label ID="vPackCodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vPackCode") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Unit Length:</b></td>
            <td><asp:Label ID="vUnitLenLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitLen","{0:#0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Cost/Ea:</b></td>
            <td><asp:Label ID="vCostLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCost","{0:#0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Unit Width:</b></td>
            <td><asp:Label ID="vUnitWidLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitWid","{0:#0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Boxes/Code:</b></td>
            <td><asp:Label ID="vBoxCodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vBoxCode") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Unit Depth:</b></td>
            <td><asp:Label ID="vUnitDepLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnitDep","{0:#0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Bundles/Pallet:</b></td>
            <td><asp:Label ID="vBundlLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vBundl") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Wt/Pack Code:</b></td>
            <td><asp:Label ID="vWtPackLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vWtPack","{0:##0.00}") %>'></asp:Label></td></tr>
            </table></fieldset>
            <fieldset style="height:120px">
            <table>
            <tr><td  align="right" style="padding-right:5px;width:92px"><b>Pallet#:</b></td>
            <td><asp:Label ID="vUnitLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vUnit") %>'></asp:Label></td>
            <td style="width:83px" align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:Label ID="vLengthLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vLength","{0:##0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Cost/ea:</b></td>
            <td><asp:Label ID="vCost2Label" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCost2","{0:#0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:Label ID="vWidthLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vWidth","{0:##0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"> <b>Count:</b></td>
            <td><asp:Label ID="vCountLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vCount") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Height:</b></td>
            <td><asp:Label ID="vHeightLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vHeight","{0:##0.00}") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b># of Layers:</b></td>
            <td><asp:Label ID="vLayerLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vLayer") %>'></asp:Label></td>
            <td colspan="2" rowspan="3" align="center">
               <asp:Label Visible="false" ID="labelImagpath" runat="server" Text='<%# Bind("vImages") %>' ></asp:Label>
                <asp:Image ID="ImageMap"  Height="60px" Width="100px"  runat="server">
                </asp:Image>
                
            </td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b># of Stacks:</b></td>
            <td><asp:Label ID="vStackLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vStack") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Stack Code:</b></td>
            <td><asp:Label ID="vStCodeLabel" Width="100px" BackColor="turquoise" runat="server" Text='<%# Bind("vStCode") %>'></asp:Label></td></tr>
            </table></fieldset>
            <fieldset style="height:95px">
            <table>
            <tr><td align="right" style="padding-right:5px"><b>freigth Charge:</b></td>
            <td colspan="2">
                <asp:RadioButtonList ID="RadioButtonList1" Enabled="false"  RepeatLayout="Flow" CellSpacing="1" RepeatColumns="5" SelectedValue='<%# Bind("vFrCharge") %>' datavaluefield='<%# Bind("vFrCharge") %>' runat="server">
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
            </table></fieldset>
            
            </td></tr>
                       
            <tr><td >
            <asp:Button ID="UpdateInkButton" runat="server" CssClass="button" OnClick="updateinkbutton_Click" CausesValidation="True" CommandName="edit"
                Text="Update Ink">                
            </asp:Button>
            
            <asp:Button ID="ResetInkButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure to reset all ink codes?')" OnClick="resetinkbutton_Click" CausesValidation="True" 
                Text="Reset Ink">                
            </asp:Button>
            
            <asp:Button ID="UnitCalcButton" runat="server" CssClass="button" OnClick="unitcalcbutton_Click" CausesValidation="True" CommandName="edit"
                Text="Unit Calc">                
            </asp:Button>
            
            <%--<input type="button" id="CopyButton" name="CopyButton" class="button" value="Copy" onclick="copybutton_click()"> --%>               
                
         
            <asp:Button ID="OverWriteCutton" runat="server" CssClass="button" OnClick="overwrite_Click" CausesValidation="True" CommandName="edit"
                Text="Override Unit">                
            </asp:Button>
            <asp:Button ID="jobButton" runat="server" CausesValidation="false" CssClass="button" OnClick="Job_Button_Click" OnClientClick=" return jobbuttonconfirm()"  Text="Job Stds" >  </asp:Button>
            </td></tr>
            </table>
            </fieldset>
        </ItemTemplate>
        
    </asp:FormView>
    <asp:ObjectDataSource ID="CorrugatedInkDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="CorrInks" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_corrugated_est" Type="String" />
            <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmFormno" Type="Int32" />
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
            <asp:Parameter Name="prmPackCode" Type="String" />
            <asp:Parameter Name="prmUnitLen" Type="Decimal" />
            <asp:Parameter Name="prmCostEa" Type="Decimal" />
            <asp:Parameter Name="prmUnitWid" Type="Decimal" />
            <asp:Parameter Name="prmBoxCode" Type="Int32" />
            <asp:Parameter Name="prmUnitDep" Type="Decimal" />
            <asp:Parameter Name="prmBundl" Type="Int32" />
            <asp:Parameter Name="prmWTPack" Type="Decimal" />
            <asp:Parameter Name="prmUnit" Type="String" />
            <asp:Parameter Name="prmCost2" Type="Decimal" />
            <asp:Parameter Name="prmCount" Type="Int32" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmHeight" Type="Decimal" />
            <asp:Parameter Name="prmLayer" Type="Int32" />
            <asp:Parameter Name="prmStack" Type="Int32" />
            <asp:Parameter Name="prmStCode" Type="String" />
            <asp:Parameter Name="prmFrCharge" Type="String" />
            <asp:Parameter Name="prmWeightPer" Type="Decimal" />
            <asp:Parameter Name="prmCarrier" Type="String" />
            <asp:Parameter Name="prmCarrDscr" Type="String" />
            <asp:Parameter Name="prmDelZon" Type="String" />
            <asp:Parameter Name="prmFreight" Type="Decimal" />
            <asp:Parameter Name="prmFreight2" Type="Decimal" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_corrugated_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>



</asp:Content>

