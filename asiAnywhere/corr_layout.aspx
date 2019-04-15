<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master" AutoEventWireup="true" Inherits="corr_layout" Title="Corrugated Layout" Codebehind="corr_layout.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<asp:ScriptManager ID="ScriptManager1" runat="server" EnablePageMethods="true">
</asp:ScriptManager> 
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
              
<script>

window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox"))
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox").disabled!=true)
        {
            var machine=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox");
            machine.focus();
        }
        else
        {
            var frontback = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFrontBackTextBox");
            frontback.focus();
        }
    }
}

    function Boardlook1(){ 
    var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_styleLabel").innerHTML;
    var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
  var NewWindow = window.open("corboard_lookup.aspx?est="+est1+"&style="+ style1+"","BoardWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function open_bom()
{
    var NewWindow = window.open("corr_bom.aspx","CorrBomWindow","width=700,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorBoardLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
    ReturnObj1 = ReturnObj1.replace(":", "\"");
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vBoardNameTextBox.value = ReturnObj2;  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vSideSideTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vFrontBackTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox.value = ReturnObj4;
    var real=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vRealLabel");
    var test=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFluteLabel");
    var flute=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vTestLabel");  
  real.innerText=ReturnObj5;
  test.innerText=ReturnObj6;
  flute.innerText = ReturnObj7;
  //var board = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vBoardNameTextBox");
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vBoardTextBox").onchange();
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vBoardTextBox").focus();

  
}

function Adderlook1()
{
    var NewWindow = window.open("adder_lookup.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup1(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders1TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders7TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders7TextBox.focus();
}
function Adderlook2()
{
    var NewWindow = window.open("adder_lookup2.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup2(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders2TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders8TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders8TextBox.focus();
}

function Adderlook3()
{
    var NewWindow = window.open("adder_lookup3.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup3(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders9TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders9TextBox.focus();
}
function Adderlook4()
{
    var NewWindow = window.open("adder_lookup4.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup4(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders4TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders10TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders10TextBox.focus();
}
function Adderlook5()
{
    var NewWindow = window.open("adder_lookup5.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup5(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders5TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders11TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders11TextBox.focus();
}
function Adderlook6()
{
    var NewWindow = window.open("adder_lookup6.aspx","Adderlook1Window","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdderLookup6(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders6TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders12TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vAdders12TextBox.focus();
}

function Leaflook1(){ 
    var typelook = "F,W";
     var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
    var NewWindow = window.open("leaf_lookup.aspx?look1="+typelook+"&leaftype="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup1(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel1TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc1TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS1TextBox").innerText = "1";
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS1TextBox").focus();
}

function Leaflook2(){
    var typelook = "F,W"; 
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
    
    var NewWindow = window.open("leaf_lookup2.aspx?look2="+typelook+"&leaf2type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup2(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel2TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc2TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS2TextBox").innerText = "1";
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS2TextBox").focus();
}

function Leaflook3(){ 
     var typelook = "F,W"; 
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
    
    var NewWindow = window.open("leaf_lookup3.aspx?look3="+typelook+"&leaf3type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup3(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel3TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc3TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS3TextBox").innerText = "1";
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS3TextBox").focus();
}

function Leaflook4(){ 
    var typelook = "F,W"; 
    var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
    
    var NewWindow = window.open("leaf_lookup4.aspx?look4="+typelook+"&leaf4type="+est+"","LeafLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function LeafLookup4(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel4TextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc4TextBox.value = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS4TextBox").innerText = "1";
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafS4TextBox").focus();
}
function Machinelook() {
    var xgrain1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vRevCorrDropDown").value;
    //var xgrain2 = xgrain1.SelectedValue;
    var board1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vBoardTextBox").value;
    var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vEstimateLabel").innerHTML;
    var NewWindow = window.open("machine_lookup.aspx?est="+ est1 +"&board="+ board1+"&xgrain="+ xgrain1 +"","MachineLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function MachineLookup(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox.value = ReturnObj1;
  var macdesc=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachDscrTextBox");
  macdesc.innerText = ReturnObj2;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox").onchange();
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vMachineTextBox").focus();
}
function frontback()
{
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFrontBackTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFrontBackTextBox").value = frontback;
    }
}
function sideside()
{
    var side=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vSideSideTextBox").value;
    if (side.indexOf(".") != -1) {
        return;
    }
    else if (side.length > 2 && side.length < 4) {
        side = side + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vSideSideTextBox").value = side;
    }
}
function costmsf()
{
   var cost=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vCostMsfTextBox").value;
   if (cost.indexOf(".") != -1) {
       return;
   }
   else if (cost.length > 4 && cost.length < 6) {
       cost = cost + ".";
       document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vCostMsfTextBox").value = cost;
   }
}
function freightcwt()
{
    var freight=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFreightCwtTextBox").value;
    if (freight.indexOf(".") != -1) {
        return;
    }
    else if (freight.length > 2 && freight.length < 4) {
        freight = freight + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vFreightCwtTextBox").value = freight;
    }
}
function grosswid()
{
    var wid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").value;
    if (wid.indexOf(".") != -1) {
        return;
    }
    else if (wid.length > 3 && wid.length < 5) {
        wid = wid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").value = wid;
    }
}
function grosslen()
{
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox").value;
    if (len.indexOf(".") != -1) {
        return;
    }
    else if (len.length > 3 && len.length < 5) {
        len = len + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox").value = len;
    }
}
function netwid()
{
    var nwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox").value;
    if (nwid.indexOf(".") != -1) {
        return;
    }
    else if (nwid.length > 3 && nwid.length < 5) {
        nwid = nwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox").value = nwid;
    }
}
function netlen()
{
    var nlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetLenTextBox").value;
    if (nlen.indexOf(".") != -1) {
        return;
    }
    else if (nlen.length > 3 && nlen.length < 5) {
        nlen = nlen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetLenTextBox").value = nlen;
    }
}
function diewid()
{
    var dwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeWidTextBox").value;
    if (dwid.indexOf(".") != -1) {
        return;
    }
    else if (dwid.length > 3 && dwid.length < 5) {
        dwid = dwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeWidTextBox").value = dwid;
    }
}
function dielen()
{
    var dlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeLenTextBox").value;
    if (dlen.indexOf(".") != -1) {
        return;
    }
    else if (dlen.length > 3 && dlen.length < 5) {
        dlen = dlen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeLenTextBox").value = dlen;
    }
}

function leafwid1()
{
    var lwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid1TextBox").value;
    if (lwid.indexOf(".") != -1) {
        return;
    }
    else if (lwid.length > 2 && lwid.length < 4) {
        lwid = lwid + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid1TextBox").value = lwid;
    }
}
function leafwid2()
{
    var lwid2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid2TextBox").value;
    if (lwid2.indexOf(".") != -1) {
        return;
    }
    else if (lwid2.length > 2 && lwid2.length < 4) {
        lwid2 = lwid2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid2TextBox").value = lwid2;
    }
}
function leafwid3()
{
    var lwid3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid3TextBox").value;
    if (lwid3.indexOf(".") != -1) {
        return;
    }
    else if (lwid3.length > 2 && lwid3.length < 4) {
        lwid3 = lwid3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid3TextBox").value = lwid3;
    }
}
function leafwid4()
{
    var lwid4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid4TextBox").value;
    if (lwid4.indexOf(".") != -1) {
        return;
    }
    else if (lwid4.length > 2 && lwid4.length < 4) {
        lwid4 = lwid4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafWid4TextBox").value = lwid4;
    }
}
function leaflen1()
{
    var llen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen1TextBox").value;
    if (llen.indexOf(".") != -1) {
        return;
    }
    else if (llen.length > 2 && llen.length < 4) {
        llen = llen + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen1TextBox").value = llen;
    }
}
function leaflen2()
{
    var llen2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen2TextBox").value;
    if (llen2.indexOf(".") != -1) {
        return;
    }
    else if (llen2.length > 2 && llen2.length < 4) {
        llen2 = llen2 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen2TextBox").value = llen2;
    }
}
function leaflen3()
{
    var llen3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen3TextBox").value;
    if (llen3.indexOf(".") != -1) {
        return;
    }
    else if (llen3.length > 2 && llen3.length < 4) {
        llen3 = llen3 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen3TextBox").value = llen3;
    }
}
function leaflen4()
{
    var llen4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen4TextBox").value;
    if (llen4.indexOf(".") != -1) {
        return;
    }
    else if (llen4.length > 2 && llen4.length < 4) {
        llen4 = llen4 + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vLeafLen4TextBox").value = llen4;
    }
}
function grosswidtest()
{
 var wid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").value;
    var val= wid.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2 = val + 2;
   
    var sum = String(wid.charAt(val1)) + String(wid.charAt(val2));    
    if(parseInt(sum) > 15)
    {
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").focus();
        return;
    }   
}
function grosslentest()
{
 var glen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox").value;
    var val= glen.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;

    var sum = String(glen.charAt(val1)) + String(glen.charAt(val2));
    if (parseInt(sum) > 15) {
        
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetLenTextBox").focus();
        return;
    }   
}
function netwidtest()
{
    var nwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox").value;
    var val= nwid.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;

    var sum = String(nwid.charAt(val1)) + String(nwid.charAt(val2));
    if (parseInt(sum) > 15) {
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox").focus();
        return;
    }  
}
function netlentest()
{
    var nlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetLenTextBox").value;
    var val= nlen.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;

    var sum = String(nlen.charAt(val1)) + String(nlen.charAt(val2));
    if (parseInt(sum) > 15) {
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetLenTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetLenTextBox").focus();
        return;
    }  
}
function diewidtest()
{
    var dwid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeWidTextBox").value;
    var val= dwid.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;

    var sum = String(dwid.charAt(val1)) + String(dwid.charAt(val2));
    if (parseInt(sum) > 15) {
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeWidTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeWidTextBox").focus();
        return;
    }  
}
function dielentest()
{
    var dlen=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeLenTextBox").value;
    var val= dlen.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;

    var sum = String(dlen.charAt(val1)) + String(dlen.charAt(val2));
    if (parseInt(sum) > 15) {
        alert("Can not have more than .15 as decimal field");
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeLenTextBox").value="";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vDieSizeLenTextBox").focus();
        return;
    }
}

function adder_out(add) {
    var adder1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders1TextBox").value;
    var adder2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders2TextBox").value;
    var adder3 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders3TextBox").value;
    var adder4 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders4TextBox").value;
    var adder5 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders5TextBox").value;
    var adder6 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders6TextBox").value;
    var obj1 = "";
    if (adder1 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders7TextBox").value = "";
    }
    if (adder2 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders8TextBox").value = "";
    }
    if (adder3 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders9TextBox").value = "";
    }
    if (adder4 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders10TextBox").value = "";
    }
    if (adder5 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders11TextBox").value = "";
    }
    if (adder6 == "") {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vAdders12TextBox").value = "";
    }
    
    if (adder1 != "" && (adder1 == adder2 || adder1 == adder3 || adder1 == adder4 || adder1 == adder5 || adder1 == adder6)) {
        alert("Adder Already on this form. Please try other1");
               
    }
    else if (adder2 != "" && (adder2 == adder3 || adder2 == adder4 || adder2 == adder5 || adder2 == adder6)) {
        alert("Adder Already on this form. Please try other2");
             
    }
   else  if (adder3 != "" && (adder3 == adder4 || adder3 == adder5 || adder3 == adder6)) {
        alert("Adder Already on this form. Please try other3");       
       
    }
   else  if (adder4 != "" && (adder4 == adder5 || adder4 == adder6)) {
        alert("Adder Already on this form. Please try other4");        
        
    }
    else if (adder5 != "" && (adder5 == adder6)) {
        alert("Adder Already on this form. Please try other5");              
    }
    
}

function leafblur1()
{
    var leaf1=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel1TextBox").value;       
       
    if(leaf1=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc1TextBox").value="";           
        }        
}
function leafblur2()
{
    var leaf2=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel2TextBox").value;       
       
    if(leaf2=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc2TextBox").value="";           
        }        
}
function leafblur3()
{
    var leaf3=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel3TextBox").value;       
       
    if(leaf3=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc3TextBox").value="";           
        }        
}
function leafblur4()
{
    var leaf4=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxLabel4TextBox").value;       
       
    if(leaf4=="") 
        {            
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vWaxDesc4TextBox").value="";           
        }       
}



    function setValue() 
    {
        var onlenval = 1;
        var onwidval = 1;
        var dieinval = 1;
        var numupval = 1;

        if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vOnLenTextBox.value) > 0)
            onlenval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vOnLenTextBox.value);

        if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vOnWidTextBox.value) > 0)
            onwidval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vOnWidTextBox.value);

        if (parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vDieInchesTextBox.value) > 0)
            dieinval = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vDieInchesTextBox.value);

        if (parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOnTotalUpLabel").innerHTML) > 0)
            numupval = parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOnTotalUpLabel").innerHTML);

        
        document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vDieInchesTextBox.value = dieinval / numupval;
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOnTotalUpLabel").innerHTML = onlenval * onwidval;
        document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vDieInchesTextBox.value = parseInt(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView_Layout$vDieInchesTextBox.value) * parseInt(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOnTotalUpLabel").innerHTML);

    }

    function sheet_cal() {
        var sheetcal = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_sheetLabel").innerHTML;
        if (sheetcal == "sheet") {
            var NewWindow = window.open("sheet_corr_parm.aspx", "SheetLookupWindow", "width=400,height=350,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
    }

    function onlenfocus() {
        var len = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOnLenTextBox");
        var grosheet = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox");
        if (document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vGrosShetWidTextBox").disabled != true) {
            grosheet.focus();
        }
        else {
            len.focus();
        }

    }

    function jobbuttonconfirm() {
        var order = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Layout_vOrderLabel");

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
    <asp:FormView ID="FormView_Layout" runat="server" DataSourceID="LayoutDataSource" OnDataBound="FormView_Layout_DataBound">
        <EditItemTemplate>
        <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
        <fieldset class="shade">
            <legend>Reference Information:</legend>
                <table>
                    <tr>
                        <td nowrap><b>Estimate#:</b></td>
                        <td nowrap><b><asp:Label ID="vEstimateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vEstimate") %>'></asp:Label></b></td>
                        <td nowrap><b>Est Date:</b></td>
                        <td nowrap><b><asp:Label ID="vEstDateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
                        <td nowrap><b>Form:</b></td>
                        <td nowrap><b>
                            <asp:Label ID="vFormLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vForm") %>'></asp:Label>
                            of &nbsp; &nbsp; &nbsp;
                            <asp:Label ID="vFormQtyLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
                        </b></td>
                        <td nowrap><b>Cust Part:</b></td>
                        <td nowrap><b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="100px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b></td>
                        
                    </tr>
                </table>                
        </fieldset>
    <asp:UpdatePanel ID="LengUpdate" runat="server">
          <ContentTemplate>
           <div >
                <asp:UpdateProgress ID="UpdateProgress1" runat="server" 
                    AssociatedUpdatePanelID="LengUpdate"
                    DisplayAfter="100" DynamicLayout="true">                    
                    <ProgressTemplate>                       
                        <asp:Label ID="lblProgress" runat="server" ></asp:Label>               
                    Please wait ...             
                    </ProgressTemplate>                    
                </asp:UpdateProgress>
                </div>
        <fieldset class="shade">
                                
            <table>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Machine:</b></td>
                    <td nowrap><b><asp:TextBox  ID="vMachineTextBox"  AutoPostBack="true" onblur="sheet_cal()" OnTextChanged="machine_textchange" runat="server" Text='<%# Bind("vMachine") %>'></asp:TextBox>
                    <a href="#" tabindex="1" onclick="Machinelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                        &nbsp;
                        <asp:Label ID="vMachDscrTextBox" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="140px" runat="server" Text='<%# Bind("vMachDscr") %>'></asp:Label>
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                    <td nowrap><b></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Side-Side:</b></td>
                    <td nowrap><b><asp:TextBox ID="vFrontBackTextBox" onkeyup="frontback()" MaxLength="6" Width="40px" runat="server" Text='<%# Bind("vFrontBack","{0:##0.00}") %>'></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator27" runat="server" ControlToValidate="vFrontBackTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Front-Back:</b></td>
                    <td nowrap><b><asp:TextBox ID="vSideSideTextBox" onkeyup="sideside()" MaxLength="6" Width="40px" runat="server" Text='<%# Bind("vSideSide","{0:##0.00}") %>'></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator28" runat="server" ControlToValidate="vSideSideTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Rev Corr:</b></td>
                    <td nowrap><b><asp:DropDownList ID="vRevCorrDropDown" Width="40px" AutoPostBack="true" OnSelectedIndexChanged="machine_textchange" runat="server" SelectedValue='<%# Bind("vRevCorr") %>'>
                                        <asp:ListItem Value=""></asp:ListItem>
                                        <asp:ListItem Value="N">N</asp:ListItem>
                                        <asp:ListItem Value="B">B</asp:ListItem>
                                        <asp:ListItem Value="S">S</asp:ListItem>
                                </asp:DropDownList></b></td>
                    
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Board:</b></td>
                    <td nowrap><b><asp:TextBox ID="vBoardTextBox" OnTextChanged="Board_textchange_click" AutoPostBack="true" runat="server" Text='<%# Bind("vBoard") %>'></asp:TextBox>
                    <a href="#" tabindex="1" onclick="Boardlook1(); return false"><asp:Image ID="img_board" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    &nbsp;
                    <asp:TextBox ID="vBoardNameTextBox" Width="140px" ReadOnly="true" runat="server" Text='<%# Bind("vBoardName") %>'></asp:TextBox>
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b></b></td>
                    <td nowrap><b></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Real:</b></td>
                    <td nowrap><b><asp:Label ID="vRealLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vReal") %>'></asp:Label></b></td>
                    
                </tr>
                <tr>
                    <td nowrap align="right" style="padding-right:5px;"><b>Flute:</b></td>
                    <td nowrap><b><asp:Label ID="vFluteLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vFlute") %>'></asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Test:</b></td>
                    <td nowrap><b><asp:Label ID="vTestLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vTest") %>'></asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Cost:</b></td>
                    <td nowrap><b>
                    <asp:TextBox ID="vCostUomTextBox" runat="server" Width="40px" Text='<%# Bind("vCostUom") %>'></asp:TextBox>
                    &nbsp;
                    <asp:TextBox ID="vCostMsfTextBox" MaxLength="9" onkeyup="costmsf()" runat="server" Width="40px" Text='<%# Bind("vCostMsf","{0:##0.000}") %>'></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator25" runat="server" ControlToValidate="vCostMsfTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Wt:</b></td>
                    <td nowrap><b><asp:Label ID="vWeightLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vWeight","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Freight:</b></td>
                    <td nowrap><b>
                    <asp:TextBox ID="vFreightUomTextBox" Width="40px" runat="server" Text='<%# Bind("vFreightUom") %>'></asp:TextBox>
                    &nbsp;
                    <asp:TextBox ID="vFreightCwtTextBox" MaxLength="7" onkeyup="freightcwt()" Width="40px" runat="server" Text='<%# Bind("vFreightCwt","{0:##0.000}") %>'></asp:TextBox>
                    <asp:CompareValidator ID="CompareValidator26" runat="server" ControlToValidate="vFreightCwtTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>    
                    </b></td>
                    <td nowrap align="right" style="padding-right:5px;"><b>Nc:</b></td>
                    <td nowrap><b> <%--<asp:TextBox ID="vNcTextBox" Width="40px" runat="server" Text='<%# Bind("vNc") %>'>
                    </asp:TextBox>--%>
                    <asp:DropDownList ID="ddl_nc" Width="40px" runat="server" onblur="onlenfocus()" SelectedValue='<%# Bind("vNc") %>'>
                                        
                                        <asp:ListItem Value="N">N</asp:ListItem>
                                        <asp:ListItem Value="C">C</asp:ListItem>
                                        
                                </asp:DropDownList>
                    </b></td>
                </tr>
            </table>
            <fieldset>
                <table>
                    <tr>                        
                        <td><b></b></td>
                        <td width="8%"><b>Width</b></td>
                        <td width="8%"><b>Length</b></td>
                        <td width="8%"><b></b></td>
                        <td width="8%"><b></b></td>
                        <td width="8%"><b>Width</b></td>
                        <td width="8%"><b>Length</b></td>
                        <td width="8%"><b>Depth</b></td>
                        <td width="8%"><b>Cut</b></td>
                        <td width="8%"><b>Total UP</b></td>
                        <td width="8%"><b>Sq Feet</b></td>
                        <td width="8%"><b>Die Inches</b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Gross Sheet:</b></td>
                        <td nowrap><b><asp:TextBox ID="vGrosShetWidTextBox" Width="40px" onblur="grosswidtest()" onkeyup="grosswid()" MaxLength="7" runat="server" Text='<%# Bind("vGrosShetWid","{0:##0.00}") %>'></asp:TextBox>
                            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vGrosShetWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vGrosShetLenTextBox" Width="40px" onblur="grosslentest();document.getElementById('ctl00_ContentPlaceHolder1_FormView_Layout_vNetShetWidTextBox').focus();" onkeyup="grosslen()" MaxLength="7" runat="server" Text='<%# Bind("vGrosShetLen","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vGrosShetLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td><b><%--<asp:TextBox ID="vGrosShetDepTextBox" Width="40px" runat="server" Text='<%# Bind("vGrosShetDep") %>'></asp:TextBox>--%></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>#Out:</b></td>
                        
                        <td nowrap><b>
                        <asp:TextBox ID="vOutWidTextBox" Width="40px" MaxLength="3" onfocus="this.select()" AutoPostBack="true" OnTextChanged="OutLenchange_Click" runat="server" Text='<%# Bind("vOutWid") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vOutWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>                        
                        </b></td>
                        <td nowrap><b>
                                <asp:TextBox ID="vOutLenTextBox" Width="40px" MaxLength="3" AutoPostBack="true" OnTextChanged="OutLenchange2_Click" runat="server"  Text='<%# Bind("vOutLen") %>'></asp:TextBox>
                        <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vOutLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td><b><%--<asp:TextBox ID="vOutDepTextBox" Width="40px" runat="server" Text='<%# Bind("vOutDep") %>'></asp:TextBox>--%></b></td>
                        <td nowrap><b><asp:TextBox ID="vOutCutTextBox" Width="40px" MaxLength="6" runat="server" Text='<%# Bind("vOutCut") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator17" runat="server" ControlToValidate="vOutCutTextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td><b><asp:Label ID="vOutTotalUpLabel" Width="40px" runat="server" Text='<%# Bind("vOutTotalUp") %>'></asp:Label></b></td>
                        <td><b><asp:Label ID="vOutSqFeetLabel" Width="40px" runat="server" Text='<%# Bind("vOutSqFeet") %>'></asp:Label></b></td>
                        <td nowrap><b><asp:TextBox ID="vDieInchesTextBox" Width="40px" MaxLength="5" runat="server" Text='<%# Bind("vDieInches") %>' onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView_Layout_vAdders1TextBox').focus();"></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vDieInchesTextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Net Sheet:</b></td>
                        <td nowrap><b><asp:TextBox ID="vNetShetWidTextBox" Width="40px" onfocus="this.select()" onblur="netwidtest()" onkeyup="netwid()" MaxLength="7" runat="server" Text='<%# Bind("vNetShetWid","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vNetShetWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vNetShetLenTextBox" Width="40px" onblur="netlentest()" onkeyup="netlen()" MaxLength="7" runat="server" Text='<%# Bind("vNetShetLen","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator20" runat="server" ControlToValidate="vNetShetLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td><b><%--<asp:TextBox ID="vNetShetDepTextBox" Width="40px" runat="server" Text='<%# Bind("vNetShetDep") %>'></asp:TextBox>--%></b></td>
                        
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Die Size:</b></td>
                        <td nowrap><b><asp:TextBox ID="vDieSizeWidTextBox" Width="40px" onblur="diewidtest()" onkeyup="diewid()" MaxLength="7" runat="server" Text='<%# Bind("vDieSizeWid","{0:##0.00}") %>'></asp:TextBox>
                               <asp:CompareValidator ID="CompareValidator21" runat="server" ControlToValidate="vDieSizeWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator> 
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vDieSizeLenTextBox" Width="40px" onblur="dielentest()" onkeyup="dielen()" MaxLength="7" runat="server" Text='<%# Bind("vDieSizeLen","{0:##0.00}") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator22" runat="server" ControlToValidate="vDieSizeLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                        </b></td>
                        <td><b><%--<asp:TextBox ID="vDieSizeDepTextBox" Width="40px" runat="server" Text='<%# Bind("vDieSizeDep") %>'></asp:TextBox>--%></b></td>
                        <td nowrap align="right" style="padding-right:5px;"><b>#On:</b></td>                        
                        <td nowrap><b><asp:TextBox ID="vOnLenTextBox" onfocus="this.select()" Width="40px" MaxLength="3" AutoPostBack="true"  OnTextChanged="OnLen_Text_Change" runat="server" onkeyup="setValue()" Text='<%# Bind("vOnLen") %>'></asp:TextBox>
                                <%--<asp:CompareValidator ID="CompareValidator23" runat="server" ControlToValidate="vOnLenTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>--%>
                        </b></td>
                        <td nowrap><b><asp:TextBox ID="vOnWidTextBox" Width="40px" MaxLength="3" runat="server" AutoPostBack="true" OnTextChanged="OnLen_Text2_Change" onkeyup="setValue()" Text='<%# Bind("vOnWid") %>'  onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView_Layout_vOutWidTextBox').focus()"></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator24" runat="server" ControlToValidate="vOnWidTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                        </b></td>
                        <td><b><%--<asp:TextBox ID="vOnDepTextBox" Width="40px" runat="server" Text='<%# Bind("vOnDep") %>'></asp:TextBox>--%></b></td>
                        <td><b></b></td>
                        <td><b><asp:Label ID="vOnTotalUpLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vOnTotalUp") %>'></asp:Label></b></td>
                        
                        
                    </tr>
                    <tr>
                        <td nowrap align="right" style="padding-right:5px;"><b>Blank:</b></td>
                        <td><b><asp:Label ID="vBlankWidLabel" Width="40px" runat="server" Text='<%# Bind("vBlankWid","{0:##0.00}") %>'></asp:Label></b></td>
                        <td><b><asp:Label ID="vBlankLenLabel" Width="40px" runat="server" Text='<%# Bind("vBlankLen","{0:##0.00}") %>'></asp:Label></b></td>
                        <td><b>&nbsp;<%--<asp:TextBox ID="vBlankDepTextBox" Width="40px" runat="server" Text='<%# Bind("vBlankDep") %>'></asp:TextBox>--%></b></td>
                        <td>&nbsp;</td>
                        <td>&nbsp;</td>
                        <td>&nbsp;</td>
                        <td>&nbsp;</td>
                        <td>&nbsp;</td>
                        <td>&nbsp;</td>
                        <td><b><asp:Label ID="vOnSqFeetLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vOnSqFeet","{0:##0.00000}") %>'></asp:Label></b></td>
                    </tr>
                </table>
            </fieldset>
            <table>
                <tr>
                    <td>
                        <fieldset style="height:190px; width:300px;">
                            <table>
                                <tr>
                                    <td><b>Adders</b></td>
                                    <td><b>Description</b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders1TextBox" onblur="adder_out(this)" onfocus="this.select()" Width="70px" runat="server" Text='<%# Bind("vAdders1") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook1(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders7TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders7") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders2TextBox" onblur="adder_out(this)" Width="70px" runat="server" Text='<%# Bind("vAdders2") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook2(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders8TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders8") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders3TextBox" onblur="adder_out(this)" Width="70px" runat="server" Text='<%# Bind("vAdders3") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook3(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders9TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders9") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders4TextBox" onblur="adder_out(this)" Width="70px" runat="server" Text='<%# Bind("vAdders4") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook4(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders10TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders10") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders5TextBox" onblur="adder_out(this)" Width="70px" runat="server" Text='<%# Bind("vAdders5") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook5(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders11TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders11") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vAdders6TextBox" onblur="adder_out(this)" Width="70px" runat="server" Text='<%# Bind("vAdders6") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Adderlook6(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vAdders12TextBox" Width="160px" runat="server" Text='<%# Bind("vAdders12") %>'></asp:TextBox></b></td>
                                </tr>
                            </table>
                        </fieldset>
                    </td>
                    <td>
                        <fieldset style="height:190px; width:600px;">
                            <table>
                                <tr>
                                    <td><b>Wax/Label</b></td>
                                    <td><b>Description</b></td>
                                    <td align="center" width="40px"><b>S</b></td>
                                    <td><b>B</b></td>
                                    <td><b>Width</b></td>
                                    <td><b>Length</b></td>
                                </tr>
                                <tr>
                                    <td nowrap><b><asp:TextBox ID="vWaxLabel1TextBox" onblur="leafblur1()" Width="70px" runat="server" Text='<%# Bind("vWaxLabel1") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Leaflook1(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vWaxDesc1TextBox" Width="160px" runat="server" Text='<%# Bind("vWaxDesc1") %>'></asp:TextBox></b></td>
                                    <td align="center"><b>
                                    <asp:Label ID="vLeafS1TextBox" Width="40px" runat="server" Text='<%# Bind("vLeafS1") %>'></asp:Label>
                                    </b></td>
                                    <td nowrap><b>
                                    <asp:TextBox ID="vLeafB1TextBox" Width="40px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB1") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vLeafB1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafWid1TextBox" MaxLength="6" onkeyup="leafwid1()" Width="40px" runat="server" Text='<%# Bind("vLeafWid1","{0:##0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vLeafWid1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafLen1TextBox" MaxLength="6" Width="40px" onkeyup="leaflen1()" runat="server" Text='<%# Bind("vLeafLen1","{0:##0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vLeafLen1TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                </tr>
                                 <tr>
                                    <td nowrap><b><asp:TextBox ID="vWaxLabel2TextBox" onblur="leafblur2()" Width="70px" runat="server" Text='<%# Bind("vWaxLabel2") %>'></asp:TextBox>
                                            <a href="#" tabindex="1" onclick="Leaflook2(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vWaxDesc2TextBox" Width="160px" runat="server" Text='<%# Bind("vWaxDesc2") %>'></asp:TextBox></b></td>
                                    <td align="center"><b>
                                    <asp:Label ID="vLeafS2TextBox" Width="40px" runat="server" Text='<%# Bind("vLeafS2") %>'></asp:Label>                                    
                                    </b></td>
                                    <td nowrap><b>
                                    <asp:TextBox ID="vLeafB2TextBox" Width="40px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB2") %>'></asp:TextBox>
                                    <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vLeafB2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafWid2TextBox" MaxLength="6" Width="40px" onkeyup="leafwid2()" runat="server" Text='<%# Bind("vLeafWid2","{0:##0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vLeafWid2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafLen2TextBox" MaxLength="6" Width="40px" onkeyup="leaflen2()" runat="server" Text='<%# Bind("vLeafLen2","{0:##0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vLeafLen2TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                </tr>
                                 <tr>
                                    <td nowrap><b><asp:TextBox ID="vWaxLabel3TextBox" onblur="leafblur3()" Width="70px" runat="server" Text='<%# Bind("vWaxLabel3") %>'> </asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Leaflook3(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vWaxDesc3TextBox" Width="160px" runat="server" Text='<%# Bind("vWaxDesc3") %>'></asp:TextBox></b></td>
                                    <td align="center"><b>
                                    <asp:Label ID="vLeafS3TextBox" Width="40px" runat="server" Text='<%# Bind("vLeafS3") %>'></asp:Label>
                                    </b></td>
                                    <td nowrap><b>
                                    <asp:TextBox ID="vLeafB3TextBox" Width="40px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB3") %>'></asp:TextBox>
                                    <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vLeafB3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafWid3TextBox" MaxLength="6" Width="40px" onkeyup="leafwid3()" runat="server" Text='<%# Bind("vLeafWid3","{0:##0.00}") %>'></asp:TextBox>
                                    <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vLeafWid3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafLen3TextBox" Width="40px" MaxLength="6" onkeyup="leaflen3()" runat="server" Text='<%# Bind("vLeafLen3","{0:##0.00}") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vLeafLen3TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                </tr>
                                 <tr>
                                    <td nowrap><b><asp:TextBox ID="vWaxLabel4TextBox" onblur="leafblur4()" Width="70px" runat="server" Text='<%# Bind("vWaxLabel4") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onclick="Leaflook4(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </b></td>
                                    <td><b><asp:TextBox ID="vWaxDesc4TextBox" Width="160px" runat="server" Text='<%# Bind("vWaxDesc4") %>'> </asp:TextBox></b></td>
                                    <td align="center" width="20px"><b>
                                    <asp:Label ID="vLeafS4TextBox" Width="40px" runat="server" Text='<%# Bind("vLeafS4") %>'></asp:Label>
                                    </b></td>
                                    <td nowrap><b>
                                    <asp:TextBox ID="vLeafB4TextBox" Width="40px" MaxLength="3" runat="server" Text='<%# Bind("vLeafB4") %>'></asp:TextBox>
                                    <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vLeafB4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafWid4TextBox" MaxLength="6" Width="40px" onkeyup="leafwid4()" runat="server" Text='<%# Bind("vLeafWid4","{0:##0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vLeafWid4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                    <td nowrap><b><asp:TextBox ID="vLeafLen4TextBox" onblur="setfocus()" MaxLength="6" Width="40px" onkeyup="leaflen4()" runat="server" Text='<%# Bind("vLeafLen4","{0:##0.00}") %>'></asp:TextBox>
                                            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vLeafLen4TextBox" Display="dynamic" Operator="DataTypeCheck" Type="Double" ErrorMessage="Only Decimal value"></asp:CompareValidator>
                                    </b></td>
                                </tr>
                            </table>
                        </fieldset>
                    </td>
                </tr>
                <tr><td style="display:none">    <asp:Label ID="styleLabel" runat="server" Text='<%# Bind("style") %>'></asp:Label>
                <asp:Label ID="sheetLabel" runat="server" ></asp:Label>
                  </td></tr>
            </table>                                          
            
         </ContentTemplate>                                             
            </asp:UpdatePanel>
            <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM" CausesValidation="True" OnClick="UpdateButton_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" OnClick="btn_update_cancel" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            
            </asp:Panel>
        </fieldset>                 
            
        </EditItemTemplate>
       
        <ItemTemplate>
        <fieldset class="shade">
            <legend>Reference Information:</legend>
            <table>
                <tr>
                    <td nowrap><b>Estimate#:</b></td>
                    <td nowrap>
                        <b><asp:Label ID="vEstimateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstimate") %>'></asp:Label></b>
                        <asp:Label ID="vTypeLabel" BackColor="Turquoise" BorderStyle="solid" Visible="false" BorderColor="white" BorderWidth="1px" Width="20px" runat="server" Text='<%# Bind("vType") %>'></asp:Label>
                    </td>
                    <td nowrap><b>Est Date:</b></td>
                    <td nowrap>
                        <b><asp:Label ID="vEstDateLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></b>
                    </td>
                    <td nowrap><b>Form:</b></td>
                    <td nowrap>
                        <b>
                        <asp:Label ID="vFormLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vForm") %>'></asp:Label>
                        of
                        <asp:Label ID="vFormQtyLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label>
                        </b>
                    </td>
                    <td nowrap><b>Cust Part:</b></td>
                    <td nowrap>
                        <b><asp:Label ID="vCustPartLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></b>
                    </td>
                    <td style="display:none"><asp:Label ID="vOrderLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("Order") %>'></asp:Label></td>
                </tr>
            </table>
        </fieldset>
        <fieldset class="shade">
            <table>
                <tr>
                    <td style="padding-right:5px;" align="right" nowrap><b>Machine:</b></td>
                    <td nowrap><b><asp:Label ID="vMachineLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vMachine") %>'></asp:Label>
                     &nbsp;
                     <asp:Label ID="vMachDscrLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="140px" runat="server" Text='<%# Bind("vMachDscr") %>'></asp:Label></b>
                    </td>
                    <td style="padding-right:5px;" align="right"><b></b></td>
                    <td><b></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Side-Side:</b></td>
                    <td nowrap><b><asp:Label ID="vFrontBackLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vFrontBack","{0:##0.00}") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Front-Back:</b></td>
                    <td nowrap><b><asp:Label ID="vSideSideLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vSideSide","{0:##0.00}") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Rev. Corr:</b></td>
                    <td nowrap><b><asp:Label ID="vRevCorrLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vRevCorr") %>'></asp:Label></b></td>
                </tr> 
                <tr>
                    <td style="padding-right:5px;" align="right" nowrap><b>Board:</b></td>
                    <td nowrap><b><asp:Label ID="vBoardLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vBoard") %>'></asp:Label>
                    &nbsp;
                    <asp:Label ID="vBoardNameLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vBoardName") %>'></asp:Label>
                    </b></td>
                    <td style="padding-right:5px;" align="right"><b></b></td>
                    <td><b></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Real:</b></td>
                    <td nowrap><b><asp:Label ID="vRealLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vReal") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right"><b></b></td>
                    <td><b></b></td>
                    <td style="padding-right:5px;" align="right"><b></b></td>
                    <td><b></b></td>
                </tr> 
                <tr>
                    <td style="padding-right:5px;" align="right" nowrap><b>Flute:</b></td>
                    <td nowrap><b><asp:Label ID="vFluteLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vFlute") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Test:</b></td>
                    <td nowrap><b><asp:Label ID="vTestLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vTest") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Cost:</b></td>
                    <td nowrap><b><asp:Label ID="vCostUomLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vCostUom") %>'></asp:Label>
                       &nbsp;
                       <asp:Label ID="vCostMsfLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vCostMsf","{0:##0.000}") %>'></asp:Label>
                    </b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Wt:</b></td>
                    <td nowrap><b><asp:Label ID="vWeightLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vWeight","{0:##0.00}") %>'></asp:Label></b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Freight:</b></td>
                    <td nowrap><b>
                    <asp:Label ID="vFreightUomLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vFreightUom") %>'></asp:Label>
                        &nbsp;
                    <asp:Label ID="vFreightCwtLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="60px" runat="server" Text='<%# Bind("vFreightCwt","{0:##0.000}") %>'></asp:Label>                     
                    </b></td>
                    <td style="padding-right:5px;" align="right" nowrap><b>Nc:</b></td>
                    <td nowrap><b><asp:Label ID="vNcLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vNc") %>'></asp:Label></b></td>
                </tr>                
                               
            </table>
       
        <fieldset>
            <table>
                <tr>
                    <td><b></b></td>
                    <td nowrap><b>Width</b></td>
                    <td nowrap><b>Length</b></td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td nowrap><b>Width</b></td>
                    <td nowrap><b>Length</b></td>
                    <td nowrap><b>Depth</b></td>
                    <td nowrap><b>Cut</b></td>
                    <td nowrap><b>TotalUp</b></td>
                    <td nowrap><b>Sq Feet</b></td>
                    <td nowrap><b>Die Inches</b></td>
                </tr>
                <tr>
                    <td nowrap><b>Gross Sheet:</b></td>
                    <td nowrap><b><asp:Label ID="vGrosShetWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vGrosShetWid","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vGrosShetLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vGrosShetLen","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><%--<asp:Label ID="vGrosShetDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vGrosShetDep") %>'></asp:Label>--%></b></td> 
                    <td>&nbsp;</td>  
                    <td nowrap><b>#Out:</b></td>                                                     
                    <td nowrap><b><asp:Label ID="vOutWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutWid") %>'></asp:Label></b></td>   
                    <td nowrap><b><asp:Label ID="vOutLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutLen") %>'></asp:Label></b></td>                       
                    <td nowrap><b><%--<asp:Label ID="vOutDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutDep") %>'></asp:Label>--%></b></td> 
                    <td nowrap><b><asp:Label ID="vOutCutLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutCut") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vOutTotalUpLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutTotalUp") %>'></asp:Label></b></td>
                    <td nowrap><b><%--<asp:Label ID="vOutSqFeetLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOutSqFeet") %>'></asp:Label>--%></b></td>
                    <td nowrap><b><asp:Label ID="vDieInchesLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vDieInches") %>'></asp:Label></b></td>                                               
                </tr>
                <tr>
                    <td nowrap><b>Net Sheet:</b></td>
                    <td nowrap><b><asp:Label ID="vNetShetWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vNetShetWid","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vNetShetLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vNetShetLen","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><%--<asp:Label ID="vNetShetDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vNetShetDep") %>'></asp:Label>--%></b></td>
                </tr>
                <tr>
                    <td nowrap><b>Die Size:</b></td>
                    <td nowrap><b><asp:Label ID="vDieSizeWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vDieSizeWid","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vDieSizeLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vDieSizeLen","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><%--<asp:Label ID="vDieSizeDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vDieSizeDep") %>'></asp:Label>--%></b></td>
                    <td>&nbsp;</td>                    
                                       
                    <td nowrap><b>#On:</b></td>
                    <td nowrap><b><asp:Label ID="vOnLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOnLen") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vOnWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOnWid") %>'></asp:Label></b></td>                    
                    <td nowrap><b><%--<asp:Label ID="vOnDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOnDep") %>'></asp:Label>--%></b></td>
                    <td>&nbsp;</td>
                    <td nowrap><b><asp:Label ID="vOnTotalUpLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOnTotalUp") %>'></asp:Label></b></td>
                    <td nowrap><b>&nbsp;</b></td>
                </tr>
                <tr>
                    <td nowrap><b>Blank:</b></td>
                    <td nowrap><b><asp:Label ID="vBlankWidLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vBlankWid","{0:##0.00}") %>'></asp:Label></b></td>
                    <td nowrap><b><asp:Label ID="vBlankLenLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vBlankLen","{0:##0.00}") %>'></asp:Label></b></td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td>&nbsp;</td>
                    <td nowrap><b><%--<asp:Label ID="vBlankDepLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vBlankDep") %>'></asp:Label>--%>
                                    <asp:Label ID="vOnSqFeetLabel" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vOnSqFeet","{0:##0.00000}") %>'></asp:Label>
                    </b></td>
                </tr>
            </table>
        </fieldset>
                
        <table>
            <tr>
                <td>
                    <fieldset style="height:140px; width:350px;">
                        <table>
                            <tr>
                                <td nowrap><b>Adders</b></td>
                                <td nowrap><b>Description</b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders1") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vAdders7Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders7") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders2") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vAdders8Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders8") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders3") %>'></asp:Label></b></td>
                                <td nowrap><b> <asp:Label ID="vAdders9Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders9") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders4") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vAdders10Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders10") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders5Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders5") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vAdders11Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders11") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vAdders6Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vAdders6") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vAdders12Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vAdders12") %>'></asp:Label></b></td>
                            </tr>
                        </table>
                    </fieldset>
                </td>
                <td>
                    <fieldset style="height:140px; width:650px;">
                        <table>
                            <tr>
                                <td nowrap><b>Wax/Label</b></td>
                                <td nowrap><b>Description</b></td>           
                                <td nowrap><b>S</b></td>
                                <td nowrap><b>B</b></td>
                                <td nowrap><b>Width</b></td>
                                <td nowrap><b>Length</b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vWaxLabel1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vWaxLabel1") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vWaxDesc1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vWaxDesc1") %>'></asp:Label></b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafS1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafS1") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafB1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafB1") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b><asp:Label ID="vLeafWid1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafWid1","{0:##0.00}") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vLeafLen1Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafLen1","{0:##0.00}") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vWaxLabel2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vWaxLabel2") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vWaxDesc2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vWaxDesc2") %>'></asp:Label></b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafS2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafS2") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafB2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafB2") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b><asp:Label ID="vLeafWid2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafWid2","{0:##0.00}") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vLeafLen2Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafLen2","{0:##0.00}") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vWaxLabel3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vWaxLabel3") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vWaxDesc3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vWaxDesc3") %>'></asp:Label></b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafS3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafS3") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafB3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafB3") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b><asp:Label ID="vLeafWid3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafWid3","{0:##0.00}") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vLeafLen3Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafLen3","{0:##0.00}") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td nowrap><b><asp:Label ID="vWaxLabel4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vWaxLabel4") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vWaxDesc4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="180px" runat="server" Text='<%# Bind("vWaxDesc4") %>'></asp:Label></b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafS4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafS4") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b>
                                <asp:Label ID="vLeafB4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="40px" runat="server" Text='<%# Bind("vLeafB4") %>'></asp:Label>
                                </b></td>
                                <td nowrap><b><asp:Label ID="vLeafWid4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafWid4","{0:##0.00}") %>'></asp:Label></b></td>
                                <td nowrap><b><asp:Label ID="vLeafLen4Label" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" Width="80px" runat="server" Text='<%# Bind("vLeafLen4","{0:##0.00}") %>'></asp:Label></b></td>
                            </tr>
                        </table>
                    </fieldset> 
                </td>
            </tr>
            <tr>
                <td>
                    <asp:Button ID="btn_override" runat="server" Text="Override" CommandName="Edit" CssClass="buttonM" />
                    <asp:Button ID="btn_auto_calc" runat="server" Text="Auto-Calc" CommandName="Edit" CssClass="buttonM" OnClick="btn_auto_calc_click" />
                    <input type="button" class="buttonM" value="Bom" id="btn_bom" onclick="open_bom()" />
                     <asp:Button ID="btn_sheet_calc" runat="server" Text="Sheet Calc" CommandName="Edit" CssClass="buttonM" OnClick="btn_sheet_calc_click" />
                     <asp:Button ID="jobButton" runat="server" CausesValidation="false" CssClass="button" OnClick="Job_Button_Click" OnClientClick=" return jobbuttonconfirm()"  Text="Job Stds" >  </asp:Button>
                </td>
            </tr>
        </table>  
      </fieldset> 
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="LayoutDataSource" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectCorrugateLayout" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmForm" Type="Int32" />
            <asp:Parameter Name="prmFormQty" Type="Int32" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmMachine" Type="String" />
            <asp:Parameter Name="prmMachDscr" Type="String" />
            <asp:Parameter Name="prmFrontBack" Type="Decimal" />
            <asp:Parameter Name="prmSideSide" Type="Decimal" />
            <asp:Parameter Name="prmRevCorr" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmBoardName" Type="String" />
            <asp:Parameter Name="prmReal" Type="String" />
            <asp:Parameter Name="prmFlute" Type="String" />
            <asp:Parameter Name="prmTest" Type="String" />
            <asp:Parameter Name="prmCostMsf" Type="Decimal" />
            <asp:Parameter Name="prmCostUom" Type="String" />
            <asp:Parameter Name="prmWeightt" Type="Decimal" />
            <asp:Parameter Name="prmFreightMsf" Type="Decimal" />
            <asp:Parameter Name="prmFreightUom" Type="String" />
            <asp:Parameter Name="prmNc" Type="String" />
            <asp:Parameter Name="prmGrosShetWid" Type="Decimal" />
            <asp:Parameter Name="prmGrosShetLen" Type="Decimal" />
            <asp:Parameter Name="prmGrosShetDep" Type="Decimal" />
            <asp:Parameter Name="prmOutWid" Type="Decimal" />
            <asp:Parameter Name="prmOutLen" Type="Decimal" />
            <asp:Parameter Name="prmOutDep" Type="Decimal" />
            <asp:Parameter Name="prmOutCut" Type="Int32" />
            <asp:Parameter Name="prmOutTotalUp" Type="String" />
            <asp:Parameter Name="prmOutSqFeet" Type="String" />
            <asp:Parameter Name="prmDieInches" Type="Decimal" />
            <asp:Parameter Name="prmNetShetWid" Type="Decimal" />
            <asp:Parameter Name="prmNetShetLen" Type="Decimal" />
            <asp:Parameter Name="prmNetShetDep" Type="Decimal" />
            <asp:Parameter Name="prmDieSizeWid" Type="Decimal" />
            <asp:Parameter Name="prmDieSizeLen" Type="Decimal" />
            <asp:Parameter Name="prmDieSizeDep" Type="Decimal" />
            <asp:Parameter Name="prmOnWid" Type="Decimal" />
            <asp:Parameter Name="prmOnLen" Type="Decimal" />
            <asp:Parameter Name="prmOnDep" Type="Decimal" />
            <asp:Parameter Name="prmOnTotalUp" Type="Int32" />
            <asp:Parameter Name="prmOnSqFeet" Type="decimal" />
            <asp:Parameter Name="prmBlankWid" Type="Decimal" />
            <asp:Parameter Name="prmBlankLen" Type="Decimal" />
            <asp:Parameter Name="prmBlankDep" Type="Decimal" />
            <asp:Parameter Name="prmAdder1" Type="String" />
            <asp:Parameter Name="prmAdder2" Type="String" />
            <asp:Parameter Name="prmAdder3" Type="String" />
            <asp:Parameter Name="prmAdder4" Type="String" />
            <asp:Parameter Name="prmAdder5" Type="String" />
            <asp:Parameter Name="prmAdder6" Type="String" />
            <asp:Parameter Name="prmAdder7" Type="String" />
            <asp:Parameter Name="prmAdder8" Type="String" />
            <asp:Parameter Name="prmAdder9" Type="String" />
            <asp:Parameter Name="prmAdder10" Type="String" />
            <asp:Parameter Name="prmAdder11" Type="String" />
            <asp:Parameter Name="prmAdder12" Type="String" />
            <asp:Parameter Name="prmWaxLabel1" Type="String" />
            <asp:Parameter Name="prmWaxDesc1" Type="String" />
            <asp:Parameter Name="prmS1" Type="Int32" />
            <asp:Parameter Name="prmB1" Type="Int32" />
            <asp:Parameter Name="prmLeafWid1" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen1" Type="Decimal" />
            <asp:Parameter Name="prmWaxLabel2" Type="String" />
            <asp:Parameter Name="prmWaxDesc2" Type="String" />
            <asp:Parameter Name="prmS2" Type="Int32" />
            <asp:Parameter Name="prmB2" Type="Int32" />
            <asp:Parameter Name="prmLeafWid2" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen2" Type="Decimal" />
            <asp:Parameter Name="prmWaxLabel3" Type="String" />
            <asp:Parameter Name="prmWaxDesc3" Type="String" />
            <asp:Parameter Name="prmS3" Type="Int32" />
            <asp:Parameter Name="prmB3" Type="Int32" />
            <asp:Parameter Name="prmLeafWid3" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen3" Type="Decimal" />
            <asp:Parameter Name="prmWaxLabel4" Type="String" />
            <asp:Parameter Name="prmWaxDesc4" Type="String" />
            <asp:Parameter Name="prmS4" Type="Int32" />
            <asp:Parameter Name="prmB4" Type="Int32" />
            <asp:Parameter Name="prmLeafWid4" Type="Decimal" />
            <asp:Parameter Name="prmLeafLen4" Type="Decimal" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_corrugated_blankno" Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
</asp:Content>




