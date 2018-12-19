<%@ Page Language="C#" MasterPageFile="~/MasterPageCorrugated.master"  AutoEventWireup="true" Inherits="corr_specs" Title="Specs Estimate" Codebehind="corr_specs.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
<script language = "JavaScript" type="text/javascript" src="include/CalendarControl.js"></script>
<asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
<script language="javascript" type="text/javascript">

window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox"))
    {
        if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox").disabled!=true)
        {
            var cust = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox");
            cust.focus();
            cust.select();
        }
        else
        {
            var stylecode=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox");
            stylecode.focus();
            stylecode.select();
        }
    }
}


function clickButton(e, ctl00$ContentPlaceHolder1$FormView_Specs$auto_cal_save_Button) {

    var evt = e ? e : window.event;
    if (document.getElementById("ctl00$ContentPlaceHolder1$FormView_Specs$auto_cal_save_Button")) {
        var bt = document.getElementById(ctl00$ContentPlaceHolder1$FormView_Specs$auto_cal_save_Button);

        if (bt) {

            if (evt.keyCode == 13) {

                bt.click();

                return false;

            }

        }
    }

}
function clickButton1(e, ctl00_ContentPlaceHolder1_FormView_Specs_UpdateButton) {

    var evt = e ? e : window.event;
    if (document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_UpdateButton")) {
        var bt = document.getElementById(ctl00_ContentPlaceHolder1_FormView_Specs_UpdateButton);

        if (bt) {

            if (evt.keyCode == 13) {

                bt.click();

                return false;

            }

        }
    }

}
    

function flutelookup(){ 
  var NewWindow = window.open("flute_lookup.aspx","FluteLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function FluteLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFluteTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFluteTextBox.focus();
  }
  
  function customerlook(){ 
  var NewWindow = window.open("corcust_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function currcustLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vShipToTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vShipNameTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vAddrTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vAddr2TextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCityTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStateTextBox.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vZipTextBox.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox.focus();
}

function fglook(){ 
  var NewWindow = window.open("cor_fgitem_look.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGItemLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFgItemTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFgItemTextBox.focus();
}

function custpartlook(){ 
  var NewWindow = window.open("cor_custpart_look.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustpartLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCustPartTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vItemNameTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCustPartTextBox.focus();
}


function stylelook(){ 
var style1 = "2";
  var NewWindow = window.open("corstyle_Lookup.aspx?style="+style1+"","StyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorStyleLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox.focus();
}
  
function Boardlook1(){ 
  var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vEstNumTextBox").innerText;
  var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox").value;
  var NewWindow = window.open("corboard_lookup.aspx?est="+est1+"&style="+style1+"","BoardWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorBoardLookup(ReturnObj1, ReturnObj2) {
    if (ReturnObj2.indexOf(":")) {
        var val = ReturnObj2;
        ReturnObj2 = val.replace(":", "\"");
    }
    if (ReturnObj1.indexOf(":")) {
        var val2 = ReturnObj1;
        ReturnObj1 = val2.replace(":", "\"");
    }
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBrdDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBoardTextBox.focus();

}
 function categorylookup(){ 
  var NewWindow =window.open("corfg_catlookup.aspx","CategoryWindow","width=600,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFgCategoryTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFgCatDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vFgCategoryTextBox.focus();  
}

 
function ShipTOLook(){ 
var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vCustNumTextBox").value;
 var NewWindow = window.open("ShipIdCustLook.aspx?look="+lookHidden +"","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipToLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vShipToTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vShipNameTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vAddrTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vAddr2TextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCityTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStateTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vZipTextBox.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vShipToTextBox.focus();
  } 
  
function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.value = obj;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.focus();
}

function Datelook1()
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.value="";
  Datelook();
}

function testlook(){ 
 var lookHidden1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vFluteTextBox").value;
  var NewWindow =window.open("test_Lookup.aspx?look1="+lookHidden1+"","TestLookupWindow","width=500,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function testLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vTestTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vTestTextBox.focus();  
}
function salesmanlook(){ 
  var NewWindow = window.open("salesman_lookup.aspx","SalesManLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup1(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vSalesmanTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vSmanDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vSalesmanTextBox.focus(); 
}
function dielook(){
 var NewWindow = window.open("die_lookup.aspx","DieLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function DieLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vDieNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vDieNumTextBox.focus();
  }
function cadlook(){
 var NewWindow = window.open("Cad_lookup.aspx","CadLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function CadLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCadNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCadNumTextBox.focus();
  }
function upclook(){
 var NewWindow = window.open("upc_lookup.aspx","UPCLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function UPCLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vUpcNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vUpcNumTextBox.focus();
  }
function spclook(){
 var NewWindow = window.open("spc_lookup.aspx","SPCLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function SpcLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vSpcNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vSpcNumTextBox.focus();
  }
function Platelook(){
 var NewWindow = window.open("Plate_lookup.aspx","DieLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
 }
function plateLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vPlateNumTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vPlateNumTextBox.focus();
  }
  
function adhesiveLook(){ 
  var looktype = "G,T,S";
  var est=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vEstNumTextBox").innerText;
  var NewWindow = window.open("adhesive_lookup.aspx?look2="+ looktype +"&lookest="+est+"","AdhesiveLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdhesiveLookup1(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vJointMatTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vJointMatTextBox.focus();
}
function blursqwid()
{
    var wid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value;    
    
    var val= wid.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    
    if(wid.charAt(val1) !=0)
        {
            if(parseInt(wid.charAt(val1)) > 1 || parseInt(wid.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").focus();
            return;
        }  
        }  
    
}
 function sqwidth()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value = frontback;
    }
   }
   
   function blursqlen()
  {
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value;    
    
    var val= len.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
       if(len.charAt(val1) !=0)
        {
            if(parseInt(len.charAt(val1)) > 1 || parseInt(len.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").focus();
            return;
        }  
        }   
    }
  }
 function sqlen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value = frontback;
    }
   }
   
   function blurtuck()
  {
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value;    
    
    var val= len.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(len.charAt(val1) !=0)
        {
            if(parseInt(len.charAt(val1)) > 1 || parseInt(len.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").focus();
            return;
        }  
        }      
    }
  }
  
 function tuckval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value = frontback;
    }
   }
  
  function blurjointlen()
  {
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value;    
    
    var val= len.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(len.charAt(val1) !=0)
        {
            if(parseInt(len.charAt(val1)) > 1 || parseInt(len.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").focus();
            return;
        }  
        }  
    }
  }
   
   
 function jointlen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value = frontback;
    }
 }
 
 function keylen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value = frontback;
    }
   }
   
 function blurlen()
  {
    var len=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value;    
    
    var val= len.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(len.charAt(val1) !=0)
        {
            if(parseInt(len.charAt(val1)) > 1 || parseInt(len.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").focus();
            return;
        }  
        }   
    }
    var length=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox"); 
    if(length.value == "" || length.value == "0" )
    {
    alert("Length must be Enter");
    length.focus();
    }
  }
  
  function keywid()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value = frontback;
    }
   }
  function blurwid()
  {
    var wid=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value;    
    
    var val= wid.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;    
    if(val!=-1)
    {
        if(wid.charAt(val1) !=0)
        {
            if(parseInt(wid.charAt(val1)) > 1 || parseInt(wid.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").focus();
            return;
        }  
        }
           
    }
    var width=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox"); 
    if(width.value == "" || width.value == "0" )
    {
    alert("Width must be Enter");
    width.focus();
    }
  }
  
  function keydep()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value = frontback;
    }
   }
  function blurdep()
  {
    var dep=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value;    
    
    var val= dep.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    
    if(val!=-1)
    {
        if(dep.charAt(val1) !=0)
        {
            if(parseInt(dep.charAt(val1)) > 1 || parseInt(dep.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").focus();
            return;
        }  
        }            
    }
    var depth=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox"); 
    if(depth.value == "" || depth.value == "0" )
    {
    alert("Depth must be Enter");
    depth.focus();
    }
  }
  function keytop()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value = frontback;
    }
   }
  function blurtop()
  {
    var top=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value;    
    
    var val= top.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(top.charAt(val1) !=0)
        {
            if(parseInt(top.charAt(val1)) > 1 || parseInt(top.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").focus();
            return;
        }  
        }   
    }
  }
  
  function keybottom()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value = frontback;
    }
   }
  function blurbottom()
  {
    var bottom=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value;    
    
    var val= bottom.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(bottom.charAt(val1) !=0)
        {
            if(parseInt(bottom.charAt(val1)) > 1 || parseInt(bottom.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").focus();
            return;
        }  
        }    
    }    
  }
  
  function keylock()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value = frontback;
    }
   }
  function blurlock()
  {
    var lock =document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value;    
    
    var val= lock.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
       if(lock.charAt(val1) !=0)
        {
            if(parseInt(lock.charAt(val1)) > 1 || parseInt(lock.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").focus();
            return;
        }  
        }  
    }
  }
  
  function keyjointtab()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value = frontback;
    }
   }
  function blurjointtab()
  {
    var jtab=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value;    
    
    var val= jtab.indexOf(".");
    //alert(val);
    var val1;
    var val2;
    val1=val+1;
    val2=val+2;
    if(val!=-1)
    {
        if(jtab.charAt(val1) !=0)
        {
            if(parseInt(jtab.charAt(val1)) > 1 || parseInt(jtab.charAt(val2)) >5 )
        {
            alert("Can not have more than .15 as decimal field");        
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value="";
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").focus();
            return;
        }  
        }   
    }
  }


 
</script>

    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />  
    
    <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource_list"
            Style="position: static"  EmptyDataText="No Records Found" Width="100%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False"  ForeColor="White" CssClass="headcolor" />
        <Columns>
         <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                    
            <asp:BoundField ItemStyle-Wrap="false" DataField="vEst" HeaderText="Estimate" SortExpression="vEst" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCust" HeaderText="Cust" SortExpression="vCust" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCustPart" HeaderText="CustPart" SortExpression="vCustPart" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vShipTo" HeaderText="ShipTo" SortExpression="vShipTo" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vItemName" HeaderText="Item Name" SortExpression="vItemName" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vFgItem" HeaderStyle-Wrap="false" HeaderText="Fg Item" SortExpression="vFgItem" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vEstQty" HeaderText="Qty" SortExpression="vEstQty" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vStyle" HeaderText="Style" SortExpression="vStyle" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vFlute" HeaderText="Flute" SortExpression="vFlute" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vTest" HeaderText="Test" SortExpression="vTest" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vBoard" HeaderText="Board" SortExpression="vBoard" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCaliper" HeaderText="Caliper" SortExpression="vCaliper" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCategory" HeaderText="Category" SortExpression="vCategory" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vLenght" HeaderText="Lenght" SortExpression="vLenght" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vWidth" HeaderText="Width" SortExpression="vWidth" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vDepth" HeaderText="Depth" SortExpression="vDepth" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vForm" HeaderText="Form" SortExpression="vForm" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vBlank" HeaderText="Blank" SortExpression="vBlank" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vTab" HeaderText="Tab" SortExpression="vTab" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vColor" HeaderText="Color" SortExpression="vColor" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vPasses" HeaderText="Passes" SortExpression="vPasses" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCoating" HeaderText="Coating" SortExpression="vCoating" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCoatPasses" HeaderText="CoatPasses" SortExpression="vCoatPasses" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vQtySet" HeaderText="QtySet" SortExpression="vQtySet" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vInkFrom" HeaderText="InkFrom" SortExpression="vInkFrom" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vPassesFrom" HeaderText="PassesFrom" SortExpression="vPassesFrom" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCoatingFrom" HeaderText="CoatingFrom" SortExpression="vCoatingFrom" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vCoatPassesFrom" HeaderText="CoatPassesFrom" SortExpression="vCoatPassesFrom" />
            <asp:BoundField ItemStyle-Wrap="false" DataField="vPurchManuf" HeaderText="PurchManuf" SortExpression="vPurchManuf" />
           
            <asp:TemplateField HeaderText="Date" SortExpression="vEstDate">                    
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[vEstDate]","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                    <ItemStyle Wrap="False" />
                </asp:TemplateField>  
            
        </Columns>
            
            </asp:GridView>
    <br />
    
      
    <asp:FormView ID="FormView_Specs"  runat="server" DataSourceID="CorrugatedSpecsDataSource" OnDataBound="FormView_Specs_DataBound" >
        <EditItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server"   >
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Estimate:</b></td>
            <td><asp:Label ID="vEstNumTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstNum") %>'>
            </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>From:</b></td>
            <td><asp:TextBox ID="vFromDtTextBox" Width="100px"  runat="server" Text='<%# Bind("vFromDt") %>'>
            </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Date:</b></td>
            <td> <asp:Label ID="vEstDateTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'>
            </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Mod:</b></td>
            <td><asp:Label ID="vModDateTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vModDate","{0:MM/dd/yyyy}") %>'>
            </asp:Label></td>
            <td nowrap align="right" style="padding-right:5px"><b>Last Order:</b></td>
            <td><asp:Label ID="vLastOrdTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLastOrd") %>'>
            </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Date:</b></td>
            <td><asp:Label ID="vOrdDateTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vOrdDate","{0:MM/dd/yyyy}") %>'>
            </asp:Label></td>            
            </tr>
            <tr>
            <td colspan="6">
            <fieldset style="border-color:Black">
            <table >
            <tr><td align="right" style="padding-right:5px"><b>Cust#:</b></td>
            <td><asp:TextBox ID="vCustNumTextBox" runat="server" Text='<%# Bind("vCustNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>Ship To:</b></td>
            <td><asp:TextBox ID="vShipToTextBox" runat="server" Text='<%# Bind("vShipTo") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Company:</b></td>
            <td colspan="3"><asp:TextBox ID="vShipNameTextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vShipName") %>'>
            </asp:TextBox></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
            <td colspan="3"><asp:TextBox ID="vAddrTextBox" Width="160px" Enabled="false" runat="server" Text='<%# Bind("vAddr") %>'>
            </asp:TextBox></td>
            </tr>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
            <td colspan="3"><asp:TextBox ID="vAddr2TextBox" Width="160px" Enabled="false" runat="server" Text='<%# Bind("vAddr2") %>'>
            </asp:TextBox></td>
            </tr>            
            <tr><td align="right" style="padding-right:5px"><b>City/State/Zip:</b></td>
            <td colspan="3">
            <asp:TextBox ID="vCityTextBox" runat="server" Width="100px" Enabled="false" Text='<%# Bind("vCity") %>'>
            </asp:TextBox>            
            <asp:TextBox ID="vStateTextBox" runat="server" Width="40px" Enabled="false" Text='<%# Bind("vState") %>'>
            </asp:TextBox>            
            <asp:TextBox ID="vZipTextBox" runat="server" Width="60px" Enabled="false" Text='<%# Bind("vZip") %>'>
            </asp:TextBox>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Sales Rep:</b></td>
            <td nowrap><asp:TextBox ID="vSalesmanTextBox" Width="100px" runat="server" Text='<%# Bind("vSalesman") %>'>
            </asp:TextBox> <a href="#" tabindex="1" onclick="salesmanlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vSalesmanTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Salesman Must Be Enter"></asp:RequiredFieldValidator></td>
            <td colspan="2" nowrap ><asp:TextBox ID="vSmanDscrTextBox" Enabled="false" Width="160px"  runat="server" Text='<%# Bind("vSmanDscr") %>'>
            </asp:TextBox>
            <b>%:</b><asp:TextBox ID="vCommTextBox" runat="server" Width="80px"  Text='<%# Bind("vComm") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCommTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>FG Category:</b></td>
            <td><asp:TextBox ID="vFgCategoryTextBox" Width="100px" runat="server"  Text='<%# Bind("vFgCategory") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="categorylookup(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="vFgCategoryTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Invalid FG Category Try Help"></asp:RequiredFieldValidator></td>
            <td colspan="2"><asp:TextBox ID="vFgCatDscrTextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vFgCatDscr") %>'>
            </asp:TextBox></td>
            </tr>
            </table></fieldset></td>
            <td colspan="6">
            <fieldset style="border-color:Black">
            <table>
            <tr  height="15px"><td nowrap colspan="4">
            <b>Quantity:</b><asp:Label ID="vQtyTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQty") %>'>
            </asp:Label>
            <b>Qty/Set:</b><asp:Label ID="vQtySetTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQtySet") %>'>
            </asp:Label>
            <b>MSF:</b><asp:Label ID="vMsfTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vMsf","{0:###,##0.00##}") %>'>
            </asp:Label>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
            <td><asp:TextBox ID="vCustPartTextBox" runat="server" Text='<%# Bind("vCustPart") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="custpartlook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>FG Item:</b></td>
            <td><asp:TextBox ID="vFgItemTextBox" runat="server" Text='<%# Bind("vFgItem") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="fglook(); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Item Name:</b></td>
            <td colspan="3"><asp:TextBox ID="vItemNameTextBox" Width="160px" runat="server" Text='<%# Bind("vItemName") %>'>
            </asp:TextBox></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Description:</b></td>
            <td colspan="3"><asp:TextBox ID="vDescrTextBox" Width="160px" runat="server" Text='<%# Bind("vDescr") %>'>
            </asp:TextBox></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Die#:</b></td>
            <td><asp:TextBox ID="vDieNumTextBox" runat="server" Text='<%# Bind("vDieNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="dielook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>Images#:</b></td>
            <td nowrap ><asp:TextBox ID="vImageTextBox" runat="server" Text='<%# Bind("vImage") %>'>
            </asp:TextBox>  <asp:FileUpload  ID="FileUpload1" Height="20px" onfocus="window.scroll(800,800)" Width="150px" runat="server" />
             <asp:RegularExpressionValidator id="FileUpLoadValidator"  Display="Dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Invalid File only JPG Image" 
                  ValidationExpression="^(([a-zA-Z]:)|(\\{2}\w+)\$?)(\\(\w[\w].*))(.jpg|.JPG)$" ControlToValidate="FileUpload1">
                  </asp:RegularExpressionValidator>
            
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Cad#:</b></td>
            <td><asp:TextBox ID="vCadNumTextBox" runat="server" Text='<%# Bind("vCadNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="cadlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>Plate#:</b></td>
            <td><asp:TextBox ID="vPlateNumTextBox" runat="server" Text='<%# Bind("vPlateNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="Platelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>SPC/QC#:</b></td>
            <td><asp:TextBox ID="vSpcNumTextBox" runat="server" Text='<%# Bind("vSpcNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="spclook(); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>UPC#:</b></td>
            <td><asp:TextBox ID="vUpcNumTextBox" runat="server" Text='<%# Bind("vUpcNum") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="upclook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>            
            </table></fieldset>
            </td></tr>
            <tr>
            <td colspan="12">
            
            
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
            
            <fieldset  style="border-color:Black">            
            <table>
            <tr><td align="right" style="padding-right:5px"><b>Style Code:</b></td>
            <td><asp:TextBox ID="vStyleTextBox" Width="100px" runat="server" Text='<%# Bind("vStyle") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="stylelook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="vStyleTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Style Must Be Enter"></asp:RequiredFieldValidator></td>
            <td><asp:TextBox ID="vStyleDscrTextBox" Enabled="false" runat="server" Text='<%# Bind("vStyleDscr") %>'>
            </asp:TextBox></td>
            <td nowrap colspan="5">
            <b>Flute:</b><asp:TextBox ID="vFluteTextBox" runat="server" Text='<%# Bind("vFlute") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="flutelookup(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="vFluteTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Flute Must Be Enter"></asp:RequiredFieldValidator>
            <b>Test:</b><asp:TextBox ID="vTestTextBox" AutoPostBack="true" OnTextChanged="test_change" runat="server" Text='<%# Bind("vTest") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="testlook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <b>Tab:</b><asp:DropDownList ID="DropDownList1" Width="60px" SelectedValue='<%# Bind("vTab") %>' DataValueField='<%# Bind("vTab") %>' runat="server">
            <asp:ListItem Text="Out" Value="Out"></asp:ListItem>
            <asp:ListItem Text="In" Value="In"></asp:ListItem>
            </asp:DropDownList>
            <b>Metric?:</b><asp:DropDownList ID="DropDownList2" Width="60px" SelectedValue='<%# Bind("vMetric") %>' DataValueField='<%# Bind("vMetric") %>' runat="server">
            <asp:ListItem Text="Yes" Value="Yes"></asp:ListItem>
            <asp:ListItem Text="No" Value="No"></asp:ListItem>
            </asp:DropDownList>
            </td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Board:</b></td>
            <td ><asp:TextBox ID="vBoardTextBox" AutoPostBack="true" OnTextChanged="board_textbox_change" Width="100px" runat="server" Text='<%# Bind("vBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="Boardlook1(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator5" ControlToValidate="vBoardTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Board Must Be Enter"></asp:RequiredFieldValidator></td>
            <td colspan="4"><asp:TextBox ID="vBrdDscrTextBox" Width="160px" runat="server" Text='<%# Bind("vBrdDscr") %>'>
            </asp:TextBox></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:TextBox ID="vLengthTextBox" runat="server" MaxLength="6" onkeyup="keylen()" onblur="blurlen()" Text='<%# Bind("vLength") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vLengthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:TextBox ID="vWidthTextBox" runat="server" MaxLength="6" onkeyup="keywid()" onblur="blurwid()" Text='<%# Bind("vWidth") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Depth:</b></td>
            <td><asp:TextBox ID="vDepthTextBox" runat="server" MaxLength="6" onkeyup="keydep()" onblur="blurdep()" Text='<%# Bind("vDepth") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vDepthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Joint Material:</b></td>
            <td><asp:TextBox ID="vJointMatTextBox" runat="server" Text='<%# Bind("vJointMat") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="adhesiveLook(); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Top/Dust Flap:</b></td>
            <td><asp:TextBox ID="vDustFlapTextBox" runat="server" MaxLength="6" onkeyup="keytop()" onblur="blurtop()" Text='<%# Bind("vDustFlap") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vDustFlapTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Bottom Flap:</b></td>
            <td><asp:TextBox ID="vBotFlapTextBox" runat="server" MaxLength="6" onkeyup="keybottom()" onblur="blurbottom()" Text='<%# Bind("vBotFlap") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vBotFlapTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Lock Tab</b></td>
            <td><asp:TextBox ID="vLockTabTextBox" runat="server" MaxLength="6" onkeyup="keylock()" onblur="blurlock()" Text='<%# Bind("vLockTab") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vLockTabTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Joint Tab Width:</b></td>
            <td><asp:TextBox ID="vTabWidTextBox" runat="server" MaxLength="6" onkeyup="keyjointtab()" onblur="blurjointtab()" Text='<%# Bind("vTabWid") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vTabWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Scores on Width:</b></td>
            <td><asp:TextBox ID="vScoreWidTextBox" onkeyup="sqwidth()"  onblur="blursqwid()" MaxLength="6" runat="server" Text='<%# Bind("vScoreWid") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vScoreWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Scores on Length:</b></td>
            <td><asp:TextBox ID="vScoreLenTextBox" onkeyup="sqlen()" onblur="blursqlen()" MaxLength="6" runat="server" Text='<%# Bind("vScoreLen") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vScoreLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Tuck:</b></td>
            <td><asp:TextBox ID="vTuckTextBox" onkeyup="tuckval()" onblur="blurtuck()" MaxLength="6" runat="server" Text='<%# Bind("vTuck") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vTuckTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Joint Length:</b></td>
            <td><asp:TextBox ID="vJointLenTextBox" onkeyup="jointlen()" onblur="blurjointlen()" MaxLength="6" runat="server" Text='<%# Bind("vJointLen") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vJointLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Blank width:</b></td>
            <td><asp:TextBox ID="vBlankWidTextBox" runat="server" Text='<%# Bind("vBlankWid") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vBlankWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Blank Length:</b></td>
            <td><asp:TextBox ID="vBlankLenTextBox" runat="server" Text='<%# Bind("vBlankLen") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vBlankLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Blank Square Feet:</b></td>
            <td><asp:TextBox ID="vBlankSqFtTextBox" runat="server" onblur="setfocus()" Text='<%# Bind("vBlankSqFt") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vBlankSqFtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            </tr>
            </table></fieldset>
            
            </ContentTemplate>                                             
            </asp:UpdatePanel>
            
            </td>
            </tr>
            <tr><td style="display:none">
            <asp:Label ID="ImagePathLabel" runat="server" Text='<%# Bind("vImagePath") %>' ></asp:Label>
            </td></tr>
            
            <tr><td colspan="4"><asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClick="Save_Click" CausesValidation="True" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="auto_cal_save_Button" runat="server" CssClass="button" OnClick="Auto_Save_Click" CausesValidation="False" Text="Save">  </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button></td></tr>
            </table>
            </asp:Panel>
        </EditItemTemplate>
        
        <ItemTemplate>
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Estimate:</b></td>
            <td><asp:Label ID="vEstNumLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstNum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>From:</b></td>
            <td><asp:Label ID="vFromDtLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFromDt") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Date:</b></td>
            <td><asp:Label ID="vEstDateLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Mod:</b></td>
            <td><asp:Label ID="vModDateLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vModDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px" nowrap><b>Last Order:</b></td>
            <td><asp:Label ID="vLastOrdLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLastOrd") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Date:</b></td>
            <td><asp:Label ID="vOrdDateLabel" Width="70px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vOrdDate","{0:MM/dd/yyyy}") %>'></asp:Label></td></tr>
            <tr><td colspan="6">
            <fieldset style="border:solid 1px black;">
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Cust#:</b></td>
            <td><asp:Label ID="vCustNumLabel" runat="server" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px"  Text='<%# Bind("vCustNum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Ship To:</b></td>
            <td><asp:Label ID="vShipToLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vShipTo") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Company:</b></td>
            <td colspan="3"><asp:Label ID="vShipNameLabel" Width="180px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vShipName") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
            <td colspan="3"><asp:Label ID="vAddrLabel" Width="180px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vAddr") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
            <td colspan="3"><asp:Label ID="vAddr2Label" Width="180px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vAddr2") %>'></asp:Label></td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>City/State/Zip:</b></td>
            <td colspan="3">
            <asp:Label ID="vCityLabel" Width="90px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vCity") %>'></asp:Label>            
            <asp:Label ID="vStateLabel" Width="30px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vState") %>'></asp:Label>            
            <asp:Label ID="vZipLabel" Width="60px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vZip") %>'></asp:Label>
            </td> </tr>
            <tr ><td align="right" style="padding-right:5px"><b>Sales Rep:</b></td>
            <td><asp:Label ID="vSalesmanLabel" Width="100px"  BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vSalesman") %>'></asp:Label></td>
            <td colspan="2"><asp:Label ID="vSmanDscrLabel" Width="150px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vSmanDscr") %>'></asp:Label>
            <b>%:</b>
            <asp:Label ID="vCommLabel" Width="50px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vComm") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Fg Category:</b></td>
            <td><asp:Label ID="vFgCategoryLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFgCategory") %>'></asp:Label></td>
            <td colspan="2"><asp:Label ID="vFgCatDscrLabel" Width="160px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFgCatDscr") %>'> </asp:Label></td>
            </tr>
            </table>
            </fieldset>
            </td>
            <td colspan="6">
            <fieldset style="border:solid 1px black;">
            <table class="shade">
            <tr><td nowrap colspan="4">
            <b>Quantity:</b> <asp:Label ID="vQtyLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQty") %>'></asp:Label>
            <b>Qty/Set:</b> <asp:Label ID="vQtySetLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQtySet") %>'></asp:Label>
            <b>MSF:</b> <asp:Label ID="vMsfLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vMsf","{0:###,##0.00##}") %>'></asp:Label>
            </td> </tr>
            <tr><td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
            <td><asp:Label ID="vCustPartLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>FG Item</b></td>
            <td><asp:Label ID="vFgItemLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFgItem") %>'></asp:Label></td> </tr>
            <tr><td align="right" style="padding-right:5px"><b>Item Name:</b></td>
            <td colspan="3"><asp:Label ID="vItemNameLabel" Width="160px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vItemName") %>'></asp:Label></td>
             </tr>
            <tr><td align="right" style="padding-right:5px"><b>Description:</b></td>
            <td colspan="2"><asp:Label ID="vDescrLabel" Width="160px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDescr") %>'></asp:Label></td>
             </tr>
            <tr><td align="right" style="padding-right:5px"><b>Die#:</b></td>
            <td><asp:Label ID="vDieNumLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDieNum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Images:</b></td>
            <td><asp:Label ID="vImageLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vImage") %>'></asp:Label></td> </tr>
            <tr><td align="right" style="padding-right:5px"><b>Cad#:</b></td>
            <td><asp:Label ID="vCadNumLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vCadNum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Plate#:</b></td>
            <td><asp:Label ID="vPlateNumLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vPlateNum") %>'></asp:Label></td> </tr>
            <tr><td align="right" style="padding-right:5px"><b>Spc/QC#:</b></td>
            <td><asp:Label ID="vSpcNumLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vSpcNum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>UPC#:</b></td>
            <td><asp:Label ID="vUpcNumLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vUpcNum") %>'></asp:Label></td> </tr>
            </table>
            </fieldset>
            </td>
            </tr>
            <tr>
            <td colspan="12">
            <fieldset style="border:solid 1px black;">
            <table class="shado">
            <tr><td align="right" style="padding-right:5px"><b>Style Code:</b></td>
            <td><asp:Label ID="vStyleLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vStyle") %>'></asp:Label></td>
            <td colspan="6"><asp:Label ID="vStyleDscrLabel" Width="150px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vStyleDscr") %>'> </asp:Label>
            
            <b>Flute: </b><asp:Label ID="vFluteLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vFlute") %>'></asp:Label>
            <b>Test:</b><asp:Label ID="vTestLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vTest") %>'></asp:Label>
            <b>Tab:</b><asp:Label ID="vTabLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vTab") %>'></asp:Label>
            <b>Metric?:</b><asp:Label ID="vMetricLabel" Width="50px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vMetric") %>'></asp:Label>
            </td>   </tr>            
            <tr><td align="right" style="padding-right:5px"><b>Board:</b></td>
            <td><asp:Label ID="vBoardLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBoard") %>'></asp:Label></td>
            <td colspan="5"><asp:Label ID="vBrdDscrLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBrdDscr") %>'></asp:Label>
            </td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:Label ID="vLengthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLength","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:Label ID="vWidthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vWidth","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Depth:</b></td>
            <td><asp:Label ID="vDepthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDepth","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Joint Material:</b></td>
            <td><asp:Label ID="vJointMatLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vJointMat") %>'></asp:Label></td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Top/Dust Flap:</b></td>
            <td><asp:Label ID="vDustFlapLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDustFlap","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Bottom Flap:</b></td>
            <td><asp:Label ID="vBotFlapLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBotFlap","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Lock Tab:</b></td>
            <td><asp:Label ID="vLockTabLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLockTab","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Joint Tab Width:</b></td>
            <td><asp:Label ID="vTabWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vTabWid","{0:###,##0.00}") %>'></asp:Label></td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Scores on Width:</b></td>
            <td><asp:Label ID="vScoreWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vScoreWid","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Scores on length:</b></td>
            <td><asp:Label ID="vScoreLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vScoreLen","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Tuck:</b></td>
            <td><asp:Label ID="vTuckLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vTuck","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Joint Length:</b></td>
            <td><asp:Label ID="vJointLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vJointLen","{0:###,##0.00}") %>'></asp:Label></td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Blank Width:</b></td>
            <td><asp:Label ID="vBlankWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankWid","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Blank Length:</b></td>
            <td><asp:Label ID="vBlankLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankLen","{0:###,##0.00}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Blank Square Feet:</b></td>
            <td><asp:Label ID="vBlankSqFtLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankSqFt","{0:###,##0.00##}") %>'> </asp:Label></td></tr>            
            </table>
            </fieldset>
            </td>
            </tr>
           
            
            <tr><td colspan="4">           
            <asp:Button ID="updatebutton" runat="server" CssClass="button" CausesValidation="true" Text="Override" CommandName="edit" OnClick="updatebutton_click" ></asp:Button>
            <asp:Button ID="AutoCalculateButton" runat="server" CssClass="button" CausesValidation="true" Text="Auto-Calculate" CommandName="Edit" OnClick="AutoCalculateButton_Click" ></asp:Button>
            </td></tr></table>
        </ItemTemplate>
        
    </asp:FormView>
    <asp:ObjectDataSource ID="CorrugatedSpecsDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="CorrugatedSpecs" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmCustNum" Type="String" />
            <asp:Parameter Name="prmShipTo" Type="String" />
            <asp:Parameter Name="prmFgItem" Type="String" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter Name="prmPartDscr" Type="String" />
            <asp:Parameter Name="prmDieNum" Type="String" />
            <asp:Parameter Name="prmCadNum" Type="String" />
            <asp:Parameter Name="prmSpcNum" Type="String" />
            <asp:Parameter Name="prmPlateNum" Type="String" />
            <asp:Parameter Name="prmImage" Type="String" />
            <asp:Parameter Name="prmUpcNum" Type="String" />
            <asp:Parameter Name="prmSman" Type="String" />
            <asp:Parameter Name="prmSmanDscr" Type="String" />
            <asp:Parameter Name="prmComm" Type="Decimal" />
            <asp:Parameter Name="prmFgCat" Type="String" />
            <asp:Parameter Name="prmFgCatDscr" Type="String" />
            <asp:Parameter Name="prmStyle" Type="String" />
            <asp:Parameter Name="prmStyDscr" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmBrdDscr" Type="String" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmDepth" Type="Decimal" />
            <asp:Parameter Name="prmFlute" Type="String" />
            <asp:Parameter Name="prmTest" Type="String" />
            <asp:Parameter Name="prmTab" Type="String" />
            <asp:Parameter Name="prmMetric" Type="String" />
            <asp:Parameter Name="prmJointMat" Type="String" />
            <asp:Parameter Name="prmDustFlap" Type="Decimal" />
            <asp:Parameter Name="prmBotFlap" Type="Decimal" />
            <asp:Parameter Name="prmLockTab" Type="Decimal" />
            <asp:Parameter Name="prmTabWid" Type="Decimal" />
            <asp:Parameter Name="prmScWid" Type="Decimal" />
            <asp:Parameter Name="prmScLen" Type="Decimal" />
            <asp:Parameter Name="prmTuck" Type="Decimal" />
            <asp:Parameter Name="prmJointLen" Type="Decimal" />
            <asp:Parameter Name="prmBlankWid" Type="Decimal" />
            <asp:Parameter Name="prmBlankLen" Type="Decimal" />
            <asp:Parameter Name="prmBlankSqFt" Type="Decimal" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmFromDate" Type="DateTime" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:Parameter Name="prmModDate" Type="DateTime" />
            <asp:Parameter Name="prmOrderNum" Type="Int32" />
            <asp:Parameter Name="prmOrdDate" Type="DateTime" />
            <asp:Parameter Name="prmQty" Type="Decimal" />
            <asp:Parameter Name="prmQtySet" Type="Decimal" />
            <asp:Parameter Name="prmMsf" Type="Decimal" />
            <asp:Parameter Name="prmShipName" Type="String" />
            <asp:Parameter Name="prmAddr" Type="String" />
            <asp:Parameter Name="prmCity" Type="String" />
            <asp:Parameter Name="prmState" Type="String" />
            <asp:Parameter Name="prmZip" Type="String" />
            <asp:Parameter Name="prmAddr2" Type="String" />
            <asp:SessionParameter SessionField="order_corrugated_formno" Name="prmFormno" Type="int32" />
            <asp:Parameter Name="prmEstFrom" Type="string" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_corrugated_blankno" Type="int32" />
            <asp:Parameter Name="prmAutocalcSelected" Type="String" />            
        </SelectParameters>
    </asp:ObjectDataSource>


<asp:ObjectDataSource ID="ObjectDataSource_list" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectCorrugateEstimate" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter DefaultValue="ListEst" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstimate" SessionField="order_corrugated_est" Type="String" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmShipTo" Type="String" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmEstQty" Type="Decimal" />
            <asp:Parameter Name="prmStyle" Type="String" />
            <asp:Parameter Name="prmFlute" Type="String" />
            <asp:Parameter Name="prmTest" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmCalliper" Type="Decimal" />
            <asp:Parameter Name="prmCategory" Type="String" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmDepth" Type="Decimal" />
            
            <asp:SessionParameter Name="prmFrom" SessionField="order_corrugated_formno" Type="Int32" />
            <asp:Parameter Name="prmBlank" Type="Int32" />
            <asp:Parameter Name="prmTab" Type="String" />
            <asp:Parameter Name="prmColor" Type="Int32" />
            <asp:Parameter Name="prmPasses" Type="Int32" />
            <asp:Parameter Name="prmCoating" Type="Int32" />
            <asp:Parameter Name="prmCoatPasses" Type="Int32" />
            <asp:Parameter Name="prmQtySet" Type="Decimal" />
            <asp:Parameter Name="prmInkFrom" Type="Int32" />
            <asp:Parameter Name="prmPassesFrom" Type="Int32" />
            <asp:Parameter Name="prmCoatingFrom" Type="Int32" />
            <asp:Parameter Name="prmCoatPassesFrom" Type="Int32" />
            <asp:Parameter Name="prmPurchManuf" Type="String" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:Parameter Name="prmType" Type="String" />
            <asp:Parameter Name="prmMassType" Type="String" />
            <asp:Parameter Name="prmEstQty2" Type="decimal" />
            <asp:Parameter Name="prmEstQty3" Type="decimal" />
            <asp:Parameter Name="prmEstQty4" Type="decimal" />
            <asp:Parameter Name="prmEstQty5" Type="decimal" />
            <asp:Parameter Name="prmEstQty6" Type="decimal" />
            <asp:Parameter Name="prmEstQty7" Type="decimal" />
            <asp:Parameter Name="prmEstQty8" Type="decimal" />
            <asp:Parameter Name="prmEstQty9" Type="decimal" />
            <asp:Parameter Name="prmEstQty10" Type="decimal" />
            <asp:Parameter Name="prmEstQty11" Type="decimal" />
            <asp:Parameter Name="prmEstQty12" Type="decimal" />
            <asp:Parameter Name="prmEstQty13" Type="decimal" />
            <asp:Parameter Name="prmEstQty14" Type="decimal" />
            <asp:Parameter Name="prmEstQty15" Type="decimal" />
            <asp:Parameter Name="prmEstQty16" Type="decimal" />
            <asp:Parameter Name="prmEstQty17" Type="decimal" />
            <asp:Parameter Name="prmEstQty18" Type="decimal" />
            <asp:Parameter Name="prmEstQty19" Type="decimal" />
            <asp:Parameter Name="prmEstQty20" Type="decimal" />
            
            <asp:Parameter Name="prmRelQty1" Type="decimal" />
            <asp:Parameter Name="prmRelQty2" Type="decimal" />
            <asp:Parameter Name="prmRelQty3" Type="decimal" />
            <asp:Parameter Name="prmRelQty4" Type="decimal" />
            <asp:Parameter Name="prmRelQty5" Type="decimal" />
            <asp:Parameter Name="prmRelQty6" Type="decimal" />
            <asp:Parameter Name="prmRelQty7" Type="decimal" />
            <asp:Parameter Name="prmRelQty8" Type="decimal" />
            <asp:Parameter Name="prmRelQty9" Type="decimal" />
            <asp:Parameter Name="prmRelQty10" Type="decimal" />
            <asp:Parameter Name="prmRelQty11" Type="decimal" />
            <asp:Parameter Name="prmRelQty12" Type="decimal" />
            <asp:Parameter Name="prmRelQty13" Type="decimal" />
            <asp:Parameter Name="prmRelQty14" Type="decimal" />
            <asp:Parameter Name="prmRelQty15" Type="decimal" />
            <asp:Parameter Name="prmRelQty16" Type="decimal" />
            <asp:Parameter Name="prmRelQty17" Type="decimal" />
            <asp:Parameter Name="prmRelQty18" Type="decimal" />
            <asp:Parameter Name="prmRelQty19" Type="decimal" />
            <asp:Parameter Name="prmRelQty20" Type="decimal" />
            <asp:Parameter Name="prmlvcopied" Type="String" />
            
        </SelectParameters>
    </asp:ObjectDataSource>


</asp:Content>

