<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="false" AutoEventWireup="true" Inherits="folding_estimate" Title="Folding Estimate" MaintainScrollPositionOnPostback="true" Codebehind="folding_estimate.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
<script language = "JavaScript" src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
<script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmAdd() {
        var width = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vWidthTextBox");
        var length = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vLenghtTextBox");
       
        if (parseFloat(width.value) > parseFloat(length.value)) {
            var retVal = makeMsgBox("Confirmation", "This is an abnormal box, carton width should not be greater then length. \n Would you like to continue with abnormal box?", 48, 4, 256, 4096);

            if (retVal == 6) {
                var depth = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vDepthTextBox");
                depth.focus();
            }
            else {
                width.value = "";
                width.focus();
            }
        }

    }
    function changecolor(v) {
        if (v == "1") {
            var color = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vColorTextBox");
            var inkpass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vInkFromTextBox");
            inkpass.value = color.value;
        }
        if (v == "2") {
            var color = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vColorTextBox");
            var inkpass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vInkFromTextBox");
            inkpass.innerHTML = color.value;
        }
    }
    function changecoating(v) {
        if (v == "1") {
            var coating = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCoatingTextBox");
            var coatpass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCoatingFromTextBox");
            coatpass.value = coating.value;
        }
        if (v == "2") {
            var coating = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCoatingTextBox");
            var coatpass = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCoatingFromTextBox");
            coatpass.innerHTML = coating.value; 
        }
    }
    function radioqtyset(a) {
        
        var qtyset = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtySetTextBox");
        if (parseFloat(a) == 2) {           
            qtyset.disabled = false;
        }
        else {           
            qtyset.disabled = true;
        }     
        

    }
      
    
</script>

<script type="text/javascript" language="javascript">
   
function showsavebutton() {
    var show = document.getElementById("buttondiv");
    show.style.display = 'inline';
    var show2 = document.getElementById("displayqty");
    show2.style.display = 'none';

    stylefocus();
}

//function leaveboard(obj) {
//    if (obj.value != "") {
//        if (document.getElementById('ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox')) {           
//            document.getElementById('ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox').focus();
//        }
//        else {
//            document.getElementById('ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCategoryTextBox').focus();
//        }
//    }
//}
function leavecategory(obj) {
    window.scroll(900, 900);
    }
//window.onload=setfocus;
function setfocus()
{
    if(document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox"))
    {
        var cust=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox");
        cust.focus();
    }
}
function stylefocus() {
    if (document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox")) {
        var style = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox");
        style.focus();
    }
}
function style2focus() {
    if (document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox")) {
        var style = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox");
        style.focus();
        
        var show2 = document.getElementById("displayqty");
        show2.style.display = 'none';
    }
}

function samevalue() {
    var beginc = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox");
    var endc = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vShipToTextBox");
    endc.value = beginc.value;
}

function msfcount()
{

    var lvyld = 0;
    var msf1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label1");
    var tlen = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_tlenlabel").innerText;
    var twid = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_twidlabel").innerText;
    var qtyset = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtySetTextBox").value;
    var qty1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent1TextBox"); 
    var type = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstTypeLabel").innerText;
    var qty2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent2TextBox"); 
    var qty3 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent3TextBox"); 
    var qty4 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent4TextBox"); 
    var qty5 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent5TextBox"); 
    var qty6 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent6TextBox"); 
    var qty7 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent7TextBox"); 
    var qty8 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent8TextBox"); 
    var qty9 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent9TextBox"); 
    var qty10 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent10TextBox"); 
    var qty11 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent11TextBox"); 
    var qty12 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent12TextBox"); 
    var qty13 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent13TextBox"); 
    var qty14 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent14TextBox"); 
    var qty15 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent15TextBox");
   
    var msf2 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label2");
    var msf3 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label3");
    var msf4 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label4");
    var msf5 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label5");
    var msf6 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label6");
    var msf7 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label7");
    var msf8 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label8");
    var msf9 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label9");
    var msf10 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label10");
    var msf11 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label11");
    var msf12 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label12");
    var msf13 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label13");
    var msf14 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label14");
    var msf15 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_Label15"); 
    
//     if( type == 4)
//        lvyld =1;
        lvyld =1;

        var total = ((qty1.value * tlen * twid / 144) * lvyld / 1000 );        
        msf1.innerText =   total.toFixed(4); 
        msf2.innerText =  ((qty2.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf3.innerText =  ((qty3.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);           
        msf4.innerText =  ((qty4.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf5.innerText =  ((qty5.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf6.innerText =  ((qty6.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf7.innerText =  ((qty7.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf8.innerText =  ((qty8.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf9.innerText =  ((qty9.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf10.innerText =  ((qty10.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf11.innerText =  ((qty11.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf12.innerText =  ((qty12.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf13.innerText =  ((qty13.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf14.innerText =  ((qty14.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
        msf15.innerText =  ((qty15.value * tlen * twid / 144) * lvyld / 1000 ).toFixed(4);     
    
}
function showqty()
{
    var savebutton = document.getElementById("buttondiv");
    savebutton.style.display = 'inline';     
  var show=document.getElementById("displayqty");
  show.style.display='inline';
  var originalqty=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstQtyTextBox");
  var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent1TextBox");
  if(originalqty.value<=0)
  {
  alert("Qty must be greater than 0");
  originalqty.focus();
  }
  else
  {
  qty.value=originalqty.value;
  qty.focus();
  }
 }
 function reverseqty()
 {
  
  var originalqty=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstQtyTextBox");
  var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vQtyExtent1TextBox");
  originalqty.value=qty.value;
 }

function estdateval()
{
    var duedate=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox").value;
    
    if(duedate.length>1 && duedate.length<3 && duedate.indexOf('/')!=1)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox").value = duedate + "/";
    }
    if(duedate.length>4 && duedate.length<6 && duedate.indexOf('/')!=3)
    {
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox").value = duedate + "/";
    }
}

function flutelookup(){ 
  var NewWindow = window.open("flute_lookup.aspx","FluteLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function FluteLookup(ReturnObj1){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox.value = ReturnObj1;
  }
  
  function customerlook(){ 
  var NewWindow = window.open("customer_lookup.aspx","CustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerLookup(ReturnObj1){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vShipToTextBox.value = ReturnObj1;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox").focus();
}

function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFgItemTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFgItemTextBox").focus();
}

function custpartlook(){ 
  var NewWindow = window.open("custpart_lookup.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustPartLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustPartTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustPartTextBox").focus();
}


function stylelook(){ 
var style1 = "1";
  var NewWindow = window.open("corstyle_lookup.aspx?style="+style1+"","StyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorStyleLookup(ReturnObj1,ReturnObj2,ReturnObj3){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vBoardTextBox.value = ReturnObj3;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox").focus();
}

function Boardlook1() {
    var style = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox");
    if (style.value == "") {
        alert("Style may not be blank");
        style.focus();
    }
    else {
        if (document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstLabel")) {
            var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstLabel").innerText;
        }
        else {
            var est1 = "";
        }
        var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox").value;

        var NewWindow = window.open("corboard_lookup.aspx?est=" + est1 + "&style=" + style1 + "", "BoardWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
}

function CorBoardLookup(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8){ 
   if(ReturnObj1.indexOf(":"))
    {
    var val1=ReturnObj1;    
    ReturnObj1=val1.replace(":", "\"");    
    }
  if(ReturnObj2.indexOf(":"))
    {
    var val2=ReturnObj2;    
    ReturnObj2=val2.replace(":", "\"");    
    }
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCaliperTextBox.value = ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldCaliper.value=ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vWidthTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vLenghtTextBox.value = ReturnObj4;
  document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vBoardTextBox").focus();
}
 function categorylookup(){ 
  var NewWindow =window.open("CategoryLookup.aspx","CategoryWindow","width=600,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookUp(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCategoryTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCategoryTextBox").focus();
  
}

 function QFgItemlook()
  { 
  var cust1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox").value;
  var NewWindow = window.open("FgItemEstLook.aspx?cust="+cust1+"&type="+"fold"+"","FgItemWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function QFgItemLookUp(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9,ReturnObj10,ReturnObj11,ReturnObj12,ReturnObj13,ReturnObj14)
{ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFgItemTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vItemNameTextBox.value=ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustPartTextBox.value = ReturnObj3;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCategoryTextBox.value=ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vColorTextBox.value = ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCoatingTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vLenghtTextBox.value=ReturnObj8;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vWidthTextBox.value = ReturnObj9;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vDepthTextBox.value = ReturnObj10;
  
// document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_HiddenField1.value=ReturnObj11;
 document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vBoardTextBox.value=ReturnObj12;
 document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCaliperTextBox.value=ReturnObj13;
 document.forms[0].ctl00_ContentPlaceHolder1_HiddenFieldCaliper.value=ReturnObj13;
 document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vDieInTextBox.value = ReturnObj14;
 document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFgItemTextBox").focus();
  
} 
function ShipTOLook(){ 
var lookHidden = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustTextBox").value;
 var NewWindow = window.open("ShipIdCustLook.aspx?look="+lookHidden +"","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ShipToLookup(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vShipToTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vShipToTextBox").focus();
    
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

function testlook(){ 
 var lookHidden1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox").value;
  var NewWindow =window.open("test_Lookup.aspx?look1="+lookHidden1+"","TestLookupWindow","width=500,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function testLookup(ReturnObj1){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vTestTextBox.value = ReturnObj1;  
}

function vallen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vLenghtTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vLenghtTextBox").value = frontback;
    }
        
   }
 function valwid()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vWidthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vWidthTextBox").value = frontback;
    }
   }
 function valdep()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vDepthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vDepthTextBox").value = frontback;
    }
   }
   
   function showbutton()
{
    var tab=document.getElementById("showbutton");
    var rs_button=document.getElementById("btn_reset");
    rs_button.style.display='inline';
    var s_button=document.getElementById("btn_set");
    s_button.style.display='none';
    if(tab.style.display='none')
        tab.style.display='inline';
    
}
function showbutton2()
{
    var tab=document.getElementById("showbutton");
    if(tab.style.display='inline')
        tab.style.display='none';
    var rs_button=document.getElementById("btn_reset");
    rs_button.style.display='none';
    var s_button=document.getElementById("btn_set");
    s_button.style.display='inline';
}

function Setlook()
  { 
  var est = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstLabel").innerText;
  var NewWindow = window.open("set_update_est.aspx?setype="+ 2 +"&estno="+est+"","SetWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
  
            
        function mediumlook()
        {
            var code = "1";
            var item = "P";
            var NewWindow = window.open("corr_bom_laminate_lookup1.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=520,height=550,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup1(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }

            document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox.value = ReturnObj1;
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vFluteTextBox").focus();            
        }
        function linerlook()
        {
            var code = "1";
            var item = "P";
            var NewWindow = window.open("corr_bom_laminate_lookup2.aspx?code="+code+"&item="+item+"","PaperLookupWindow","width=520,height=550,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ItemLaminateLookup2(ReturnObj1, ReturnObj2, ReturnObj3)
        { 
            if(ReturnObj1.indexOf(":"))
            {
                var val=ReturnObj1;    
                ReturnObj1=val.replace(":", "\"");    
            }
            document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vTestTextBox.value = ReturnObj1;
            document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vTestTextBox").focus();   
        }

        function setestdate() {
            
            var da = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox");
            da.focus();

        }
        function defaultVal(val) {
            var str = "ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_" + val;
            qtyval = eval("document.forms[0]." + str + ".value");

            if (qtyval <= 0) {
                showsavebutton();
                stylefocus();
            }
        }

        function blankcustpart() {
            var custpart = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCustPartTextBox");
            if (custpart.value == "") {
                alert("Cust Part# may not be blank...");
                custpart.focus();
            }
        }       

</script>
<script>
    function blankstyle() {
        var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vStyleTextBox");
        if (style1.value == "") {
            alert("Style may not be blank...");
            style1.focus();
        }
    }
    function blankboard() {
        var board = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vBoardTextBox");
        if (board.value == "") {
            alert("Board may not be blank...");
            board.focus();
        }
    }
    function blankcat() {
        var board = document.getElementById("ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vCategoryTextBox");
        if (board.value == "") {
            alert("Category may not be blank...");
            board.focus();
        }
    }
</script>

    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />
     <asp:HiddenField ID="HiddenFieldCaliper" runat="server" />
     <asp:HiddenField ID="HiddenField4" runat="server" />
    
    <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource_list" OnDataBound="Gridviewdatabound"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
            </asp:CommandField>
            <asp:BoundField DataField="vEst" ItemStyle-Wrap="false" HeaderText="Est" SortExpression="vEst" />
            <asp:BoundField DataField="vCust" ItemStyle-Wrap="false" HeaderText="Cust" SortExpression="vCust" />
            <asp:BoundField DataField="vCustPart" ItemStyle-Wrap="false" HeaderText="CustPart" SortExpression="vCustPart" />
            <asp:BoundField DataField="vShipTo" ItemStyle-Wrap="false" HeaderText="ShipTo" SortExpression="vShipTo" />
            <asp:BoundField DataField="vItemName" ItemStyle-Wrap="false" HeaderText="ItemName" SortExpression="vItemName" />
            <asp:BoundField DataField="vFgItem" ItemStyle-Wrap="false" HeaderText="FgItem" SortExpression="vFgItem" />
            <asp:BoundField DataField="vEstQty" ItemStyle-Wrap="false" HeaderText="EstQty" SortExpression="vEstQty" />
            <asp:BoundField DataField="vStyle" ItemStyle-Wrap="false" HeaderText="Style" SortExpression="vStyle" />
            <asp:BoundField DataField="vPaper1" ItemStyle-Wrap="false" HeaderText="Paper 1" SortExpression="vPaper1" />
            <asp:BoundField DataField="vPaper2" ItemStyle-Wrap="false" HeaderText="Paper 2" SortExpression="vPaper2" />
            <asp:BoundField DataField="vBoard" ItemStyle-Wrap="false" HeaderText="Board" SortExpression="vBoard" />
            <asp:BoundField DataField="vCaliper" ItemStyle-Wrap="false" HeaderText="Caliper" SortExpression="vCaliper" />
            <asp:BoundField DataField="vCategory" ItemStyle-Wrap="false" HeaderText="Category" SortExpression="vCategory" />
            <asp:BoundField DataField="vLenght" ItemStyle-Wrap="false" HeaderText="Length" SortExpression="vLenght" />
            <asp:BoundField DataField="vWidth" ItemStyle-Wrap="false" HeaderText="Width" SortExpression="vWidth" />
            <asp:BoundField DataField="vDepth" ItemStyle-Wrap="false" HeaderText="Depth" SortExpression="vDepth" />
            <asp:BoundField DataField="vForm" ItemStyle-Wrap="false" HeaderText="S" SortExpression="vForm" />
            <asp:BoundField DataField="vBlank" ItemStyle-Wrap="false" HeaderText="B" SortExpression="vBlank" />
            <asp:BoundField DataField="vTab" ItemStyle-Wrap="false" HeaderText="Tab" SortExpression="vTab" />
            <asp:BoundField DataField="vColor" ItemStyle-Wrap="false" HeaderText="Color" SortExpression="vColor" />
            <%--<asp:BoundField DataField="vPasses" ItemStyle-Wrap="false" HeaderText="Passes" SortExpression="vPasses" />--%>
            <asp:BoundField DataField="vCoating" ItemStyle-Wrap="false" HeaderText="Coating" SortExpression="vCoating" />
            <asp:BoundField DataField="vNoW" ItemStyle-Wrap="false" HeaderText="# on Width" SortExpression="vNoW" />
            <asp:BoundField DataField="vNoL" ItemStyle-Wrap="false" HeaderText="# on Length" SortExpression="vNoL" />
            <asp:BoundField DataField="vUp" ItemStyle-Wrap="false" HeaderText="# Up" SortExpression="vUp" />
            
            
            <asp:BoundField DataField="vDieIn" ItemStyle-Wrap="false" HeaderText="Die Inches" SortExpression="vDieIn" />
            <asp:BoundField DataField="vQtySet" ItemStyle-Wrap="false" HeaderText="QtySet" SortExpression="vQtySet" />
            <asp:BoundField DataField="vInkFrom" ItemStyle-Wrap="false" HeaderText="InkFrom" SortExpression="vInkFrom" />
            <asp:BoundField DataField="vPassesFrom" HeaderText="PassesFrom" SortExpression="vPassesFrom" />
            <asp:BoundField DataField="vCoatingFrom" ItemStyle-Wrap="false" HeaderText="CoatingFrom" SortExpression="vCoatingFrom" />
            <asp:BoundField DataField="vCoatPassesFrom" ItemStyle-Wrap="false" HeaderText="CoatPassesFrom" SortExpression="vCoatPassesFrom" />
            <asp:BoundField DataField="vPurchManuf" ItemStyle-Wrap="false" HeaderText="PurchManuf" SortExpression="vPurchManuf" />
            <asp:BoundField DataField="vEstDate" ItemStyle-Wrap="false" HeaderText="EstDate" SortExpression="vEstDate" HtmlEncode="false" DataFormatString="{0:MM/dd/yyyy}" />
            <%--<asp:BoundField DataField="vefbrowseval" Visible="false" ItemStyle-Wrap="false" HeaderText="Ef Browse" SortExpression="vefbrowseval" />--%>
            
            <asp:TemplateField Visible="false">
                <ItemTemplate>
                    <asp:Label runat="server" ID="vefbrowseval" Text='<%# Bind("vefbrowseval") %>'></asp:Label>
                </ItemTemplate>
            </asp:TemplateField>
            
        </Columns>
            
            </asp:GridView>
    <br />
    
    <asp:Button ID="newaddButton" OnClick="new_button_Click" CssClass="button"  runat="server" Text="Add" />    
    
    <asp:FormView ID="FormView_CorrugatedEstimate" Width="100%" runat="server" OnDataBound="formview_databound" DataSourceID="CorrugatedEstimateDataSource" OnUnload="Formview_corrugated_unload" OnPreRender="FormView_CorrugatedEstimate_PreRender">
        <EditItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server" BorderColor="#ACA899" BorderWidth="1px" DefaultButton="UpdateButton">
             <table class="shade">
            <tr><td>            
            <table class="shade">
                <tr>
                    <td colspan="2" align="left" nowrap style="padding-right:5px"><b>Estimate:</b>&nbsp;&nbsp;&nbsp;
                    <b>
                        <asp:Label Width="100px" BackColor="Turquoise" ID="vEstLabel" runat="server" Text='<%# Bind("vEst") %>'>
                        </asp:Label>
                    </b></td>                    
                </tr>                                                                
                
                <tr>
                    <td nowrap><b>EstDate:</b><br /><b>                        
                        <asp:TextBox ID="vEstDateTextBox" Width="60px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10"  runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'>
                        </asp:TextBox><a href="#" onblur="setestdate()" tabindex="1"  onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                        
                    </b></td>
                    <td nowrap><b>Customer:</b><br />                    
                    <b>
                        <asp:TextBox ID="vCustTextBox" Width="65px" runat="server" Text='<%# Bind("vCust") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>                                                          
                    <td nowrap><b>Ship To:</b><br /><b>
                        <asp:TextBox ID="vShipToTextBox" Width="65px" runat="server" Text='<%# Bind("vShipTo") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td> 
                    <td nowrap><b>Cust Part:</b><br /><b>
                        <asp:TextBox ID="vCustPartTextBox" Width="100px" MaxLength="15"  runat="server" Text='<%# Bind("vCustPart") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>                                  
                    <td nowrap><b>Item Name:</b><br /><b>
                        <asp:TextBox ID="vItemNameTextBox" Width="120px" MaxLength="30" onfocus="blankcustpart()" runat="server" Text='<%# Bind("vItemName") %>'>
                        </asp:TextBox>
                    </b></td>
                    <td nowrap><b>FgItem:</b><br /><b>
                        <asp:TextBox ID="vFgItemTextBox" Width="120px" MaxLength="15" runat="server" Text='<%# Bind("vFgItem") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>               
                    <td nowrap><b>EstQty:</b><br /><b>
                        <asp:TextBox ID="vEstQtyTextBox" MaxLength="8" Width="60px" onblur="showqty()" runat="server" Text='<%# Bind("vEstQty") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vEstQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>
                    <td nowrap><b>Style:</b><br /><b>
                        <asp:TextBox ID="vStyleTextBox" Width="36px"  MaxLength="5" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("vStyle") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td> 
                    <td nowrap><b>Board:</b><br /><b>
                        <asp:TextBox ID="vBoardTextBox" Width="65px" MaxLength="10"  onfocus="blankstyle()" AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("vBoard") %>'>                                                
                        </asp:TextBox>
                        <a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>              
                    <td nowrap><b><asp:Label runat="server" ID="paper1id" Text="Paper1:"></asp:Label></b><br /><b>
                        <asp:TextBox ID="vFluteTextBox" Width="65px" onfocus="blankboard()" MaxLength="10" runat="server" Text='<%# Bind("vPaper1") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="mediumlook(); return false"><asp:Image ID="FluteLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    </b></td>
                    <td nowrap><b><asp:Label runat="server" ID="paper2id" Text="Paper2:"></asp:Label></b><br /><b>
                        <asp:TextBox ID="vTestTextBox" Width="65px" MaxLength="10" runat="server" Text='<%# Bind("vPaper2") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="linerlook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>                                  
                    <td nowrap><b>Caliper:</b><br /><b>
                        <asp:TextBox ID="vCaliperTextBox" Width="40px" Enabled="false" runat="server" Text='<%# Bind("vCaliper") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vCaliperTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>               
                    <td nowrap><b>Category:</b><br /><b>
                        <asp:TextBox ID="vCategoryTextBox" onblur="leavecategory(this);" onfocus="blankboard();this.select();" AutoPostBack="true" OnTextChanged="CategoryTextChanged" Width="50px" runat="server" Text='<%# Bind("vCategory") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="categorylookup(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>
                    <td nowrap><b>Length:</b><br /><b>
                        <asp:TextBox ID="vLenghtTextBox" Width="43px" onkeyup="vallen()" onfocus="blankcat();this.select();" MaxLength="8" runat="server" Text='<%# Bind("vLenght") %>'>
                        </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator2" Display="dynamic" ControlToValidate="vLenghtTextBox" runat="server" ErrorMessage="Length must be enter"></asp:RequiredFieldValidator>
                        <asp:RangeValidator ID="RangeValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vLenghtTextBox" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Length must be greater than 0"></asp:RangeValidator>                               
                    </b></td>               
                    <td nowrap><b>Width:</b><br /><b>
                         <asp:TextBox ID="vWidthTextBox" Width="43px" onkeyup="valwid()" onblur="confirmAdd()" MaxLength="8" runat="server" Text='<%# Bind("vWidth") %>'>
                         </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator1" Display="dynamic" ControlToValidate="vWidthTextBox" runat="server" ErrorMessage="Width must be enter"></asp:RequiredFieldValidator>
                         <asp:RangeValidator ID="RangeValidator2" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Width must be greater than 0"></asp:RangeValidator>
                         
                    </b></td>
                    <td nowrap><b>Depth:</b><br /><b>
                        <asp:TextBox ID="vDepthTextBox" Width="43px" onkeyup="valdep()" MaxLength="8" runat="server" Text='<%# Bind("vDepth") %>'>
                        </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator3" Display="dynamic" ControlToValidate="vDepthTextBox" runat="server" ErrorMessage="Depth must be enter"></asp:RequiredFieldValidator>
                        <asp:RangeValidator ID="RangeValidator3" runat="server" SetFocusOnError="true" ControlToValidate="vDepthTextBox" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Depth must be greater than 0"></asp:RangeValidator>
                    </b></td>   
                    <td nowrap><b>QtySet:</b><br /><b>
                        <asp:TextBox ID="vQtySetTextBox" MaxLength="7" Width="41px" runat="server" Text='<%# Bind("vQtySet") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vQtySetTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td> 
                    <td nowrap><b>Color:</b><br /><b>
                        <asp:TextBox ID="vColorTextBox" Width="30px" MaxLength="2" onkeyup="changecolor(2)" runat="server" Text='<%# Bind("vColor") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vColorTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>
                    <td nowrap><b>Coating:</b><br /><b>
                        <asp:TextBox ID="vCoatingTextBox" Width="41px" MaxLength="2" onkeyup="changecoating(2)"  runat="server" Text='<%# Bind("vCoating") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vCoatingTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>          
                    <td nowrap><b>S:</b><br /><b>
                        <asp:Label ID="vFormTextBox" Width="20px" Height="20px" runat="server" Text='<%# Bind("vForm") %>'>
                        </asp:Label>
                    </b></td>
                    <td nowrap><b>B:</b><br /><b>
                        <asp:Label ID="vBlankTextBox" Width="20px" Height="20px" runat="server" Text='<%# Bind("vBlank") %>'>
                        </asp:Label>
                    </b></td>                                          
                    <td nowrap><b>#Up:</b><br /><b>
                        <asp:Label ID="vUpLabel" runat="server" Height="20px" Text='<%# Bind("vUp") %>' Width="30px">   
                        </asp:Label>
                    </b></td>
                                    
                    <td nowrap><b># on Width:</b><br /><b>
                        <asp:Label ID="vNoWLabel" runat="server" Height="20px" Text='<%# Bind("vNoW") %>'  Width="70px">   
                        </asp:Label>
                    </b></td>
                    <td nowrap><b># on Length:</b><br /><b>
                        <asp:Label ID="vNoLLabel" runat="server" Height="20px" Text='<%# Bind("vNoL") %>'  Width="70px">   
                        </asp:Label>
                    </b></td>     
                    <td nowrap><b>Die Inches:</b><br /><b>
                        <asp:TextBox ID="vDieInTextBox" MaxLength="9" Width="55px" runat="server" Text='<%# Bind("vDieIn") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vDieInTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>                                                                                                                         
                    <td nowrap><b>Ink/Form:</b><br /><b>
                        <asp:Label ID="vInkFromTextBox" Width="60px" runat="server" Text='<%# Bind("vInkFrom") %>'>
                        </asp:Label>
                    </b></td>
                    <td nowrap><b>Passes/Form:</b><br /><b>
                        <asp:Label ID="vPassesFromTextBox" Width="80px" runat="server" Text='<%# Bind("vPassesFrom") %>'>
                        </asp:Label>             
                    <td nowrap><b>Coatings/Form:</b><br /><b>
                        <asp:Label ID="vCoatingFromTextBox" runat="server" Text='<%# Bind("vCoatingFrom") %>'>
                        </asp:Label>
                    </b></td>
                    <td nowrap><b>CoatPasses/Form:</b><br /><b>
                        <asp:Label ID="vCoatPassesFromTextBox" Width="100px" runat="server" Text='<%# Bind("vCoatPassesFrom") %>'>
                        </asp:Label>
                    </b></td>                                                                                            
                   <td nowrap><b>Purch/Manuf:</b><br /><b>
                        <asp:DropDownList Width="80px" ID="DropDownList2" onblur="window.scroll(0,0);" SelectedValue='<%# Bind("vPurchManuf") %>' DataValueField= '<%# Bind("vPurchManuf") %>'  runat="server">
                        <asp:ListItem Text="M" Value="M" ></asp:ListItem>
                        <asp:ListItem Text="P" Value="P" ></asp:ListItem>                        
                        </asp:DropDownList>
                    </b></td>                                                                    
                </tr>
            </table></td></tr>
            
            <tr><td>           
           <fieldset id="displayqty" class="shade"   style="display:none;">
           <table >
           <tr><td><b>Quantity</b></td>
           <td><b>Releases</b></td>
           <td><b>Qty MSF</b></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent1TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="reverseqty()" runat="server" Text='<%# Bind("vQtyExtent1") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vQtyExtent1TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td>  <asp:TextBox ID="vRelQtyExtent1TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent1") %>'> </asp:TextBox></td>
           <td><asp:Label ID="Label1" runat="server" ></asp:Label>            
           <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vRelQtyExtent1TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
           </td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent2TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent2TextBox')" runat="server" Text='<%# Bind("vQtyExtent2") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vQtyExtent2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent2TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent2") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vRelQtyExtent2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label2" runat="server" ></asp:Label>
           
           </td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent3TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent3TextBox')" runat="server" Text='<%# Bind("vQtyExtent3") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vQtyExtent3TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent3TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent3") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vRelQtyExtent3TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label3" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent4TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent4TextBox')" runat="server" Text='<%# Bind("vQtyExtent4") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vQtyExtent4TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent4TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent4") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator25" runat="server" ControlToValidate="vRelQtyExtent4TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label4" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent5TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent5TextBox')" runat="server" Text='<%# Bind("vQtyExtent5") %>'>  </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator26" runat="server" ControlToValidate="vQtyExtent5TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent5TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent5") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator27" runat="server" ControlToValidate="vRelQtyExtent5TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label5" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent6TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent6TextBox')" runat="server" Text='<%# Bind("vQtyExtent6") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator28" runat="server" ControlToValidate="vQtyExtent6TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent6TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent6") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator29" runat="server" ControlToValidate="vRelQtyExtent6TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label6" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent7TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent7TextBox')" runat="server" Text='<%# Bind("vQtyExtent7") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator30" runat="server" ControlToValidate="vQtyExtent7TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent7TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent7") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator31" runat="server" ControlToValidate="vRelQtyExtent7TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label7" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent8TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent8TextBox')" runat="server" Text='<%# Bind("vQtyExtent8") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator32" runat="server" ControlToValidate="vQtyExtent8TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent8TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent8") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator33" runat="server" ControlToValidate="vRelQtyExtent8TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label8" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent9TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent9TextBox')" runat="server" Text='<%# Bind("vQtyExtent9") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator34" runat="server" ControlToValidate="vQtyExtent9TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent9TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent9") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator35" runat="server" ControlToValidate="vRelQtyExtent9TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label9" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent10TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent10TextBox')" runat="server" Text='<%# Bind("vQtyExtent10") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator36" runat="server" ControlToValidate="vQtyExtent10TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent10TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent10") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator37" runat="server" ControlToValidate="vRelQtyExtent10TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label10" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent11TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent11TextBox')" runat="server" Text='<%# Bind("vQtyExtent11") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator38" runat="server" ControlToValidate="vQtyExtent11TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent11TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent11") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator39" runat="server" ControlToValidate="vRelQtyExtent11TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label11" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent12TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent12TextBox')" runat="server" Text='<%# Bind("vQtyExtent12") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator40" runat="server" ControlToValidate="vQtyExtent12TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent12TextBox" MaxLength="8" Width="80"  runat="server" Text='<%# Bind("vRelQtyExtent12") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator41" runat="server" ControlToValidate="vRelQtyExtent12TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label12" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent13TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent13TextBox')" runat="server" Text='<%# Bind("vQtyExtent13") %>'>  </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator42" runat="server" ControlToValidate="vQtyExtent13TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent13TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent13") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator43" runat="server" ControlToValidate="vRelQtyExtent13TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label13" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent14TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent14TextBox')" runat="server" Text='<%# Bind("vQtyExtent14") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator44" runat="server" ControlToValidate="vQtyExtent14TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent14TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent14") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator45" runat="server" ControlToValidate="vRelQtyExtent14TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label14" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent15TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="defaultVal('vQtyExtent15TextBox')" runat="server" Text='<%# Bind("vQtyExtent15") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator46" runat="server" ControlToValidate="vQtyExtent15TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent15TextBox" MaxLength="8" Width="80" onblur="style2focus()" runat="server" Text='<%# Bind("vRelQtyExtent15") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator47" runat="server" ControlToValidate="vRelQtyExtent15TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label15" runat="server" ></asp:Label></td></tr>
           <tr><td style="display:none">
           <asp:Label ID="tlenlabel" runat="server" Text='<%# Bind("vTlen") %>' ></asp:Label>
           <asp:Label ID="twidlabel" runat="server" Text='<%# Bind("vTwid") %>' ></asp:Label> 
           <asp:Label ID="vEstTypeLabel" runat="server"  Text='<%# Bind("vEstType") %>'></asp:Label>          
           </td></tr>
           
                      
           </table></fieldset>
           </td></tr></table>    
            
            <div id="buttondiv">
            <asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" OnClick="update_corr_Click" CausesValidation="True" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="btn_Form_estimate" CssClass="buttonM" runat="server" OnClick="Form_estimate_Click" CausesValidation="True" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="BlankButton" CssClass="buttonM" runat="server" OnClick="blank_save_Click" CausesValidation="True" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </div>
            </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server" BorderColor="#ACA899"  BorderWidth="1px" DefaultButton="InsertButton">
            <table class="shade">
            <tr><td>                 
            <table class="shade">
                <tr> <td colspan="5"><b>Estimate#:</b> &nbsp;&nbsp;<asp:Label ID="EatimateLabel" runat="server" Width="60px" BackColor="Turquoise"></asp:Label>
                &nbsp;&nbsp;&nbsp;&nbsp;
                    <b>Type:</b>&nbsp;
                    <b>
                        <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="3" runat="server">
                        <asp:ListItem Text="Single" Selected="True" ></asp:ListItem>
                        <asp:ListItem Text="Sets" ></asp:ListItem>
                        <asp:ListItem Text="Tandem" ></asp:ListItem>
                        </asp:RadioButtonList>
                    </b></td>                    
                </tr>
                <tr>                                                
                    <td nowrap><b>EstDate:</b><br /><b>                        
                        <asp:TextBox ID="vEstDateTextBox" Width="60px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'>
                        </asp:TextBox>
                        <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                                                                        
                    </b></td>
                    <td nowrap><b>Customer:</b><br /><b>
                        <asp:TextBox ID="vCustTextBox" Width="65px" runat="server" onkeyup="samevalue()" onblur="samevalue()" Text='<%# Bind("vCust") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="customerlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>
                    <td nowrap><b>Ship To:</b><br /><b>
                        <asp:TextBox ID="vShipToTextBox" Width="65px" runat="server" Text='<%# Bind("vShipTo") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>
                    <td nowrap><b>Cust Part:</b><br /><b>
                        <asp:TextBox ID="vCustPartTextBox" Width="100px" MaxLength="15" runat="server" Text='<%# Bind("vCustPart") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>               
                    <td nowrap><b>Item Name:</b><br /><b>
                        <asp:TextBox ID="vItemNameTextBox" Width="120px" MaxLength="30" onfocus="blankcustpart()" runat="server" Text='<%# Bind("vItemName") %>'>
                        </asp:TextBox>
                    </b></td>
                    <td nowrap><b>FgItem:</b><br /><b>
                        <asp:TextBox ID="vFgItemTextBox" Width="120px" MaxLength="15" runat="server" Text='<%# Bind("vFgItem") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>               
                    <td nowrap><b>EstQty:</b><br /><b>
                        <asp:TextBox ID="vEstQtyTextBox" MaxLength="8" Width="60px" onblur="showqty()" runat="server" Text='<%# Bind("vEstQty") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vEstQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="vEstQtyTextBox" Display="Dynamic" runat="server" ErrorMessage="Enter the Quantity"></asp:RequiredFieldValidator>
                    </b></td>
                    <td nowrap><b>Style:</b><br /><b>
                        <asp:TextBox ID="vStyleTextBox" Width="36px"  MaxLength="5" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("vStyle") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>     
                     <td nowrap><b>Board:</b><br /><b>
                        <asp:TextBox ID="vBoardTextBox" Width="65px"  MaxLength="10" onfocus="blankstyle()"  AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("vBoard") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>         
                    <td nowrap><b><asp:Label runat="server" ID="paper1id" Text="Paper1:"></asp:Label></b><br /><b>
                        <asp:TextBox ID="vFluteTextBox" Width="65px" onfocus="blankboard()" MaxLength="10" runat="server" Text='<%# Bind("vPaper1") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="mediumlook(); return false"><asp:Image ID="FluteLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                    </b></td>
                    <td nowrap><b><asp:Label runat="server" ID="paper2id" Text="Paper2:"></asp:Label></b><br /><b>
                        <asp:TextBox ID="vTestTextBox" Width="65px" MaxLength="10" runat="server" Text='<%# Bind("vPaper2") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="linerlook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>               
                   
                    <td nowrap><b>Caliper:</b><br /><b>
                        <asp:TextBox ID="vCaliperTextBox" Width="40px" Enabled="false" runat="server" Text='<%# Bind("vCaliper") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vCaliperTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>                
                    <td nowrap><b>Category:</b><br /><b>
                        <asp:TextBox ID="vCategoryTextBox" onblur="leavecategory(this);" onfocus="blankboard()" Width="50px" AutoPostBack="true" OnTextChanged="CategoryTextChanged" runat="server" Text='<%# Bind("vCategory") %>'>
                        </asp:TextBox><a href="#" tabindex="1" onClick="categorylookup(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                    </b></td>
                    <td nowrap><b>Length:</b><br /><b>
                        <asp:TextBox ID="vLenghtTextBox" Width="43px" onkeyup="vallen()" onfocus="blankcat();this.select();" MaxLength="8" runat="server" Text='<%# Bind("vLenght") %>'>
                        </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator2" Display="dynamic" ControlToValidate="vLenghtTextBox" runat="server" ErrorMessage="Length must be enter"></asp:RequiredFieldValidator>
                        <asp:RangeValidator ID="RangeValidator1" runat="server" SetFocusOnError="true" ControlToValidate="vLenghtTextBox" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Length must be greater than 0"></asp:RangeValidator>
                    </b></td>               
                    <td nowrap><b>Width:</b><br /><b>
                         <asp:TextBox ID="vWidthTextBox" Width="43px" onkeyup="valwid()" onblur="confirmAdd()" MaxLength="8" runat="server" Text='<%# Bind("vWidth") %>'>
                         </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator1" Display="dynamic" ControlToValidate="vWidthTextBox" runat="server" ErrorMessage="Width must be enter"></asp:RequiredFieldValidator>
                         <asp:RangeValidator ID="RangeValidator2" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Width must be greater than 0"></asp:RangeValidator>
                    </b></td>
                    <td nowrap><b>Depth:</b><br /><b>
                        <asp:TextBox ID="vDepthTextBox" Width="43px" onkeyup="valdep()" MaxLength="8" runat="server" Text='<%# Bind("vDepth") %>'>
                        </asp:TextBox><asp:RequiredFieldValidator ID="RequiredFieldValidator3" Display="dynamic" ControlToValidate="vDepthTextBox" runat="server" ErrorMessage="Depth must be enter"></asp:RequiredFieldValidator>
                        <asp:RangeValidator ID="RangeValidator3" runat="server" SetFocusOnError="true" ControlToValidate="vDepthTextBox" Display="dynamic"  Type="Double" MinimumValue="1" MaximumValue="100"  ErrorMessage="Depth must be greater than 0"></asp:RangeValidator>
                    </b></td>                
                    <td nowrap><b>QtySet:</b><br /><b>
                        <asp:TextBox ID="vQtySetTextBox" MaxLength="7"  Width="41px" runat="server" Text='<%# Bind("vQtySet") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator24" runat="server" ControlToValidate="vQtySetTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>                                                                                                                                                                                      
                    <td nowrap><b>Color:</b><br /><b>
                        <asp:TextBox ID="vColorTextBox" Width="30px" MaxLength="2" onkeyup="changecolor(1)" runat="server" Text='<%# Bind("vColor") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator20" runat="server" ControlToValidate="vColorTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>
                    <td nowrap><b>Coating:</b><br /><b>
                        <asp:TextBox ID="vCoatingTextBox" Width="41px" MaxLength="2" onkeyup="changecoating(1)"  runat="server" Text='<%# Bind("vCoating") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator22" runat="server" ControlToValidate="vCoatingTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>   
                    <td nowrap><b>S:</b><br /><b>                        
                        <asp:TextBox ID="vFormTextBox" Enabled="false" Width="20px" runat="server" Text='<%# Bind("vForm") %>'></asp:TextBox>                                                
                    </b></td>
                    <td nowrap><b>B:</b><br /><b>                    
                        <asp:TextBox ID="vBlankTextBox" Enabled="false" Width="20px" runat="server" Text='<%# Bind("vBlank") %>'></asp:TextBox>                                                                                            
                    </b></td>       
                    <td nowrap><b>#Up:</b><br /><b>                    
                        <asp:TextBox ID="vUpTextBox" Enabled="false" Width="20px" runat="server" Text='<%# Bind("vUp") %>'></asp:TextBox>                                           
                    </b></td>                                                                                              
                    <td nowrap><b># on Width:</b><br /><b>                                               
                        <asp:TextBox ID="vNoWTextBox" Enabled="false" Width="60px" runat="server" Text='<%# Bind("vNoW") %>'></asp:TextBox>                        
                    </b></td>
                    <td nowrap><b># on Length:</b><br /><b>                    
                        <asp:TextBox ID="vNoLTextBox" Enabled="false" Width="60px" runat="server" Text='<%# Bind("vNoL") %>'></asp:TextBox>                                            
                    </b></td> 
                                                                                                                                                          
                    <td nowrap><b>Die Inches:</b><br /><b>
                        <asp:TextBox ID="vDieInTextBox" MaxLength="9" Width="55px" runat="server" Text='<%# Bind("vDieIn") %>'>
                        </asp:TextBox><asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vDieInTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
                    </b></td>                                        
                    <td nowrap><b>Ink/Form:</b><br /><b>
                        <asp:TextBox ID="vInkFromTextBox" Enabled="false" Width="45px" runat="server" Text='<%# Bind("vInkFrom") %>'></asp:TextBox>                                            
                    </b></td>
                    <td nowrap><b>Passes/Form:</b><br /><b>
                        <asp:TextBox ID="vPassesFromTextBox" Enabled="false" Width="70px" runat="server" Text='<%# Bind("vPassesFrom") %>'></asp:TextBox>                        
                    </b></td>                
                    <td nowrap><b>Coatings/Form:</b><br /><b>
                        <asp:TextBox ID="vCoatingFromTextBox" Enabled="false" Width="75px" runat="server" Text='<%# Bind("vCoatingFrom") %>'></asp:TextBox>                        
                    </b></td>
                    <td nowrap><b>CoatPasses/Form:</b><br /><b>
                        <asp:TextBox ID="vCoatPassesFromTextBox" Enabled="false" Width="90px" runat="server" Text='<%# Bind("vCoatPassesFrom") %>'></asp:TextBox>                        
                    </b></td>
                    
                    
                    <td nowrap><b>Purch/Manuf:</b><br /><b>
                        <asp:DropDownList Width="80px" ID="DropDownList2" onblur="window.scroll(0,0);" SelectedValue='<%# Bind("vPurchManuf") %>' DataValueField= '<%# Bind("vPurchManuf") %>'  runat="server">
                        <asp:ListItem Text="M" Value="M" ></asp:ListItem>
                        <asp:ListItem Text="P" Value="P" ></asp:ListItem>                        
                        </asp:DropDownList>
                    </b></td>                    
                
                   
                    
                </tr>
           </table>
           </td></tr><tr><td>
           
            
           <fieldset id="displayqty" class="shade"   style="display:none;">
           <table >
           <tr><td><b>Quantity</b></td>
           <td><b>Releases</b></td>
           <td><b>Qty MSF</b></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent1TextBox" MaxLength="8" Width="80" onkeyup="msfcount()" onblur="reverseqty()" runat="server" Text='<%# Bind("vQtyExtent1") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vQtyExtent1TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td>  <asp:TextBox ID="vRelQtyExtent1TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent1") %>'> </asp:TextBox></td>
           <td><asp:Label ID="Label1" runat="server" ></asp:Label>            
           <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vRelQtyExtent1TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator>
           </td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent2TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent2TextBox')" runat="server" Text='<%# Bind("vQtyExtent2") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vQtyExtent2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent2TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent2") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vRelQtyExtent2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label2" runat="server" ></asp:Label>
           
           </td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent3TextBox" MaxLength="8" Width="80"  runat="server" onblur="defaultVal('vQtyExtent3TextBox')" Text='<%# Bind("vQtyExtent3") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vQtyExtent3TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent3TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent3") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vRelQtyExtent3TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label3" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent4TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent4TextBox')" runat="server" Text='<%# Bind("vQtyExtent4") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vQtyExtent4TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent4TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent4") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator25" runat="server" ControlToValidate="vRelQtyExtent4TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label4" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent5TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent5TextBox')" runat="server" Text='<%# Bind("vQtyExtent5") %>'>  </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator26" runat="server" ControlToValidate="vQtyExtent5TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent5TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent5") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator27" runat="server" ControlToValidate="vRelQtyExtent5TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label5" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent6TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent6TextBox')" runat="server" Text='<%# Bind("vQtyExtent6") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator28" runat="server" ControlToValidate="vQtyExtent6TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent6TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent6") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator29" runat="server" ControlToValidate="vRelQtyExtent6TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label6" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent7TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent7TextBox')" runat="server" Text='<%# Bind("vQtyExtent7") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator30" runat="server" ControlToValidate="vQtyExtent7TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent7TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent7") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator31" runat="server" ControlToValidate="vRelQtyExtent7TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label7" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent8TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent8TextBox')" runat="server" Text='<%# Bind("vQtyExtent8") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator32" runat="server" ControlToValidate="vQtyExtent8TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent8TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent8") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator33" runat="server" ControlToValidate="vRelQtyExtent8TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label8" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent9TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent9TextBox')"  runat="server" Text='<%# Bind("vQtyExtent9") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator34" runat="server" ControlToValidate="vQtyExtent9TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent9TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent9") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator35" runat="server" ControlToValidate="vRelQtyExtent9TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label9" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent10TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent10TextBox')" runat="server" Text='<%# Bind("vQtyExtent10") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator36" runat="server" ControlToValidate="vQtyExtent10TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent10TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent10") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator37" runat="server" ControlToValidate="vRelQtyExtent10TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label10" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent11TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent11TextBox')" runat="server" Text='<%# Bind("vQtyExtent11") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator38" runat="server" ControlToValidate="vQtyExtent11TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent11TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent11") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator39" runat="server" ControlToValidate="vRelQtyExtent11TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label11" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent12TextBox" MaxLength="8" Width="80"  runat="server" onblur="defaultVal('vQtyExtent12TextBox')" Text='<%# Bind("vQtyExtent12") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator40" runat="server" ControlToValidate="vQtyExtent12TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent12TextBox" MaxLength="8" Width="80"  runat="server" Text='<%# Bind("vRelQtyExtent12") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator41" runat="server" ControlToValidate="vRelQtyExtent12TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label12" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent13TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent13TextBox')" runat="server" Text='<%# Bind("vQtyExtent13") %>'>  </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator42" runat="server" ControlToValidate="vQtyExtent13TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent13TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent13") %>'></asp:TextBox>
           <asp:CompareValidator ID="CompareValidator43" runat="server" ControlToValidate="vRelQtyExtent13TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label13" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent14TextBox" MaxLength="8" Width="80" onblur="defaultVal('vQtyExtent14TextBox')" runat="server" Text='<%# Bind("vQtyExtent14") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator44" runat="server" ControlToValidate="vQtyExtent14TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent14TextBox" MaxLength="8" Width="80" runat="server" Text='<%# Bind("vRelQtyExtent14") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator45" runat="server" ControlToValidate="vRelQtyExtent14TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label14" runat="server" ></asp:Label></td></tr>
           
           <tr><td><asp:TextBox ID="vQtyExtent15TextBox" MaxLength="8" Width="80"  runat="server" onblur="defaultVal('vQtyExtent15TextBox')" Text='<%# Bind("vQtyExtent15") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator46" runat="server" ControlToValidate="vQtyExtent15TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:TextBox ID="vRelQtyExtent15TextBox" MaxLength="8" Width="80" onblur="style2focus()" runat="server" Text='<%# Bind("vRelQtyExtent15") %>'> </asp:TextBox>
           <asp:CompareValidator ID="CompareValidator47" runat="server" ControlToValidate="vRelQtyExtent15TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
           <td><asp:Label ID="Label15" runat="server" ></asp:Label></td></tr>
           <tr><td style="display:none">
           <asp:Label ID="tlenlabel" runat="server" Text='<%# Bind("vTlen") %>' ></asp:Label>
           <asp:Label ID="twidlabel" runat="server" Text='<%# Bind("vTwid") %>' ></asp:Label> 
           <asp:Label ID="vEstTypeLabel" runat="server"  Text='<%# Bind("vEstType") %>'></asp:Label>          
           </td></tr>
                      
           
           </table></fieldset>
           </td></tr></table> 
            <div id="buttondiv">
            <asp:Button ID="InsertButton" CssClass="buttonM" runat="server" OnClick="save_corr_Click" CausesValidation="True" 
                Text="Save">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel" OnClick="btn_insert_cancel_click">
            </asp:Button>
            </div>
            </asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table class="shade">
                <tr>
                    <td nowrap><b>Estimate:</b><br /><b>
                        <asp:Label ID="vEstLabel" Width="60px" runat="server" Text='<%# Bind("vEst") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>    
                    <td nowrap><b>EstDate:</b><br /><b>
                        <asp:Label ID="vEstDateLabel" Width="60px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>' BackColor="Turquoise"></asp:Label>
                    </b></td>                
                    <td nowrap><b>Customer:</b><br /><b>
                        <asp:Label ID="vCustLabel" Width="65px" runat="server" Text='<%# Bind("vCust") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td> 
                    <td nowrap><b>Ship To:</b><br /><b>
                        <asp:Label ID="vShipToLabel" Width="65px" runat="server" Text='<%# Bind("vShipTo") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>               
                    <td nowrap><b>CustPart:</b><br /><b>
                        <asp:Label ID="vCustPartLabel" Width="100px" runat="server" Text='<%# Bind("vCustPart") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>                                  
                    <td nowrap><b>Item Name:</b><br /><b>
                        <asp:Label ID="vItemNameLabel" Width="180px" runat="server" Text='<%# Bind("vItemName") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>
                    <td nowrap><b>FgItem:</b><br /><b>
                        <asp:Label ID="vFgItemLabel" Width="120px" runat="server" Text='<%# Bind("vFgItem") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>                
                    <td nowrap><b>EstQty:</b><br /><b>
                        <asp:Label ID="vEstQtyLabel" Width="60px" runat="server" Text='<%# Bind("vEstQty") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>
                    <td nowrap><b>Style:</b><br /><b>
                        <asp:Label ID="vStyleLabel" Width="36px" runat="server" Text='<%# Bind("vStyle") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>   
                    <td nowrap><b>Board:</b><br /><b>
                        <asp:Label ID="vBoardLabel" Width="65px" runat="server" Text='<%# Bind("vBoard") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>            
                    <td nowrap><b><asp:Label runat="server" ID="paper1id" Text="Paper1:"></asp:Label></b><br /><b>
                        <asp:Label ID="vFluteLabel" Width="65px" runat="server" Text='<%# Bind("vPaper1") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>
                    <td nowrap><b><asp:Label runat="server" ID="paper2id" Text="Paper2:"></asp:Label></b><br /><b>
                        <asp:Label ID="vTestLabel" Width="65px" runat="server" Text='<%# Bind("vPaper2") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>                                 
                    <td nowrap><b>Caliper:</b><br /><b>
                        <asp:Label ID="vCaliperLabel" Width="40px" runat="server" Text='<%# Bind("vCaliper") %>' BackColor="Turquoise" ></asp:Label>
                    </b></td>                
                    <td nowrap><b>Category:</b><br /><b>
                        <asp:Label ID="vCategoryLabel" runat="server" Text='<%# Bind("vCategory") %>' BackColor="Turquoise" Width="50px"></asp:Label>
                    </b></td>
                    <td nowrap><b>Length:</b><br /><b>
                        <asp:Label ID="vLenghtLabel" runat="server" Text='<%# Bind("vLenght","{0:###0.00000}") %>' BackColor="Turquoise" Width="43px"></asp:Label>
                    </b></td>               
                    <td nowrap><b>Width:</b><br /><b>
                        <asp:Label ID="vWidthLabel" runat="server" Text='<%# Bind("vWidth","{0:###0.00000}") %>' BackColor="Turquoise" Width="43px"></asp:Label>
                    </b></td>
                    <td nowrap><b>Depth:</b><br /><b>
                        <asp:Label ID="vDepthLabel" runat="server" Text='<%# Bind("vDepth","{0:###0.00000}") %>' BackColor="Turquoise" Width="43px"></asp:Label>
                    </b></td>  
                    <td nowrap><b>QtySet:</b><br /><b>
                        <asp:Label ID="vQtySetLabel" runat="server" Text='<%# Bind("vQtySet") %>' BackColor="Turquoise" Width="41px"></asp:Label>
                    </b></td>  
                    <td nowrap><b>Color:</b><br /><b>
                        <asp:Label ID="vColorLabel" runat="server" Text='<%# Bind("vColor") %>' BackColor="Turquoise" Width="30px"></asp:Label>
                    </b></td>
                    <td nowrap><b>Coating:</b><br /><b>
                        <asp:Label ID="vCoatingLabel" runat="server" Text='<%# Bind("vCoating") %>' BackColor="Turquoise" Width="41px"></asp:Label>
                    </b></td>            
                    <td nowrap><b>S:</b><br /><b>
                        <asp:Label ID="vFormLabel" runat="server" Text='<%# Bind("vForm") %>' BackColor="Turquoise" Width="20px"></asp:Label>
                    </b></td>
                    <td nowrap><b>B:</b><br /><b>
                        <asp:Label ID="vBlankLabel" runat="server" Text='<%# Bind("vBlank") %>' BackColor="Turquoise" Width="20px"></asp:Label>
                    </b></td> 
                    <td nowrap><b>#Up:</b><br /><b>
                        <asp:Label ID="vUpLabel" runat="server" Text='<%# Bind("vUp") %>'  BackColor="Turquoise" Width="30px">   
                        </asp:Label>
                    </b></td>
                                    
                    <td nowrap><b># on Width:</b><br /><b>
                        <asp:Label ID="vNoWLabel" runat="server" Text='<%# Bind("vNoW") %>'  BackColor="Turquoise" Width="70px">   
                        </asp:Label>
                    </b></td>
                    <td nowrap><b># on Length:</b><br /><b>
                        <asp:Label ID="vNoLLabel" runat="server" Text='<%# Bind("vNoL") %>'  BackColor="Turquoise" Width="70px">   
                        </asp:Label>
                    </b></td>   
                    <td nowrap><b>Die Inches:</b><br /><b>
                        <asp:Label ID="vDieInTextBox" runat="server" Text='<%# Bind("vDieIn") %>'  BackColor="Turquoise" Width="55px">   
                        </asp:Label>
                    </b></td>                                                                                        
                    <td nowrap><b>Ink/Form:</b><br /><b>
                        <asp:Label ID="vInkFromLabel" runat="server" Text='<%# Bind("vInkFrom") %>' BackColor="Turquoise" Width="60px"></asp:Label>
                    </b></td>
                    <td nowrap><b>Passes/Form:</b><br /><b>
                        <asp:Label ID="vPassesFromLabel" runat="server" Text='<%# Bind("vPassesFrom") %>' BackColor="Turquoise" Width="80px">
                        </asp:Label>
                    </b></td>                
                    <td nowrap><b>Coatings/Form:</b><br /><b>
                        <asp:Label ID="vCoatingFromLabel" runat="server" Text='<%# Bind("vCoatingFrom") %>' BackColor="Turquoise" Width="90px">
                        </asp:Label>
                    </b></td>
                    <td nowrap><b>CoatPasses/Form:</b><br /><b>
                        <asp:Label ID="vCoatPassesFromLabel" runat="server" Text='<%# Bind("vCoatPassesFrom") %>' BackColor="Turquoise" Width="100px">
                        </asp:Label>
                    </b></td>                                                                                       
                    <td nowrap><b>Purch/Manuf:</b><br /><b>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("vPurchManuf") %>'  BackColor="Turquoise" Width="80px"></asp:Label>                        
                    </b></td>
                                                                          
                </tr>
                <tr><td><asp:Label ID="vEstTypeLabel" Visible="false" runat="server"  Text='<%# Bind("vEstType") %>'></asp:Label>
                <asp:Label ID="Label_reckey" Visible="false" runat="server"  Text='<%# Bind("vRec_key") %>'></asp:Label>
                <asp:Label ID="Label_efbrws" Visible="false" runat="server"  Text='<%# Bind("vefbrowseval") %>'></asp:Label>
                </td></tr>
            </table>
           <table id="showbutton" style="display:none;">
            
            <tr>
                <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                    
                    <asp:Button ID="set_button" runat="server" Text="Set" CssClass="buttonM" OnClick="set_button_change_click" OnClientClick=" return confirm('Are you sure you wish to change estimate type to Set?')" />
                </td>
            
                <td>
                                    
                    <asp:Button ID="combo_button" runat="server" Text="Combo/Tandem" CssClass="buttonM" OnClick="combo_button_change_click" OnClientClick="return confirm('Are you sure you wish to change estimate type to Combo/Tandem?')" />
                </td>
            </tr>
            <%--<tr>
                <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                
                    <asp:Button ID="update_set_button" runat="server" Text="Update" CssClass="buttonM" />
                </td>
            </tr>--%>
           </table>
            <br />
            
             <asp:Button ID="btn_add" runat="server" CssClass="buttonM" Text="Add" CommandName="New" />
            <asp:Button ID="btn_update" runat="server" CssClass="buttonM" OnClick="btn_update_click" Text="Update" CommandName="Edit" />           
            <asp:Button ID="btn_delete" runat="server" CssClass="buttonM" OnClick="delete_Click" Text="Delete" OnClientClick="return confirm('Are you sure you want to delete this record')" />
            <asp:Button ID="btn_formest" runat="server" CssClass="button" OnClick="form_est_Click" CommandName="Edit" Text="Add Form to Estimate" />
            <asp:Button ID="Button_blank_est" runat="server" CssClass="button" OnClick="blank_est_Click" CommandName="Edit" Text="Add Blank to Form" />
            <input type="button" id="btn_set_update" value="Set" runat="server" class="buttonM" onClick="Setlook()" />
            <input type="button" id="btn_set" onclick="showbutton()" style="display:inline;" class="buttonM" value="Change Type" />
            <input type="button" id="btn_reset" onclick="showbutton2()" style="display:none;" class="buttonM" value="Change Type" />
            
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="CorrugatedEstimateDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectfoldingEstimate" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmShipTo" Type="String" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter  Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmEstQty" Type="Decimal" />
            <asp:Parameter Name="prmStyle" Type="String" />
            <asp:Parameter Name="prmPaper1" Type="String" />
            <asp:Parameter Name="prmPaper2" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmCalliper" Type="Decimal" />
            <asp:Parameter Name="prmCategory" Type="String" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmDepth" Type="Decimal" />
            <asp:SessionParameter Name="prmFrom" SessionField="order_folding_formno" Type="Int32" />
            <asp:SessionParameter SessionField="order_folding_blankno"  Name="prmBlank" Type="Int32" />
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
            <asp:Parameter Name="prmType" Type="string" />
            <asp:Parameter Name="prmDiein" Type="decimal" />
            
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
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource_list" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectfoldingEstimate" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter DefaultValue="ListSelect" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstimate" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter Name="prmCustPart" Type="String" />
            <asp:Parameter Name="prmShipTo" Type="String" />
            <asp:Parameter Name="prmItemName" Type="String" />
            <asp:Parameter  Name="prmFgItem"  Type="String" />
            <asp:Parameter Name="prmEstQty" Type="Decimal" />
            <asp:Parameter Name="prmStyle" Type="String" />
            <asp:Parameter Name="prmPaper1" Type="String" />
            <asp:Parameter Name="prmPaper2" Type="String" />
            <asp:Parameter Name="prmBoard" Type="String" />
            <asp:Parameter Name="prmCalliper" Type="Decimal" />
            <asp:Parameter Name="prmCategory" Type="String" />
            <asp:Parameter Name="prmLength" Type="Decimal" />
            <asp:Parameter Name="prmWidth" Type="Decimal" />
            <asp:Parameter Name="prmDepth" Type="Decimal" />
            <asp:SessionParameter Name="prmFrom" SessionField="order_folding_formno" Type="Int32" />
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
            <asp:Parameter Name="prmType" Type="string" />
            <asp:Parameter Name="prmDiein" Type="decimal" />
            
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
        </SelectParameters>
    </asp:ObjectDataSource>



</asp:Content>

