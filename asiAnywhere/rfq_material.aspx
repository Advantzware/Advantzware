<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfqmaterial" Title="Request for Quote" Codebehind="rfq_material.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">

<script type="text/javascript">
function Materiallook()
  { 
  var NewWindow = window.open("MatLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function MaterialLookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno1TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr1TextBox.value=ReturnObj2;
  //alert("Qty/FG field must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQtyTextBox.focus();  
}
function Materiallook1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno1TextBox.value="";
  Materiallook();
  }
 
function Mat1look()
  { 
  var NewWindow = window.open("MatLook1.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  
  }
  function matt1()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQtyTextBox.focus(); 
  }
  function matt2()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty2TextBox.focus(); 
  }
  function matt3()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty3TextBox.focus(); 
  }
  function matt4()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty4TextBox.focus(); 
  }
  function matt5()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty5TextBox.focus(); 
  }
  function matt6()
  {
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty6TextBox.focus(); 
  }

function MatLookUp1(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf1TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLeaf1TextBox.focus();
  
}
function Mat2look()
  { 
  var NewWindow = window.open("MatLook2.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function MatLookUp2(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf2TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscr2TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLeaf2TextBox.focus();
  
}
function Mat3look()
  { 
  var NewWindow = window.open("MatLook3.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function MatLookUp3(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf3TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscr3TextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vLeaf3TextBox.focus();
  
}
function Boardlook(){ 
  var NewWindow = window.open("BoardLook.aspx","BoardWindow","width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function BoardLookUp(ReturnObj1, ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBrdDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vBoardTextBox.focus();
}
function Itemlook()
  { 
  var NewWindow = window.open("ItemLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function ItemLookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno2TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr2TextBox.value=ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty2TextBox.focus(); 
  
}
function Itemlook1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno2TextBox.value="";
  Itemlook();
  }
function Item1look()
  { 
  var NewWindow = window.open("ItemLook1.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Item1LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno3TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr3TextBox.value=ReturnObj2; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty3TextBox.focus(); 
}
function Item1look1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno3TextBox.value="";
  Item1look();
  }
function Item2look()
  { 
  var NewWindow = window.open("ItemLook2.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Item2LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno4TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr4TextBox.value=ReturnObj2; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty4TextBox.focus(); 
}
function Item2look1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno4TextBox.value="";
  Item2look();
  }
function Item3look()
  { 
  var NewWindow = window.open("ItemLook3.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Item3LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno5TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr5TextBox.value=ReturnObj2; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty5TextBox.focus(); 
}
function Item3look1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno5TextBox.value="";
  Item3look();
  }
function Item4look()
  { 
  var NewWindow = window.open("ItemLook4.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Item4LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno6TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr6TextBox.value=ReturnObj2; 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty6TextBox.focus(); 
}
function Item4look1()
  { 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno6TextBox.value="";
  Item4look();
  }
function Adderlook()
  { 
  var NewWindow = window.open("AdderLook.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function AdderLookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder1TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder7TextBox.value=ReturnObj2;  
}
function Adderlook1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder1TextBox.value="";
Adderlook();
}
function Adder2look()
  { 
  var NewWindow = window.open("Adder2Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Adder2LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder2TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder8TextBox.value=ReturnObj2;  
}
function Adder2look1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder2TextBox.value="";
Adder2look();
}
function Adder3look()
  { 
  var NewWindow = window.open("Adder3Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Adder3LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder3TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder9TextBox.value=ReturnObj2;  
}
function Adder3look1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder3TextBox.value="";
Adder3look();
}
function Adder4look()
  { 
  var NewWindow = window.open("Adder4Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Adder4LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder4TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder10TextBox.value=ReturnObj2;  
}
function Adder4look1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder4TextBox.value="";
Adder4look();
}
function Adder5look()
  { 
  var NewWindow = window.open("Adder5Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Adder5LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder5TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder11TextBox.value=ReturnObj2;  
}
function Adder5look1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder5TextBox.value="";
Adder5look();
}
function Adder6look()
  { 
  var NewWindow = window.open("Adder6Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Adder6LookUp(ReturnObj1,ReturnObj2)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder6TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder12TextBox.value=ReturnObj2;  
}
function Adder6look1()
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder6TextBox.value="";
Adder6look();
}

function Boardclearlook()
{
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBoardTextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vBrdDscrTextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf1TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscrTextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf2TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscr2TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf3TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscr3TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeaf4TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafdscr4TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno1TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr1TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno2TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr2TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno3TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr3TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno4TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr4TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno5TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr5TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno6TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecdscr6TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder1TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder7TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder2TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder8TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder3TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder9TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder4TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder10TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder5TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder11TextBox.value="";
}

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder6TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vAdder12TextBox.value="";
}
}

function checkNumeric(objName,comma,period)
{
	var numberfield = objName;
	if (chkNumeric(objName,comma,period) == false)
	{
		numberfield.select();
		numberfield.focus();
		return false;
	}
	else
	{
		return true;
	}
}

function chkNumeric(objName,comma,period)
{
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno1TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQtyTextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQtyTextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno1TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQtyTextBox.focus();
   }
   
   if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno2TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty2TextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty2TextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno2TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty2TextBox.focus();
   }
   
   if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno3TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty3TextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty3TextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno3TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty3TextBox.focus();
   }
   
   if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno4TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty4TextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty4TextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno4TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty4TextBox.focus();
   }
   
   if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno5TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty5TextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty5TextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno5TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty5TextBox.focus();
   }
   
   if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno6TextBox.value=="")
   {
    document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty6TextBox.value="0";
   }
   if(document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty6TextBox.value<=0 && document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecno6TextBox.value!="")
   {
   alert("Qty/FG item must have some value");
   document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vSpecQty6TextBox.focus();
   }
var checkOK = "0123456789" + comma + period ;
var checkStr = objName;
var allValid = true;
var decPoints = 0;
var allNum = "";

for (i = 0;  i < checkStr.value.length;  i++)
{
ch = checkStr.value.charAt(i);
for (j = 0;  j < checkOK.length;  j++)
if (ch == checkOK.charAt(j))
break;
if (j == checkOK.length)
{
allValid = false;
break;
}
if (ch != ",")
allNum += ch;
}
if (!allValid)
{	
alertsay = "Please enter only these values \""
alertsay = alertsay + checkOK + "\" in the \"" + checkStr.name + "\" field."
alert(alertsay);
return (false);
}

//var positionOfC = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value.indexOf(".");

////if(positionOfC<1 )
////{
////  checkStr.value=checkStr.value + ".00000";
////}
//var positionOfCw = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value.indexOf(".");
//var positionOfC1 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value.indexOf(".");
//var positionOfC2 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value.indexOf(".");
//var positionOfC3 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value.indexOf(".");
//var positionOfC4 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value.indexOf(".");
//var positionOfC5 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value.indexOf(".");
//var positionOfC6 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value.indexOf(".");
//var positionOfC7 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value.indexOf(".");
//var positionOfC8 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value.indexOf(".");
//var positionOfC9 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value.indexOf(".");

//var qtypos=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value.indexOf(".");
//var qtypos2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value.indexOf(".");
//var qtypos3=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value.indexOf(".");
//var qtypos4=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value.indexOf(".");
//var qtypos5=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value.indexOf(".");
//var qtypos6=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value.indexOf(".");

//if(positionOfC<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value + ".00000";
//}
// if(positionOfCw<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value + ".0000";
//}
// if(positionOfC1<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value + ".0000";
//}
//if(positionOfC2<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value + ".0000";
//}
// if(positionOfC3<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value + ".0000";
//}
// if(positionOfC4<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value + ".0000";
//}
// if(positionOfC5<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value + ".0000";
//}
// if(positionOfC6<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value + ".0000";
//}
// if(positionOfC7<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value + ".0000";
//}
// if(positionOfC8<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value + ".0000";
//}
// if(positionOfC9<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value + ".0000";
//}
//if(qtypos<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value + ".0";
//}
//if(qtypos2<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value + ".0";
//}
//if(qtypos3<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value + ".0";
//}
//if(qtypos4<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value + ".0";
//}
//if(qtypos5<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value + ".0";
//}
//if(qtypos6<1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value + ".0";
//}
}


function validatecaliper()
{
//var data=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value;
//var len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value.length;

//var wid1=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value;
//var len1=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value;
//var wid1len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value.length;
//var len1len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value.length;

//var wid2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value;
//var len2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value;
//var wid2len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value.length;
//var len2len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value.length;

//var wid3=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value;
//var len3=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value;
//var wid3len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value.length;
//var len3len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value.length;

//var wid4=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value;
//var len4=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value;
//var wid4len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value.length;
//var len4len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value.length;

//var wid5=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value;
//var len5=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value;
//var wid5len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value.length;
//var len5len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value.length;

//if(len>0)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value=data + ".";
//}
//if(len>1)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCalTextBox.value=data;
//}
//if(wid1len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value=wid1 + "." 
//}
//if(wid1len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshwidTextBox.value=wid1;
//}
//if(len1len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value=len1 + "." 
//}
//if(len1len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vGshlenTextBox.value=len1;
//}

//if(wid2len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value=wid2 + "." 
//}
//if(wid2len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw1TextBox.value=wid2;
//}
//if(len2len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value=len2 + "." 
//}
//if(len2len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl1TextBox.value=len2;
//}

//if(wid3len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value=wid3 + "." 
//}
//if(wid3len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw2TextBox.value=wid3;
//}
//if(len3len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value=len3 + "." 
//}
//if(len3len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl2TextBox.value=len3;
//}

//if(wid4len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value=wid4 + "." 
//}
//if(wid4len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw3TextBox.value=wid4;
//}
//if(len4len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value=len4 + "." 
//}
//if(len4len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl3TextBox.value=len4;
//}

//if(wid5len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value=wid5 + "." 
//}
//if(wid5len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafw4TextBox.value=wid5;
//}
//if(len5len>2)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value=len5 + "." 
//}
//if(len5len>3)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vLeafl4TextBox.value=len5;
//}
//var qty=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value;
//var qtylen=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value.length;
//var qty2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value;
//var qty2len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value.length;
//var qty3=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value;
//var qty3len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value.length;
//var qty4=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value;
//var qty4len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value.length;
//var qty5=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value;
//var qty5len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value.length;
//var qty6=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value;
//var qty6len=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value.length;

//if(qtylen>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value=qty + "." 
//}
//if(qtylen>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQtyTextBox.value=qty;
//}
//if(qty2len>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value=qty2 + "." 
//}
//if(qty2len>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty2TextBox.value=qty2;
//}
//if(qty3len>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value=qty3 + "." 
//}
//if(qty3len>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty3TextBox.value=qty3;
//}
//if(qty4len>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value=qty4 + "." 
//}
//if(qty4len>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty4TextBox.value=qty4;
//}
//if(qty5len>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value=qty5 + "." 
//}
//if(qty5len>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty5TextBox.value=qty5;
//}
//if(qty6len>8)
//{
// document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value=qty6 + "." 
//}
//if(qty6len>9)
//{
//  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vSpecQty6TextBox.value=qty6;
//}


}
</script>

<div>
<fieldset style="background-color:#EFF3FB; width:620px;">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" Width="620px">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="60px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b>Cust Part#:</b>
            <asp:Label ID="vRfqPartLabel" runat="server" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqPart") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Style:</b>
            <asp:Label ID="vRfqstyleLabel" runat="server" BackColor="Turquoise" Width="120px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqstyle") %>'></asp:Label>
            <b> </b>
            <asp:Label ID="vRfqstyleDscrLabel" runat="server" BackColor="Turquoise" Width="115px"  BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqstyleDscr") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqmatno" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    
    &nbsp; &nbsp;&nbsp;&nbsp;
    <table style="width: 500px">
    <tr><td>
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" Style="position: static" >
        <EditItemTemplate>
        <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdateButton">
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 600px">
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b></b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Material</b></td>
        <td style="width: 145px;"><b>Description</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Caliper</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Width</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Length</b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Board</b></td>
        <td><b><asp:TextBox ID="vBoardTextBox" Width="60px" onkeyup = "Boardclearlook()" MaxLength="10" runat="server" Text='<%# Bind("vBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
        <td style="width:145px;"><b><asp:TextBox ID="vBrdDscrTextBox" Enabled="false" runat="server" Text='<%# Bind("vBrdDscr") %>'>
            </asp:TextBox>
            </b></td>
        <td style="width:70px;"><b><asp:TextBox ID="vCalTextBox" Enabled="false" onkeypress = "validatecaliper()" onBlur="checkNumeric(this,',','.')" Width="50px" MaxLength="7" runat="server" Text='<%# Bind("vCal") %>'>
            </asp:TextBox></b></td>
        <td style="width:70px;"><b><asp:TextBox ID="vGshwidTextBox" onkeypress="validatecaliper()" Width="50px" MaxLength="8" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vGshwid") %>'>
            </asp:TextBox></b></td>
        <td style="width:70px;"><b><asp:TextBox ID="vGshlenTextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vGshlen") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Window/Wax</b></td>
        <td><b><asp:TextBox ID="vLeaf1TextBox" Width="60px" onkeyup = "Boardclearlook()" MaxLength="10" runat="server" Text='<%# Bind("vLeaf1") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Mat1look(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br />
           
        <td><b><asp:TextBox ID="vLeafdscrTextBox" Enabled="false" runat="server" Text='<%# Bind("vLeafdscr") %>'>
            </asp:TextBox>
            </b></td>
        <td><b></b></td>
        <td><b><asp:TextBox ID="vLeafw1TextBox" Width="50px" MaxLength="8" runat="server" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" Text='<%# Bind("vLeafw1") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vLeafl1TextBox" Width="50px" MaxLength="8" runat="server" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" Text='<%# Bind("vLeafl1") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Foil</b></td>
        <td><b><asp:TextBox ID="vLeaf2TextBox" Width="60px" onkeyup = "Boardclearlook()" MaxLength="10" runat="server" Text='<%# Bind("vLeaf2") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Mat2look(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vLeafdscr2TextBox" Enabled="false" runat="server" Text='<%# Bind("vLeafdscr2") %>'>
            </asp:TextBox>
            </b></td>
        <td><b></b></td>
        <td><b><asp:TextBox ID="vLeafw2TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafw2") %>'>
            </asp:TextBox></b></td>
        <td ><b><asp:TextBox ID="vLeafl2TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafl2") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Stamp</b></td>
        <td><b><asp:TextBox ID="vLeaf3TextBox" Width="60px" onkeyup = "Boardclearlook()" MaxLength="10" runat="server" Text='<%# Bind("vLeaf3") %>'>
           </asp:TextBox><a href="#" tabindex="1" onClick="Mat3look(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vLeafdscr3TextBox" Enabled="false" runat="server" Text='<%# Bind("vLeafdscr3") %>'>
            </asp:TextBox>
           </b></td>
        <td><b></b></td>
        <td><b></b><asp:TextBox ID="vLeafw3TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafw3") %>'>
            </asp:TextBox></td>
        <td><b><asp:TextBox ID="vLeafl3TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafl3") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Laminate</b></td>
        <td><b><asp:TextBox ID="vLeaf4TextBox" Width="60px" onkeyup = "Boardclearlook()" MaxLength="10" runat="server" Text='<%# Bind("vLeaf4") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vLeafdscr4TextBox" Enabled="false" runat="server" Text='<%# Bind("vLeafdscr4") %>'>
            </asp:TextBox>
            </b></td>
        <td ><b></b></td>
        <td><b><asp:TextBox ID="vLeafw4TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafw4") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vLeafl4TextBox" Width="50px" MaxLength="8" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vLeafl4") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Flute</b></td>
        <td><b>
            <asp:Label ID="fluteTextBox" runat="server" Text='<%# Bind("flute") %>'></asp:Label></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Test</b></td>
        <td><b>
            <asp:Label ID="testTextBox" runat="server" Text='<%# Bind("test") %>'></asp:Label></b></td>
        </tr>
        </table>
        </fieldset>
        
        <table><tr>
        <td>
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 350px; height:220px;">
        <tr>
        
        <td align="right" style="width: 35px; padding-right:5px;"><b>Item#</b></td>
        <td align="right" style="width: 55px; padding-right:5px;"><b>Description</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Qty/Finished Good</b></td>
        
        </tr>
        <tr>
        
        <td><b><asp:TextBox ID="vSpecno1TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt1()"  MaxLength="10" runat="server" Text='<%# Bind("vSpecno1") %>'></asp:TextBox>
        <a href="#" tabindex="1" onClick="Materiallook(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
            
             
        <td><b><asp:TextBox ID="vSpecdscr1TextBox" Enabled="false" Width="100px"  MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr1") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vSpecQtyTextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty") %>'>
            </asp:TextBox></b>           
            </td>
        </tr>
          <tr>
          
        <td><b><asp:TextBox ID="vSpecno2TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt2()" MaxLength="10" runat="server" Text='<%# Bind("vSpecno2") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Itemlook(); return false" ><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vSpecdscr2TextBox" Enabled="false" Width="100px" MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr2") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vSpecQty2TextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty2") %>'>
            </asp:TextBox></b></td>
        </tr>  
         <tr>
        <td><b><asp:TextBox ID="vSpecno3TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt3()" MaxLength="10" runat="server" Text='<%# Bind("vSpecno3") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Item1look(); return false" ><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vSpecdscr3TextBox" Enabled="false" Width="100px" MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr3") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vSpecQty3TextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty3") %>'>
            </asp:TextBox></b></td>
        </tr> 
        <tr>
        <td><b><asp:TextBox ID="vSpecno4TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt4()" MaxLength="10" runat="server" Text='<%# Bind("vSpecno4") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Item2look(); return false" ><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vSpecdscr4TextBox" Enabled="false" Width="100px" MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr4") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vSpecQty4TextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty4") %>'>
            </asp:TextBox></b></td>
        </tr>  
         <tr>
        <td><b><asp:TextBox ID="vSpecno5TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt5()" MaxLength="10" runat="server" Text='<%# Bind("vSpecno5") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Item3look(); return false" ><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vSpecdscr5TextBox" Enabled="false" Width="100px" MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr5") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:TextBox ID="vSpecQty5TextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty5") %>'>
            </asp:TextBox></b></td>
        </tr>  
         <tr>
          <td><b><asp:TextBox ID="vSpecno6TextBox" onkeyup = "Boardclearlook()" Width="60px" onblur="matt6()" MaxLength="10" runat="server" Text='<%# Bind("vSpecno6") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Item4look(); return false" ><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
          <td><b><asp:TextBox ID="vSpecdscr6TextBox" Enabled="false" Width="100px" MaxLength="20" runat="server" Text='<%# Bind("vSpecdscr6") %>'>
            </asp:TextBox></b></td>
          <td><b><asp:TextBox ID="vSpecQty6TextBox" Width="65px" MaxLength="11" onkeypress="validatecaliper()" onBlur="checkNumeric(this,',','.')" runat="server" Text='<%# Bind("vSpecQty6") %>'>
            </asp:TextBox></b></td>
        </tr>
            
            
            
        </table>
        </fieldset>
        </td>
        <td>
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 250px; height:220px;">
        <tr>
        
        <td align="right" style="width: 35px; padding-right:5px;"><b>Item#</b></td>
        <td style="width: 85px;"><b>Description</b></td>
        
        
        </tr>
        <tr>
        
        <td><b><asp:TextBox ID="vAdder1TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder1") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adderlook(); return false" ><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder7TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder7") %>'>
            </asp:TextBox>
            </b></td>
        </tr>
          <tr>
          
        <td><b><asp:TextBox ID="vAdder2TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder2") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adder2look(); return false" ><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder8TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder8") %>'>
            </asp:TextBox>
            </b></td>
        </tr>  
         <tr>
         
        <td><b><asp:TextBox ID="vAdder3TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder3") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adder3look(); return false" ><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder9TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder9") %>'>
            </asp:TextBox>
            </b></td>
        
        
        </tr> 
        <tr>
        
        <td><b><asp:TextBox ID="vAdder4TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder4") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adder4look(); return false" ><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder10TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder10") %>'>
            </asp:TextBox>
            </b></td>
        
        
        </tr>  
         <tr>
         
        <td><b><asp:TextBox ID="vAdder5TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder5") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adder5look(); return false" ><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder11TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder11") %>'>
            </asp:TextBox>
            </b></td>
        
        
        </tr>  
         <tr>
         
          <td><b><asp:TextBox ID="vAdder6TextBox" onkeyup = "Boardclearlook()" Width="60px" MaxLength="10" runat="server" Text='<%# Bind("vAdder6") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Adder6look(); return false" ><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></b></td>
        <td><b><asp:TextBox ID="vAdder12TextBox" Enabled="false" runat="server" Text='<%# Bind("vAdder12") %>'>
            </asp:TextBox>
            </b></td>
            
            <asp:TextBox ID="HiddenRowid" Visible="false" runat="server" Text='<%# Bind("MatRowid") %>'>
            </asp:TextBox>
        
        </tr>
          
        </table>
        
        </fieldset>
        </td>
        
        </tr></table>
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="UpdateButton" runat="server" CssClass="buttonM"  onclick="updateRfqMaterial"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button></asp:Panel>
        </EditItemTemplate>
        <%--<InsertItemTemplate>
            vBoard:
            <asp:TextBox ID="vBoardTextBox" runat="server" Text='<%# Bind("vBoard") %>'>
            </asp:TextBox><br />
            vBrdDscr:
            <asp:TextBox ID="vBrdDscrTextBox" runat="server" Text='<%# Bind("vBrdDscr") %>'>
            </asp:TextBox><br />
            vCal:
            <asp:TextBox ID="vCalTextBox" runat="server" Text='<%# Bind("vCal") %>'>
            </asp:TextBox><br />
            vGshwid:
            <asp:TextBox ID="vGshwidTextBox" runat="server" Text='<%# Bind("vGshwid") %>'>
            </asp:TextBox><br />
            vGshlen:
            <asp:TextBox ID="vGshlenTextBox" runat="server" Text='<%# Bind("vGshlen") %>'>
            </asp:TextBox><br />
            vLeaf1:
            <asp:TextBox ID="vLeaf1TextBox" runat="server" Text='<%# Bind("vLeaf1") %>'>
            </asp:TextBox><br />
            vLeafw1:
            <asp:TextBox ID="vLeafw1TextBox" runat="server" Text='<%# Bind("vLeafw1") %>'>
            </asp:TextBox><br />
            vLeafl1:
            <asp:TextBox ID="vLeafl1TextBox" runat="server" Text='<%# Bind("vLeafl1") %>'>
            </asp:TextBox><br />
            vLeaf2:
            <asp:TextBox ID="vLeaf2TextBox" runat="server" Text='<%# Bind("vLeaf2") %>'>
            </asp:TextBox><br />
            vLeafw2:
            <asp:TextBox ID="vLeafw2TextBox" runat="server" Text='<%# Bind("vLeafw2") %>'>
            </asp:TextBox><br />
            vLeafl2:
            <asp:TextBox ID="vLeafl2TextBox" runat="server" Text='<%# Bind("vLeafl2") %>'>
            </asp:TextBox><br />
            vLeaf3:
            <asp:TextBox ID="vLeaf3TextBox" runat="server" Text='<%# Bind("vLeaf3") %>'>
            </asp:TextBox><br />
            vLeafw3:
            <asp:TextBox ID="vLeafw3TextBox" runat="server" Text='<%# Bind("vLeafw3") %>'>
            </asp:TextBox><br />
            vLeafl3:
            <asp:TextBox ID="vLeafl3TextBox" runat="server" Text='<%# Bind("vLeafl3") %>'>
            </asp:TextBox><br />
            vLeaf4:
            <asp:TextBox ID="vLeaf4TextBox" runat="server" Text='<%# Bind("vLeaf4") %>'>
            </asp:TextBox><br />
            vLeafw4:
            <asp:TextBox ID="vLeafw4TextBox" runat="server" Text='<%# Bind("vLeafw4") %>'>
            </asp:TextBox><br />
            vLeafl4:
            <asp:TextBox ID="vLeafl4TextBox" runat="server" Text='<%# Bind("vLeafl4") %>'>
            </asp:TextBox><br />
            vSpecdscr1:
            <asp:TextBox ID="vSpecdscr1TextBox" runat="server" Text='<%# Bind("vSpecdscr1") %>'>
            </asp:TextBox><br />
            vSpecdscr2:
            <asp:TextBox ID="vSpecdscr2TextBox" runat="server" Text='<%# Bind("vSpecdscr2") %>'>
            </asp:TextBox><br />
            vSpecdscr3:
            <asp:TextBox ID="vSpecdscr3TextBox" runat="server" Text='<%# Bind("vSpecdscr3") %>'>
            </asp:TextBox><br />
            vSpecdscr4:
            <asp:TextBox ID="vSpecdscr4TextBox" runat="server" Text='<%# Bind("vSpecdscr4") %>'>
            </asp:TextBox><br />
            vSpecdscr5:
            <asp:TextBox ID="vSpecdscr5TextBox" runat="server" Text='<%# Bind("vSpecdscr5") %>'>
            </asp:TextBox><br />
            vSpecdscr6:
            <asp:TextBox ID="vSpecdscr6TextBox" runat="server" Text='<%# Bind("vSpecdscr6") %>'>
            </asp:TextBox><br />
            vAdder1:
            <asp:TextBox ID="vAdder1TextBox" runat="server" Text='<%# Bind("vAdder1") %>'>
            </asp:TextBox><br />
            vAdder2:
            <asp:TextBox ID="vAdder2TextBox" runat="server" Text='<%# Bind("vAdder2") %>'>
            </asp:TextBox><br />
            vAdder3:
            <asp:TextBox ID="vAdder3TextBox" runat="server" Text='<%# Bind("vAdder3") %>'>
            </asp:TextBox><br />
            vAdder4:
            <asp:TextBox ID="vAdder4TextBox" runat="server" Text='<%# Bind("vAdder4") %>'>
            </asp:TextBox><br />
            vAdder5:
            <asp:TextBox ID="vAdder5TextBox" runat="server" Text='<%# Bind("vAdder5") %>'>
            </asp:TextBox><br />
            vAdder6:
            <asp:TextBox ID="vAdder6TextBox" runat="server" Text='<%# Bind("vAdder6") %>'>
            </asp:TextBox><br />
            vAdder7:
            <asp:TextBox ID="vAdder7TextBox" runat="server" Text='<%# Bind("vAdder7") %>'>
            </asp:TextBox><br />
            vAdder8:
            <asp:TextBox ID="vAdder8TextBox" runat="server" Text='<%# Bind("vAdder8") %>'>
            </asp:TextBox><br />
            vAdder9:
            <asp:TextBox ID="vAdder9TextBox" runat="server" Text='<%# Bind("vAdder9") %>'>
            </asp:TextBox><br />
            vAdder10:
            <asp:TextBox ID="vAdder10TextBox" runat="server" Text='<%# Bind("vAdder10") %>'>
            </asp:TextBox><br />
            vAdder11:
            <asp:TextBox ID="vAdder11TextBox" runat="server" Text='<%# Bind("vAdder11") %>'>
            </asp:TextBox><br />
            vAdder12:
            <asp:TextBox ID="vAdder12TextBox" runat="server" Text='<%# Bind("vAdder12") %>'>
            </asp:TextBox><br />
            vSpecno1:
            <asp:TextBox ID="vSpecno1TextBox" runat="server" Text='<%# Bind("vSpecno1") %>'>
            </asp:TextBox><br />
            vSpecno2:
            <asp:TextBox ID="vSpecno2TextBox" runat="server" Text='<%# Bind("vSpecno2") %>'>
            </asp:TextBox><br />
            vSpecno3:
            <asp:TextBox ID="vSpecno3TextBox" runat="server" Text='<%# Bind("vSpecno3") %>'>
            </asp:TextBox><br />
            vSpecno4:
            <asp:TextBox ID="vSpecno4TextBox" runat="server" Text='<%# Bind("vSpecno4") %>'>
            </asp:TextBox><br />
            vSpecno5:
            <asp:TextBox ID="vSpecno5TextBox" runat="server" Text='<%# Bind("vSpecno5") %>'>
            </asp:TextBox><br />
            vSpecno6:
            <asp:TextBox ID="vSpecno6TextBox" runat="server" Text='<%# Bind("vSpecno6") %>'>
            </asp:TextBox><br />
            vLeafdscr:
            <asp:TextBox ID="vLeafdscrTextBox" runat="server" Text='<%# Bind("vLeafdscr") %>'>
            </asp:TextBox><br />
            vLeafdscr2:
            <asp:TextBox ID="vLeafdscr2TextBox" runat="server" Text='<%# Bind("vLeafdscr2") %>'>
            </asp:TextBox><br />
            vLeafdscr3:
            <asp:TextBox ID="vLeafdscr3TextBox" runat="server" Text='<%# Bind("vLeafdscr3") %>'>
            </asp:TextBox><br />
            vLeafdscr4:
            <asp:TextBox ID="vLeafdscr4TextBox" runat="server" Text='<%# Bind("vLeafdscr4") %>'>
            </asp:TextBox><br />
            flute:
            <asp:TextBox ID="fluteTextBox" runat="server" Text='<%# Bind("flute") %>'>
            </asp:TextBox><br />
            test:
            <asp:TextBox ID="testTextBox" runat="server" Text='<%# Bind("test") %>'>
            </asp:TextBox><br />
            <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:LinkButton>
            <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </InsertItemTemplate>--%>
        <ItemTemplate>
        <asp:Panel ID="Item_Panel" runat="server" DefaultButton="EditButton">
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 500px">
        <tr>
        <td align="right" style="width: 35px; padding-right:5px;"><b></b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Material</b></td>
        <td align="right" style="width: 55px; padding-right:5px;"><b>Description</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Caliper</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Width</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Length</b></td>
        </tr>
        <tr>
        <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Board</b></td>
        <td style="height: 4px"><b><asp:Label ID="vBoardLabel" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vBoard") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vBrdDscrLabel" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vBrdDscr") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vCal") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label4" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vGshwid") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label5" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vGshlen") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
          <tr>
          <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Window/Wax</b></td>
        <td style="height: 4px"><b><asp:Label ID="Label1" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeaf1") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label17" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafdscr") %>'></asp:Label></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label7" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafw1") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label8" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafl1") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>  
            <tr>
         <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Foil</b></td>
        <td style="height: 4px"><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeaf2") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label19" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafdscr2") %>'></asp:Label></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label6" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafw2") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafl2") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
            </tr>
            <tr>
         <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Stamp</b></td>
        <td style="height: 4px"><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeaf3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label20" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafdscr3") %>'></asp:Label></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafw3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label12" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafl3") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
            </tr>
         <tr>
          <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Laminate</b></td>
        <td style="height: 4px"><b><asp:Label ID="Label13" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeaf4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label21" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafdscr4") %>'></asp:Label></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label14" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafw4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="Label15" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vLeafl4") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
         <tr>
          <td align="left" style="width: 35px; padding-right:5px; height: 4px;"><b>Flute</b></td>
        <td style="height: 4px"><b><asp:Label ID="Label16" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("flute") %>'></asp:Label></b></td>
        
        
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
         <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
         </tr>
        <tr>
        <td align="left" style="width: 35px; padding-right:5px;"><b>Test</b></td>
        <td><b><asp:Label ID="Label18" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("test") %>'></asp:Label></b></td>
        </tr>    
        </table>
        </fieldset>
        
        
        <table><tr>
        <td>
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 300px">
        <tr>
        
        <td align="right" style="width: 35px; padding-right:5px;"><b>Item#</b></td>
        <td align="right" style="width: 55px; padding-right:5px;"><b>Description</b></td>
        <td align="right" style="width: 35px; padding-right:5px;"><b>Qty/Finished Good</b></td>
        
        </tr>
        <tr>
        
        <td style="height: 4px"><b><asp:Label ID="vSpecno1Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno1") %>'></asp:Label></b></td>
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b><asp:Label ID="vSpecdscr1Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr1") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQtyLabel" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty") %>'></asp:Label></b></td>
        
        <td align="right" style="width: 71px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
          <tr>
          
        <td style="height: 4px"><b><asp:Label ID="vSpecno2Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno2") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecdscr2Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr2") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQty2Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty2") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
        <td style="height: 4px"><b><asp:Label ID="vSpecno3Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecdscr3Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQty3Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b></b></td>
        </tr> 
        <tr>
        <td style="height: 4px"><b><asp:Label ID="vSpecno4Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecdscr4Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQty4Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
        <td style="height: 4px"><b><asp:Label ID="vSpecno5Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno5") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecdscr5Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr5") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQty5Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty5") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
          <td style="height: 4px"><b><asp:Label ID="vSpecno6Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecno6") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecdscr6Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecdscr6") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vSpecQty6Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vSpecQty6") %>'></asp:Label></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
            
            
            
        </table>
        </fieldset>
        </td>
        <td>
        <fieldset style="background-color:#EFF3FB;">
        <table style="width: 300px">
        <tr>
        
        <td align="right" style="width: 35px; padding-right:5px;"><b>Item#</b></td>
        <td align="right" style="width: 55px; padding-right:5px;"><b>Description</b></td>
        
        
        </tr>
        <tr>
        
        <td style="height: 4px"><b><asp:Label ID="vAdder1Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder1") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder7Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder7") %>'></asp:Label></b></td>
        
        <td align="right" style="width: 35px; padding-right:5px; height: 4px;"><b></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
          <tr>
          
        <td style="height: 4px"><b><asp:Label ID="vAdder2Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder2") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder8Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder8") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
        <td style="height: 4px"><b><asp:Label ID="vAdder3Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder3") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder9Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder9") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr> 
        <tr>
        <td style="height: 4px"><b><asp:Label ID="vAdder4Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder4") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder10Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder10") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
        <td style="height: 4px"><b><asp:Label ID="vAdder5Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder5") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder11Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder11") %>'></asp:Label></b></td>
        
        <td style="height: 4px"><b></b></td>
        </tr>  
         <tr>
          <td style="height: 4px"><b><asp:Label ID="vAdder6Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder6") %>'></asp:Label></b></td>
        <td style="height: 4px"><b><asp:Label ID="vAdder12Label" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vAdder12") %>'></asp:Label></b></td>
        <td style="height: 4px"><b></b></td>
        </tr>
            
            
            
             
        </table>
        
        </fieldset>
        </td>
        
        </tr></table> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            &nbsp;&nbsp;&nbsp;&nbsp;
         <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM"  onclick = "update_RfqMaterial"
                    Text="Update">
                </asp:Button>
             </asp:Panel>
        </ItemTemplate>
        
    </asp:FormView>
    </td>
    </tr></table>
    

    <br />
    
   
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqMaterial" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue ="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmRfqNo" SessionField="rfqmatno" Type="Int32" />
            <asp:SessionParameter DefaultValue="" Name="prmPartNo" SessionField="rfqmatpart" Type="string" />
            <asp:Parameter Name="MatRowid" Type="Int64" />
                <asp:Parameter Name="prmBoard" Type="String" />
                <asp:Parameter Name="prmBrdDscr" Type="String" />
                <asp:Parameter Name="prmCal" Type="Decimal" />
                <asp:Parameter Name="prmGshwid" Type="Decimal" />
                <asp:Parameter Name="prmGshlen" Type="Decimal" />
                <asp:Parameter Name="prmLeaf1" Type="String" />
                <asp:Parameter Name="prmLeafw1" Type="Decimal" />
                <asp:Parameter Name="prmLeafl1" Type="Decimal" />
                <asp:Parameter Name="prmLeaf2" Type="String" />
                <asp:Parameter Name="prmLeafw2" Type="Decimal" />
                <asp:Parameter Name="prmLeafl2" Type="Decimal" />
                <asp:Parameter Name="prmLeaf3" Type="String" />
                <asp:Parameter Name="prmvLeafw3" Type="Decimal" />
                <asp:Parameter Name="prmLeafl3" Type="Decimal" />
                <asp:Parameter Name="prmLeaf4" Type="String" />
                <asp:Parameter Name="prmLeafw4" Type="Decimal" />
                <asp:Parameter Name="prmLeafl4" Type="Decimal" />
                <asp:Parameter Name="prmSpecdscr1" Type="String" />
                <asp:Parameter Name="prmSpecdscr2" Type="String" />
                <asp:Parameter Name="prmSpecdscr3" Type="String" />
                <asp:Parameter Name="prmSpecdscr4" Type="String" />
                <asp:Parameter Name="prmSpecdscr5" Type="String" />
                <asp:Parameter Name="prmSpecdscr6" Type="String" />
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
                <asp:Parameter Name="prmSpecno1" Type="String" />
                <asp:Parameter Name="prmSpecno2" Type="String" />
                <asp:Parameter Name="prmSpecno3" Type="String" />
                <asp:Parameter Name="prmSpecno4" Type="String" />
                <asp:Parameter Name="prmSpecno5" Type="String" />
                <asp:Parameter Name="prmSpecno6" Type="String" />
                <asp:Parameter Name="prmLeafdscr" Type="String" />
                <asp:Parameter Name="prmLeafdscr2" Type="String" />
                <asp:Parameter Name="prmLeafdscr3" Type="String" />
                <asp:Parameter Name="prmLeafdscr4" Type="String" />
                <asp:Parameter Name="prmSpecQty" Type="Decimal" />
                <asp:Parameter Name="prmSpecQty2" Type="Decimal" />
                <asp:Parameter Name="prmSpecQty3" Type="Decimal" />
                <asp:Parameter Name="vSpecQty4" Type="Decimal" />
                <asp:Parameter Name="prmSpecQty5" Type="Decimal" />
                <asp:Parameter Name="prmSpecQty6" Type="Decimal" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
</asp:Content>
