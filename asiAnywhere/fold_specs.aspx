<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="true" AutoEventWireup="true" Inherits="fold_specs" Title="Specs Estimate" Codebehind="fold_specs.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
<script language = "JavaScript" type="text/javascript" src="include/CalendarControl.js"></script>

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
            var style = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox");
            style.focus();
            style.select();
        }
    }
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
function adhesiveLook(){ 
  var looktype = "G,T";
  var est=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vEstNumTextBox").value;
  var NewWindow = window.open("adhesive_lookup.aspx?look2="+ looktype +"&lookest="+est+"","AdhesiveLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AdhesiveLookup1(ReturnObj1){
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vJointMatTextBox.value = ReturnObj1;
    document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vJointMatTextBox.focus();
}


function custpartlook(){ 
  var NewWindow = window.open("cor_custpart_look.aspx","CustPartLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustpartLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vCustPartTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vItemNameTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vItemNameTextBox.focus();
}


function stylelook(){ 
 var style1 = "1"
  var NewWindow = window.open("corstyle_lookup.aspx?style="+style1+"","StyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorStyleLookup(ReturnObj1,ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox.focus();
}
  
function Boardlook1(){ 
  var est1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vEstNumTextBox").value;
  var style1 = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vStyleTextBox").value;
  var NewWindow = window.open("corboard_lookup.aspx?est="+est1+"&style="+style1+"","BoardWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CorBoardLookup(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBoardTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBrdDscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Specs_vBrdDscrTextBox.focus();

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
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_CorrugatedEstimate_vEstDateTextBox.value=obj;
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
  
  function sqwidth()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreWidTextBox").value = frontback;
    }
   }
 function sqlen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vScoreLenTextBox").value = frontback;
    }
   }
 function tuckval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTuckTextBox").value = frontback;
    }
   }
 function jointlen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vJointLenTextBox").value = frontback;
    }
   }
   
   function vallen()
  {
  
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLengthTextBox").value = frontback;
    }        
   }
 function valwid()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vWidthTextBox").value = frontback;
    }
   }
 function valdep()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDepthTextBox").value = frontback;
    }
   }
   
   function destval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vDustFlapTextBox").value = frontback;
    }
   }
 function panelval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBotFlapTextBox").value = frontback;
    }
   }
 function tabval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vLockTabTextBox").value = frontback;
    }
   }
 function glueval()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 1 && frontback.length < 3) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vTabWidTextBox").value = frontback;
    }
   }
function blankwid()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBlankWidTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 3 && frontback.length < 5) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBlankWidTextBox").value = frontback;
    }
   }
 function blanklen()
  {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBlankLenTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 3 && frontback.length < 5) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Specs_vBlankLenTextBox").value = frontback;
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
            <asp:BoundField DataField="vPaper1" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" HeaderText="Paper 1" SortExpression="vPaper1" />
            <asp:BoundField DataField="vPaper2" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" HeaderText="Paper 2" SortExpression="vPaper2" />
            <asp:BoundField DataField="vBoard" ItemStyle-Wrap="false" HeaderText="Board" SortExpression="vBoard" />
            <asp:BoundField DataField="vCaliper" ItemStyle-Wrap="false" HeaderText="Caliper" SortExpression="vCaliper" />
            <asp:BoundField DataField="vCategory" ItemStyle-Wrap="false" HeaderText="Category" SortExpression="vCategory" />
            <asp:BoundField DataField="vLenght" ItemStyle-Wrap="false" HeaderText="Lenght" SortExpression="vLenght" />
            <asp:BoundField DataField="vWidth" ItemStyle-Wrap="false" HeaderText="Width" SortExpression="vWidth" />
            <asp:BoundField DataField="vDepth" ItemStyle-Wrap="false" HeaderText="Depth" SortExpression="vDepth" />
            <asp:BoundField DataField="vForm" ItemStyle-Wrap="false" HeaderText="Form" SortExpression="vForm" />
            <asp:BoundField DataField="vBlank" ItemStyle-Wrap="false" HeaderText="Blank" SortExpression="vBlank" />
            <asp:BoundField DataField="vTab" ItemStyle-Wrap="false" HeaderText="Tab" SortExpression="vTab" />
            <asp:BoundField DataField="vColor" ItemStyle-Wrap="false" HeaderText="Color" SortExpression="vColor" />
            <asp:BoundField DataField="vPasses" ItemStyle-Wrap="false" HeaderText="Passes" SortExpression="vPasses" />
            <asp:BoundField DataField="vCoating" ItemStyle-Wrap="false" HeaderText="Coating" SortExpression="vCoating" />
            <asp:BoundField DataField="vCoatPasses" ItemStyle-Wrap="false" HeaderText="CoatPasses" SortExpression="vCoatPasses" />
            <asp:BoundField DataField="vQtySet" ItemStyle-Wrap="false" HeaderText="QtySet" SortExpression="vQtySet" />
            <asp:BoundField DataField="vInkFrom" ItemStyle-Wrap="false" HeaderText="InkFrom" SortExpression="vInkFrom" />
            <asp:BoundField DataField="vPassesFrom" HeaderText="PassesFrom" SortExpression="vPassesFrom" />
            <asp:BoundField DataField="vCoatingFrom" ItemStyle-Wrap="false" HeaderText="CoatingFrom" SortExpression="vCoatingFrom" />
            <asp:BoundField DataField="vCoatPassesFrom" ItemStyle-Wrap="false" HeaderText="CoatPassesFrom" SortExpression="vCoatPassesFrom" />
            <asp:BoundField DataField="vPurchManuf" ItemStyle-Wrap="false" HeaderText="PurchManuf" SortExpression="vPurchManuf" />
            <asp:BoundField DataField="vEstDate" ItemStyle-Wrap="false" HeaderText="EstDate" SortExpression="vEstDate" HtmlEncode="false" DataFormatString="{0:MM/dd/yyyy}" />
            
            
        </Columns>
            
            </asp:GridView>
    <br />
    
    <asp:FormView ID="FormView_Specs"  runat="server" DataSourceID="CorrugatedSpecsDataSource" OnDataBound="FormView_Specs_DataBound" >
        <EditItemTemplate>
            <asp:Panel ID="Panel_Edit" runat="server" DefaultButton="UpdateButton">
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Estimate:</b></td>
            <td><asp:TextBox ID="vEstNumTextBox" Width="100px" ReadOnly="true" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vEstNum") %>'>
            </asp:TextBox></td>
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
            <td colspan="3"><asp:TextBox ID="vAddrTextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vAddr") %>'>
            </asp:TextBox></td>
            </tr>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Address:</b></td>
            <td colspan="3"><asp:TextBox ID="vAddr2TextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vAddr2") %>'>
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
            <td><asp:TextBox ID="vSalesmanTextBox" Width="100px" runat="server" Text='<%# Bind("vSalesman") %>'>
            </asp:TextBox> <a href="#" tabindex="1" onclick="salesmanlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vSalesmanTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Salesman Must Be Enter"></asp:RequiredFieldValidator></td>
            <td><asp:TextBox ID="vSmanDscrTextBox" Enabled="false" runat="server" Text='<%# Bind("vSmanDscr") %>'>
            </asp:TextBox></td>
            <td><b>%:</b><asp:TextBox ID="vCommTextBox" runat="server" Text='<%# Bind("vComm") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vCommTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>FG Category:</b></td>
            <td><asp:TextBox ID="vFgCategoryTextBox" Width="100px" runat="server" Text='<%# Bind("vFgCategory") %>'>
            </asp:TextBox><a href="#" tabindex="1" onclick="categorylookup(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="vFgCategoryTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Invalid FG Category Try Help"></asp:RequiredFieldValidator></td>
            <td colspan="2"><asp:TextBox ID="vFgCatDscrTextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vFgCatDscr") %>'>
            </asp:TextBox></td>
            </tr>
            </table></fieldset></td>
            <td colspan="6">
            <fieldset style="border-color:Black">
            <table>
            <tr  height="15px">
            <%--<b>Quantity:</b><asp:Label ID="vQtyTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQty") %>'>
            </asp:Label>
            <b>Qty/Set:</b><asp:Label ID="vQtySetTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQtySet") %>'>
            </asp:Label>
            <b>MSF:</b><asp:Label ID="vMsfTextBox" Width="100px" BackColor="turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vMsf") %>'>
            </asp:Label>--%>
            <td align="right" style="padding-right:5px"><b>Quantity:</b></td>
            <td> <asp:TextBox ID="vQtyTextBox" runat="server" Text='<%# Bind("vQty") %>'></asp:TextBox></td>
            </tr>
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
            <td><asp:TextBox ID="vImageTextBox" runat="server" Text='<%# Bind("vImage") %>'>
            </asp:TextBox> <asp:FileUpload  ID="FileUpload1" Height="20px" Width="150px" runat="server" />
            
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
            <fieldset style="border-color:Black">
            <table>
            <tr><td align="right" style="padding-right:5px"><b>Style Code:</b></td>
            <td><asp:TextBox ID="vStyleTextBox" Width="100px" runat="server" Text='<%# Bind("vStyle") %>'>
            </asp:TextBox><a href="#" TabIndex="1" onclick="stylelook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="vStyleTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Style Must Be Enter"></asp:RequiredFieldValidator></td>
            <td colspan="2"><asp:TextBox ID="vStyleDscrTextBox" Enabled="false" Width="160px" runat="server" Text='<%# Bind("vStyleDscr") %>'>
            </asp:TextBox></td>
            <td nowrap colspan="4">
            
            <b>Metric?:</b><asp:DropDownList ID="DropDownList2" Width="60px" SelectedValue='<%# Bind("vMetric") %>' DataValueField='<%# Bind("vMetric") %>' runat="server">
            <asp:ListItem Text="Yes" Value="Yes"></asp:ListItem>
            <asp:ListItem Text="No" Value="No"></asp:ListItem>
            </asp:DropDownList>
            </td>
            </tr>
            <tr><td align="right" style="padding-right:5px"><b>Board:</b></td>
            <td ><asp:TextBox ID="vBoardTextBox" Width="100px" runat="server" Text='<%# Bind("vBoard") %>'>
            </asp:TextBox><a href="#" TabIndex="1" onclick="Boardlook1(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator5" ControlToValidate="vBoardTextBox" runat="server" SetFocusOnError="true" Display="Dynamic" ErrorMessage="Board Must Be Enter"></asp:RequiredFieldValidator></td>
            <td colspan="4"><asp:TextBox ID="vBrdDscrTextBox" Width="160px" runat="server" Text='<%# Bind("vBrdDscr") %>'>
            </asp:TextBox></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:TextBox ID="vLengthTextBox" onkeyup="vallen()" MaxLength="8" runat="server" Text='<%# Bind("vLength") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vLengthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:TextBox ID="vWidthTextBox" onkeyup="valwid()" MaxLength="8" runat="server" Text='<%# Bind("vWidth") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vWidthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Depth:</b></td>
            <td><asp:TextBox ID="vDepthTextBox" onkeyup="valdep()" MaxLength="8" runat="server" Text='<%# Bind("vDepth") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vDepthTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Adhesive:</b></td>
            <td><asp:TextBox ID="vJointMatTextBox" runat="server" Text='<%# Bind("vAdhesive") %>'>
            </asp:TextBox><a href="#" TabIndex="1" onclick="adhesiveLook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Top/Dust Flap:</b></td>
            <td><asp:TextBox ID="vDustFlapTextBox" onkeyup="destval()" MaxLength="8" runat="server" Text='<%# Bind("vDustFlap") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vDustFlapTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Fifth Panel:</b></td>
            <td><asp:TextBox ID="vBotFlapTextBox" onkeyup="panelval()" MaxLength="8" runat="server" Text='<%# Bind("vPanel") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vBotFlapTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Lock Tab</b></td>
            <td><asp:TextBox ID="vLockTabTextBox" onkeyup="tabval()" MaxLength="8" runat="server" Text='<%# Bind("vLockTab") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vLockTabTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Glue Lab:</b></td>
            <td><asp:TextBox ID="vTabWidTextBox" onkeyup="glueval()" MaxLength="8" runat="server" Text='<%# Bind("vGlue") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="vTabWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>DK on Length:</b></td>
            <td><asp:TextBox ID="vScoreLenTextBox" onkeyup="sqlen()" MaxLength="8" runat="server" Text='<%# Bind("vDkLen") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vScoreLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>DK on Width:</b></td>
            <td><asp:TextBox ID="vScoreWidTextBox" onkeyup="sqwidth()" MaxLength="8" runat="server" Text='<%# Bind("vDkWid") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vScoreWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>            
            <td align="right" style="padding-right:5px"><b>Tuck:</b></td>
            <td><asp:TextBox ID="vTuckTextBox" onkeyup="tuckval()" MaxLength="8" runat="server" Text='<%# Bind("vTuck") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vTuckTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Lin Inches:</b></td>
            <td><asp:TextBox ID="vJointLenTextBox" onkeyup="jointlen()" MaxLength="8" runat="server" Text='<%# Bind("vLinInc") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vJointLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Blank width:</b></td>
            <td><asp:TextBox ID="vBlankWidTextBox" onkeyup="blankwid()" MaxLength="8" runat="server" Text='<%# Bind("vBlankWid") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vBlankWidTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Blank Length:</b></td>
            <td><asp:TextBox ID="vBlankLenTextBox" onkeyup="blanklen()" MaxLength="8" runat="server" Text='<%# Bind("vBlankLen") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vBlankLenTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Blank Square Feet:</b></td>
            <td><asp:TextBox ID="vBlankSqFtTextBox" onblur="setfocus()" runat="server" Text='<%# Bind("vBlankSqFt") %>'>
            </asp:TextBox><asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vBlankSqFtTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator>
            </td>
            </tr>
            </table></fieldset>
            </td>
            </tr>
            
            <tr><td colspan="4"><asp:Button ID="UpdateButton" runat="server" CssClass="button" OnClick="Save_Click" CausesValidation="True" 
                Text="Save"> </asp:Button>
                <asp:Button ID="auotButton" runat="server" CssClass="button" OnClick="Auto_Save_Click" CausesValidation="False" 
                Text="Save"> </asp:Button>
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
            <td colspan="2"><asp:Label ID="vSmanDscrLabel" Width="160px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vSmanDscr") %>'></asp:Label>
            <b>%:</b>
            <asp:Label ID="vCommLabel" Width="40px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vComm") %>'></asp:Label></td></tr>
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
            <tr><td nowrap align="right" style="padding-right:5px" >
            <b>Quantity:</b> </td>
            <td><asp:Label ID="vQtyLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vQty") %>'></asp:Label></td>
            </tr>
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
            <td colspan="3"><asp:Label ID="vStyleDscrLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vStyleDscr") %>'> </asp:Label></td>
            <td colspan="2">
            
            <b>Metric?:</b><asp:Label ID="vMetricLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vMetric") %>'></asp:Label>
            </td>   </tr>            
            <tr><td align="right" style="padding-right:5px"><b>Board:</b></td>
            <td><asp:Label ID="vBoardLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBoard") %>'></asp:Label></td>
            <td colspan="5"><asp:Label ID="vBrdDscrLabel" Width="200px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBrdDscr") %>'></asp:Label>
            </td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Length:</b></td>
            <td><asp:Label ID="vLengthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLength","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Width:</b></td>
            <td><asp:Label ID="vWidthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vWidth","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Depth:</b></td>
            <td><asp:Label ID="vDepthLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDepth","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Adhesive:</b></td>
            <td><asp:Label ID="vJointMatLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vAdhesive") %>'></asp:Label></td></tr>            
            <tr><td nowrap align="right" style="padding-right:5px"><b>Top/Dust Flap:</b></td>
            <td><asp:Label ID="vDustFlapLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDustFlap","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Fifth Panel:</b></td>
            <td><asp:Label ID="vBotFlapLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vPanel","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Lock Tab:</b></td>
            <td><asp:Label ID="vLockTabLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLockTab","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Glue Lab:</b></td>
            <td><asp:Label ID="vTabWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vGlue","{0:###,##0.0000}") %>'></asp:Label></td></tr>            
            
            <tr>
            <td align="right" style="padding-right:5px"><b>DK on length:</b></td>
            <td><asp:Label ID="vScoreLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDkLen","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>DK on Width:</b></td>
            <td><asp:Label ID="vScoreWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vDkWid","{0:###,##0.0000}") %>'></asp:Label></td>            
            <td align="right" style="padding-right:5px"><b>Tuck:</b></td>
            <td><asp:Label ID="vTuckLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vTuck","{0:###,##0.0000}") %>'></asp:Label></td>
            <td nowrap align="right" style="padding-right:5px"><b>Lin Inches:</b></td>
            <td><asp:Label ID="vJointLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vLinInc","{0:###,##0.0000}") %>'></asp:Label></td></tr>            
            <tr><td align="right" style="padding-right:5px"><b>Blank Width:</b></td>
            <td><asp:Label ID="vBlankWidLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankWid","{0:###,##0.0000}") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Blank Length:</b></td>
            <td><asp:Label ID="vBlankLenLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankLen","{0:###,##0.0000}") %>'></asp:Label></td>
            <td nowrap align="right" style="padding-right:5px"><b>Blank Square Feet:</b></td>
            <td><asp:Label ID="vBlankSqFtLabel" Width="100px" BackColor="Turquoise" BorderStyle="solid" BorderColor="white" BorderWidth="1px" runat="server" Text='<%# Bind("vBlankSqFt","{0:###,##0.0000}") %>'> </asp:Label></td></tr>            
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
        SelectMethod="FoldingSpecs" TypeName="Corrugated">
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
            
            <asp:Parameter Name="prmMetric" Type="String" />
            <asp:Parameter Name="prmAdhe" Type="String" />
            <asp:Parameter Name="prmDustFlap" Type="Decimal" />
            <asp:Parameter Name="prmPanel" Type="Decimal" />
            <asp:Parameter Name="prmLockTab" Type="Decimal" />
            <asp:Parameter Name="prmGlue" Type="Decimal" />
            <asp:Parameter Name="prmScWid" Type="Decimal" />
            <asp:Parameter Name="prmScLen" Type="Decimal" />
            <asp:Parameter Name="prmTuck" Type="Decimal" />
            <asp:Parameter Name="prmLinInc" Type="Decimal" />
            <asp:Parameter Name="prmBlankWid" Type="Decimal" />
            <asp:Parameter Name="prmBlankLen" Type="Decimal" />
            <asp:Parameter Name="prmBlankSqFt" Type="Decimal" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:Parameter Name="prmFromDate" Type="DateTime" />
            <asp:Parameter Name="prmEstDate" Type="DateTime" />
            <asp:Parameter Name="prmModDate" Type="DateTime" />
            <asp:Parameter Name="prmOrderNum" Type="Int32" />
            <asp:Parameter Name="prmOrdDate" Type="DateTime" />
            <asp:Parameter Name="prmQty" Type="Decimal" />
            
            <asp:Parameter Name="prmShipName" Type="String" />
            <asp:Parameter Name="prmAddr" Type="String" />
            <asp:Parameter Name="prmCity" Type="String" />
            <asp:Parameter Name="prmState" Type="String" />
            <asp:Parameter Name="prmZip" Type="String" />
            <asp:Parameter Name="prmAddr2" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="int32" />
            <asp:Parameter Name="prmEstFrom" Type="string" />
            <asp:SessionParameter Name="prmBlankno" SessionField="order_folding_blankno" Type="int32" />
            <asp:Parameter Name="prmAutocalcSelected" Type="string" />
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

