<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfqship" Title="Request for Quote" Codebehind="rfq_ship.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">

<script type="text/javascript">
function ShipIdlook(){ 
  var NewWindow = window.open("ShipIdLook.aspx","ShipToLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ShipIdLookup(ReturnObj1, ReturnObj2, ReturnObj3,ReturnObj4, ReturnObj5,ReturnObj6, ReturnObj7,ReturnObj8, ReturnObj9,ReturnObj10, ReturnObj11,ReturnObj12, ReturnObj13, ReturnObj14,ReturnObj15,ReturnObj16, ReturnObj17,ReturnObj18, ReturnObj19, ReturnObj20)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vShipidTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCarrierTextBox.value = ReturnObj2;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasnoTextBox.value = ReturnObj3;
  
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCascntTextBox.value = ReturnObj4;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCaslenTextBox.value = ReturnObj5;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrwidTextBox.value = ReturnObj6;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasdepTextBox.value = ReturnObj7;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaspalTextBox.value = ReturnObj8;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaswtTextBox.value = ReturnObj9;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$VTrnoTextBox.value = ReturnObj10;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrlenTextBox.value = ReturnObj11;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrdepTextBox.value = ReturnObj12;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasWidTextBox.value = ReturnObj13;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$VShipnameTextBox.value = ReturnObj14;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$VcarrdscrTextBox.value = ReturnObj15;
  //document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vPalletTextBox.value = ReturnObj16;
  
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$TextBox1.value=ReturnObj16;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$TextBox2.value=ReturnObj17;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$TextBox3.value=ReturnObj18;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$TextBox4.value=ReturnObj19;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$TextBox5.value = ReturnObj20;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vShipidTextBox.focus();
}
function Shipcarrlook(){ 
  var NewWindow = window.open("shipcarrlook.aspx","CarrierLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ShipcarrLookup(ReturnObj1, ReturnObj2){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCarrierTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$VcarrdscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCarrierTextBox.focus()
}
function packingcodeLookup(){ 
  var NewWindow = window.open("packingcode.aspx","PackCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function PackingCodeLook(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7){ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasnoTextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCascntTextBox.value = ReturnObj2;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaslenTextBox.value = ReturnObj3;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasWidTextBox.value = ReturnObj4;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasdepTextBox.value = ReturnObj5;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaspalTextBox.value = ReturnObj6;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaswtTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vCasnoTextBox.focus();
 
}
function UnitNoLookup(){ 
  var NewWindow = window.open("UnitNoLook.aspx","UnitNumLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function UnitNoLook(ReturnObj1, ReturnObj2){

  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTrnoTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vPalletTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VTrnoTextBox.focus();
 
}

function decimalval()
{
var len1=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaslenTextBox.value;
var len2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrlenTextBox.value;
var wid1=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasWidTextBox.value;
var wid2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrwidTextBox.value;
var ht1=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasdepTextBox.value;
var ht2=document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrdepTextBox.value;

if(len1>2)
{
var val=len1.indexOf(".");
if(val==-1)
{
if(len1.length>2 && len1.length<4)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCaslenTextBox.value=len1 + ".";
}
}

if(len2>2)
{
var val=len2.indexOf(".");
if(val==-1)
{
if(len2.length>2 && len2.length<4)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrlenTextBox.value=len2 + ".";
}
}

if(wid1>2)
{
var val=wid1.indexOf(".");
if(val==-1)
{
if(wid1.length>2 && wid1.length<4)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasWidTextBox.value=wid1 + ".";
}
}

if(wid2>2)
{
var val=wid2.indexOf(".");
if(val==-1)
{
if(wid2.length>2 && wid2.length<4)
document.forms[0].document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrwidTextBox.value=wid2 + ".";
}
}

if(ht1>2)
{
var val=ht1.indexOf(".");
if(val==-1)
{
if(ht1.length>2 && ht1.length<4)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCasdepTextBox.value=ht1 + ".";
}
}
if(ht2>2)
{
var val=ht2.indexOf(".");
if(val==-1)
{
if(ht2.length>2 && ht2.length<4)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vTrdepTextBox.value=ht2 + ".";
}
}
}
</script>

<div>
<fieldset style="background-color:#EFF3FB; width:620px;">
<legend>Reference Information</legend>
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource3" Width="620px">
       
        <ItemTemplate>
        
           <b>RFQ#:</b>
            <asp:Label ID="aRfqNoLabel" runat="server" BackColor="Turquoise" Width="123px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("aRfqNo") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b>Cust Part#:</b>
            <asp:Label ID="vRfqPartLabel" runat="server" BackColor="Turquoise" Width="133px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqPart") %>'></asp:Label>
            &nbsp;&nbsp;&nbsp;&nbsp;
           <b> Style:</b>
            <asp:Label ID="vRfqstyleLabel" runat="server" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqstyle") %>'></asp:Label>
            <b> </b>
            <asp:Label ID="vRfqstyleDscrLabel" runat="server" BackColor="Turquoise" Width="115px"  BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vRfqstyleDscr") %>'></asp:Label>
        </ItemTemplate>
    </asp:FormView>
    
</fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqItemDscr" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqshipno" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound"
        Style="position: static">
        <EditItemTemplate>
        <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdateButton">
        <fieldset style="background-color:#EFF3FB;">
        <table  style="width:620px;">
        <tr><td><b>Ship To Code:</b> </td>
        <td><asp:TextBox ID="vShipidTextBox" runat="server"   Text='<%# Bind("vShipid") %>'></asp:TextBox><a href="#" tabindex="1" onClick="ShipIdlook(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td align="left" style="padding-left:5px"><asp:TextBox ID="VShipnameTextBox" MaxLength="8" Enabled="false" runat="server" Text='<%# Bind("VShipname") %>'>
            </asp:TextBox> </td></tr>
            
             <tr><td style="width:50px"><b>Address:</b></td>
            <td>
                <asp:TextBox ID="TextBox1" Enabled="false" runat="server" Text='<%# Bind("vShipAddr") %>'></asp:TextBox></td>
                <td></td></tr>
                <tr><td><b></b></td>
                <td><asp:TextBox ID="TextBox2" Enabled="false" runat="server" Text='<%# Bind("vShipAddr2") %>'></asp:TextBox></td></tr>
                <tr><td><b>City:</b></td>
                <td>
                    <asp:TextBox ID="TextBox3" Width="60px" Enabled="false" runat="server" Text='<%# Bind("vShipCity") %>'></asp:TextBox>
                    <asp:TextBox ID="TextBox4" Width="20px" Enabled="false" runat="server" Text='<%# Bind("vShipState") %>'></asp:TextBox>
                    <asp:TextBox ID="TextBox5" Width="30px" Enabled="false"  runat="server" Text='<%# Bind("vShipZip") %>'></asp:TextBox>
                </td></tr>
                
            <tr><td><b>Carrier: </b></td>
        <td><asp:TextBox ID="vCarrierTextBox" MaxLength="5" runat="server"   Text='<%# Bind("vCarrier") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Shipcarrlook(); return false" ><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="left" style="padding-left:5px"><asp:TextBox ID="VcarrdscrTextBox" Enabled="false" runat="server" Text='<%# Bind("Vcarrdscr") %>'>
            </asp:TextBox></td></tr>
            <tr>
            <td><b>Packing Code:</b> </td>
            <td><asp:TextBox ID="vCasnoTextBox" MaxLength="10" runat="server"   Text='<%# Bind("vCasno") %>'>
            </asp:TextBox><a href="#"  tabindex="1" onClick="packingcodeLookup(); return false" ><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="center" style="padding-left:5px;"><b>Unit#:</b></td>
            <td><asp:TextBox ID="VTrnoTextBox" MaxLength="10" runat="server"  Text='<%# Bind("VTrno") %>'>
            </asp:TextBox><a href="#"  tabindex="1" onClick="UnitNoLookup(); return false" ><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td><b>:</b></td>
            <td><asp:TextBox ID="vPalletTextBox" Width="100px" Enabled="false"  runat="server" Text='<%# Bind("vPallet") %>'>
            </asp:TextBox></td></tr>  
           <%-- <tr><td><b>Cost/ea: </b></td>
            <td><asp:TextBox ID="vCasCostTextBox" runat="server" Text='<%# Bind("vCasCost") %>'>
            </asp:TextBox></td>
            <td align="center" style="padding-left:5px;"><b>Cost/ea</b></td>
            <td><asp:TextBox ID="vTrCostTextBox" runat="server" Text='<%# Bind("vTrCost") %>'>
            </asp:TextBox></td></tr>--%>  
             <tr><td><b>Count:</b></td>
            <td><asp:TextBox ID="vCascntTextBox" MaxLength="5" runat="server" Text='<%# Bind("vCascnt") %>'>
            </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Number" ControlToValidate="vCascntTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
            </td>
            <td align="center" style="padding-left:5px;"><b>Count:</b></td>
            <td> <asp:TextBox ID="vTrcntTextBox" MaxLength="5" runat="server" Text='<%# Bind("vTrcnt") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Number" ControlToValidate="vTrcntTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="integer"></asp:CompareValidator>
            </td></tr>  
             <tr><td><b>Length:</b></td>
            <td><asp:TextBox ID="vCaslenTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vCaslen") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Number" ControlToValidate="vCaslenTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td>
            <td align="center" style="padding-left:5px;"><b>Length:</b></td>
            <td><asp:TextBox ID="vTrlenTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vTrlen") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only Number" ControlToValidate="vTrlenTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td></tr>  
             <tr><td><b>width:</b></td>
            <td><asp:TextBox ID="vCasWidTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vCasWid") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only Number" ControlToValidate="vTrwidTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td>
            <td align="center" style="padding-left:5px;"><b>width:</b></td>
            <td><asp:TextBox ID="vTrwidTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vTrwid") %>'>
            </asp:TextBox>
            
            <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Number" ControlToValidate="vCasWidTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td></tr>  
             <tr><td><b>Depth:</b></td>
            <td> <asp:TextBox ID="vCasdepTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vCasdep") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Number" ControlToValidate="vCasdepTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td>
            <td align="center" style="padding-left:5px;"><b>Height:</b></td>
            <td><asp:TextBox ID="vTrdepTextBox" MaxLength="8" onkeyup="javascript:decimalval()" runat="server" Text='<%# Bind("vTrdep") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator8" runat="server" ErrorMessage="Only Number" ControlToValidate="vTrdepTextBox" Display="Dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
            </td></tr>  
             <tr><td><b>Cases/Pall:</b></td>
            <td><asp:TextBox ID="vCaspalTextBox" Enabled="false"  runat="server" Text='<%# Bind("vCaspal") %>'>
            </asp:TextBox></td>
            <td align="center" style="padding-left:5px;"><b># of Layer</b></td>
            <td><asp:TextBox ID="vTrcasTextBox" Enabled="false" runat="server" Text='<%# Bind("vTrcas") %>'>
            </asp:TextBox></td></tr>   
            <tr><td align="center" style="padding-left:5px;"><b>Lbs/Case:</b></td>
            <td><asp:TextBox ID="vCaswtTextBox" Enabled="false"  runat="server" Text='<%# Bind("vCaswt") %>'>
            </asp:TextBox></td>
            <td align="center" style="padding-left:5px;"><b>Weight Per M:</b></td>
            <td><asp:TextBox ID="vWeightTextBox" Enabled="false"  Width="100px" runat="server" Text='<%# Bind("vWeight") %>'>
            </asp:TextBox></td>
            </tr>  
        </table>
        <table>
           <tr>
           <td width="100"></td>
           <td align="center"><asp:Button ID="UpdateButton" CssClass="buttonM" runat="server" OnClick="UpdateButton_click"
                Text="Save" Font-Bold="true">
            </asp:Button></td>
           <td align="center"><asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel" Font-Bold="true"></asp:Button></td>
           </tr>
           
            </table>
      <%-- <table style="width:620px;"><tr><td>Ship To: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;<asp:TextBox ID="vShipidTextBox" runat="server" Text='<%# Bind("vShipid") %>'>
            </asp:TextBox><a href="#" onClick="ShipIdlook(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            &nbsp; &nbsp; Name &nbsp; :&nbsp; &nbsp;<asp:TextBox ID="VShipnameTextBox" runat="server" Text='<%# Bind("VShipname") %>'>
            </asp:TextBox> </td></tr>           
            
             
            <tr><td>Carrier: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;<asp:TextBox ID="vCarrierTextBox" runat="server" Text='<%# Bind("vCarrier") %>'>
            </asp:TextBox><a href="#" onClick="Shipcarrlook(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            Description:
                &nbsp;<asp:TextBox ID="vCarrdscrTextBox" runat="server" Text='<%# Bind("Vcarrdscr") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Packing Code: &nbsp;<asp:TextBox ID="vCasnoTextBox" runat="server" Text='<%# Bind("vCasno") %>'>
            </asp:TextBox><a href="#" onClick="PackCodeLookup(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                &nbsp; &nbsp;&nbsp; &nbsp;Unit#:&nbsp; &nbsp;<asp:TextBox ID="VTrnoTextBox" runat="server" Text='<%# Bind("VTrno") %>'>
            </asp:TextBox><a href="#" onClick="UnitNoLookup(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a><br /></td></tr>
          <tr><td>Weight Per M: &nbsp;<asp:TextBox ID="vWeightTextBox" runat="server" Text='<%# Bind("vWeight") %>'>
            </asp:TextBox>
              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              &nbsp; &nbsp; &nbsp; &nbsp; Cost/ea:&nbsp; &nbsp;<asp:TextBox ID="vCasCostTextBox" runat="server" Text='<%# Bind("vCasCost") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Cost/ea: &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;<asp:TextBox ID="vTrCostTextBox" runat="server" Text='<%# Bind("vTrCost") %>'>
            </asp:TextBox>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; Count:&nbsp; &nbsp;<asp:TextBox ID="vCascntTextBox" runat="server" Text='<%# Bind("vCascnt") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Count: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="vTrcntTextBox" runat="server" Text='<%# Bind("vTrcnt") %>'>
            </asp:TextBox>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; Length:&nbsp; &nbsp;<asp:TextBox ID="vCaslenTextBox" runat="server" Text='<%# Bind("vCaslen") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Length: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;<asp:TextBox ID="vTrlenTextBox" runat="server" Text='<%# Bind("vTrlen") %>'>
            </asp:TextBox>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Width:&nbsp;
                <asp:TextBox ID="vTrwidTextBox" runat="server" Text='<%# Bind("vTrwid") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Width: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="vCasWidTextBox" runat="server" Text='<%# Bind("vCasWid") %>'>
            </asp:TextBox></td></tr>
           <tr><td>Depth: &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
               <asp:TextBox ID="vCasdepTextBox" runat="server" Text='<%# Bind("vCasdep") %>'>
            </asp:TextBox>
               &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
               &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Depth: &nbsp;<asp:TextBox ID="vTrdepTextBox" runat="server" Text='<%# Bind("vTrdep") %>'>
            </asp:TextBox><br /></td></tr> 
            <tr><td>Cases/Pall: &nbsp; &nbsp; &nbsp;
            <asp:TextBox ID="vCaspalTextBox" runat="server" Text='<%# Bind("vCaspal") %>'>
            </asp:TextBox>
                &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
            # Of Layers:&nbsp; &nbsp;<asp:TextBox ID="vTrcasTextBox" runat="server" Text='<%# Bind("vTrcas") %>'>
            </asp:TextBox><br /></td></tr>
            <tr><td>Lbs/Case: &nbsp; &nbsp; &nbsp; &nbsp;<asp:TextBox ID="vCaswtTextBox" runat="server" Text='<%# Bind("vCaswt") %>'>
            </asp:TextBox></td></tr>
            
            <br /><br />--%>
           
            </fieldset></asp:Panel>
        </EditItemTemplate>
        <%--<InsertItemTemplate>
            vShipid:
            <asp:TextBox ID="vShipidTextBox" runat="server" Text='<%# Bind("vShipid") %>'>
            </asp:TextBox><br />
             VShipname
            <asp:TextBox ID="VShipnameTextBox" runat="server" Text='<%# Bind("VShipname") %>'></asp:TextBox><br />
            
            vCarrier:
            <asp:TextBox ID="vCarrierTextBox" runat="server" Text='<%# Bind("vCarrier") %>'>
            </asp:TextBox><br />
             vCarrdscr:
            <asp:TextBox ID="vCarrdscrTextBox" runat="server" Text='<%# Bind("Vcarrdscr") %>'></asp:TextBox><br />
            vCasno:
            <asp:TextBox ID="vCasnoTextBox" runat="server" Text='<%# Bind("vCasno") %>'>
            </asp:TextBox><br />
            VTrno:
            <asp:TextBox ID="VTrnoTextBox" runat="server" Text='<%# Bind("VTrno") %>'>
            </asp:TextBox><br />
            vWeight:
            <asp:TextBox ID="vWeightTextBox" runat="server" Text='<%# Bind("vWeight") %>'>
            </asp:TextBox><br />
            vCasCost:
            <asp:TextBox ID="vCasCostTextBox" runat="server" Text='<%# Bind("vCasCost") %>'>
            </asp:TextBox><br />
            vTrCost:
            <asp:TextBox ID="vTrCostTextBox" runat="server" Text='<%# Bind("vTrCost") %>'>
            </asp:TextBox><br />
            vCascnt:
            <asp:TextBox ID="vCascntTextBox" runat="server" Text='<%# Bind("vCascnt") %>'>
            </asp:TextBox><br />
            vTrcnt:
            <asp:TextBox ID="vTrcntTextBox" runat="server" Text='<%# Bind("vTrcnt") %>'>
            </asp:TextBox><br />
            vCaslen:
            <asp:TextBox ID="vCaslenTextBox" runat="server" Text='<%# Bind("vCaslen") %>'>
            </asp:TextBox><br />
            vTrlen:
            <asp:TextBox ID="vTrlenTextBox" runat="server" Text='<%# Bind("vTrlen") %>'>
            </asp:TextBox><br />
            vTrwid:
            <asp:TextBox ID="vTrwidTextBox" runat="server" Text='<%# Bind("vTrwid") %>'>
            </asp:TextBox><br />
            vCasWid:
            <asp:TextBox ID="vCasWidTextBox" runat="server" Text='<%# Bind("vCasWid") %>'>
            </asp:TextBox><br />
            vCasdep:
            <asp:TextBox ID="vCasdepTextBox" runat="server" Text='<%# Bind("vCasdep") %>'>
            </asp:TextBox><br />
            vTrdep:
            <asp:TextBox ID="vTrdepTextBox" runat="server" Text='<%# Bind("vTrdep") %>'>
            </asp:TextBox><br />
            vCaspal:
            <asp:TextBox ID="vCaspalTextBox" runat="server" Text='<%# Bind("vCaspal") %>'>
            </asp:TextBox><br />
            vTrcas:
            <asp:TextBox ID="vTrcasTextBox" runat="server" Text='<%# Bind("vTrcas") %>'>
            </asp:TextBox><br />
            vCaswt:
            <asp:TextBox ID="vCaswtTextBox" runat="server" Text='<%# Bind("vCaswt") %>'>
            </asp:TextBox><br />
            RfqSRowid:
            <asp:TextBox ID="RfqSRowidTextBox" runat="server" Text='<%# Bind("RfqSRowid") %>'>
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
        <table style="width:620px;">
        <tr>
        <td align="right" style="padding-right:5px;"><b>Ship To Code:</b>
            
            <asp:Label ID="vShipidLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipid") %>'></asp:Label><br /></td>
            
            <td align="left" style="padding-left:5px;"><b></b>
            <asp:Label ID="Label1" BackColor="Turquoise" Width="200px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("VShipname") %>'></asp:Label><br /></td></tr>
            
            <tr><td align="right" style="padding-right:5px;"><b>Address:</b>           
            <asp:Label ID="address1Label" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipAddr") %>' ></asp:Label><br /></td></tr>
            <td align="right" style="padding-right:5px;"><b></b>            
            <asp:Label ID="Address2Label" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipAddr2") %>'></asp:Label><br /></td>
            <tr>
            <td align="right" style="padding-right:5px;" nowrap><b>City:</b>            
            <asp:Label ID="CityLabel" BackColor="Turquoise" Width="65px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipCity") %>'></asp:Label>
            <asp:Label ID="stateLabel" BackColor="Turquoise" Width="20px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipState") %>'></asp:Label>
            <asp:Label ID="zipLabel" BackColor="Turquoise" Width="35px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vShipZip") %>'></asp:Label>
            <br /></td></tr>
            
            <tr>
        <td align="right" style="padding-right:5px;"><b> Carrier:</b>
            <asp:Label ID="vCarrierLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCarrier") %>'></asp:Label><br /></td>
            <td align="left" style="padding-left:5px;"><b></b>
            <asp:Label ID="Label2" BackColor="Turquoise" Width="200px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("Vcarrdscr") %>'></asp:Label><br /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b>Packing Code:</b>
            <asp:Label ID="vCasnoLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCasno") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b>Unit#:</b>
            <asp:Label ID="VTrnoLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("VTrno") %>'></asp:Label><br /></td>
           <td align="left" style="padding-left:5px;"><b> :</b>
            <asp:Label ID="vPalletLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vPallet") %>'></asp:Label><br /></td></tr>
           <%--<tr>
           <td align="right" style="padding-right:5px;"><b>  Cost/ea:</b>
           <asp:Label ID="vCasCostLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCasCost") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> Cost/ea:</b>
            <asp:Label ID="vTrCostLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrCost") %>'></asp:Label><br /></td>
            </tr>--%>
            <tr><td align="right" style="padding-right:5px;"><b> Count:</b>
            <asp:Label ID="vCascntLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCascnt") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> Count:</b>
            <asp:Label ID="vTrcntLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrcnt") %>'></asp:Label><br /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b> Length:</b>
            <asp:Label ID="vCaslenLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCaslen") %>'></asp:Label><br />
            <td align="right" style="padding-right:5px;"><b> Length:</b>
            <asp:Label ID="vTrlenLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrlen") %>'></asp:Label><br /></td></tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b> Width:</b>
            <asp:Label ID="vCasWidLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCasWid") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> Width:</b>
            <asp:Label ID="vTrwidLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrwid") %>'></asp:Label><br /></td>
            </tr>
            <tr><td align="right" style="padding-right:5px;"><b> Depth:</b>
            <asp:Label ID="vCasdepLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCasdep") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> Height:</b>
            <asp:Label ID="vTrdepLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrdep") %>'></asp:Label><br /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b> Cases/Pall: </b>
            <asp:Label ID="vCaspalLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCaspal") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> # Of Layer:</b>
            <asp:Label ID="vTrcasLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vTrcas") %>'></asp:Label><br /></td></tr>
            <tr><td align="right" style="padding-right:5px;"><b> Lbs/Cases:</b>
            <asp:Label ID="vCaswtLabel" BackColor="Turquoise" Width="130px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCaswt") %>'></asp:Label><br /></td>
            <td align="right" style="padding-right:5px;"><b> Weight Per M:</b>
            <asp:Label ID="vWeightLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vWeight") %>'></asp:Label><br /></td></tr>
            <%--RfqSRowid:
            <asp:Label ID="RfqSRowidLabel" runat="server" Text='<%# Bind("RfqSRowid") %>'></asp:Label><br />--%>
            </table>
            <br /><br />
        <table><tr>
        <td width="100px"></td>
        
        <td align="center"><asp:Button ID="EditButton" CausesValidation="false" Text="Update" CssClass="buttonM" runat="server" CommandName="Edit" /></td>
        </tr></table>
            </fieldset></asp:Panel>
        </ItemTemplate>
        
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqShipping" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue ="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmRfqNo" SessionField="rfqshipno" Type="Int32" />
            <asp:SessionParameter Name="RfqSeq" SessionField="rfqshippart" Type="Int32" />
            <asp:Parameter Name="prmShipid" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmCarrier" Type="String" />
            <asp:Parameter Name="prmdscr" Type="String" />
            <asp:Parameter Name="prmCasno" Type="String" />
            <asp:Parameter Name="prmTrno" Type="String" />
            <asp:Parameter Name="prmWeight" Type="Decimal" />
            <asp:Parameter Name="prmCasCost" Type="Decimal" />
            <asp:Parameter Name="prmTrCost" Type="Decimal" />
            <asp:Parameter Name="prmCascnt" Type="Int32" />
            <asp:Parameter Name="prmTrcnt" Type="Int32" />
            <asp:Parameter Name="prmCaslen" Type="Decimal" />
            <asp:Parameter Name="prmTrlen" Type="Decimal" />
            <asp:Parameter Name="prmCasWid" Type="Decimal" />
            <asp:Parameter Name="prmTrwid" Type="Decimal" />
            <asp:Parameter Name="prmCasdep" Type="Decimal" />
            <asp:Parameter Name="prTrdep" Type="Decimal" />
            <asp:Parameter Name="prmCaspal" Type="Int32" />
            <asp:Parameter Name="prmTrcas" Type="Int32" />
            <asp:Parameter Name="prmCaswt" Type="Decimal" />
            <asp:Parameter Name="RfqSRowid" Type="Int64" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
</asp:Content>
 