<%@ Page Language="C#" MasterPageFile="MasterPage5.master" Debug="true" AutoEventWireup="true" Inherits="rfqprinting" Title="Request for Quote" Codebehind="rfq_printing.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="server">

<script type="text/javascript">
function assignval()
{
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps1TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps1TextBox.value=1;

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps2TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps2TextBox.value=1;

if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps3TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps3TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps4TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps4TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps5TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps5TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps6TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps6TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps7TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps7TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps8TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps8TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps9TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps9TextBox.value=1;
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps10TextBox.value==0)
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIps10TextBox.value=1;


if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode1TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr1TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper1TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode2TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr2TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper2TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode3TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr3TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper3TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode4TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr4TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper4TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode5TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr5TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper5TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode6TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr6TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper6TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode7TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr7TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper7TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode8TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr8TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper8TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode9TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr9TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper9TextBox.value="0";
}
if(document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode10TextBox.value=="")
{
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr10TextBox.value="";
document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper10TextBox.value="0";
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
function Code2look()
  { 
  var NewWindow = window.open("Code2Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code2LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode2TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr2TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper2TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper2TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode2TextBox.focus();
}
function Code3look()
  { 
  var NewWindow = window.open("Code3Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code3LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode3TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr3TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper3TextBox.value="25";
  else
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper3TextBox.value="100";
document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode3TextBox.focus();
}
function Code4look()
  { 
  var NewWindow = window.open("Code4Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code4LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode4TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr4TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper4TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper4TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode4TextBox.focus();
}
function Code5look()
  { 
  var NewWindow = window.open("Code5Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code5LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode5TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr5TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper5TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper5TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode5TextBox.focus();
}
function Code6look()
  {
  var NewWindow = window.open("Code6Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code6LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode6TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr6TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper6TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper6TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode6TextBox.focus();
}
function Code7look()
  { 
  var NewWindow = window.open("Code7Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code7LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode7TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr7TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper7TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper7TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode7TextBox.focus();
}
function Code8look()
  { 
  var NewWindow = window.open("Code8Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code8LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode8TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr8TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper8TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper8TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode8TextBox.focus();
}
function Code9look()
  { 
  var NewWindow = window.open("Code9Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code9LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode9TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr9TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper9TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper9TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode9TextBox.focus();
}
function Code10look()
  { 
  var NewWindow = window.open("Code10Look.aspx","MaterialWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }

function Code10LookUp(ReturnObj1,ReturnObj2,ReturnObj3)
{ 
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIcode10TextBox.value = ReturnObj1;
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vCdscr10TextBox.value=ReturnObj2;
  if(ReturnObj3=="I")
  document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper10TextBox.value="25";
  else
      document.forms[0].ctl00$ContentPlaceHolder1$FormView1$vIper10TextBox.value = "100";
  document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vIcode10TextBox.focus();
}

 function validate(evt)
 {
   var charcode = (evt.which) ? evt.which : event.keyCode
   if(charcode > 31 && (charcode < 48 || charcode > 57) )
   return false;
   
   return true;
  
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
            <asp:SessionParameter Name="prmRfqNo" SessionField="rfqPrintno" Type="Int32" />
            <asp:SessionParameter SessionField="list_rfq_cust_part_no" Name="prmPartNo" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
<div>
    <br />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound"
        Style="position: static">
        <EditItemTemplate>
        <asp:Panel ID="Edit1_Panel" runat="server" DefaultButton="UpdateButton">
        <fieldset style="background-color:#EFF3FB;">
        <table style="width:620px;">
        <tr>
        <td><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Color: &nbsp;&nbsp;&nbsp;
         <asp:TextBox ID="vPcolTextBox" onkeyup="assignval()" Width="15px" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("vPcol") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vPcolTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Cannot Blank"></asp:RequiredFieldValidator>
            </b></td>
        <td><b>Passes: &nbsp;&nbsp;&nbsp;
        <asp:TextBox ID="vPassTextBox" Width="15px" onkeyup="assignval()" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("vPass") %>'>
            </asp:TextBox></b></td>
        <td><b>Coating:&nbsp;&nbsp;&nbsp;
        <asp:TextBox ID="vCoatTextBox" Width="15px" onkeyup="assignval()" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("vCoat") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="vCoatTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="Cannot Blank"></asp:RequiredFieldValidator>
            </b></td>
        </tr>
        </table>
        <b>Colordesc.:</b> &nbsp;&nbsp;&nbsp;<b><asp:TextBox ID="vColdscrTextBox" onkeyup="assignval()" Width="220px" runat="server" Text='<%# Bind("vColdscr") %>'>
            </asp:TextBox></b>
            <table style="width:620px;">
            <tr>
            <td><b>Pass</b></td>
            <td><b>Code</b></td>
            <td><b>Description</b></td>
            <td><b>Coverage%</b></td>
            </tr>
            <tr>
            <td width="70px"><b><asp:TextBox ID="vIps1TextBox"  onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" runat="server" Text='<%# Bind("vIps1") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode1TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode1") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Codelook(); return false" ><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td width="180px"><b><asp:TextBox ID="vCdscr1TextBox" Enabled="false" onkeyup="assignval()" Width="130px" MaxLength="20" runat="server" Text='<%# Bind("vCdscr1") %>'>
            </asp:TextBox></b></td>
            <td width="100px"><b><asp:TextBox ID="vIper1TextBox" onkeyup="assignval()" Width="20px" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper1") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps2TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps2") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode2TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode2") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code2look(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr2TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr2") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper2TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper2") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps3TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps3") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode3TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode3") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code3look(); return false" ><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr3TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr3") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper3TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper3") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps4TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)"  Width="15px" MaxLength="2" Text='<%# Bind("vIps4") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode4TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode4") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code4look(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr4TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr4") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper4TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper4") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps5TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps5") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode5TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode5") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code5look(); return false" ><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr5TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr5") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper5TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper5") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps6TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps6") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode6TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode6") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code6look(); return false" ><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr6TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr6") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper6TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper6") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps7TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps7") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode7TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode7") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code7look(); return false" ><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr7TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr7") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper7TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper7") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps8TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps8") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode8TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode8") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code8look(); return false" ><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr8TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr8") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper8TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper8") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps9TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps9") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode9TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode9") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code9look(); return false" ><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr9TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr9") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper9TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper9") %>'>
            </asp:TextBox></b></td>
            </tr>
            <tr>
            <td><b><asp:TextBox ID="vIps10TextBox" runat="server" onkeyup="assignval()" onkeypress="return validate(event)" Width="15px" MaxLength="2" Text='<%# Bind("vIps10") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIcode10TextBox" Width="70px" onkeyup="assignval()"  MaxLength="10" runat="server" Text='<%# Bind("vIcode10") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Code10look(); return false" ><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>
            <td><b><asp:TextBox ID="vCdscr10TextBox" Enabled="false" Width="130px" onkeyup="assignval()" MaxLength="20" runat="server" Text='<%# Bind("vCdscr10") %>'>
            </asp:TextBox></b></td>
            <td><b><asp:TextBox ID="vIper10TextBox" Width="20px" onkeyup="assignval()" onkeypress="return validate(event)" MaxLength="3" runat="server" Text='<%# Bind("vIper10") %>'>
            </asp:TextBox></b></td>
            </tr>
            </table>
        
           <br /><br />
           <table>
           <tr>
           <td width="100px"></td>
           <td align="center"><asp:Button ID="UpdateButton" runat="server" OnClick="UpdateButton_click"
                Text="Save" Font-Bold="true">
            </asp:Button></td>
           <td align="center"><asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel" Font-Bold="true"></asp:Button></td>
           </tr>
           </table>
           <%-- RfqRowid:
            <asp:TextBox ID="RfqRowidTextBox" runat="server" Text='<%# Bind("RfqRowid") %>'>
            </asp:TextBox><br />--%>
            
            
            
            </fieldset></asp:Panel>
        </EditItemTemplate>
        <%--<InsertItemTemplate>
            vPcol:
            <asp:TextBox ID="vPcolTextBox" runat="server" Text='<%# Bind("vPcol") %>'>
            </asp:TextBox><br />
            vPass:
            <asp:TextBox ID="vPassTextBox" runat="server" Text='<%# Bind("vPass") %>'>
            </asp:TextBox><br />
            vCoat:
            <asp:TextBox ID="vCoatTextBox" runat="server" Text='<%# Bind("vCoat") %>'>
            </asp:TextBox><br />
            vColdscr:
            <asp:TextBox ID="vColdscrTextBox" runat="server" Text='<%# Bind("vColdscr") %>'>
            </asp:TextBox><br />
            vIps1:
            <asp:TextBox ID="vIps1TextBox" runat="server" Text='<%# Bind("vIps1") %>'>
            </asp:TextBox><br />
            vIps2:
            <asp:TextBox ID="vIps2TextBox" runat="server" Text='<%# Bind("vIps2") %>'>
            </asp:TextBox><br />
            vIps3:
            <asp:TextBox ID="vIps3TextBox" runat="server" Text='<%# Bind("vIps3") %>'>
            </asp:TextBox><br />
            vIps4:
            <asp:TextBox ID="vIps4TextBox" runat="server" Text='<%# Bind("vIps4") %>'>
            </asp:TextBox><br />
            vIps5:
            <asp:TextBox ID="vIps5TextBox" runat="server" Text='<%# Bind("vIps5") %>'>
            </asp:TextBox><br />
            vIps6:
            <asp:TextBox ID="vIps6TextBox" runat="server" Text='<%# Bind("vIps6") %>'>
            </asp:TextBox><br />
            vIps7:
            <asp:TextBox ID="vIps7TextBox" runat="server" Text='<%# Bind("vIps7") %>'>
            </asp:TextBox><br />
            vIps8:
            <asp:TextBox ID="vIps8TextBox" runat="server" Text='<%# Bind("vIps8") %>'>
            </asp:TextBox><br />
            vIps9:
            <asp:TextBox ID="vIps9TextBox" runat="server" Text='<%# Bind("vIps9") %>'>
            </asp:TextBox><br />
            vIps10:
            <asp:TextBox ID="vIps10TextBox" runat="server" Text='<%# Bind("vIps10") %>'>
            </asp:TextBox><br />
            vIcode1:
            <asp:TextBox ID="vIcode1TextBox" runat="server" Text='<%# Bind("vIcode1") %>'>
            </asp:TextBox><br />
            vIcode2:
            <asp:TextBox ID="vIcode2TextBox" runat="server" Text='<%# Bind("vIcode2") %>'>
            </asp:TextBox><br />
            vIcode3:
            <asp:TextBox ID="vIcode3TextBox" runat="server" Text='<%# Bind("vIcode3") %>'>
            </asp:TextBox><br />
            vIcode4:
            <asp:TextBox ID="vIcode4TextBox" runat="server" Text='<%# Bind("vIcode4") %>'>
            </asp:TextBox><br />
            vIcode5:
            <asp:TextBox ID="vIcode5TextBox" runat="server" Text='<%# Bind("vIcode5") %>'>
            </asp:TextBox><br />
            vIcode6:
            <asp:TextBox ID="vIcode6TextBox" runat="server" Text='<%# Bind("vIcode6") %>'>
            </asp:TextBox><br />
            vIcode7:
            <asp:TextBox ID="vIcode7TextBox" runat="server" Text='<%# Bind("vIcode7") %>'>
            </asp:TextBox><br />
            vIcode8:
            <asp:TextBox ID="vIcode8TextBox" runat="server" Text='<%# Bind("vIcode8") %>'>
            </asp:TextBox><br />
            vIcode9:
            <asp:TextBox ID="vIcode9TextBox" runat="server" Text='<%# Bind("vIcode9") %>'>
            </asp:TextBox><br />
            vIcode10:
            <asp:TextBox ID="vIcode10TextBox" runat="server" Text='<%# Bind("vIcode10") %>'>
            </asp:TextBox><br />
            vCdscr1:
            <asp:TextBox ID="vCdscr1TextBox" runat="server" Text='<%# Bind("vCdscr1") %>'>
            </asp:TextBox><br />
            vCdscr2:
            <asp:TextBox ID="vCdscr2TextBox" runat="server" Text='<%# Bind("vCdscr2") %>'>
            </asp:TextBox><br />
            vCdscr3:
            <asp:TextBox ID="vCdscr3TextBox" runat="server" Text='<%# Bind("vCdscr3") %>'>
            </asp:TextBox><br />
            vCdscr4:
            <asp:TextBox ID="vCdscr4TextBox" runat="server" Text='<%# Bind("vCdscr4") %>'>
            </asp:TextBox><br />
            vCdscr5:
            <asp:TextBox ID="vCdscr5TextBox" runat="server" Text='<%# Bind("vCdscr5") %>'>
            </asp:TextBox><br />
            vCdscr6:
            <asp:TextBox ID="vCdscr6TextBox" runat="server" Text='<%# Bind("vCdscr6") %>'>
            </asp:TextBox><br />
            vCdscr7:
            <asp:TextBox ID="vCdscr7TextBox" runat="server" Text='<%# Bind("vCdscr7") %>'>
            </asp:TextBox><br />
            vCdscr8:
            <asp:TextBox ID="vCdscr8TextBox" runat="server" Text='<%# Bind("vCdscr8") %>'>
            </asp:TextBox><br />
            vCdscr9:
            <asp:TextBox ID="vCdscr9TextBox" runat="server" Text='<%# Bind("vCdscr9") %>'>
            </asp:TextBox><br />
            vCdscr10:
            <asp:TextBox ID="vCdscr10TextBox" runat="server" Text='<%# Bind("vCdscr10") %>'>
            </asp:TextBox><br />
            vIper1:
            <asp:TextBox ID="vIper1TextBox" runat="server" Text='<%# Bind("vIper1") %>'>
            </asp:TextBox><br />
            vIper2:
            <asp:TextBox ID="vIper2TextBox" runat="server" Text='<%# Bind("vIper2") %>'>
            </asp:TextBox><br />
            vIper3:
            <asp:TextBox ID="vIper3TextBox" runat="server" Text='<%# Bind("vIper3") %>'>
            </asp:TextBox><br />
            vIper4:
            <asp:TextBox ID="vIper4TextBox" runat="server" Text='<%# Bind("vIper4") %>'>
            </asp:TextBox><br />
            vIper5:
            <asp:TextBox ID="vIper5TextBox" runat="server" Text='<%# Bind("vIper5") %>'>
            </asp:TextBox><br />
            vIper6:
            <asp:TextBox ID="vIper6TextBox" runat="server" Text='<%# Bind("vIper6") %>'>
            </asp:TextBox><br />
            vIper7:
            <asp:TextBox ID="vIper7TextBox" runat="server" Text='<%# Bind("vIper7") %>'>
            </asp:TextBox><br />
            vIper8:
            <asp:TextBox ID="vIper8TextBox" runat="server" Text='<%# Bind("vIper8") %>'>
            </asp:TextBox><br />
            vIper9:
            <asp:TextBox ID="vIper9TextBox" runat="server" Text='<%# Bind("vIper9") %>'>
            </asp:TextBox><br />
            vIper10:
            <asp:TextBox ID="vIper10TextBox" runat="server" Text='<%# Bind("vIper10") %>'>
            </asp:TextBox><br />--%>
           <%-- RfqRowid:
            <asp:TextBox ID="RfqRowidTextBox" runat="server" Text='<%# Bind("RfqRowid") %>'>
            </asp:TextBox><br />--%>
            <%--<asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:LinkButton>
            <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </InsertItemTemplate>--%>
        <ItemTemplate>
        <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="EditButton">
        <fieldset style="background-color:#EFF3FB;">
        <table style="width:620px;">
        <tr>
        <td><b>Colors:</b></td>
        <td><b><asp:Label ID="Label1" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vPcol") %>'></asp:Label></b></td>
        <td><b>Passes:</b></td>
        <td><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vPass") %>'></asp:Label></b></td>
        <td><b>Coating:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("vCoat") %>'></asp:Label></b></td>
        <%--<td><b>Pass:</b></td>
        <td><b></b></td>--%>
        </tr>
        </table>
        <b>Colordesc.:</b> &nbsp;&nbsp;&nbsp;<b><asp:Label ID="vColdscrLabel" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vColdscr") %>'></asp:Label></b>
        <table style="width:620px;">
        <tr>
        <td><b>Pass</b></td>
        <td><b>Code</b></td>
        <td><b>Description</b></td>
        <td><b>Coverage%</b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps1Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps1") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode1Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode1") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr1Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr1") %>'></asp:Label></b></td>
        <td ><b><asp:Label ID="vIper1Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper1") %>'></asp:Label><asp:Label ID="lab1" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps2Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps2") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode2Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode2") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr2Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr2") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper2Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper2") %>'></asp:Label><asp:Label ID="Label4" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps3Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps3") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode3Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode3") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr3Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr3") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper3Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper3") %>'></asp:Label><asp:Label ID="Label5" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps4Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps4") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode4Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode4") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr4Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr4") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper4Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper4") %>'></asp:Label><asp:Label ID="Label6" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps5Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps5") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode5Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode5") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr5Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr5") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper5Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper5") %>'></asp:Label><asp:Label ID="Label7" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps6Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps6") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode6Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode6") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr6Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr6") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper6Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper6") %>'></asp:Label><asp:Label ID="Label8" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps7Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps7") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode7Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode7") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr7Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr7") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper7Label" dir="rtl"  BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper7") %>'></asp:Label><asp:Label ID="Label9" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps8Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps8") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode8Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode8") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr8Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr8") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper8Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper8") %>'></asp:Label><asp:Label ID="Label10" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps9Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps9") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode9Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode9") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr9Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr9") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper9Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper9") %>'></asp:Label><asp:Label ID="Label11" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        <tr>
        <td><b><asp:Label ID="vIps10Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIps10") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIcode10Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIcode10") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vCdscr10Label" BackColor="Turquoise" Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCdscr10") %>'></asp:Label></b></td>
        <td><b><asp:Label ID="vIper10Label" dir="rtl" BackColor="Turquoise" Width="50px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("vIper10") %>'></asp:Label><asp:Label ID="Label12" BackColor="Turquoise" runat="server" Text="%"></asp:Label></b></td>
        </tr>
        
        </table>
        <br /><br />
        <table><tr>
        <td width="100"></td>
        
        <td align="center"><asp:Button ID="EditButton" CausesValidation="false" Text="Update" CssClass="buttonM" runat="server" CommandName="Edit" /></td>
        </tr></table>
        </fieldset>
            </asp:Panel>
            <%--RfqRowid:
            <asp:Label ID="RfqRowidLabel" runat="server" Text='<%# Bind("RfqRowid") %>'></asp:Label><br />--%>
        </ItemTemplate>
        
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="RfqPrinting" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue ="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmRfqNo" SessionField="rfqPrintno" Type="Int32" />
            <asp:SessionParameter Name="RfqSeq" SessionField="rfqprintpart" Type="Int32" />
            
            
            <asp:Parameter Name="prmPcol" Type="Int32" />
            <asp:Parameter Name="prmPass" Type="Int32" />
            <asp:Parameter Name="prmCoat" Type="Int32" />
            <asp:Parameter Name="prmColdscr" Type="String" />
            <asp:Parameter Name="prmIps1" Type="Int32" />
            <asp:Parameter Name="prmIps2" Type="Int32" />
            <asp:Parameter Name="prmIps3" Type="Int32" />
            <asp:Parameter Name="prmIps4" Type="Int32" />
            <asp:Parameter Name="prmIps5" Type="Int32" />
            <asp:Parameter Name="prmIps6" Type="Int32" />
            <asp:Parameter Name="prmIps7" Type="Int32" />
            <asp:Parameter Name="prmvIps8" Type="Int32" />
            <asp:Parameter Name="prmIps9" Type="Int32" />
            <asp:Parameter Name="prmIps10" Type="Int32" />
            <asp:Parameter Name="prmIcode1" Type="String" />
            <asp:Parameter Name="prmIcode2" Type="String" />
            <asp:Parameter Name="prmIcode3" Type="String" />
            <asp:Parameter Name="prmIcode4" Type="String" />
            <asp:Parameter Name="prmIcode5" Type="String" />
            <asp:Parameter Name="prmIcode6" Type="String" />
            <asp:Parameter Name="prmIcode7" Type="String" />
            <asp:Parameter Name="prmIcode8" Type="String" />
            <asp:Parameter Name="prmIcode9" Type="String" />
            <asp:Parameter Name="prmIcode10" Type="String" />
            <asp:Parameter Name="prmCdscr1" Type="String" />
            <asp:Parameter Name="prmCdscr2" Type="String" />
            <asp:Parameter Name="prmCdscr3" Type="String" />
            <asp:Parameter Name="prmCdscr4" Type="String" />
            <asp:Parameter Name="prmCdscr5" Type="String" />
            <asp:Parameter Name="prmCdscr6" Type="String" />
            <asp:Parameter Name="prmCdscr7" Type="String" />
            <asp:Parameter Name="prmCdscr8" Type="String" />
            <asp:Parameter Name="prmCdscr9" Type="String" />
            <asp:Parameter Name="prmCdscr10" Type="String" />
            <asp:Parameter Name="prmIper1" Type="Int32" />
            <asp:Parameter Name="prmIper2" Type="Int32" />
            <asp:Parameter Name="prmIper3" Type="Int32" />
            <asp:Parameter Name="prmIper4" Type="Int32" />
            <asp:Parameter Name="prmIper5" Type="Int32" />
            <asp:Parameter Name="prmIper6" Type="Int32" />
            <asp:Parameter Name="prmIper7" Type="Int32" />
            <asp:Parameter Name="prmIper8" Type="Int32" />
            <asp:Parameter Name="prmIper9" Type="Int32" />
            <asp:Parameter Name="prmIper10" Type="Int32" />
            <asp:Parameter Name="RfqPRowid" Type="Int64" />
        </SelectParameters>
    </asp:ObjectDataSource>
</div>
</asp:Content>
 