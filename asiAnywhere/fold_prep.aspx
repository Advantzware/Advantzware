<%@ Page Language="C#" MasterPageFile="~/MasterPageFolding.master" Debug="true" AutoEventWireup="true" EnableEventValidation="false" Inherits="fold_prep" Title="Prep/Route Estimate" Codebehind="fold_prep.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

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
window.onload=setfocus;
function setfocus() {
    if (document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vSnumTextBox")) {
        var snum = document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vSnumTextBox");
        snum.focus();
    }
}
  function validate(evt)
 {
   var charcode = (evt.which) ? evt.which : event.keyCode
   if(charcode > 31 && (charcode < 48 || charcode > 57) )
   return false;
   
   return true;

}

function checkconfirm() {
    if (confirm("Do you want to build a new routing? \n (NOTE: This will NOT DELETE any machines manually added)")) {
        return true;
    }
    else {
        return false;
    }
}
function importvalidate() {
    if (confirm("NO = Import Standards for Only Machine Imported? \n YES = Import Standards for All Machines on Routing?")) {
        return true;
    }
    else {
        return false;
    }
}
 
 
function preplook()
{
var NewWindow = window.open("prep_lookup.aspx","CarrierLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function PrepLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7) 
{

  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vCodeTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vDescTextBox.value= ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vCostTextBox.value = ReturnObj3;
  if(ReturnObj4 == "M")  
  {
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList1.SelectedIndex = 0 ;
  }
  else
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList1.SelectedIndex = 1 ;
  
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_DropDownList2.DataValueField = ReturnObj5;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vMarkTextBox.value= ReturnObj6;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vAmortTextBox.value = ReturnObj7;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vCodeTextBox.focus(); 
}
function machine()
{
var NewWindow = window.open("machine_lookup.aspx","MachineLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function  MachineLookup(ReturnObj1,ReturnObj2)
{

  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vMcodeTextBox.value = ReturnObj1;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vMdscrTextBox.value = ReturnObj2;
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vMcodeTextBox.focus();
}

function adderlook()
{
var adder = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vMcodeTextBox").value;
var NewWindow = window.open("coradd_look.aspx?mtype="+ adder +"","AdderLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function MachinLookup(ReturnObj1,ReturnObj2)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vAtype1TextBox.value = ReturnObj1;
 
}
function adder2look()
{
var adder = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vMcodeTextBox").value;
var NewWindow = window.open("coradd2_look.aspx?mtype="+ adder +"","AdderLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Machin2Lookup(ReturnObj1,ReturnObj2)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vAtype2TextBox.value = ReturnObj1;
 
}
function adder3look()
{
var adder = document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vMcodeTextBox").value;
var NewWindow = window.open("coradd3_look.aspx?mtype="+ adder +"","AdderLookup","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Machin3Lookup(ReturnObj1,ReturnObj2)
{
  document.forms[0].ctl00_ContentPlaceHolder1_FormView_Route_vAtype3TextBox.value = ReturnObj1;
 
}


function valsim()
{
//  var sim = document.getElementById("document.forms[0].ctl00_ContentPlaceHolder1_FormView2_vSimonTextBox");
//  var valsim[5];
//  valsim.value = "S,I,M,O,N";
//  for(int i =0; i<5; i++)
//  {
//   if (valsim [i] != sim.value)
//   alert("hello");
//   }
  
}

function valhr()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpmrTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpmrTextBox").value = frontback;
    }
    }
function spoil()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpspoilTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpspoilTextBox").value = frontback;
    }
    }
function crown()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpcrewTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 0 && frontback.length < 2) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpcrewTextBox").value = frontback;
    }
    }
function crown2()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpcrew2TextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 0 && frontback.length < 2) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vOpcrew2TextBox").value = frontback;
    }
    }
function prepcost()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vCostTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 4 && frontback.length < 6) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vCostTextBox").value = frontback;
    }
    }
function prepmark()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vMarkTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vMarkTextBox").value = frontback;
    }
    }
function prepamort()
   {
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vAmortTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 2 && frontback.length < 4) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vAmortTextBox").value = frontback;
    }
    }
function valiqty()
   {   
    var frontback=document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vQtyTextBox").value;
    if (frontback.indexOf(".") != -1) {
        return;
    }
    else if (frontback.length > 4 && frontback.length < 6) {
        frontback = frontback + ".";
        document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vQtyTextBox").value = frontback;
    }
    }
    
function shblurval()
{
    
    var sh=document.getElementById("ctl00_ContentPlaceHolder1_FormView2_vSnumTextBox");
    sh.focus();
}
function focusnum()
{
 var snum=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Route_vSnumTextBox");
 snum.focus();
}
 function qtyfocus()
 {
  var qty=document.getElementById("ctl00_ContentPlaceHolder1_FormView_Qty_vCorQtyTextBox");
    qty.focus();
}

function jobbuttonconfirm() {
    var order = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_vOrderLabel");

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
       <div>
    <asp:HiddenField ID="HiddenField1" runat="server" /> 
    <asp:HiddenField ID="HiddenField2" runat="server" />   
    <asp:HiddenField ID="HiddenField3" runat="server" />  
    <asp:FormView ID="FormView1"  runat="server" DataSourceID="est_ObjectDataSource">
                
        <ItemTemplate>
            <table class="shade"><tr><td><b>Estimate:</b></td>
            <td><asp:Label ID="vEstNumLabel" BackColor="turquoise" Width="70px" runat="server" Text='<%# Bind("vEstNum") %>'></asp:Label></td>            
            <td><b>EstDate:</b></td>
            <td><asp:Label ID="vEstDateLabel" BackColor="turquoise" Width="70px" runat="server" Text='<%# Bind("vEstDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
            <td><b>Frm:</b></td>
            <td><asp:Label ID="vFormNoLabel" BackColor="turquoise" Width="30px" runat="server" Text='<%# Bind("vFormNo") %>'></asp:Label></td>
            <td><b>Of</b></td>
            <td><asp:Label ID="vFormQtyLabel" BackColor="turquoise" Width="30px" runat="server" Text='<%# Bind("vFormQty") %>'></asp:Label></td>
            <td><b>Blk:</b></td>
            <td><asp:Label ID="vBlankNoLabel" BackColor="turquoise" Width="30px" runat="server" Text='<%# Bind("vBlankNo") %>'></asp:Label></td>
            <td><b>Of</b></td>
            <td><asp:Label ID="vBlankQtyLabel" BackColor="turquoise" Width="30px" runat="server" Text='<%# Bind("vBlankQty") %>'></asp:Label></td>
            <td><b>Cust Part#:</b></td>
            <td><asp:Label ID="vCustPartLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("vCustPart") %>'></asp:Label></td>
            
            <td style="display:none"><asp:Label ID="vOrderLabel" BackColor="turquoise" Width="150px" runat="server" Text='<%# Bind("vOrder") %>'></asp:Label></td>
            </tr></table>
        </ItemTemplate>
    </asp:FormView>
    <fieldset style="width:800px">
    <legend> Preparation</legend>
     <asp:GridView ID="GridView_Prep" Width="750px" runat="server" AutoGenerateColumns="False" DataKeyNames="vLine"
            DataSourceID="FoldPrepDataSource" AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" PageSize="10" OnSelectedIndexChanged="GridView_Prep_SelectedIndexChanged" OnRowDataBound="GridView_Prep_RowDataBound">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
            <Columns >
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                
                <asp:BoundField DataField="vSnum" HeaderText="Sht#" SortExpression="vSnum" />
                <asp:BoundField DataField="vBnum" HeaderText="B#" SortExpression="vBnum" />
                <asp:BoundField DataField="vCode" HeaderText="Code" SortExpression="vCode" />
                <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
                <asp:BoundField DataField="vDesc" HeaderText="Desc" SortExpression="vDesc" />
                <asp:BoundField DataField="vCost" HeaderText="Cost" SortExpression="vCost" />
                <asp:BoundField DataField="vMl" HeaderText="Ml" SortExpression="vMl" />
                <asp:BoundField DataField="vSimon" HeaderText="Simon" SortExpression="vSimon" />
                <asp:BoundField DataField="vMark" HeaderText="Mark" SortExpression="vMark" />
                <asp:BoundField DataField="vAmort" HeaderText="Amort" SortExpression="vAmort" />
                 <asp:TemplateField Visible="false" HeaderText="line">
                 <ItemTemplate>
                 <asp:Label id="vline" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                 </ItemTemplate>                     
                 </asp:TemplateField>
                
            </Columns>
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" Height="40px" />
            
        </asp:GridView> <br /><br />
        <table class="shade"><tr><td>
             <asp:Button ID="newadd_prep_Button" OnClick="new_button_prep_Click" CssClass="button"  runat="server" Text="Add" />
            <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource1" OnItemUpdated="FormView2_ItemUpdated" OnDataBound="Formview2_ondatabound" OnUnload="FormView2_Unload" >
            <EditItemTemplate>
            <asp:Panel ID="edit_panel" runat="server" DefaultButton="UpdateButton">
                <table class="shade">
                <tr><td align="right" style="padding-left:5px"><b>Sht#:</b></td>
                <td><asp:TextBox ID="vSnumTextBox" runat="server" MaxLength="3" Width="30px" Text='<%# Bind("vSnum") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vSnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><B>B#:</B></td>
                <td><asp:TextBox ID="vBnumTextBox" runat="server" MaxLength="3" Width="30px" Text='<%# Bind("vBnum") %>'> </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vBnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>Qty:</b></td>
                <td><asp:TextBox ID="vQtyTextBox" runat="server" MaxLength="7" onkeyup="valiqty()" Width="40px" Text='<%# Bind("vQty") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>Code:</b></td>
                <td><asp:TextBox ID="vCodeTextBox" runat="server" Width="100px" Text='<%# Bind("vCode") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="preplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                <td align="right" style="padding-left:5px"><b>Markup:</b></td>
                <td><asp:TextBox ID="vMarkTextBox" runat="server" MaxLength="6" onkeyup="prepmark()" Width="40px" Text='<%# Bind("vMark") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vMarkTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td></tr>
                <tr><td align="right" style="padding-left:5px"><b>Cost:</b></td>
                <td><asp:TextBox ID="vCostTextBox" MaxLength="8" runat="server" Width="50px" onkeyup="prepcost()" Text='<%# Bind("vCost") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>M/L:</b></td>
                <td><asp:DropDownList ID="DropDownList1" selectedvalue='<%# Bind("vMl") %>'  Width="40px" DataValueField='<%# Bind("vMl") %>' RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   runat="server">
                    <asp:ListItem Text="M" Value="M"></asp:ListItem>
                    <asp:ListItem Text="L" Value="L"></asp:ListItem>
                    </asp:DropDownList></td>
                <td align="right" style="padding-left:5px"><b>SIMON:</b></td>
                <td><asp:DropDownList ID="DropDownList2" Width="40px" DataValueField='<%# Bind("vSimon") %>' selectedValue='<%# Bind("vSimon") %>'  runat="server">
                    <asp:ListItem Text="S" Value="S"></asp:ListItem>
                    <asp:ListItem Text="I" Value="I"></asp:ListItem>
                    <asp:ListItem Text="M" Value="M"></asp:ListItem>
                    <asp:ListItem Text="O" Value="O"></asp:ListItem>
                    <asp:ListItem Text="N" Value="N"></asp:ListItem>
                    </asp:DropDownList>
                </td>
                <td align="right" style="padding-left:5px"><b>Desc:</b></td>
                <td><asp:TextBox ID="vDescTextBox" runat="server" Text='<%# Bind("vDesc") %>'></asp:TextBox></td>
                <td align="right" style="padding-left:5px"><b>Amort:</b></td>
                <td><asp:TextBox ID="vAmortTextBox" runat="server" MaxLength="6" Width="40px" onkeyup="prepamort()" onblur="shblurval()" Text='<%# Bind("vAmort") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vAmortTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td></tr>
                <tr><td>
                    <asp:Label ID="vLineLabel" Visible="false"  runat="server" Text='<%# Bind("vLine") %>'></asp:Label></td></tr>
                <tr><td colspan="5">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="update_Click"  Text="Save">
                </asp:Button>
                <asp:Button ID="Button1" runat="server" CssClass="button" CausesValidation="True" OnClick="copy_save_Click"  Text="Save">
                </asp:Button>
                <asp:Button ID="Button2" runat="server" CssClass="button" CausesValidation="false" OnClick="reset_update_Click"  Text="Reset">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"   Text="Cancel">
                </asp:Button>
                </td></tr></table></asp:Panel>
            </EditItemTemplate>
            <InsertItemTemplate>
            <asp:Panel ID="insert_panel" runat="server" DefaultButton="InsertButton">
                <table class="shade">
                <tr><td align="right" style="padding-left:5px"><b>Sht#:</b></td>
                <td><asp:TextBox ID="vSnumTextBox" runat="server" Width="30px" MaxLength="3" Text='<%# Bind("vSnum") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vSnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><B>B#:</B></td>
                <td><asp:TextBox ID="vBnumTextBox" runat="server" Width="30px" MaxLength="3" Text='<%# Bind("vBnum") %>'> </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vBnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>Qty:</b></td>
                <td><asp:TextBox ID="vQtyTextBox" runat="server" MaxLength="7" onkeyup="valiqty()" Width="40px" Text='<%# Bind("vQty") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="vQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>Code:</b></td>
                <td><asp:TextBox ID="vCodeTextBox" runat="server" Width="100px" Text='<%# Bind("vCode") %>'></asp:TextBox>
                <a href="#" tabindex="1" onclick="preplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                <td align="right" style="padding-left:5px"><b>Markup:</b></td>
                <td><asp:TextBox ID="vMarkTextBox" runat="server" Width="40px" MaxLength="6" onkeyup="prepmark()" Text='<%# Bind("vMark") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="vMarkTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td></tr>
                <tr><td align="right" style="padding-left:5px"><b>Cost:</b></td>
                <td><asp:TextBox ID="vCostTextBox" runat="server" MaxLength="8" Width="50px" onkeyup="prepcost()" Text='<%# Bind("vCost") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="vCostTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td>
                <td align="right" style="padding-left:5px"><b>M/L:</b></td>
                <td><asp:DropDownList ID="DropDownList1" selectedvalue='<%# Bind("vMl") %>' Width="40px" DataValueField='<%# Bind("vMl") %>' RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2"   runat="server">
                    <asp:ListItem Text="M" Value="M"></asp:ListItem>
                    <asp:ListItem Text="L" Value="L"></asp:ListItem>
                    </asp:DropDownList></td>
                <td align="right" style="padding-left:5px"><b>SIMON:</b></td>
                <td><asp:DropDownList ID="DropDownList2" Width="40px" DataValueField='<%# Bind("vSimon") %>' selectedValue='<%# Bind("vSimon") %>'  runat="server">
                    <asp:ListItem Text="S" Value="S"></asp:ListItem>
                    <asp:ListItem Text="I" Value="I"></asp:ListItem>
                    <asp:ListItem Text="M" Value="M"></asp:ListItem>
                    <asp:ListItem Text="O" Value="O"></asp:ListItem>
                    <asp:ListItem Text="N" Value="N"></asp:ListItem>
                    </asp:DropDownList>
                </td>
                <td align="right" style="padding-left:5px"><b>Desc:</b></td>
                <td><asp:TextBox ID="vDescTextBox" runat="server" Text='<%# Bind("vDesc") %>'></asp:TextBox></td>
                <td align="right" style="padding-left:5px"><b>Amort:</b></td>
                <td><asp:TextBox ID="vAmortTextBox" runat="server" Width="40px" onblur="shblurval(This)"  MaxLength="6" onkeyup="prepamort()" Text='<%# Bind("vAmort") %>'></asp:TextBox>
                <asp:CompareValidator ID="CompareValidator6" runat="server" ControlToValidate="vAmortTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Not a valid Number"></asp:CompareValidator></td></tr>
                <tr><td>
                <asp:Label ID="vLineLabel" Visible="false" runat="server" Text='<%# Bind("vLine") %>'></asp:Label></td></tr>
                <tr><td colspan="5">
                <asp:Button ID="InsertButton" runat="server" CssClass="button"  CausesValidation="True" OnClick="save_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="Button2" runat="server" CssClass="button" CausesValidation="false" OnClick="reset_insert_Click"  Text="Reset">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>
                </td></tr></table></asp:Panel>
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade">
                <%--<tr><td align="right" style="padding-left:5px"><b>Sht#:</b></td>
                <td><asp:Label ID="vSnumLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vSnum") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>B#:</b></td>
                <td><asp:Label ID="vBnumLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vBnum") %>'></asp:Label></td>                
                <td align="right" style="padding-left:5px"><b>Qty:</b></td>
                <td><asp:Label ID="vQtyLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vQty") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>Code:</b></td>
                <td><asp:Label ID="vCodeLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vCode") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>Markup:</b></td>
                <td><asp:Label ID="vMarkLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vMark") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-left:5px"><b>Cost:</b></td>
                <td><asp:Label ID="vCostLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vCost") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>M/L</b></td>
                <td><asp:Label ID="vMlLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vMl") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>SIMON:</b></td>
                <td><asp:Label ID="vSimonLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vSimon") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>Desc:</b></td>
                <td><asp:Label ID="vDescLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vDesc") %>'></asp:Label></td>
                <td align="right" style="padding-left:5px"><b>Amort:</b></td>
                <td><asp:Label ID="vAmortLabel" runat="server" Width="50px" BackColor="turquoise" Text='<%# Bind("vAmort") %>'></asp:Label></td></tr>--%>
                 <tr><td>
                <asp:Label ID="vLineLabel" Visible="false" runat="server" Text='<%# Bind("vLine") %>'></asp:Label></td></tr>        
                <tr><td colspan="6">
                <asp:Button ID="updateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click" CommandName="edit" Text="Update">
                </asp:Button>
                <asp:Button ID="addButton" runat="server" CausesValidation="true" CssClass="button" CommandName="new"  Text="Add">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server" CausesValidation="true" CssClass="button" OnClick="delete_Click"  Text="Delete" OnClientClick= 'return confirm ("Are you sure to Delete This Record")'>
                </asp:Button>
                <asp:Button ID="copyButton" runat="server" CausesValidation="True" CssClass="button" OnClick="copyButton_click" CommandName="edit" Text="Copy">
                </asp:Button>                
                <asp:Button ID="jobButton" runat="server" CausesValidation="false" CssClass="button" OnClick="Job_Button_Click" OnClientClick=" return jobbuttonconfirm()"  Text="Job Stds" >  </asp:Button>
                </td></tr></table>
            </ItemTemplate>
    </asp:FormView>
    </td>
    <td>
    
    
    </td>
    </tr> </table>
    <asp:ObjectDataSource ID="FoldPrepDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectPrep" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmActSelect" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmCode" Type="String" />
            <asp:Parameter Name="prmQty" Type="decimal" />
            <asp:Parameter Name="prmDesc" Type="String" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMl" Type="String" />
            <asp:Parameter Name="prmSimon" Type="String" />
            <asp:Parameter Name="prmMark" Type="Decimal" />
            <asp:Parameter Name="prmAmort" Type="Decimal" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmAction" Type="string" />
              <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
            <asp:Parameter Direction="InputOutput" Name="vmessage" Type="String" />
          
        </SelectParameters>
    </asp:ObjectDataSource>
    <asp:ObjectDataSource ID="est_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectPrep" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="EstSelect" Name="prmActSelect" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmCode" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmDesc" Type="String" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMl" Type="String" />
            <asp:Parameter Name="prmSimon" Type="String" />
            <asp:Parameter Name="prmMark" Type="Decimal" />
            <asp:Parameter Name="prmAmort" Type="Decimal" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmAction" Type="string" />
             <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
            <asp:Parameter Direction="InputOutput" Name="vmessage" Type="String" />
           
        </SelectParameters>
    </asp:ObjectDataSource>
   <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectPrep" TypeName="Corrugated" OnUpdated="ObjectDataSource1_Updated" OnSelecting="ObjectDataSource1_Selecting">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmCode" Type="String" />
            <asp:Parameter Name="prmQty" Type="decimal" />
            <asp:Parameter Name="prmDesc" Type="String" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmMl" Type="String" />
            <asp:Parameter Name="prmSimon" Type="String" />
            <asp:Parameter Name="prmMark" Type="Decimal" />
            <asp:Parameter Name="prmAmort" Type="Decimal" />
            <asp:SessionParameter SessionField="Fold_prep_line" Name="prmLine" Type="Int32" />
            <asp:Parameter Name="prmActSelect" Type="string" />
            <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="int32" />
            <asp:Parameter Direction="InputOutput" Name="vmessage" Type="String" />
          
        </SelectParameters>
    </asp:ObjectDataSource>
</fieldset><br /><br />
</div>
<div align="center" style="width:800px">

<fieldset id="Qty_fieldset" style="width:300px" runat="server">
    <legend>Estimate Run Qty</legend>
    <table>
    <tr><td>
    <asp:GridView ID="GridView_Qty"  runat="server" AutoGenerateColumns="False"
            DataSourceID="ObjectDataSource_qty" AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" PageSize="5" OnSelectedIndexChanged="GridView_Qty_SelectedIndexChanged" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
        <Columns>
         <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
            <asp:BoundField DataField="vCorQty" HeaderText="Qty" SortExpression="vCorQty" />
            <asp:TemplateField HeaderText="type" Visible="false" >
            <ItemTemplate>
                <asp:Label ID="qty_typeLabel"  runat="server" Text='<%# Bind("vType") %>'></asp:Label>
            </ItemTemplate>
            </asp:TemplateField>
        </Columns>
        <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" Height="40px" />
            </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource_qty" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectCorrQty" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmActSelect" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstNum" SessionField="order_folding_est" Type="String" />
                <asp:Parameter Name="prmFormNo" Type="Int32" />
                <asp:Parameter Name="prmQty" Type="Int32" />
                <asp:Parameter Name="prmReckey" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
        <asp:ObjectDataSource ID="ObjectDataSource_qtydetail" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectCorrQty" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                <asp:Parameter  Name="prmActSelect" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstNum" SessionField="order_folding_est" Type="String" />
                <asp:Parameter Name="prmFormNo" Type="Int32" />
                <asp:SessionParameter SessionField="Fold_order_qty" Name="prmQty" Type="Int32" />
                <asp:Parameter Name="prmReckey" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </td>
    <td>
        <asp:FormView ID="FormView_Qty" CssClass="shade" DataSourceID="ObjectDataSource_qtydetail"  runat="server" OnUnload="FormView_Qty_unload" OnDataBound="FormView_Qty_DataBound">
            <EditItemTemplate>
             <asp:Panel ID="qtyupdate_panel" runat="server" DefaultButton="UpdateButton">
                <b>Qty:</b>
                <asp:TextBox ID="vCorQtyTextBox" MaxLength="12" onblur="qtyfocus()" runat="server" Text='<%# Bind("vCorQty") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vCorQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                <asp:Label ID="corridLabel" Visible="false" runat="server"  Text='<%# Bind("vCorid") %>'></asp:Label><br />
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="update_qty_Click"
                    Text="Save">
                </asp:Button>                
                <asp:Button ID="copysaveButton" runat="server" CssClass="button" CausesValidation="True" OnClick="copy_eqty_save_Click"  Text="Save">
                </asp:Button>
                <asp:Button ID="Button2" runat="server" CssClass="button" CausesValidation="false" OnClick="reset_eqty_update_Click"  Text="Reset">
                </asp:Button>                 
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></asp:Panel>
            </EditItemTemplate>
            <InsertItemTemplate>
            <asp:Panel ID="qtyinsert_panel" runat="server" DefaultButton="InsertButton">
                <b>Qty:</b>
                <asp:TextBox ID="vCorQtyTextBox" MaxLength="12" onblur="qtyfocus()" runat="server" Text='<%# Bind("vCorQty") %>'>
                </asp:TextBox>
                <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vCorQtyTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                <br />
                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Add_qty_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="Button2" runat="server" CssClass="button" CausesValidation="false" OnClick="reset_eqty_insert_Click"  Text="Reset">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></asp:Panel>
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade"> <tr><td align="right" style="padding-right:5px"><b>Qty:</b></td>
                <td><asp:Label ID="vCorQtyLabel" BackColor="turquoise" runat="server" Width="100" Text='<%# Bind("vCorQty") %>'></asp:Label>
                <asp:Label ID="corridLabel" Visible="false" runat="server"  Text='<%# Bind("vCorid") %>'></asp:Label></td></tr>
                <tr><td nowrap colspan="2"><asp:Button ID="updateButton" runat="server" CausesValidation="True" OnClick="updeqtybtn_click" CssClass="button" CommandName="edit" Text="Update">
                </asp:Button>
                <asp:Button ID="addButton" runat="server" CausesValidation="true" CssClass="button" CommandName="new"  Text="Add">
                </asp:Button>
                <asp:Button ID="copyButton" runat="server" CausesValidation="True" CssClass="button" OnClick="copyeqtybtn_click" CommandName="edit" Text="Copy">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server" CausesValidation="true" CssClass="button" OnClick="delete_qty_Click"  Text="Delete" OnClientClick= 'return confirm ("Are you sure to Delete This Record")'>
                </asp:Button></td></tr></table>
            </ItemTemplate>
        </asp:FormView>
    </td>
    </tr>
    </table>
    </fieldset><br /><br />

</div>
<div>
<fieldset style="width:800px">
<legend>Operations</legend>
    <asp:Panel ID="Panel1" runat="server" ScrollBars="Both" Width="800px">    

<asp:GridView ID="GridView1" Width="790px" runat="server" AutoGenerateColumns="False"
            DataSourceID="Route_ListObjectDataSource" AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid" PageSize="10" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnRowDataBound="GridView1_RowDataBound">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
            <Columns >
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                <asp:BoundField ItemStyle-Wrap="false"  DataField="vSnum" HeaderText="S" SortExpression="vSnum" />
                <asp:BoundField ItemStyle-Wrap="false" DataField="vBnum" HeaderText="B" SortExpression="vBnum" />
                <asp:BoundField  DataField="vMcode" HeaderText="Machine" SortExpression="vMcode" >
                <ItemStyle Wrap="false" />
                </asp:BoundField>
                <asp:BoundField  DataField="vMdscr" HeaderText="Desc" SortExpression="vMdscr" >
                <ItemStyle Wrap="false" />                
                </asp:BoundField>
                
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vNout" HeaderText="Out" SortExpression="vNout" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vMR" HeaderText="MR-Hrs" SortExpression="vMR" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpwaste" HeaderText="Waste" SortExpression="vOpwaste" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpspeed" HeaderText="Speed" SortExpression="vOpspeed" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpspoil" HeaderText="Spoil%" SortExpression="vOpspoil" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpcrew" HeaderText="MRCrew" SortExpression="vOpcrew" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpcrew2" HeaderText="RunCrew" SortExpression="vOpcrew2" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpRate" HeaderText="MRate" SortExpression="vOpRate" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vOpRate2" HeaderText="RRate" SortExpression="vOpRate2" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vInks" HeaderText="Inks" SortExpression="vInks" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vCoat" HeaderText="Varnish" SortExpression="vCoat" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vPlates" HeaderText="Plate Changes" SortExpression="vPlates" />
                <asp:BoundField HeaderStyle-Wrap="false" DataField="vFountains" HeaderText="Fountain Changes" SortExpression="vFountains" />
                
                
                <asp:TemplateField HeaderText="Line" Visible="false">
                <ItemTemplate>
                    <asp:Label ID="Label1" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                                
            </Columns>
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" Height="40px" />
            
        </asp:GridView><br />
        </asp:Panel><br />
    <asp:Button ID="rout_add_Button" OnClick="new_button_rout_Click" CssClass="button"  runat="server" Text="Add" />
    <asp:Button ID="rout_buildButton" runat="server" CssClass="button" OnClientClick="return checkconfirm();" OnClick="BuildButtonClick" Text="Build"></asp:Button>            
    <asp:FormView ID="FormView_Route" runat="server" DataSourceID="Detail_ObjectDataSource" OnDataBound="FormView_Route_DataBound" OnPreRender="FormView_Route_PreRender" OnLoad="FormView_Route_Load" OnUnload="FormView_Route_Unload">
        <EditItemTemplate>
        <asp:Panel ID="update_panel" runat="server" DefaultButton="UpdateButton">
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>S:</b></td>
            <td><asp:TextBox ID="vSnumTextBox" Width="50px" runat="server" Text='<%# Bind("vSnum") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vSnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>B:</b></td>
            <td><asp:TextBox ID="vBnumTextBox" Width="50px" runat="server" Text='<%# Bind("vBnum") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator7" runat="server" ControlToValidate="vBnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>Machine:</b></td>
            <td><asp:TextBox ID="vMcodeTextBox" Width="100px" runat="server" Text='<%# Bind("vMcode") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onclick="machine(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
             <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ControlToValidate="vMcodeTextBox" SetFocusOnError="true"  Display="Dynamic" ErrorMessage="Machine Must be Enter"></asp:RequiredFieldValidator></td>
            <td align="right" style="padding-right:5px"><b>Desc:</b></td>
            <td><asp:TextBox ID="vMdscrTextBox" Width="100px" runat="server" Text='<%# Bind("vMdscr") %>'> </asp:TextBox></td></tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Out:</b></td>
            <td><asp:TextBox ID="vNoutTextBox" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("vNout") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="vNoutTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>MH-Hrs:</b></td>
            <td><asp:TextBox ID="vOpmrTextBox" Width="50px" MaxLength="6" onkeyup="valhr()" runat="server" Text='<%# Bind("vMR") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ControlToValidate="vOpmrTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>Waste:</b></td>
            <td><asp:TextBox ID="vOpwasteTextBox" Width="50px" MaxLength="6" runat="server" Text='<%# Bind("vOpwaste") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator11" runat="server" ControlToValidate="vOpwasteTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>Speed:</b></td>
            <td><asp:TextBox ID="vOpspeedTextBox" Width="50px" MaxLength="5" runat="server" Text='<%# Bind("vOpspeed") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator12" runat="server" ControlToValidate="vOpspeedTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Spoil%:</b></td>
            <td><asp:TextBox ID="vOpspoilTextBox" Width="50px" MaxLength="6" onkeyup="spoil()" runat="server" Text='<%# Bind("vOpspoil") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator13" runat="server" ControlToValidate="vOpspoilTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>MRCrew:</b></td>
            <td><asp:TextBox ID="vOpcrewTextBox" Width="50px" MaxLength="4" onkeyup="crown()" runat="server" Text='<%# Bind("vOpcrew") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator14" runat="server" ControlToValidate="vOpcrewTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>RunCrew:</b></td>
            <td><asp:TextBox ID="vOpcrew2TextBox" Width="50px" MaxLength="4" runat="server" onkeyup="crown2()" onblur="focusnum()" Text='<%# Bind("vOpcrew2") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator15" runat="server" ControlToValidate="vOpcrew2TextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>MRate:</b></td>
            <td><asp:TextBox ID="vOpRateTextBox" Width="50px" runat="server" Text='<%# Bind("vOpRate") %>'></asp:TextBox></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>RRate:</b></td>
            <td><asp:TextBox ID="vOpRate2TextBox" Width="50px" runat="server" Text='<%# Bind("vOpRate2") %>'></asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Inks:</b></td>
            <td><asp:TextBox ID="vInksTextBox" Width="100px" runat="server" Text='<%# Bind("vInks") %>'> </asp:TextBox>
            </td>
            <td align="right" style="padding-right:5px"><b>Varnish:</b></td>
            <td><asp:TextBox ID="vCoatTextBox" Width="50px" MaxLength="5" runat="server" Text='<%# Bind("vCoat") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator16" runat="server" ControlToValidate="vCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>            
            <td align="right" style="padding-right:5px"><b>Plate Changes:</b></td>
            <td><asp:TextBox ID="vPlatesTextBox" Width="50px" runat="server" Text='<%# Bind("vPlates") %>'></asp:TextBox></td>
            </tr>
            <tr>  
            <td align="right" style="padding-right:5px"><b>Fountains:</b></td>
            <td><asp:TextBox ID="vFountainsTextBox" Width="50px" runat="server" Text='<%# Bind("vFountains") %>'> </asp:TextBox></td>
            </tr>
            <tr>
            <td><asp:Label ID="vLineTextBox" runat="server" Visible="false" Text='<%# Bind("vLine") %>'></asp:Label>
            <asp:Label ID="typeLabel" runat="server" Visible="false" Text='<%# Bind("vtype") %>'></asp:Label></td>
            <td></td>
            <td></td>
            <td></td></tr>            
            <tr><td colspan="6">
            <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="RouteUpdate_Click"
                Text="Save">
            </asp:Button>            
            <asp:Button ID="copyButton" runat="server" CssClass="button" CausesValidation="True" OnClick="CopyButton_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="ImportBtn" runat="server" CssClass="button" OnClientClick="return importvalidate();" CausesValidation="True" OnClick="ImportButton_Click"
                Text="Save">
            </asp:Button> 
            <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </td></tr></table></asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
        <asp:Panel ID="insert_panel" runat="server" DefaultButton="InsertButton">
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>S:</b></td>
            <td><asp:TextBox ID="vSnumTextBox" Width="50px" runat="server" Text='<%# Bind("vSnum") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vSnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>B:</b></td>
            <td><asp:TextBox ID="vBnumTextBox" Width="50px" runat="server" Text='<%# Bind("vBnum") %>'> </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator19" runat="server" ControlToValidate="vBnumTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>Machine:</b></td>
            <td><asp:TextBox ID="vMcodeTextBox" Width="100px" runat="server" Text='<%# Bind("vMcode") %>'> </asp:TextBox>
            <a href="#" tabindex="1" onclick="machine(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
            <td align="right" style="padding-right:5px"><b>Desc:</b></td>
            <td><asp:TextBox ID="vMdscrTextBox" Width="100px" runat="server" onblur="focusnum()" Text='<%# Bind("vMdscr") %>'> </asp:TextBox></td></tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Out:</b></td>
            <td><asp:TextBox ID="vNoutTextBox" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("vNout") %>'> </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>MH-Hrs:</b></td>
            <td><asp:TextBox ID="vOpmrTextBox" Width="50px" MaxLength="6" onkeyup="valhr()" runat="server" Text='<%# Bind("vMR") %>'></asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Waste:</b></td>
            <td><asp:TextBox ID="vOpwasteTextBox" Width="50px" MaxLength="6" runat="server" Text='<%# Bind("vOpwaste") %>'></asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Speed:</b></td>
            <td><asp:TextBox ID="vOpspeedTextBox" Width="50px" MaxLength="5" runat="server" Text='<%# Bind("vOpspeed") %>'></asp:TextBox></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Spoil%:</b></td>
            <td><asp:TextBox ID="vOpspoilTextBox" Width="50px" MaxLength="6" runat="server" Text='<%# Bind("vOpspoil") %>'> </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>MRCrew:</b></td>
            <td><asp:TextBox ID="vOpcrewTextBox" Width="50px" MaxLength="4" runat="server" Text='<%# Bind("vOpcrew") %>'> </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>RunCrew:</b></td>
            <td><asp:TextBox ID="vOpcrew2TextBox" Width="50px" MaxLength="4" runat="server" Text='<%# Bind("vOpcrew2") %>'> </asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>MRate:</b></td>
            <td><asp:TextBox ID="vOpRateTextBox" Width="50px" runat="server" Text='<%# Bind("vOpRate") %>'></asp:TextBox></td></tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>RRate:</b></td>
            <td><asp:TextBox ID="vOpRate2TextBox" Width="50px" runat="server" Text='<%# Bind("vOpRate2") %>'></asp:TextBox></td>
            <td align="right" style="padding-right:5px"><b>Inks:</b></td>
            <td><asp:TextBox ID="vInkTextBox" Width="100px" runat="server" Text='<%# Bind("vInks") %>'> </asp:TextBox>
            </td>
            <td align="right" style="padding-right:5px"><b>Varnish:</b></td>
            <td><asp:TextBox ID="vCoatTextBox" Width="50px" MaxLength="5" runat="server" Text='<%# Bind("vCoat") %>'></asp:TextBox>
            <asp:CompareValidator ID="CompareValidator22" runat="server" ControlToValidate="vCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="integer" ErrorMessage="Invalid Number"></asp:CompareValidator></td>
            <td align="right" style="padding-right:5px"><b>Plate Changes:</b></td>
            <td><asp:TextBox ID="vPlatesTextBox" Width="50px" runat="server" Text='<%# Bind("vPlates") %>'></asp:TextBox></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Fountains:</b></td>
            <td><asp:TextBox ID="vFountainsTextBox" Width="50px" runat="server" Text='<%# Bind("vFountains") %>'> </asp:TextBox></td>            
            <td><asp:Label ID="vLineTextBox" runat="server" Text='<%# Bind("vLine") %>'></asp:Label>
            <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("vtype") %>'></asp:Label>
            </td>
            </tr>           
            <tr><td colspan="6">
            <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="RoureSave_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="InsertStdsButton" runat="server" CssClass="button" CausesValidation="True" OnClick="AddStdsSave_Click" Text="Save">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button>
            </td></tr></table></asp:Panel>
        </InsertItemTemplate>
        <ItemTemplate>
            <table class="shade">
            <%--<tr><td align="right" style="padding-right:5px"><b>S:</b></td>
            <td><asp:Label ID="vSnumLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vSnum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>B:</b></td>
            <td><asp:Label ID="vBnumLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vBnum") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Machine:</b></td>
            <td><asp:Label ID="vMcodeLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("vMcode") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Desc:</b></td>
            <td><asp:Label ID="vMdscrLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("vMdscr") %>'></asp:Label></td>            
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Out:</b></td>
            <td><asp:Label ID="vNoutLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vNout") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>MR-Hrs:</b></td>
            <td><asp:Label ID="vOpmrLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vMR") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Waste:</b></td>
            <td><asp:Label ID="vOpwasteLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpwaste") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Speed:</b></td>
            <td><asp:Label ID="vOpspeedLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpspeed") %>'></asp:Label></td></tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>Spoil%:</b></td>
            <td><asp:Label ID="vOpspoilLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpspoil") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>MRCrew:</b></td>
            <td><asp:Label ID="vOpcrewLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpcrew") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>RunCrew:</b></td>
            <td><asp:Label ID="vOpcrew2Label" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpcrew2") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>MRate:</b></td>
            <td><asp:Label ID="vOpRateLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpRate") %>'></asp:Label></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px"><b>RRate:</b></td>
            <td><asp:Label ID="vOpRate2Label" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vOpRate2") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Inks:</b></td>
            <td><asp:Label ID="vAtype1Label" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("vInks") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Varnish:</b></td>
            <td><asp:Label ID="vAtqty1Label" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vCoat") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Plate Changes:</b></td>
            <td><asp:Label ID="vPlatesLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vPlates") %>'></asp:Label></td>
            </tr>--%>
            <tr>
            <%--<td align="right" style="padding-right:5px"><b>Fountains Changes:</b></td>
            <td><asp:Label ID="vFountainsLabel" runat="server" Width="70px" BackColor="turquoise" Text='<%# Bind("vFountains") %>'></asp:Label></td>            --%>
            <td><asp:Label ID="vLineLabel" runat="server" Visible="false" Text='<%# Bind("vLine") %>'></asp:Label>
            <asp:Label ID="typeLabel" runat="server" Visible="false" Text='<%# Bind("vtype") %>'></asp:Label></td>
            <td></td>
            <td></td>
            <td></td></tr>
            <tr><td colspan="6">
            <asp:Button ID="updateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButtonClick" CommandName="edit" Text="Override"></asp:Button>
            <asp:Button ID="addButton" runat="server" CausesValidation="true" CssClass="button" CommandName="new" OnClick="Op_Add_Click" Text="Add"></asp:Button>
            <asp:Button ID="addstdsButton" runat="server" CausesValidation="true" CssClass="button" CommandName="new" OnClick="Op_AddStds_Click"  Text="Add Stds"></asp:Button>
            <asp:Button ID="importButton" runat="server" CausesValidation="true" CssClass="button" CommandName="edit" OnClick="import_Click"  Text="Import"></asp:Button>            
            <asp:Button ID="copyButton" runat="server" CausesValidation="True" CssClass="button" OnClick="CopyButtonClick" CommandName="edit" Text="Copy"></asp:Button>
            <asp:Button ID="buildButton" runat="server" CausesValidation="True" CssClass="button" OnClientClick="return checkconfirm();" OnClick="BuildButtonClick" Text="Build"></asp:Button>            
            <asp:Button ID="DeleteButton" runat="server" CausesValidation="true" CssClass="button" OnClick="Route_Delete_Click"  Text="Delete" OnClientClick= 'return confirm ("Are you sure to Delete This Record")'>
            </asp:Button>
            </td></tr></table>
        </ItemTemplate>
    </asp:FormView>
    
    
    <asp:ObjectDataSource ID="Route_ListObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldRoute" TypeName="Corrugated">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmActSelect" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmMcode" Type="String" />
            <asp:Parameter Name="prmMdscr" Type="String" />
            
            <asp:Parameter Name="prmNout" Type="Int32" />
            <asp:Parameter Name="prmOpmr" Type="Decimal" />
            <asp:Parameter Name="prmOpwaste" Type="Int32" />
            <asp:Parameter Name="prmOpspeed" Type="Int32" />
            <asp:Parameter Name="prmOpspoil" Type="Decimal" />
            <asp:Parameter Name="prmOpcrew" Type="Decimal" />
            <asp:Parameter Name="prmOpcrew2" Type="Decimal" />
            <asp:Parameter Name="prmOpRate" Type="Decimal" />
            <asp:Parameter Name="prmOpRate2" Type="Decimal" />
            <asp:Parameter Name="prmPlates" Type="Int32" />
            <asp:Parameter Name="prmFountains" Type="Int32" />
            <asp:Parameter Name="prmInk" Type="int32" />
            <asp:Parameter Name="prmCoat" Type="Int32" />
            <asp:Parameter Name="prmLine" Type="Int32" />
            <asp:SessionParameter SessionField="Fold_order_qty" Name="prmQty"  Type="int32" />
        </SelectParameters>
    </asp:ObjectDataSource>
   <asp:ObjectDataSource ID="Detail_ObjectDataSource" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFoldRoute" TypeName="Corrugated" OnUpdated="ObjectDataSource1_Updated" OnSelecting="ObjectDataSource1_Selecting">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmActSelect" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmEstNum" SessionField="order_folding_est" Type="String" />
            <asp:SessionParameter SessionField="order_folding_formno" Name="prmFormno" Type="Int32" />
            <asp:Parameter Name="prmSnum" Type="Int32" />
            <asp:Parameter Name="prmBnum" Type="Int32" />
            <asp:Parameter Name="prmMcode" Type="String" />
            <asp:Parameter Name="prmMdscr" Type="String" />
            
            <asp:Parameter Name="prmNout" Type="Int32" />
            <asp:Parameter Name="prmOpmr" Type="Decimal" />
            <asp:Parameter Name="prmOpwaste" Type="Int32" />
            <asp:Parameter Name="prmOpspeed" Type="Int32" />
            <asp:Parameter Name="prmOpspoil" Type="Decimal" />
            <asp:Parameter Name="prmOpcrew" Type="Decimal" />
            <asp:Parameter Name="prmOpcrew2" Type="Decimal" />
            <asp:Parameter Name="prmOpRate" Type="Decimal" />
            <asp:Parameter Name="prmOpRate2" Type="Decimal" />
            <asp:Parameter Name="prmPlates" Type="Int32" />
            <asp:Parameter Name="prmFountains" Type="Int32" />
            <asp:Parameter Name="prmInk" Type="int32" />
            <asp:Parameter Name="prmCoat" Type="Int32" />
            
            <asp:SessionParameter SessionField="Fold_order_qty" Name="prmQty"  Type="int32" />
            <asp:SessionParameter SessionField="Fold_route_line" Name="prmLine" Type="Int32" />
        </SelectParameters>
    </asp:ObjectDataSource>



</fieldset>
 </div>   

</asp:Content>

