<%@ Page Language="C#" AutoEventWireup="True" Inherits="set_update_est2" Codebehind="set_update_est.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Set Information</title>
    <link href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
<script type="text/javascript">
function categorylookup(){ 
  var NewWindow =window.open("CategoryLookup.aspx","CategoryWindow","width=450,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookUp(ReturnObj1){ 
  document.forms[0].FormView1_vProcatTextBox.value = ReturnObj1;
  
}
function ebstocklook(){ 
  var NewWindow =window.open("eb_stock_look.aspx","CategoryWindow","width=450,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function EbStockLook(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8){ 
  document.forms[0].FormView1_vStockTextBox.value = ReturnObj1;
  document.forms[0].FormView1_vPartnoTextBox.value = ReturnObj2;
  document.forms[0].FormView1_vPartDscrTextBox.value = ReturnObj3;
  document.forms[0].FormView1_vPartDscr2TextBox.value = ReturnObj4;
  document.forms[0].FormView1_vProcatTextBox.value = ReturnObj5;
  document.forms[0].FormView1_vLenTextBox.value = ReturnObj6;
  document.forms[0].FormView1_vWidTextBox.value = ReturnObj7;
  document.forms[0].FormView1_vDepTextBox.value = ReturnObj8;
  
}

function btn_close_click()
{
    window.close();
}
</script>

</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" >
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
            <EditItemTemplate>
                 <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Set FG Item#:</b></td>
                <td><asp:TextBox ID="vStockTextBox" runat="server" Text='<%# Bind("vStock") %>'>
                </asp:TextBox><a href="#" onClick="ebstocklook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td></tr>
                 <tr><td align="right" style="padding-right:5px"><b>Set Cust Part#:</b></td>
                <td><asp:TextBox ID="vPartnoTextBox" runat="server" Text='<%# Bind("vPartno") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                <td><asp:TextBox ID="vPartDscrTextBox" runat="server" Text='<%# Bind("vPartDscr") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Part Description:</b></td>
                <td><asp:TextBox ID="vPartDscr2TextBox" runat="server" Text='<%# Bind("vPartDscr2") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Category:</b></td>
                <td><asp:TextBox ID="vProcatTextBox" runat="server" Text='<%# Bind("vProcat") %>'>
                </asp:TextBox><a href="#" onClick="categorylookup(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>F.G. Length:</b></td>
                <td><asp:TextBox ID="vLenTextBox" runat="server" Text='<%# Bind("vLen") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Width:</b></td>
                <td><asp:TextBox ID="vWidTextBox" runat="server" Text='<%# Bind("vWid") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Depth:</b></td>
                <td><asp:TextBox ID="vDepTextBox" runat="server" Text='<%# Bind("vDep") %>'>
                </asp:TextBox></td></tr>
                <tr><td colspan="2">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Qty:</b>&nbsp;&nbsp;<asp:Label ID="vQtyLabel" BackColor="turquoise" Width="50px" runat="server" Text='<%# Bind("vQty") %>'></asp:Label>
                <b>MSF:</b><asp:Label ID="vMsfLabel" BackColor="turquoise" Width="50px" runat="server" Text='<%# Bind("vMsf") %>'></asp:Label></td></tr>
                
                 <tr>
                <td align="right" style="padding-right:5px"> <b>Set Allocation:</b></td> 
                 <td>
                 <b> <asp:RadioButtonList ID="RadioButtonList1" SelectedValue='<%# Bind("vAllo") %>' RepeatLayout="Flow" CellSpacing="1" RepeatColumns="3" runat="server">
                        <asp:ListItem Text="Assembled" Value="Yes" Selected="True" ></asp:ListItem>
                        <asp:ListItem Text="Unassembled" Value="No"  ></asp:ListItem>
                        <asp:ListItem Text="Assembled W/Part Receipts" Value="?" ></asp:ListItem>
                        </asp:RadioButtonList></b>
                </td></tr>
                <tr><td></td>
                <td >
                    <b><asp:CheckBox ID="CheckBox1" text="Unitize" Checked='<%# Bind("vUnit") %>'  runat="server" /></b>
                </td></tr>   
                <tr><td colspan="2">
                <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True" OnClick="updatebutton_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table>
            </EditItemTemplate>
            <InsertItemTemplate>
                vStock:
                <asp:TextBox ID="vStockTextBox" runat="server" Text='<%# Bind("vStock") %>'>
                </asp:TextBox><br />
                vPartno:
                <asp:TextBox ID="vPartnoTextBox" runat="server" Text='<%# Bind("vPartno") %>'>
                </asp:TextBox><br />
                vPartDscr:
                <asp:TextBox ID="vPartDscrTextBox" runat="server" Text='<%# Bind("vPartDscr") %>'>
                </asp:TextBox><br />
                vPartDscr2:
                <asp:TextBox ID="vPartDscr2TextBox" runat="server" Text='<%# Bind("vPartDscr2") %>'>
                </asp:TextBox><br />
                vProcat:
                <asp:TextBox ID="vProcatTextBox" runat="server" Text='<%# Bind("vProcat") %>'>
                </asp:TextBox><br />
                vLen:
                <asp:TextBox ID="vLenTextBox" runat="server" Text='<%# Bind("vLen") %>'>
                </asp:TextBox><br />
                vWid:
                <asp:TextBox ID="vWidTextBox" runat="server" Text='<%# Bind("vWid") %>'>
                </asp:TextBox><br />
                vDep:
                <asp:TextBox ID="vDepTextBox" runat="server" Text='<%# Bind("vDep") %>'>
                </asp:TextBox><br />
                vQty:
                <asp:TextBox ID="vQtyTextBox" runat="server" Text='<%# Bind("vQty") %>'>
                </asp:TextBox><br />
                vMsf:
                <asp:TextBox ID="vMsfTextBox" runat="server" Text='<%# Bind("vMsf") %>'>
                </asp:TextBox><br />
                vAllo:
                <asp:TextBox ID="vAlloTextBox" runat="server" Text='<%# Bind("vAllo") %>'>
                </asp:TextBox><br />
                vUnit:
                <asp:TextBox ID="vUnitTextBox" runat="server" Text='<%# Bind("vUnit") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>
            <ItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Set FG Item#:</b></td>
                <td><asp:Label ID="vStockLabel" BackColor="turquoise" Width="200px" runat="server" Text='<%# Bind("vStock") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Set Cust Part#:</b></td>
                <td><asp:Label ID="vPartnoLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vPartno") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Item Name:</b></td>
                <td><asp:Label ID="vPartDscrLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vPartDscr") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Part Description:</b></td>
                <td><asp:Label ID="vPartDscr2Label" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vPartDscr2") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Category:</b></td>
                <td><asp:Label ID="vProcatLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vProcat") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>F.G. Length:</b></td>
                <td><asp:Label ID="vLenLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vLen") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Width:</b></td>
                <td><asp:Label ID="vWidLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vWid") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Depth:</b></td>
                <td><asp:Label ID="vDepLabel" runat="server" BackColor="turquoise" Width="200px" Text='<%# Bind("vDep") %>'></asp:Label></td></tr>
                <tr><td colspan="2"  >&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Qty:</b>&nbsp;&nbsp;<asp:Label ID="vQtyLabel" BackColor="turquoise" Width="80px" runat="server" Text='<%# Bind("vQty") %>'></asp:Label>
                <b>MSF:</b><asp:Label ID="vMsfLabel" BackColor="turquoise" Width="90px" runat="server" Text='<%# Bind("vMsf") %>'></asp:Label></td></tr>
                <tr>
                <td align="right" style="padding-right:5px"><b>Set Allocation:</b></td>
                <td>
                <b><asp:RadioButtonList ID="RadioButtonList1" SelectedValue='<%# Bind("vAllo") %>' Enabled="false" RepeatLayout="Flow" CellSpacing="1"   RepeatColumns="3" runat="server">
                        <asp:ListItem Text="Assembled" Value="Yes"  ></asp:ListItem>
                        <asp:ListItem Text="Unassembled" Value="No"  ></asp:ListItem>
                        <asp:ListItem Text="Assembled W/Part Receipts" Value="?" ></asp:ListItem>
                        </asp:RadioButtonList></b>
                </td></tr>
                <tr><td></td><td>
                   <b> <asp:CheckBox ID="CheckBox1" text="Unitize" Checked='<%# Bind("vUnit") %>' Enabled="false"  runat="server" /></b>
                </td></tr>                
             <%-- vAllo:
                <asp:Label ID="vAlloLabel" runat="server" Text='<%# Bind("vAllo") %>'></asp:Label><br />
                vUnit:
                <asp:Label ID="vUnitLabel" runat="server" Text='<%# Bind("vUnit") %>'></asp:Label><br />--%>
                <tr><td colspan="2">
                 <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" CommandName="Edit"
                    Text="Update">
                </asp:Button>
                <%--<asp:Button ID="CancelButton" runat="server" CssClass="button"  CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button>--%>
                <input type="button" id="btn_close" class="buttonM" value="Close" onclick="btn_close_click()" />
                </td></tr>
                </table>
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectUpdateSetEst" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmStock" Type="String" />
                <asp:Parameter Name="prmPartno" Type="String" />
                <asp:Parameter Name="prmPartDscr" Type="String" />
                <asp:Parameter Name="prmPartDscr2" Type="String" />
                <asp:Parameter Name="prmProcat" Type="String" />
                <asp:Parameter Name="prmlen" Type="Decimal" />
                <asp:Parameter Name="prmWid" Type="Decimal" />
                <asp:Parameter Name="prmDep" Type="Decimal" />
                <asp:Parameter Name="prmAllo" Type="String" />
                <asp:Parameter Name="prmUnit" Type="String" />
                <asp:QueryStringParameter Name="prmEst" QueryStringField="estno"  Type="String" />
                <asp:QueryStringParameter Name="prmType" QueryStringField="setype" Type="int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </form>
</body>
</html>


