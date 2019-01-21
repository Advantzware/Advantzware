<%@ Page Language="C#" AutoEventWireup="True" Debug="true" Inherits="est_goto2" Codebehind="est_goto.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Item Information</title>
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

function closewin()
{
    window.close();
}
function cal_up()
{
  var wid= document.getElementById("FormView1_widTextBox");
  var len= document.getElementById("FormView1_lenTextBox");
  var up= document.getElementById("FormView1_numupTextBox");
  up.value = wid.value * len.value;
}
</script>

</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" >
        <asp:HiddenField ID="HiddenField1" runat="server" />
        <asp:HiddenField ID="HiddenField2" runat="server" />
        
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
            <asp:BoundField DataField="formno" HeaderText="Form" SortExpression="formno" />
            <asp:BoundField DataField="blankno" HeaderText="Blank" SortExpression="blankno" />
            <asp:BoundField DataField="partno" HeaderText="Cust Part#" SortExpression="partno" />
            <asp:BoundField DataField="blqty" HeaderText="Request Qty" SortExpression="blqty" />
            <asp:BoundField DataField="yldqty" HeaderText="Yield Qty" SortExpression="yldqty" />
            <asp:BoundField DataField="price" HeaderText="Price By" SortExpression="price" />
            <asp:BoundField DataField="wid" HeaderText="# on Wid" SortExpression="wid" />
            <asp:BoundField DataField="len" HeaderText="# on Len" SortExpression="len" />
            <asp:BoundField DataField="numup" HeaderText="# Up" SortExpression="numup" />
            <asp:TemplateField HeaderText="type" Visible="false">
            <ItemTemplate>
            <asp:Label ID="type_label" runat="server" Text='<%# Bind("vtype") %>' ></asp:Label>
            </ItemTemplate>
            <ItemStyle Wrap="false" />
            </asp:TemplateField>
            
            </Columns>
            </asp:GridView>
            <br />
            
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource_view" OnDataBound="formview_OnDataBound">
            <EditItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Form#:</b></td>
                <td><asp:TextBox ID="formnoTextBox" runat="server" Text='<%# Bind("formno") %>'></asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b>Blank#:</b></td>
                <td><asp:TextBox ID="blanknoTextBox" runat="server" Text='<%# Bind("blankno") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                <td><asp:TextBox ID="partnoTextBox" runat="server" Text='<%# Bind("partno") %>'>
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b>Request Qty:</b></td>
                <td><asp:TextBox ID="blqtyTextBox" runat="server" Text='<%# Bind("blqty") %>'>
                </asp:TextBox></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Yield Qty:</b></td>
                <td><asp:TextBox ID="yldqtyTextBox" runat="server" Text='<%# Bind("yldqty") %>'>
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b>Price By:</b></td>
                <td>
                <b><asp:RadioButtonList ID="Price_radiobuttonlist" runat="server" selectedvalue='<%# Bind("price") %>'  DataValueField='<%# Bind("price") %>' RepeatLayout="Flow" CellSpacing="1" RepeatColumns="3">
                <asp:ListItem Value="Yield"></asp:ListItem>
                <asp:ListItem Value="Request"></asp:ListItem>
                </asp:RadioButtonList></b>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b># on Width:</b></td>
                <td><asp:TextBox ID="widTextBox" onkeyup="cal_up()" onblur="cal_up()"  runat="server" Text='<%# Bind("wid") %>'>
                </asp:TextBox></td>
                <td align="right" style="padding-right:5px"><b># on Length:</b></td>
                <td><asp:TextBox ID="lenTextBox" onkeyup="cal_up()" onblur="cal_up()"  runat="server" Text='<%# Bind("len") %>'>
                </asp:TextBox></td></tr>
               <tr><td align="right" style="padding-right:5px"><b># Up:</b></td>
               <td> <asp:TextBox ID="numupTextBox" Enabled="false" runat="server" Text='<%# Bind("numup") %>'>
                </asp:TextBox></td></tr>
               <tr><td colspan="2">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="update_button_Clock"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table>
            </EditItemTemplate>
            
            <ItemTemplate>
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Form#:</b></td>
                <td><asp:Label ID="formnoLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("formno") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>Blank#:</b></td>
                <td><asp:Label ID="blanknoLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("blankno") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Cust Part#:</b></td>
                <td><asp:Label ID="partnoLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("partno") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>Request Qty:</b></td>
                <td><asp:Label ID="blqtyLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("blqty") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Yield Qty:</b></td>
                <td><asp:Label ID="yldqtyLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("yldqty") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>Price By:</b></td>
                <td><asp:Label ID="priceLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("price") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b># on Width:</b></td>
                <td><asp:Label ID="widLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("wid") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b># on Length:</b></td>
                <td><asp:Label ID="lenLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("len") %>'></asp:Label></td></tr>
                <tr><td align="right" style="padding-right:5px"><b># Up:</b></td>
                <td><asp:Label ID="numupLabel" runat="server" BackColor="turquoise" Width="150px" Text='<%# Bind("numup") %>'></asp:Label></td></tr>
                
                <tr><td colspan="3">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button"  CausesValidation="True" CommandName="Edit"
                    Text="Update">
                </asp:Button>
                <input type="button" id="goto_Button" value="Close" runat="server" class="buttonM" onClick="closewin()" />
                </td></tr></table>
            </ItemTemplate>
        </asp:FormView>
        
        <asp:ObjectDataSource ID="ObjectDataSource_list" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectGoTo" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:QueryStringParameter  Name="prmEstNum" QueryStringField="estgo" Type="String" />
                <asp:Parameter Name="prmFormNo"  Type="Int32" />
                <asp:Parameter Name="prmBlankNo"  Type="Int32" />
                <asp:Parameter Name="prmPartno" Type="String" />
                <asp:Parameter Name="prmBlQty" Type="Int32" />
                <asp:Parameter Name="prmYldQty" Type="Int32" />
                <asp:Parameter Name="prmPrice" Type="String" />
                <asp:Parameter Name="prmWid" Type="Decimal" />
                <asp:Parameter Name="prmlen" Type="Decimal" />
                <asp:Parameter Name="prmUp" Type="Decimal" />
            </SelectParameters>
        </asp:ObjectDataSource>
        <asp:ObjectDataSource ID="ObjectDataSource_view" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectGoTo" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="View" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:QueryStringParameter  Name="prmEstNum" QueryStringField="estgo" Type="String" />
                <asp:SessionParameter Name="prmFormNo" SessionField="order_corr_go_formno" Type="Int32" />
                <asp:SessionParameter Name="prmBlankNo" SessionField="order_corr_go_blankno" Type="Int32" />
                <asp:Parameter Name="prmPartno" Type="String" />
                <asp:Parameter Name="prmBlQty" Type="Int32" />
                <asp:Parameter Name="prmYldQty" Type="Int32" />
                <asp:Parameter Name="prmPrice" Type="String" />
                <asp:Parameter Name="prmWid" Type="Decimal" />
                <asp:Parameter Name="prmlen" Type="Decimal" />
                <asp:Parameter Name="prmUp" Type="Decimal" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </form>
</body>
</html>


