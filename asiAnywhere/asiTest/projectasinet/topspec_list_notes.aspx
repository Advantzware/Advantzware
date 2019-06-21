<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="topspec_list_notes" Codebehind="topspec_list_notes.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Spec Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <script>
        function datelook()
        {
            var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup(obj)
        {
            document.forms[0].txt_note_date.value=obj;
        }
    </script>
</head>
<body>
    <form id="form1" runat="server">
        <div>
            <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD >&nbsp;</TD>
          <TD  nowrap><font size=+0><b>Item for Spec Notes &nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
          <TD align="right"><font size=+0><b></b></font></TD>
          <TD nowrap valign="middle" >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;
            <%--<asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>--%>
            
            <%--&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;--%>
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
       <tr><td colspan="4">
       
        <asp:GridView ID="GridView1" CssClass="Grid" runat="server" Width="500px" AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No FG Item entered." OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnUnload="grid_unload" DataSourceID="ObjectDataSource1">
        <RowStyle CssClass="shade" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True"  />
            <Columns>
                 <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vItem" HeaderText="Item No" HeaderStyle-Wrap="false" SortExpression="vItem" />
                <asp:BoundField DataField="vName" HeaderText="Name" SortExpression="vName" />
                <asp:TemplateField HeaderText="reckey" Visible="false" >
                <ItemTemplate>
                <asp:Label ID="rec_key_label" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
                
            </Columns>
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White"></HeaderStyle>
        <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="ListTopSpecNotes" TypeName="orderentry">
            <SelectParameters>
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter SessionField="order_entry_est_no"  Name="prmEst" Type="String" />
                <asp:SessionParameter SessionField="item"  Name="prmItem" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </TD>        
       </TR><tr>
       <td colspan="3"> <asp:Button ID="button_ok" Text="Ok" runat="server" CssClass="button" OnClick="Button_ok_Click" />
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        </td>
       </tr>
       </TABLE>
    </div>
    </form>
</body>
</html>
