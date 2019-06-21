<%@ Page Language="C#" AutoEventWireup="true" Inherits="replacejobmt" Title="Job Material" Codebehind="replacejobmt.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Job Material</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<script type="text/javascript">
    

</script>

<body>
    <form id="form1" runat="server" >               
              <br /><br />                                    
          
    <div id="griddiv" runat="server"><fieldset id="gridfield">
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" DataSourceID="ObjectDataSource2" AllowPaging="true" PageSize ="10" runat="server" AllowSorting="true"
        AutoGenerateColumns="false" Style="position: static" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted"
        DataKeyNames="rec_key"  CssClass="Grid">
          <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
        <Columns>
                 <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                    ShowSelectButton="True">
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vfrm" HeaderText="From" SortExpression="vfrm" />
                <asp:BoundField DataField="vblk" HeaderText="Blank" SortExpression="vblk" />
                <asp:BoundField DataField="vout" HeaderText="vout" Visible="false" SortExpression="vout" />
                <asp:BoundField DataField="rm-no" HeaderText="RM Item#" SortExpression="rm-no" />
                <asp:BoundField DataField="fgitem" HeaderText="FG Item#" 
                    SortExpression="fgitem" />
                <asp:BoundField DataField="rec_key" Visible="false" HeaderText="rec_key" 
                    SortExpression="rec_key" />
            </Columns>
             <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        </asp:GridView>
        <br /> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        
        <asp:Button ID="okButton" runat="server" Width="50px" class="buttonM" CausesValidation="True" OnClick="okButton_Click" Text="Ok" />
        </fieldset>
        </div>
        
        <div id="formviewdiv" runat="server">
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
        <fieldset>
        <table>
        <tr><td align="right" style="padding-right:5px;"><b>Form:</b></td>
        <td><asp:Label ID="vfrmTextBox" runat="server" Text='<%# Bind("vfrm") %>' Width="100px" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" /></td>
        <td><b>Blank:</b></td>
        <td> <asp:Label ID="vblkTextBox" runat="server" Text='<%# Bind("vblk") %>' Width="100px" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" /></td>
        </tr>
        <tr>
        
        <td><b>Number Out(RC Dept):</b></td>
        <td><asp:TextBox ID="voutTextBox" Width="100px" runat="server" Text='<%# Bind("vout") %>' />
        <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="voutTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Number Only"></asp:CompareValidator></td>
        </tr>
        <tr><td colspan="4"><br /> <asp:Label ID="rec_keyLabel" Visible="false" runat="server" Text='<%# Bind("rec_key") %>' />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        <asp:Button ID="UpdateButton" runat="server" Width="50px" class="buttonM" CausesValidation="True" OnClick="UpdateButton_Click" Text="Ok" />
            
        </td></tr>
        
        </table>
        
        </fieldset>
                      
        </EditItemTemplate>
        
        
        <ItemTemplate>
            vfrm:
            <asp:Label ID="vfrmLabel" runat="server" Text='<%# Bind("vfrm") %>' />
            <br />
            vblk:
            <asp:Label ID="vblkLabel" runat="server" Text='<%# Bind("vblk") %>' />
            <br />
            vout:
            <asp:Label ID="voutLabel" runat="server" Text='<%# Bind("vout") %>' />
            <br />
        </ItemTemplate>
    </asp:FormView>
    </div>
        
        
        <div>
       <%--<input type="button" name="OK" class="buttonM" id="ok" value="Close" onclick="javascript:top.opener.window.caljob()" />--%>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="Selectreplacejobmat" TypeName="browspo" >
            <SelectParameters>            
                    
                <asp:Parameter DefaultValue="View" Name="prmAction"  Type="String" />                               
                <asp:Parameter Name="prmUser" Type="String" />                
                <asp:QueryStringParameter QueryStringField="sno" Name="prmsno" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="bno" Name="prmbno" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="job" Name="prmjob" Type="String" />
                <asp:QueryStringParameter QueryStringField="job2" Name="prmjob2" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="item" Name="prmitem" Type="String" />
                <asp:QueryStringParameter QueryStringField="qty" Name="prmqty" Type="Decimal" />
                <asp:QueryStringParameter QueryStringField="qtyuom" Name="prmqtyuom" Type="String" />
                <asp:QueryStringParameter QueryStringField="pruom" Name="prmpruom" Type="String" />
                <asp:QueryStringParameter QueryStringField="cost" Name="prmcost" Type="Decimal" />
                <asp:SessionParameter SessionField="pur_ord_po" Name="prmpo" Type="String" />
                <asp:Parameter Name="prmvout" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="line" Name="prmline" Type="Int32" />
                <asp:ControlParameter ControlID="GridView1" DefaultValue="" Name="prmReckey" 
                    PropertyName="SelectedValue" Type="String" />
                            
            </SelectParameters>
        </asp:ObjectDataSource>
        <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="Selectreplacejobmat" TypeName="browspo" >
            <SelectParameters>            
                    
                <asp:Parameter DefaultValue="Select" Name="prmAction"  Type="String" />                               
                <asp:Parameter Name="prmUser" Type="String" />                
                <asp:QueryStringParameter QueryStringField="sno" Name="prmsno" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="bno" Name="prmbno" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="job" Name="prmjob" Type="String" />
                <asp:QueryStringParameter QueryStringField="job2" Name="prmjob2" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="item" Name="prmitem" Type="String" />
                <asp:QueryStringParameter QueryStringField="qty" Name="prmqty" Type="Decimal" />
                <asp:QueryStringParameter QueryStringField="qtyuom" Name="prmqtyuom" Type="String" />
                <asp:QueryStringParameter QueryStringField="pruom" Name="prmpruom" Type="String" />
                <asp:QueryStringParameter QueryStringField="cost" Name="prmcost" Type="Decimal" />
                <asp:SessionParameter SessionField="pur_ord_po" Name="prmpo" Type="String" />
                <asp:Parameter Name="prmvout" Type="Int32" />
                <asp:QueryStringParameter QueryStringField="line" Name="prmline" Type="Int32" />
                <asp:Parameter  Name="prmReckey" Type="String" />
                            
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
