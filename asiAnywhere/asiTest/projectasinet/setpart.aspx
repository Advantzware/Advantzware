<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" AutoEventWireup="true" Inherits="setpart" Title="Set Part" Codebehind="setpart.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script  language="javascript" >
function fglook()
{ 
  var NewWindow = window.open("fgitemlook.aspx","fgItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function FGitemLookup(ReturnObj1)
{ 
document.forms[0].ctl00_ContentPlaceHolder1_FormView1_vFGPartTextBox.value = ReturnObj1;
}
</script>
<div>
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource_itemfg">
        
        
        <ItemTemplate>
        <fieldset style="width:630px">
           <table class="shade">
            <tr>
           <td><b>Item no:&nbsp;</b></td> 
           <td> <asp:Label ID="itemfgLabel" runat="server" BackColor="turquoise"  Text='<%# Bind("itemfg") %>'></asp:Label>&nbsp;&nbsp;
           </td>
           <td><asp:Label ID="itemfgnameLabel" runat="server" BackColor="turquoise" Text='<%# Bind("itemfgname") %>'> </asp:Label>&nbsp;&nbsp;</td>
           <td> <asp:Label ID="itemdscrLabel" runat="server" BackColor="turquoise" Text='<%# Bind("itemdscr") %>'></asp:Label>&nbsp;&nbsp;</td>
           <td><b>Qty On-Hand:&nbsp;</b></td>
           <td><asp:Label ID="itemqhandLabel" runat="server" BackColor="turquoise" Text='<%# Bind("itemqhand") %>'></asp:Label></td>
            
            </tr>
           
            
            </table>
            </fieldset>
        </ItemTemplate>
    </asp:FormView>
   
    
   <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False"
             AllowPaging="True" AllowSorting="True" 
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid"  OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnRowDataBound="GridView1_RowDataBound" DataSourceID="ObjectDataSource_setpart" >
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade"  />
            <Columns >
             <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                    </asp:CommandField>
                <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
                <asp:BoundField DataField="vFGPart" HeaderText="FG Part#" SortExpression="vFGPart" />
                <asp:BoundField DataField="vName" HeaderText="Name" SortExpression="vName" />
                <asp:BoundField DataField="vOnHand" HeaderText="On Hand" SortExpression="vOnHand" />
                <asp:BoundField DataField="vPoJob" HeaderText="Pos/Jobs On order" SortExpression="vPoJob" />
                <asp:BoundField DataField="vToOrder" HeaderText="Allocated To Orders" SortExpression="vToOrder" />
                <asp:BoundField DataField="vBackOrder" HeaderText="BackOrder" SortExpression="vBackOrder" />
                <asp:BoundField DataField="vAvail" HeaderText="Available" SortExpression="vAvail" />
                
                
                <asp:TemplateField HeaderText="reckey" Visible="false" >
                <ItemTemplate>
                <asp:Label ID="vReckeyLabel" runat="server"  Text='<%# Bind("vReckey") %>'></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <%--<asp:BoundField DataField="vSno" HeaderText="vSno" SortExpression="vSno" />
                <asp:BoundField DataField="vReckey" HeaderText="vReckey" SortExpression="vReckey" />
                <asp:BoundField DataField="vLine" HeaderText="vLine" SortExpression="vLine" />
                <asp:BoundField DataField="vSetno" HeaderText="vSetno" SortExpression="vSetno" />--%>
                
                
            </Columns>
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" ForeColor="White" CssClass="headcolor" Height="40px" />
            
        </asp:GridView>
        <br /><br />
    <asp:FormView ID="FormView1" DataSourceID="ObjectDataSource1" runat="server" OnUnload="formview1_unload">
        <EditItemTemplate>
            <table class="shade" >
            <tr><td align="right" style="padding-right:5px"><b>Qty:</b></td>
            <td><asp:TextBox ID="vQtyTextBox" Width="80px" runat="server" MaxLength="5" Text='<%# Bind("vQty") %>'></asp:TextBox></td>            
            <td align="right" style="padding-right:5px"><b>FG Part#:</b></td>
            <td><asp:Label ID="vFGPartLabel" Width="110px" BackColor="turquoise" runat="server" Text='<%# Bind("vFGPart") %>'></asp:Label> </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
            <td><asp:Label ID="vNameLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vName") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>On Hand:</b></td>
            <td><asp:Label ID="vOnHandLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vOnHand") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Pos/Jobs On Order:</b></td>
            <td><asp:Label ID="vPoJobLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vPoJob") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Allocated TO Order:</b></td>
            <td><asp:Label ID="vToOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vToOrder") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>BackOrder:</b></td>
            <td><asp:Label ID="vBackOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vBackOrder") %>'> </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Available:</b></td>
            <td><asp:Label ID="vAvailLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vAvail") %>'></asp:Label></td></tr>
            <tr><td>          
           
            
            <asp:Label ID="vReckeyLabel" Visible="false" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
            
            <asp:Label ID="vSnoLabel" runat="server" Visible="false" Text='<%# Bind("vSno") %>'></asp:Label>
            
            <asp:Label ID="vLineLabel" runat="server" Visible="false" Text='<%# Bind("vLine") %>'></asp:Label>
            </td></tr
            <tr><td colspan="4">
           
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="button"  Text="Cancel">
            </asp:Button></td></tr></table>
        </EditItemTemplate>
        <InsertItemTemplate>
            <table class="shade">
            <tr><td align="right" style="padding-right:5px"><b>Qty:</b></td>
            <td><asp:TextBox ID="vQtyTextBox" runat="server" Width="80px" MaxLength="5" Text='<%# Bind("vQty") %>'></asp:TextBox></td>            
            <td align="right" style="padding-right:5px"><b>FG Part#:</b></td>
            <td><asp:TextBox ID="vFGPartTextBox" runat="server" Width="110px" Text='<%# Bind("vFGPart") %>'></asp:TextBox>
            <a href="#" onClick="fglook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
                      
            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
            <td><asp:Label ID="vNameLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vName") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>On Hand:</b></td>
            <td><asp:Label ID="vOnHandLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vOnHand") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Pos/Jobs On Order:</b></td>
            <td><asp:Label ID="vPoJobLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vPoJob") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Allocated TO Order:</b></td>
            <td><asp:Label ID="vToOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vToOrder") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>BackOrder:</b></td>
            <td><asp:Label ID="vBackOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vBackOrder") %>'> </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Available:</b></td>
            <td><asp:Label ID="vAvailLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vAvail") %>'></asp:Label></td></tr>
            <tr><td>          
           
            
            <asp:Label ID="vReckeyLabel" Visible="false" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
            
            <asp:Label ID="vSnoLabel" runat="server" Visible="false" Text='<%# Bind("vSno") %>'></asp:Label>
            
            <asp:Label ID="vLineLabel" runat="server" Visible="false" Text='<%# Bind("vLine") %>'></asp:Label>
            </td></tr
            <tr><td colspan="4">
            <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="Insert_Button_Click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel"   Text="Cancel">
            </asp:Button></td></tr>
            </table>
        </InsertItemTemplate>
        <ItemTemplate>
            <table class="shade" >
            <tr><td align="right" style="padding-right:5px"><b>Qty:</b></td>
            <td><asp:Label ID="vQtyLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("vQty") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>FG Part#:</b></td>
            <td><asp:Label ID="vFGPartLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vFGPart") %>'></asp:Label> </td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Name:</b></td>
            <td><asp:Label ID="vNameLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vName") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>On Hand:</b></td>
            <td><asp:Label ID="vOnHandLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vOnHand") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>Pos/Jobs On Order:</b></td>
            <td><asp:Label ID="vPoJobLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vPoJob") %>'></asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Allocated TO Order:</b></td>
            <td><asp:Label ID="vToOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vToOrder") %>'></asp:Label></td></tr>
            <tr><td align="right" style="padding-right:5px"><b>BackOrder:</b></td>
            <td><asp:Label ID="vBackOrderLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vBackOrder") %>'> </asp:Label></td>
            <td align="right" style="padding-right:5px"><b>Available:</b></td>
            <td><asp:Label ID="vAvailLabel" Width="120px" BackColor="turquoise" runat="server" Text='<%# Bind("vAvail") %>'></asp:Label></td></tr>
            <tr><td>          
           
            
            <asp:Label ID="vReckeyLabel" Visible="false" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
            
            <asp:Label ID="vSnoLabel" runat="server" Visible="false" Text='<%# Bind("vSno") %>'></asp:Label>
            
            <asp:Label ID="vLineLabel" runat="server" Visible="false" Text='<%# Bind("vLine") %>'></asp:Label>
            <asp:Label ID="vSetnoLabel" Visible="false" runat="server"  Text='<%# Bind("vSetno") %>'></asp:Label>
            </td></tr
            <tr><td colspan="4">
            <asp:Button ID="updateButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="NewButton" runat="server" CssClass="button" CausesValidation="False" CommandName="New"
                Text="New">
            </asp:Button>
            <asp:Button ID="delete_Button" runat="server" CssClass="button" CausesValidation="False" Text="Delete" OnClientClick= "return confirm('Are you sure you want to delete this record')" OnClick="Delete_Button_Click">
             </asp:Button>
             </td></tr></table>            
        </ItemTemplate>
    </asp:FormView>
        
    <asp:ObjectDataSource ID="ObjectDataSource_setpart" runat="server"
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectSetPart" TypeName="fgitem">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmNewFgItem" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmOnHand" Type="Int32" />
            <asp:Parameter Name="prmPoJob" Type="Int32" />
            <asp:Parameter Name="prmToOrder" Type="Int32" />
            <asp:Parameter Name="prmBackOrder" Type="Int32" />
            <asp:Parameter Name="prmAvail" Type="Int32" />
            <asp:SessionParameter DefaultValue="" Name="prmFGItem" SessionField="item_list_item" Type="String" />
            <asp:Parameter Name="prmReckey" Type="string" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server"
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectSetPart" TypeName="fgitem" >
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:SessionParameter SessionField="fgitem_set_part_rec" Name="prmReckey" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmOnHand" Type="Int32" />
            <asp:Parameter Name="prmPoJob" Type="Int32" />
            <asp:Parameter Name="prmToOrder" Type="Int32" />
            <asp:Parameter Name="prmBackOrder" Type="Int32" />
            <asp:Parameter Name="prmAvail" Type="Int32" />
            <asp:SessionParameter DefaultValue="" Name="prmFGItem" SessionField="item_list_item" Type="String" />
            <asp:Parameter Name="prmNewFgItem" Type="string" />
        </SelectParameters>
        
       
    </asp:ObjectDataSource>
     <asp:ObjectDataSource ID="ObjectDataSource_itemfg" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectSetPart" TypeName="fgitem"   >
        
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Itemfg" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmQty" Type="Int32" />
            <asp:Parameter Name="prmNewFgItem" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmOnHand" Type="Int32" />
            <asp:Parameter Name="prmPoJob" Type="Int32" />
            <asp:Parameter Name="prmToOrder" Type="Int32" />
            <asp:Parameter Name="prmBackOrder" Type="Int32" />
            <asp:Parameter Name="prmAvail" Type="Int32" />
            <asp:SessionParameter DefaultValue="" Name="prmFGItem" SessionField="item_list_item" Type="String" />
            <asp:Parameter Name="prmReckey" Type="String" />
        </SelectParameters>
        
    </asp:ObjectDataSource>
    
    
    
    &nbsp;

</div>
</asp:Content>

