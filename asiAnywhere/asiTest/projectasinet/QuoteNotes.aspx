<%@ Page Language="C#" MasterPageFile="~/MasterPage6.master" Debug="true" AutoEventWireup="true" Inherits="QuoteNotes" Title="Quote Notes" Codebehind="QuoteNotes.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<script src="include/CalendarControl.js"></script>
<script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>


<script>



</script>

<div>
<asp:HyperLink ID="Hyperlink1" runat="server" NavigateUrl="list_rfqs.aspx">Back to list</asp:HyperLink>
    
    <asp:FormView ID="FormView2" runat="server" 
        OnDataBound="Formview2_onbatabound"    DataSourceID="ObjectDataSource1">
        <EditItemTemplate>
            <fieldset style="width:550px"><legend>Reference Information</legend>
            <table><tr><td> <b>Quote:</b>&nbsp;&nbsp;&nbsp;
            <asp:Label ID="qtqty_quoteLabel" runat="server" Width="120px" BackColor="Turquoise" Text='<%# Bind("[qtqty-quote]") %>' />
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <b>Quote Date:</b>&nbsp;&nbsp;
            <asp:Label ID="qtqty_dateLabel" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-date]") %>' />
            </td></tr></table>
            </fieldset>
            <fieldset><asp:Panel ID="editpanel" runat="server" DefaultButton="UpdateButton">
            <table><tr><td>
            <asp:TextBox ID="qtqty_comment1TextBox" Width="500px" runat="server"  Text='<%# Bind("[qtqty-comment1]") %>' /></td></tr>
            <tr><td>
            <asp:TextBox ID="qtqty_comment2TextBox" Width="500px" runat="server" Text='<%# Bind("[qtqty-comment2]") %>' /></td></tr>
            <tr><td>
            <asp:TextBox ID="qtqty_comment3TextBox" Width="500px" runat="server" Text='<%# Bind("[qtqty-comment3]") %>' /></td></tr>
            <tr><td>
            <asp:TextBox ID="qtqty_comment4TextBox" Width="500px" runat="server" Text='<%# Bind("[qtqty-comment4]") %>' /></td></tr>
            <tr><td>
            <asp:TextBox ID="qtqty_comment5TextBox" Width="500px" runat="server" Text='<%# Bind("[qtqty-comment5]") %>'  onblur="document.getElementById('ctl00_ContentPlaceHolder1_FormView2_qtqty_comment1TextBox').focus();" /></td></tr>
            <tr><td>
            
            <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button"  Text="Save" OnClick="Formview2_update_button_click" />
            &nbsp;<asp:Button ID="UpdateCancelButton" runat="server"  CssClass="button"  CausesValidation="False" CommandName="Cancel" Text="Cancel" />
            </td></tr></table></asp:Panel></fieldset>
        </EditItemTemplate>
        
        <ItemTemplate>
        <fieldset style="width:550px"><legend>Reference Information</legend>
        <table><tr><td> <b>Quote:</b>&nbsp;&nbsp;&nbsp;
         <asp:Label ID="qtqty_quoteLabel" runat="server" Width="120px" BackColor="Turquoise" Text='<%# Bind("[qtqty-quote]") %>' />
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <b>Quote Date:</b>&nbsp;&nbsp;
            <asp:Label ID="qtqty_dateLabel" Width="120px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-date]") %>' />
        </td></tr></table>
        </fieldset>
            <fieldset>
            <table>
            <tr><td><asp:Label ID="qtqty_comment1Label" Width="550px" BackColor="Turquoise" runat="server"  Text='<%# Bind("[qtqty-comment1]") %>' /></td></tr>
            <tr><td><asp:Label ID="qtqty_comment2Label" Width="550px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-comment2]") %>' /></td></tr>
            <tr><td><asp:Label ID="qtqty_comment3Label" Width="550px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-comment3]") %>' /></td></tr>
            <tr><td> <asp:Label ID="qtqty_comment4Label" Width="550px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-comment4]") %>' /></td></tr>
            <tr><td><asp:Label ID="qtqty_comment5Label" Width="550px" BackColor="Turquoise" runat="server" Text='<%# Bind("[qtqty-comment5]") %>' /></td></tr>
            <tr>
            <td>
             <asp:Button ID="UpdateButton" runat="server"  CssClass="button" CausesValidation="False" CommandName="edit"
                Text="Update">
            </asp:Button>
           <%-- <asp:Button ID="DeleteButton" runat="server"  CssClass="button" CausesValidation="False" OnClick="Formview2_deletebutton_Click"  OnClientClick="return confirm('Delete Currently Selected Record?')"     Text="Delete">
            </asp:Button>--%>
            </td>
            </tr>
            
            </table>
            
            </fieldset>
            
        </ItemTemplate>
    </asp:FormView>
    
   
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectQuoteNotes" 
        TypeName="QuoteDetail">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
            <asp:Parameter Name="prmNote1" Type="String" />
            <asp:Parameter Name="prmNote2" Type="String" />
            <asp:Parameter Name="prmNote3" Type="String" />
            <asp:Parameter Name="prmNote4" Type="String" />
            <asp:Parameter Name="prmNote5" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
     
    
     
</div>


</asp:Content>

