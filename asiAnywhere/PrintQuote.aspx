<%@ Page Language="C#" Debug="true" AutoEventWireup="true" Inherits="printquote" Codebehind="~/PrintQuote.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Print Quote</title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
    <%--<a href="order_inquiry.aspx"><span style="font-size: 12pt; color: #0000ff; font-family: Times New Roman;
            text-decoration: underline">Back To Order Inquiry</span></a><br />--%>
        <asp:Label ID="showlabel" runat="server" Font-Bold="true" ForeColor="Red"></asp:Label>
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1">
            <%--<EditItemTemplate>
                aFile:
                <asp:TextBox ID="aFileTextBox" runat="server" Text='<%# Bind("aFile") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Update">
                </asp:LinkButton>
                <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </EditItemTemplate>
            <InsertItemTemplate>
                aFile:
                <asp:TextBox ID="aFileTextBox" runat="server" Text='<%# Bind("aFile") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>--%>
            <ItemTemplate>
                QteFile:
                <asp:Label ID="aFileLabel" runat="server" Text='<%# Bind("QteFile") %>'></asp:Label>
                <br />
                
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectPrintQuote" TypeName="browsquote">
            <SelectParameters>
                <%--<asp:SessionParameter Name="prmUser" Type="String" /> --%>
                
                <asp:Parameter DefaultValue="PrintQuote" Name="prmAction" Type="String" />
                <asp:SessionParameter Name="prmQuote" SessionField="quote_no" Type="Int32" />
                 <asp:Parameter Name="prmUser" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </div>
    </form>
</body>
</html>
