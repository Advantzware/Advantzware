<%@ Page Language="C#" AutoEventWireup="True" Inherits="printmail" Codebehind="printmail.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Print Mail</title>
</head>
<body bgcolor="gray">
    <form id="form1" runat="server">
    <div>
    
        <asp:FormView ID="FormView1" Width="600px" Height="300px" runat="server" DataSourceID="SqlDataSource1" style="position: relative; top: 2px" BackColor="white">
            <EditItemTemplate>
                <%--company:
                <asp:TextBox ID="companyTextBox" runat="server" Text='<%# Bind("company") %>'>
                </asp:TextBox><br />
                first_name:
                <asp:TextBox ID="first_nameTextBox" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:TextBox><br />
                last_name:
                <asp:TextBox ID="last_nameTextBox" runat="server" Text='<%# Bind("last_name") %>'>
                </asp:TextBox><br />
                cust_name:
                <asp:TextBox ID="cust_nameTextBox" runat="server" Text='<%# Bind("cust_name") %>'>
                </asp:TextBox><br />
                contact_loc:
                <asp:TextBox ID="contact_locTextBox" runat="server" Text='<%# Bind("contact_loc") %>'>
                </asp:TextBox><br />
                city:
                <asp:TextBox ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
                </asp:TextBox><br />
                addr1:
                <asp:TextBox ID="addr1TextBox" runat="server" Text='<%# Bind("addr1") %>'>
                </asp:TextBox><br />
                addr2:
                <asp:TextBox ID="addr2TextBox" runat="server" Text='<%# Bind("addr2") %>'>
                </asp:TextBox><br />
                state:
                <asp:TextBox ID="stateTextBox" runat="server" Text='<%# Bind("state") %>'>
                </asp:TextBox><br />
                zip:
                <asp:TextBox ID="zipTextBox" runat="server" Text='<%# Bind("zip") %>'>
                </asp:TextBox><br />
                country:
                <asp:TextBox ID="countryTextBox" runat="server" Text='<%# Bind("country") %>'>
                </asp:TextBox><br />
                phone:
                <asp:TextBox ID="phoneTextBox" runat="server" Text='<%# Bind("phone") %>'>
                </asp:TextBox><br />
                cell_phone:
                <asp:TextBox ID="cell_phoneTextBox" runat="server" Text='<%# Bind("cell_phone") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Update">
                </asp:LinkButton>
                <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>--%>
            </EditItemTemplate>
            <InsertItemTemplate>
               <%-- company:
                <asp:TextBox ID="companyTextBox" runat="server" Text='<%# Bind("company") %>'>
                </asp:TextBox><br />
                first_name:
                <asp:TextBox ID="first_nameTextBox" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:TextBox><br />
                last_name:
                <asp:TextBox ID="last_nameTextBox" runat="server" Text='<%# Bind("last_name") %>'>
                </asp:TextBox><br />
                cust_name:
                <asp:TextBox ID="cust_nameTextBox" runat="server" Text='<%# Bind("cust_name") %>'>
                </asp:TextBox><br />
                contact_loc:
                <asp:TextBox ID="contact_locTextBox" runat="server" Text='<%# Bind("contact_loc") %>'>
                </asp:TextBox><br />
                city:
                <asp:TextBox ID="cityTextBox" runat="server" Text='<%# Bind("city") %>'>
                </asp:TextBox><br />
                addr1:
                <asp:TextBox ID="addr1TextBox" runat="server" Text='<%# Bind("addr1") %>'>
                </asp:TextBox><br />
                addr2:
                <asp:TextBox ID="addr2TextBox" runat="server" Text='<%# Bind("addr2") %>'>
                </asp:TextBox><br />
                state:
                <asp:TextBox ID="stateTextBox" runat="server" Text='<%# Bind("state") %>'>
                </asp:TextBox><br />
                zip:
                <asp:TextBox ID="zipTextBox" runat="server" Text='<%# Bind("zip") %>'>
                </asp:TextBox><br />
                country:
                <asp:TextBox ID="countryTextBox" runat="server" Text='<%# Bind("country") %>'>
                </asp:TextBox><br />
                phone:
                <asp:TextBox ID="phoneTextBox" runat="server" Text='<%# Bind("phone") %>'>
                </asp:TextBox><br />
                cell_phone:
                <asp:TextBox ID="cell_phoneTextBox" runat="server" Text='<%# Bind("cell_phone") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>--%>
            </InsertItemTemplate>
            <ItemTemplate>
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
         &nbsp;&nbsp;&nbsp;&nbsp; 
          <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
         &nbsp;&nbsp;&nbsp;&nbsp;  <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
                <br />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
                <br />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
         &nbsp;&nbsp;&nbsp;&nbsp;  <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
                <br />
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
           <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>,
         &nbsp;&nbsp;&nbsp;&nbsp;  <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
         &nbsp;&nbsp;&nbsp;&nbsp;  <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>' Font-Bold="True" Font-Size="Larger"></asp:Label>
                
            
           
            
            <%--<td><b><asp:Label ID="phoneLabel" runat="server" Text='<%# Bind("phone") %>'></asp:Label></b></td>
            
            <td><b><asp:Label ID="cell_phoneLabel" runat="server" Text='<%# Bind("cell_phone") %>'>
                </asp:Label></b></td>--%>
            
                <%--company:
                <asp:Label ID="companyLabel" runat="server" Text='<%# Bind("company") %>'></asp:Label><br />--%>
                              
                
                <%--contact_loc:
                <br />--%>
               
                
               <%-- country:
                <asp:Label ID="countryLabel" runat="server" Text='<%# Bind("country") %>'></asp:Label><br />--%>
                
            </ItemTemplate>
        </asp:FormView>
        <br />
        <br />
        <br />
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printone"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
        &nbsp;
        <asp:Button ID="PrintButton" Text="Print" runat="server" CssClass="buttonM" OnClick="btnPrint_Click" />
        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
        <input type="button" value="Close" onclick="javascript:window.close()" id="closebutton" />
        </div>
        
    </form>
</body>
</html>
