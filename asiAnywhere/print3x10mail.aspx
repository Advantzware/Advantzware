<%@ Page Language="C#" AutoEventWireup="true" Inherits="print3x10mail" Codebehind="print3x10mail.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<script runat="server">

</script>

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Print Mail</title>
</head>
<body bgcolor="gray">
    <form id="form1" runat="server">
    
    <div id="printall" runat="server">
    <table><tr>
    <td>
    
        <asp:FormView ID="FormView1" runat="server" DataSourceID="SqlDataSource1" BackColor="white">
            
            
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
           
            </table>
            
                
            
           
            
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
        </td>
        
        <asp:SqlDataSource ID="SqlDataSource1" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printone"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        
        <asp:SqlDataSource ID="SqlDataSource2" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printtwo"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView2" runat="server" DataSourceID="SqlDataSource2" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
             
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            
            <asp:SqlDataSource ID="SqlDataSource3" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printthree"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView3" runat="server" DataSourceID="SqlDataSource3" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
    </tr>
            <asp:SqlDataSource ID="SqlDataSource4" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printfour"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <tr>
        <td>
         <asp:FormView ID="FormView4" runat="server" DataSourceID="SqlDataSource4" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            <asp:SqlDataSource ID="SqlDataSource5" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printfive"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView5" runat="server" DataSourceID="SqlDataSource5" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            <asp:SqlDataSource ID="SqlDataSource6" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printsix"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView6" runat="server" DataSourceID="SqlDataSource6" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            </tr>
            <asp:SqlDataSource ID="SqlDataSource7" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printseven"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <tr>
        <td>
         <asp:FormView ID="FormView7" runat="server" DataSourceID="SqlDataSource7" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
            
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            <asp:SqlDataSource ID="SqlDataSource8" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printeight"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView8" runat="server" DataSourceID="SqlDataSource8" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
           
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            <asp:SqlDataSource ID="SqlDataSource9" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printnine"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <td>
         <asp:FormView ID="FormView9" runat="server" DataSourceID="SqlDataSource9" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
           
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            </tr>
            <asp:SqlDataSource ID="SqlDataSource10" runat="server" ConnectionString="<%$ ConnectionStrings:Project1ConnectionString %>"
            SelectCommand="SELECT [company], [first_name], [last_name], [cust_name], [cust_no], [city], [addr1], [addr2], [state], [zip], [country], [phone], [cell_phone] FROM [contact] where [rec_key]=@rec_key">
            <SelectParameters>
      <asp:SessionParameter Name="rec_key" SessionField="printten"
                        Type="String" />
                                                          
        </SelectParameters>
        </asp:SqlDataSource>
        <tr>
        <td>
        
         <asp:FormView ID="FormView10" runat="server" DataSourceID="SqlDataSource10" BackColor="white">
                        
            <ItemTemplate>
            <table>
            <tr>
            <td width="250px">
            <asp:Label ID="first_nameLabel" runat="server" Text='<%# Bind("first_name") %>'>
                </asp:Label>
                <asp:Label ID="last_nameLabel" runat="server" Text='<%# Bind("last_name") %>'></asp:Label>
                <asp:Label ID="contact_locLabel" runat="server" Text='<%# Bind("cust_no") %>'>
                </asp:Label>
                <br />
                <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("cust_name") %>'></asp:Label>
                <br />
                <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>'></asp:Label>
                <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>'></asp:Label>
                <br />
                <asp:Label ID="cityLabel" runat="server" Text='<%# Bind("city") %>'></asp:Label>,
                <asp:Label ID="stateLabel" runat="server" Text='<%# Bind("state") %>'></asp:Label>
                <asp:Label ID="zipLabel" runat="server" Text='<%# Bind("zip") %>'></asp:Label>
            </td>
           
                       
            </tr>
            </table>
            </ItemTemplate>
            </asp:FormView>
            </td>
            </tr>
            </table>
            <br />
            <br />
            <br />
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
