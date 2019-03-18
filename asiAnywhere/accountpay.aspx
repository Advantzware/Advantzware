<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="accountpay" Codebehind="accountpay.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Accounts Payable Control</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" >

    var account = "";
    function AccountLook(var1) {
        account = var1;
        var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function AccountLookup(ReturnObj1, ReturnObj2) {
        if (account == "1")
            document.forms[0].FormView1_payableTextBox.value = ReturnObj1;
        else if (account == "2")
            document.forms[0].FormView1_purchTextBox.value = ReturnObj1;
        else if (account == "3")
            document.forms[0].FormView1_cashactTextBox.value = ReturnObj1;
        else if (account == "4")
            document.forms[0].FormView1_discTextBox.value = ReturnObj1;
        else if (account == "5")
            document.forms[0].FormView1_staxTextBox.value = ReturnObj1;
            else
                document.forms[0].FormView1_freightTextBox.value = ReturnObj1;
        }

        function focusval(obj) {
            obj.style.backgroundColor = 'blue';
            obj.style.color = 'white';
        }
        function blurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
        }
   </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
            <hd:header id="Header1" runat="server"></hd:header>
                <table width="100%"><tr><td><div>
        <table align="left" border="1" width="75%">
                <tr class="topheadcolor">                                      
                 
                                             
                        <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="hlnkLogOut_Click" />
                        </td>
                        <td nowrap> &nbsp;</td>
                </tr>
      </table></div>
        </td>
      </tr>
      <tr>
      <td>
                <div>            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                             <TD width=30>&nbsp;</TD>
                            <TD align=left nowrap><font size=+0><b>Accounts Payable Control  &nbsp;</b></font></TD>
                            <td nowrap>
                                <asp:LinkButton ID="backtomenuLinkButton" OnClick ="Back_tomenu_Click" runat="server">Back to menu</asp:LinkButton>
                            </td>          
                            <TD  align="left" nowrap>Logged as&nbsp;
                                <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;            
                                <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
                                &nbsp;<b>Company: &nbsp;</b><asp:label id="labelcompany"   runat="server" Font-Bold="True">&nbsp;</asp:label>
                            </TD>
          
                            <TD vAlign="middle" width="20">&nbsp;</TD>          
                            <td width=30>&nbsp;</td>
                        </TR>
                    </TABLE>
                   
                   
                   
                    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="Formview_Unload" DataSourceID="ObjectDataSource1">
                    <EditItemTemplate>
                    <asp:Panel ID="editPanel" runat="server" CssClass="shade" DefaultButton="UpdateButton">
                         <fieldset   style="width:400px">
                         <table>
                         <tr><td align="right" style="padding-top:5px;"><b>Accounts Payable:</b></td>
                         <td><asp:TextBox ID="payableTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("payable") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Purchases Account:</b></td>
                         <td><asp:TextBox ID="purchTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("purch") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Cash Account:</b></td>
                         <td><asp:TextBox ID="cashactTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("cashact") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(3); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Discount Taken:</b></td>
                         <td><asp:TextBox ID="discTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("disc") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(4); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Sales Tax:</b></td>
                         <td><asp:TextBox ID="staxTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("stax") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(5); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Freight Account:</b></td>
                         <td><asp:TextBox ID="freightTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Text='<%# Bind("freight") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(6); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
                         </table>
                         </fieldset>
                        
                        <br />
                        <asp:TextBox ID="reckeyTextBox" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                        
                        <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="UpdateButton_Click"
                           CssClass="button" Text="Save" />
                        &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button"
                            CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                       </asp:Panel>
                   </EditItemTemplate>
                   
                        <ItemTemplate>
                           <asp:Panel ID="itemPanel" runat="server" CssClass="shade" DefaultButton="UpdateItemButton">
                         <fieldset style="width:400px">
                         <table>
                         <tr><td align="right" style="padding-top:5px;"><b>Accounts Payable:</b></td>
                         <td><asp:Label ID="payableLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("payable") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Purchases Account:</b></td>
                         <td><asp:Label ID="purchLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("purch") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Cash Account:</b></td>
                         <td><asp:Label ID="cashactLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("cashact") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Discount Taken:</b></td>
                         <td><asp:Label ID="discLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("disc") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Sales Tax:</b></td>
                         <td><asp:Label ID="staxLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("stax") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Freight Account:</b></td>
                         <td><asp:Label ID="freightLabel" runat="server" Width="120px" BackColor="turquoise" Text='<%# Bind("freight") %>' /></td></tr>
                         </table>
                         </fieldset>
                        
                            <br />
                            <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                            </asp:Panel>
                        </ItemTemplate>
                    
                    </asp:FormView>
                   
            
            
                    
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectAPControl" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:Parameter Name="prmpayable" Type="String" />                  
                  <asp:Parameter Name="prmpurch" Type="String" />
                  <asp:Parameter Name="prmcashact" Type="String" />
                  <asp:Parameter Name="prmdisc" Type="String" />
                  <asp:Parameter Name="prmstax" Type="String" />
                  <asp:Parameter Name="prmfreight" Type="String" />
                  <asp:Parameter Name="prmReckey" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

