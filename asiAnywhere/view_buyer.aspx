<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="view_buyer" Codebehind="view_buyer.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>View Buyer</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript" >

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
                        <asp:ImageButton ID="img_btn_add" runat="server" Width="35px" ImageUrl="~/Images/add.bmp" ToolTip="Add" OnClick="img_btn_add_click" />
                        </td>                       
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
                            <TD align=left nowrap><font size=+0><b>Buyer &nbsp;</b></font></TD>
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
                    <table>
                        <tr bgcolor="gray">
                            <td nowrap><div  id="navigation" style="width:100%">
		                        <ul nowrap> <li >
                                <asp:LinkButton ID="lnk_Listvend" runat="server" OnClick="lnk_Listvend_Click" >Browse Buyers</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewvend" runat="server"  OnClick="lnk_viewvend_Click"  > View Buyers</asp:LinkButton></li></ul></div>
                                
                                
                            </td>      
                        </tr>
                   </table>
                   
                   
                    <asp:FormView ID="FormView1" runat="server" OnDataBound="FormView1_DataBound" OnUnload="Formview_Unload" DataSourceID="ObjectDataSource1">
                    <EditItemTemplate>
                        <asp:Panel ID="EditPanel"  DefaultButton="UpdateButton" runat="server" >
                    
                   <fieldset class="shade">
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                   <td><asp:Label ID="vbuyerLabel" BackColor="turquoise" Width="120px" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vbuyer") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer's Name:</b></td>
                   <td><asp:TextBox ID="vdscrTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="200px" MaxLength="30" Text='<%# Bind("vnum") %>' /></td></tr>
                   
                  
                   <asp:label ID="reckeyLabelEdit" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                   <br />
                   </table>
                   <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="button" OnClick="UpdateButton_Click"
                                Text="Save" />
             &nbsp;<asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" 
                               CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                   </fieldset>
                   </asp:Panel>
                   </EditItemTemplate>
                   <InsertItemTemplate>
                   <asp:Panel ID="insertPanel"  DefaultButton="InsertButton" runat="server" >
                   <fieldset class="shade">
                   <table>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                   <td><asp:TextBox ID="vbuyerTextBox" runat="server" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="100px" Text='<%# Bind("vbuyer") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer's Name:</b></td>
                   <td><asp:TextBox ID="vdscrTextBox" Width="200px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" runat="server" Text='<%# Bind("vnum") %>' /></td></tr>
                   
                  
                   <asp:label ID="reckeyLabelinsert" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                   <br />
                   </table>
                            <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True"  OnClick="addButton_Click"
                                 Text="Save" />
                            &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"
                                CausesValidation="False" CommandName="Cancel" Text="Cancel" />
                                </fieldset></asp:Panel>
                        </InsertItemTemplate>
                        <ItemTemplate>
                        <fieldset class="shade">
                        <table>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer:</b></td>
                   <td><asp:Label ID="vbuyerLabel" Width="120px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vbuyer") %>' /></td></tr>
                   <tr><td align="right" style="padding-right:5px"><b>Buyer's Name:</b></td>
                   <td><asp:Label ID="vdscrLabel" Width="200px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vnum") %>' /></td></tr>
                   
                  
                   <asp:label ID="reckeyLabel" runat="server" Visible="false" Text='<%# Bind("reckey") %>' />
                   <br />
                   </table>
                            <br />
                            <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="New" Text="Add" />
                    <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                   &nbsp;<asp:Button ID="DeleteButton" runat="server" CssClass="button" OnClientClick="return confirm('Are you sure you want to delete this record')" OnClick="Deletebutton_Click" Text="Delete" />
                </fieldset>
                        </ItemTemplate>
                    
                    </asp:FormView>
                   
            
            
                    
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectNewBuyersList" TypeName="voucherpay">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:Parameter Name="prmvbuyer" Type="String" />                  
                  <asp:Parameter Name="prmvnum" Type="String" />
                  <asp:SessionParameter SessionField="buyer_list_reckey_buyer" Name="prmReckey" 
                      Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

