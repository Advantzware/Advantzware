<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="gnrl_ldgr_ctrl" Codebehind="gnrl_ldgr_ctrl.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>G/L Control</title>
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
        function curr() {
            var cur = document.getElementById("proctr_TextBox");
            cur.focus();
        }
        function prof() {
            var pro = document.getElementById("lstjrnl_TextBox");
            pro.focus();
        }
        var crtprf;
        function AccountLook(obj1) {
            crtprf = obj1;
            if (crtprf == "1") {
                var NewWindow = window.open("accountlook.aspx?jrnl=" + "C" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
            }
            else {
                var NewWindow = window.open("accountlook.aspx?jrnl=" + "E" + " ", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
            }
        }

        function AccountLookup(ReturnObj1, ReturnObj2) {
            if (crtprf == "1") {
                document.forms[0].FormView1_curryr_TextBox.value = ReturnObj1;
                var actdsc = document.getElementById("FormView1_curdscr_TextBox");
                actdsc.innerHTML = ReturnObj2;                    
            }
            else {
                document.forms[0].FormView1_proctr_TextBox.value = ReturnObj1;
                var actdsc = document.getElementById("FormView1_prodscr_TextBox");
                actdsc.innerHTML = ReturnObj2;                                   
            }        
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
                            <TD align=left nowrap><font size=+0><b>G/L Control  &nbsp;</b></font></TD>
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
                         <fieldset   style="width:450px">
                         <table>
                         <tr><td align="right" style="padding-top:5px;"><b>Last Journal Number:</b></td>
                         <td><asp:TextBox ID="lstjrnl_TextBox" runat="server" Width="65" Text='<%# Bind("jrnl") %>' />
                         <asp:CompareValidator ID="CompareValidator1" ControlToValidate="lstjrnl_TextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Last Transaction Number:</b></td>
                         <td><asp:TextBox ID="trnsTextBox" runat="server" Width="65" Text='<%# Bind("trns") %>' />
                         <asp:CompareValidator ID="CompareValidator2" ControlToValidate="trnsTextBox" SetFocusOnError="true" Operator="DataTypeCheck" Type="Integer" Display="Dynamic" runat="server" ErrorMessage="Enter Integer Values"></asp:CompareValidator></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Current Year Earnings:</b></td>
                         <td><asp:TextBox ID="curryr_TextBox" runat="server" Width="65" Text='<%# Bind("crtyr") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                         <td><asp:Label ID="curdscr_TextBox" Width="180" BackColor="turquoise" runat="server" Text='<%# Bind("crtdscr") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Profit Contra:</b></td>
                         <td><asp:TextBox ID="proctr_TextBox" runat="server" Width="65" Text='<%# Bind("profit") %>' />
                         <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
                         <td><asp:Label ID="prodscr_TextBox" Width="180" BackColor="turquoise" runat="server" Text='<%# Bind("prodscr") %>' /></tr>
                         
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
                         <fieldset style="width:450px">
                         <table>
                         <tr><td align="right" style="padding-top:5px;"><b>Last Journal Number:</b></td>
                         <td><asp:Label ID="lstjrnl_TextBox" runat="server" Width="65" BackColor="turquoise" Text='<%# Bind("jrnl") %>' />
                         </td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Last Transaction Number:</b></td>
                         <td><asp:Label ID="trnsTextBox" runat="server" Width="65" BackColor="turquoise" Text='<%# Bind("trns") %>' />
                         </td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Current Year Earnings:</b></td>
                         <td><asp:Label ID="curryr_TextBox" runat="server" Width="65" BackColor="turquoise" Text='<%# Bind("crtyr") %>' />
                         </td>
                         <td><asp:Label ID="curdscr_TextBox" runat="server" Width="180" BackColor="turquoise" Text='<%# Bind("crtdscr") %>' /></td></tr>
                         <tr><td align="right" style="padding-top:5px;"><b>Profit Contra:</b></td>
                         <td><asp:Label ID="proctr_TextBox" runat="server" Width="65" BackColor="turquoise" Text='<%# Bind("profit") %>' />
                         </td>
                         <td><asp:Label ID="prodscr_TextBox" runat="server" Width="180" BackColor="turquoise" Text='<%# Bind("prodscr") %>' /></tr>
                         
                         </table>
                         </fieldset>
                        
                            <br />
                            <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
                            </asp:Panel>
                        </ItemTemplate>
                    
                    </asp:FormView>
                   
            
            
                    
  
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="GLCtrlView" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String"  DefaultValue="" />
                  <asp:Parameter Name="prmjrnl" Type="Int32" />                  
                  <asp:Parameter Name="prmtrns" Type="Int32" />
                  <asp:Parameter Name="prmcrtyr" Type="String" />
                  <asp:Parameter Name="prmcrtdscr" Type="String" />
                  <asp:Parameter Name="prmprofit" Type="String" />
                  <asp:Parameter Name="prmprodscr" Type="String" />
                  <asp:Parameter Name="prmReckey" Type="String" />
                  <asp:Parameter Name="ip_field" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
                                      
        
    </div></td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

