<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="cust_view_notes" Codebehind="cust_view_notes.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Spec Notes</title>
     <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <script>
        function formlook()
        { 
            var NewWindow = window.open("topformno_lookup.aspx","TopFormNoLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function TopFormLookUp(ReturnObj1)
        { 
            document.forms[0].FormView1_vNoteFrmNoTextBox.value = ReturnObj1;
            //document.forms[0].ctl00_ContentPlaceHolder1_FormView1_VCustNameTextBox.value = ReturnObj2;
        }
        function deptlook()
        { 
            var NewWindow = window.open("topdept_lookup.aspx","TopSpecLookupWindow","width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function TopDeptLookUp(ReturnObj1,ReturnObj2)
        { 
            document.forms[0].FormView1_vCustCodeTextBox.value = ReturnObj1;
            
        }
        function grouplook()
        { 
            var NewWindow = window.open("group_lookup.aspx","GroupLookupWindow","width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function GroupLookUp(ReturnObj1,ReturnObj2)
        { 
            document.forms[0].FormView1_vNoteGroupTextBox.value = ReturnObj1;
            
        }
     </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD >&nbsp;</TD>
          <TD  nowrap><font size=+0><b>Item for Spec Notes&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
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
      </TABLE> 
      
      <table>
                 <tr>
                    <td bgcolor="gray">
                        <asp:ImageButton ID="img_btn_list_notes" runat="server" ImageUrl="~/Images/list notes 0.Jpg" OnClick="img_btn_list_notes_click" />
                        <asp:ImageButton ID="img_btn_view_notes" runat="server" ImageUrl="~/Images/view notes 1.Jpg" OnClick="img_btn_view_notes_click" />
                        </td><td>
                        
                    </td>
                </tr>
            </table>
            <asp:Button ID="btn_add_new" runat="server" Text="Add" OnClick="btn_add_new_click" CssClass="buttonM" />
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload">
            <EditItemTemplate>
                <fieldset class="shade">
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Date:</td>
                            <td nowrap>
                                <asp:Label ID="vNoteDateLabel" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("vCustDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">Note Time:</td>
                            <td nowrap>
                                <asp:Label ID="vNoteTimeLabel" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("vCustNoteTime") %>'></asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">UserId:</td>
                            <td nowrap>
                                <asp:Label ID="vUserIdLabel" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("vUserId") %>'></asp:Label>
                            </td>                            
                            <td nowrap>&nbsp;</td>
                            <td nowrap>
                                <asp:Label ID="vNoteViewedLabel" Visible="false" runat="server" Text='<%# Bind("vNoteViewed") %>'></asp:Label>
                                <asp:CheckBox ID="CheckBox1" Enabled="false" Text="Viewed" runat="server" />
                            </td>
                        </tr>
                        <tr>
                        <td align="right" style="padding-right:5px"> Note Type:</td>
                        <td colspan="3"> 
                            <asp:RadioButtonList ID="RadioButtonList1" SelectedValue='<%# Bind("vNoteType") %>'  CellSpacing="3" RepeatColumns="3"  DataValueField='<%# Bind("vNoteType") %>' AutoPostBack="true" OnSelectedIndexChanged="rediobutton_selected_change"  runat="server">
                            <asp:ListItem Value="C" Text="Customer"></asp:ListItem>
                            <asp:ListItem Value="G" Text="Group"></asp:ListItem>
                            <asp:ListItem Value="D" Text="Department"></asp:ListItem>
                            </asp:RadioButtonList></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Group:</td>
                            <td nowrap >
                                <asp:TextBox ID="vNoteGroupTextBox" Width="100px" runat="server" Text='<%# Bind("vNoteGroup") %>'></asp:TextBox>
                                <a href="#" onClick="grouplook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                &nbsp;
                                
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">Dept:</td>
                            <td nowrap>
                            <asp:TextBox ID="vCustCodeTextBox" Width="100px" runat="server" Text='<%# Bind("vCustCode") %>'></asp:TextBox>
                            <a href="#" onClick="deptlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                                                       
                            <td nowrap></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Title:</td>
                            <td nowrap colspan="6">
                                <asp:TextBox ID="vNoteTitleTextBox" Width="220" runat="server" Text='<%# Bind("vCustNoteTitle") %>'></asp:TextBox>
                                <asp:RequiredFieldValidator ControlToValidate="vNoteTitleTextBox" Display="dynamic" SetFocusOnError="true" ID="RequiredFieldValidator1" runat="server" ErrorMessage="Required"></asp:RequiredFieldValidator>
                            </td>
                        
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"></td>
                            <td nowrap colspan="7">
                                <asp:TextBox ID="vNoteTextTextBox" TextMode="multiline" Height="200px" Width="400px" runat="server" Text='<%# Bind("vNoteText") %>'></asp:TextBox>
                            </td>
                            
                        </tr>
                        <tr>
                            <td>&nbsp;</td>
                        </tr>
                        <tr>
                            <td colspan="4">
                                <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" CssClass="buttonM"
                                    Text="Save" OnClick="UpdateButton_Click">
                                </asp:Button>
                                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                                    Text="Cancel">
                                </asp:Button>
                            </td>
                        </tr>
                    </table>
                </fieldset>                 
            </EditItemTemplate>
            <InsertItemTemplate>
                <fieldset class="shade">
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Date:</td>
                            <td nowrap>
                                <asp:Label ID="vNoteDateLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">Note Time:</td>
                            <td nowrap>
                                <asp:Label ID="vNoteTimeLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustNoteTime") %>'></asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">UserId:</td>
                            <td nowrap>
                                <asp:Label ID="vUserIdLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vUserId") %>'></asp:Label>
                            </td>                            
                            <td nowrap>&nbsp;</td>
                            <td nowrap>
                                <asp:Label ID="vNoteViewedTextBox" Visible="false" runat="server" Text='<%# Bind("vNoteViewed") %>'></asp:Label>
                                <asp:CheckBox ID="CheckBox1" Enabled="false" Text="Viewed" runat="server" />
                            </td>
                        </tr>
                        <tr>
                        <td align="right" style="padding-right:5px"> Note Type:</td>
                        <td colspan="3"> 
                            <asp:RadioButtonList ID="RadioButtonList1"  CellSpacing="3" RepeatColumns="3" AutoPostBack="true" OnSelectedIndexChanged="rediobutton_selected_change" SelectedValue='<%# Bind("vNoteType") %>'  DataValueField='<%# Bind("vNoteType") %>' runat="server">
                            <asp:ListItem Value="C" Text="Customer"></asp:ListItem>
                            <asp:ListItem Value="G" Text="Group"></asp:ListItem>
                            <asp:ListItem Value="D" Text="Department"></asp:ListItem>
                            </asp:RadioButtonList></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Group:</td>
                            <td nowrap >
                                <asp:TextBox ID="vNoteGroupTextBox" Width="100px" runat="server" Text='<%# Bind("vNoteGroup") %>'></asp:TextBox>
                                <a href="#" onClick="grouplook(); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                &nbsp;
                                
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">Dept:</td>
                            <td nowrap>
                            <asp:TextBox ID="vCustCodeTextBox" Width="100px" runat="server" Text='<%# Bind("vCustCode") %>'></asp:TextBox>
                            <a href="#" onClick="deptlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                                                       
                            <td nowrap></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Title:</td>
                            <td nowrap colspan="6">
                                <asp:TextBox ID="vNoteTitleTextBox" runat="server" Width="220" Text='<%# Bind("vCustNoteTitle") %>'></asp:TextBox>
                                <asp:RequiredFieldValidator ControlToValidate="vNoteTitleTextBox" Display="dynamic" SetFocusOnError="true" ID="RequiredFieldValidator1" runat="server" ErrorMessage="Required"></asp:RequiredFieldValidator>
                            </td>
                           
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"></td>
                            <td nowrap colspan="7">
                                <asp:TextBox ID="vNoteTextTextBox" TextMode="multiLine" Height="200px" Width="400px" runat="server" Text='<%# Bind("vNoteText") %>'></asp:TextBox>
                            </td>
                            
                        </tr>
                        <tr>
                            <td>&nbsp;</td>
                        </tr>
                        <tr>
                            <td colspan="4">
                                <asp:Button ID="InsertButton" runat="server" CausesValidation="True" CssClass="buttonM"
                                    Text="Save" OnClick="InsertButton_Click">
                                </asp:Button>
                                <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False" CommandName="Cancel"
                                    Text="Cancel">
                                </asp:Button>
                            </td>
                        </tr>
                    </table>
                </fieldset>                
            </InsertItemTemplate>
            <ItemTemplate>
                <fieldset class="shade">
                    <table>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Date:</td>
                            <td nowrap><asp:Label ID="vNoteDateLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustDate","{0:MM/dd/yyyy}") %>'></asp:Label></td>
                            <td nowrap align="right" style="padding-right:5px;">Note Time:</td>
                            <td nowrap><asp:Label ID="vNoteTimeLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustNoteTime") %>'></asp:Label></td>
                            <td nowrap align="right" style="padding-right:5px;">User ID:</td>
                            <td nowrap><asp:Label ID="vUserIdLabel" Width="60px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vUserId") %>'></asp:Label></td>                            
                            <td nowrap>&nbsp;</td>
                            <td nowrap>
                                <asp:Label ID="vNoteViewedLabel" Visible="false" runat="server" Text='<%# Bind("vNoteViewed") %>'></asp:Label>
                                <asp:CheckBox ID="CheckBox1" Enabled="false" Text="Viewed" runat="server" />
                            </td>
                        </tr>
                        <tr>
                        <td align="right" style="padding-right:5px"> Note Type:</td>
                        <td colspan="4"> 
                            <asp:RadioButtonList ID="RadioButtonList1"  Enabled="false" CellSpacing="3" RepeatColumns="3" SelectedValue='<%# Bind("vNoteType") %>'  DataValueField='<%# Bind("vNoteType") %>' runat="server">
                            <asp:ListItem Value="C" Text="Customer"></asp:ListItem>
                            <asp:ListItem Value="G" Text="Group"></asp:ListItem>
                            <asp:ListItem Value="D" Text="Department"></asp:ListItem>
                            </asp:RadioButtonList></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Group:</td>
                            <td nowrap >
                                <asp:Label ID="vDeptCodeLabel" Width="70px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vNoteGroup") %>'></asp:Label>
                                    &nbsp;&nbsp;&nbsp;                               
                            </td>
                            <td nowrap align="right" style="padding-right:5px;">Dept:</td>
                            <td nowrap>
                             <asp:Label ID="vDeptNameLabel" Width="70px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustCode") %>'></asp:Label> 
                            </td>
                            <%--<td nowrap align="right" style="padding-right:5px;">Form:</td>
                            <td nowrap>
                                <asp:Label ID="vNoteFrmNoLabel" Width="50px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vNoteFrmNo") %>'></asp:Label>
                            </td>   --%>                         
                            <td nowrap></td>
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;">Note Title:</td>
                            <td nowrap colspan="7">
                                <asp:Label ID="vNoteTitleLabel" Width="100%" BackColor="turquoise"  BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vCustNoteTitle") %>'>
                                </asp:Label>
                            </td>
                            <td nowrap align="right" style="padding-right:5px;"></td>
                            <td nowrap></td>
                            <td nowrap align="right" style="padding-right:5px;"></td>
                            <td nowrap></td>                            
                            
                        </tr>
                        <tr>
                            <td nowrap align="right" style="padding-right:5px;"></td>
                            <td nowrap colspan="7">
                                <asp:TextBox ID="vNoteTextLabel" ReadOnly="true" Height="200px" TextMode="multiLine" Width="400px" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vNoteText") %>'></asp:TextBox>
                            </td>
                            
                        </tr>
                        <tr>
                            <td>&nbsp;</td>
                        </tr>
                        <tr>
                            <td colspan="4">
                                <asp:Button ID="btn_add" runat="server" Text="Add" CssClass="buttonM" CommandName="New" />
                                <asp:Button ID="btn_update" runat="server" Text="Update" CssClass="buttonM" CommandName="Edit" />
                                <asp:Button ID="btn_delete" runat="server" Text="Delete" CssClass="buttonM" OnClientClick="return confirm('Are you sure you want to delete this record!')"
                                 OnClick="btn_delete_click" />
                            </td>
                        </tr>
                    </table>
                </fieldset>
               
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustNotes" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmRecKey" SessionField="ar_inv_cust_rec_key"
                    Type="String" />
                <asp:Parameter Name="prmHeader" Type="String" />
                <asp:SessionParameter Name="prmNoteDate" SessionField="spec_cust_list_notes_date" Type="String" />
                <asp:SessionParameter Name="prmNoteTime" SessionField="spec_cust_list_notes_time" Type="String" />
                <asp:Parameter Name="prmUserId" Type="String" />
                <asp:Parameter Name="prmViewed" Type="String" />
                <asp:Parameter Name="prmCode" Type="String" />
                <asp:Parameter Name="prmGroup" Type="String" />
                <asp:SessionParameter Name="prmNoteTitle" SessionField="spec_cust_list_notes_title" Type="String" />
                <asp:Parameter Name="prmNewNoteTitle" Type="String" />
                <asp:Parameter Name="prmNoteText" Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
