<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="attach_view" Codebehind="top_attach_view.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Attachment</title>
     <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
     <script>
     function estimatelook(){ 
        var NewWindow = window.open("estimate_lookup.aspx","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function EstimateLookup(ReturnObj1){ 
        document.forms[0].FormView1_vEstimateTextBox.value = ReturnObj1;
        }
function fglook(){ 
  var NewWindow = window.open("fgitemlook.aspx","FGLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

function FGitemLookup(ReturnObj1){ 
  document.forms[0].FormView1_vFGitemTextBox.value = ReturnObj1;
    }
     </script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD >&nbsp;</TD>
          <TD  nowrap><font size=+0><b>Attachment&nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
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
      <asp:Label ID="user_null_label" runat="server" ForeColor="red"></asp:Label>
      <table>
                 <tr>
                    <td bgcolor="gray">
                        <div  id="navigation" style="width:100%">
		                <ul nowrap> <li  >
                        <asp:LinkButton ID="img_btn_list_attach" runat="server"  OnClick="img_btn_list_attach_click" >Browse Attach</asp:LinkButton></li>
                        <li class="selected"><asp:LinkButton ID="img_btn_view_attach" runat="server"  OnClick="img_btn_view_attach_click" >View Attach</asp:LinkButton></li></ul></div>
                        </td>
                </tr>
            </table>
            <asp:Button ID="btn_add_new" runat="server" Text="Add" OnClick="btn_add_new_click" CssClass="buttonM" />
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" OnUnload="FormView1_Unload">
            <EditItemTemplate>
                <fieldset class="shade">
               <table class="shade"><tr><td nowrap align="right" style="padding-right:5px"><b>Attached File: </b></td>
                <td nowrap colspan="5">                
                <asp:TextBox ID="vAttFileTextBox" runat="server" Width="200px" Text='<%# Bind("vAttFile") %>'>
                </asp:TextBox> <b>Upload new file: </b>
                    <asp:FileUpload ID="FileUpload1" runat="server" />
                                   
                  <asp:RegularExpressionValidator id="FileUpLoadValidator" runat="server" ErrorMessage="Invalid File" 
                  ValidationExpression="^(([a-zA-Z]:)|(\\{2}\w+)\$?)(\\(\w[\w].*))(.jpg|.JPG|.gif|.GIF|.txt|.TXT|.doc|.DOC|.dot|.DOT|.xls|.XLS|.pdf|.PDF|.html|.HTML|.csv|.CSV)$" ControlToValidate="FileUpload1">
                  </asp:RegularExpressionValidator>

                    
                </td></tr>
                <tr><td nowrap align="right" style="padding-right:5px"><b>Open With:</b></td>
                <td colspan="3">
                 <asp:DropDownList ID="open_with_dropdownlist" runat="server" Width="120px" SelectedValue='<%# Bind("vOpenWith") %>' DataTextField='<%# Bind("vOpenWith") %>'>
                 <asp:ListItem Text="Word" Value="Word"></asp:ListItem>
                 <asp:ListItem Text="Excel" Value="Excel"></asp:ListItem>
                 <asp:ListItem Text="Acrobat" Value="Acrobat"></asp:ListItem>
                 <asp:ListItem Text="MS Paint" Value="MS Paint"></asp:ListItem>
                 <asp:ListItem Text="Photo Shop" Value="Photo Shop"></asp:ListItem>
                 <asp:ListItem Text="Notepad" Value="Notepad"></asp:ListItem>
                 <asp:ListItem Text="Wordpad" Value="Wordpad"></asp:ListItem>
                 <asp:ListItem Text="Internet Explorer" Value="Internet Explorer"></asp:ListItem>
                 <asp:ListItem Text="Other" Enabled="false" Value=""></asp:ListItem>
                 </asp:DropDownList>
                </td></tr>
                <tr><td nowrap align="right" style="padding-right:5px"><b>Estimate:</b></td>
                <td><asp:TextBox ID="vEstimateTextBox" Width="100px" runat="server" Text='<%# Bind("vEstimate") %>'>
                </asp:TextBox>
                <a href="#" onclick="estimatelook(); return false"><asp:Image ID="custLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="right" style="padding-right:5px"><b>FG Item#:</b></td>
                <td><asp:TextBox ID="vFGitemTextBox" Width="100px" runat="server" Text='<%# Bind("vFGitem") %>'>
                </asp:TextBox>
                <a href="#" onclick="fglook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="right" style="padding-right:5px"><b>Created Date:</b></td>
                <td><asp:Label ID="vDateTextBox" runat="server" BackColor="turquoise" Text='<%# Bind("vDate","{0:MM/dd/yyyy}") %>'>
                </asp:label></td></tr>
                <tr><td colspan="4">
                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="UpdateButton_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table></fieldset>
            </EditItemTemplate>
            <InsertItemTemplate>
                <fieldset class="shade">
                <table class="shade"><tr><td nowrap align="right" style="padding-right:5px"><b>Attached File: </b></td>
                <td colspan="5">                
                <asp:FileUpload ID="FileUpload1"  Width="300px" runat="server" />
                                 
                  <asp:RegularExpressionValidator id="FileUpLoadValidator" runat="server" ErrorMessage="Invalid File" 
                  ValidationExpression="^(([a-zA-Z]:)|(\\{2}\w+)\$?)(\\(\w[\w].*))(.jpg|.JPG|.gif|.GIF|.txt|.TXT|.doc|.DOC|.dot|.DOT|.xls|.XLS|.pdf|.PDF|.html|.HTML|.csv|.CSV)$" ControlToValidate="FileUpload1">
                  </asp:RegularExpressionValidator>


                
                </td></tr>
                <tr><td nowrap align="right" style="padding-right:5px"><b>Open With:</b></td>
                <td colspan="3"> <asp:DropDownList ID="open_with_dropdownlist" runat="server" Width="120px" SelectedValue='<%# Bind("vOpenWith") %>' DataTextField='<%# Bind("vOpenWith") %>'>
                 <asp:ListItem Text="Word" Value="Word"></asp:ListItem>
                 <asp:ListItem Text="Excel" Value="Excel"></asp:ListItem>
                 <asp:ListItem Text="Acrobat" Value="Acrobat"></asp:ListItem>
                 <asp:ListItem Text="MS Paint" Value="MS Paint"></asp:ListItem>
                 <asp:ListItem Text="Photo Shop" Value="Photo Shop"></asp:ListItem>
                 <asp:ListItem Text="Notepad" Value="Notepad"></asp:ListItem>
                 <asp:ListItem Text="Wordpad" Value="Wordpad"></asp:ListItem>
                 <asp:ListItem Text="Internet Explorer" Value="Internet Explorer"></asp:ListItem>
                 <%--<asp:ListItem Text="Other" Value="Other"></asp:ListItem>--%>
                 </asp:DropDownList></td></tr>
                <tr><td nowrap align="right" style="padding-right:5px"><b>Estimate:</b></td>
                <td><asp:TextBox ID="vEstimateTextBox" Width="100px" runat="server" Text='<%# Bind("vEstimate") %>'>
                </asp:TextBox>
                <a href="#" onclick="estimatelook(); return false"><asp:Image ID="custLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="right" style="padding-right:5px"><b>FG Item#:</b></td>
                <td><asp:TextBox ID="vFGitemTextBox" Width="100px" runat="server" Text='<%# Bind("vFGitem") %>'>
                </asp:TextBox>
                <a href="#" onclick="fglook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
                </td>
                <td nowrap align="right" style="padding-right:5px"><b>Created Date:</b></td>
                <td><asp:Label ID="vDateLabel" runat="server" BackColor="turquoise" >
                </asp:label></td></tr>
                        
                               
                <tr><td colspan="5"> <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="InsertButton_Click"
                    Text="Save">
                </asp:Button>
                <asp:Button ID="InsertCancelButton" runat="server" CausesValidation="False" CssClass="button" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></td></tr></table></fieldset>
            </InsertItemTemplate>
            <ItemTemplate>
                <fieldset class="shade">
                <table class="shade">
                <tr><td align="right" style="padding-right:5px"><b>Attached File: </b></td>
                <td colspan="4">
                <asp:Label ID="vAttFileLabel" runat="server" BackColor="turquoise" Text='<%# Bind("vAttFile") %>'></asp:Label>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Open With:</b></td>
                <td>
                <asp:Label ID="vOpenWithLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vOpenWith") %>'></asp:Label>
                </td></tr>
                <tr><td align="right" style="padding-right:5px"><b>Estimate:</b></td>
                <td><asp:Label ID="vEstimateLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vEstimate") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>FG Item#:</b></td>
                <td><asp:Label ID="vFGitemLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vFGitem") %>'></asp:Label></td>
                <td align="right" style="padding-right:5px"><b>Created Date:</b></td>
                <td><asp:Label ID="vDateLabel" runat="server" Width="100px" BackColor="turquoise" Text='<%# Bind("vDate","{0:MM/dd/yyyy}" ) %>'></asp:Label></td></tr>
                <tr><td><asp:Label ID="vReckeyLabel" Visible="false" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label><br /></td></tr>
                              
                <tr><td colspan="5">
               <asp:Button ID="update_button" Visible="false" runat="server" CausesValidation="true" CommandName="edit" CssClass="button" Text="Update" />
               <asp:Button ID="add_Button"  Visible="false" runat="server" CausesValidation="true" CommandName="new" CssClass="button" Text="Add" />
               <asp:Button ID="delete_Button"  Visible="false" runat="server" CausesValidation="true"  CssClass="button" Text="Delete" OnClick="btn_delete_click"  OnClientClick="return confirm('Are you sure to delete this record')" />
               <asp:Button ID="view_button" runat="server" CssClass="button" Text="View" OnClick="view_button_click" />              
               </td></tr> </table></fieldset>
            </ItemTemplate>
        </asp:FormView>
               
        
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectAttach" TypeName="orderentry">
            <SelectParameters>
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmRecKey" SessionField="top_list_attach_rec_key"
                    Type="String" />
                <asp:Parameter Name="prmAttFile" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmEst"  Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmFgitem"  Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmOpenWith" Type="String" />
                <asp:Parameter Name="prmSerchEst" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
