<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="invspec_list_notes" Codebehind="invspec_list_notes.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Spec Notes</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language="javascript" src="include/date.js"></script>
<script language="javascript" src="include/event.js"></script>
<script language="javascript" src="include/insert.js"></script>
    <script language="javascript"  src="include/CalendarControl.js" > </script>
    <script>
        function datelook()
        {
            var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup(obj)
        {
            document.forms[0].txt_note_date.value=obj;
        }
    </script>
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txt_note_date">
        <div>
            <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD >&nbsp;</TD>
          <TD  nowrap><font size=+0><b>Item for Spec Notes &nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
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
           
            <TABLE id="tblMain" cellSpacing="1" cellPadding="1" align='center' width='95%' border="0">
            
        <TR>
          <TD>
            <table>
                 <tr>
                    <td bgcolor="gray">
                        <asp:ImageButton ID="img_btn_list_notes" runat="server" ImageUrl="~/Images/list notes 1.Jpg" OnClick="img_btn_list_notes_click" />
                        <asp:ImageButton ID="img_btn_view_notes" runat="server" ImageUrl="~/Images/view notes 0.Jpg" OnClick="img_btn_view_notes_click" />
                         </td><td>
                        <%--<asp:label id="Label_fg" runat="server" Text="FG Item:"  Font-Bold="True"></asp:label>&nbsp;&nbsp;
                        <asp:label id="Label_fgitem" runat="server" Width="120px" BackColor="turquoise" Font-Bold="True">&nbsp;</asp:label>--%>
                    </td>
                </tr>
            </table>
             <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">
             <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" width="80%" border="0"  bgcolor=black>
               
              <TR>
                <TD class="shade" align="left"><br>
                  <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		        <tr>
        		    <td nowrap> <asp:Button ID="btnSearch" runat="server" Text="Go" OnClick="btnSearch_Click" CssClass="button" Width="40px" /><br />
                    <br />
                        <asp:Button ID="btn_reset" runat="server" CssClass="button" Text="All" OnClick="btn_reset_Click" Width="40px" />&nbsp;</td>
                       <td>&nbsp;</td>
                    <td nowrap="nowrap"> 
                        Note Date:           
			            <asp:TextBox ID="txt_note_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Width="65px" ></asp:TextBox>
			            
			            <a href="#" onClick="showCalendarControl(txt_note_date); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	            
          	            <td nowrap> Group:
                            <asp:TextBox ID="grp_TextBox" runat="server" Width="65px" ></asp:TextBox>			
                	    </td> 
          	            
           		        <td nowrap> Dept/Spec:
                            <asp:TextBox ID="txt_dept_spec" runat="server" Width="65px" ></asp:TextBox>			
                	    </td>                 
                </tr>
            </table>
                </TD> 
         </TR>
       </table>
       </asp:Panel>
        <asp:GridView ID="GridView1" CssClass="Grid" runat="server" Width="100%" AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataSourceID="ObjectDataSource1">
        <RowStyle CssClass="shade" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True"  />
            <Columns>
                <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectText="" SelectImageUrl="images\sel.gif" />
                <asp:BoundField DataField="vNoteDate" HeaderText="Note Date" SortExpression="vNoteDate" DataFormatString="{0:MM/dd/yyyy}" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteTime" HeaderText="Note Time" SortExpression="vNoteTime" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteTitle" HeaderText="Note Title" SortExpression="vNoteTitle" ItemStyle-Wrap="false" />               
                <asp:BoundField DataField="notegroup" HeaderText="Group" SortExpression="notegroup" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vDeptCode" HeaderText="Spec" SortExpression="vDeptCode" ItemStyle-Wrap="false" />
                <asp:TemplateField  HeaderText="Reckey" Visible="false" >
                    <ItemTemplate>
                    <asp:Label ID="reclabel" runat="server"  Text='<%# Bind("[rec_key]") %>'></asp:Label>
                    </ItemTemplate>
                    </asp:TemplateField>
                
            </Columns>
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White"></HeaderStyle>
        <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="TopViewNoteinvoice" TypeName="voucherpay">
            <SelectParameters>
                <asp:Parameter DefaultValue="GridSelect" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmRecKey" SessionField="invspec_list_notes_reckey"
                    Type="String" />
                <asp:Parameter Name="prmHeader" Type="String" />
                <asp:Parameter Name="prmNoteDate" Type="DateTime" />
                <asp:Parameter Name="prmNoteTime" Type="String" />
                <asp:Parameter Name="prmUserId" Type="String" />
                <asp:Parameter Name="prmViewed" Type="String" />
                <asp:Parameter Name="prmDeptCode" Type="String" />
                <asp:Parameter Name="prmDeptName" Type="String" />
                <asp:Parameter Name="prmForm" Type="Int32" />
                <asp:Parameter Name="prmNoteTitle" Type="String" />
                <asp:Parameter Name="prmNewNoteTitle" Type="String" />
                <asp:Parameter Name="prmNoteText" Type="String" />
                <asp:Parameter Name="prmEstimate" Type="String" />
                <asp:Parameter Name="prmType" Type="String" />
                <asp:Parameter Name="prmGroup" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </TD>
       </TR>
       </TABLE>
    </div>
    </form>
</body>
</html>
