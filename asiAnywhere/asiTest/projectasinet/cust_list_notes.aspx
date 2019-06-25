<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="cust_list_notes" Codebehind="cust_list_notes.aspx.cs" %>

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
    <form id="form1" runat="server">
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
                        
                    </td>
                </tr>
            </table>
         
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
                       <b> Note Date: </b>          
			            <asp:TextBox ID="txt_note_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY"  runat="server" Width="65px" ></asp:TextBox>
			            
			            <a href="#" onClick="showCalendarControl(txt_note_date); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	              <td nowrap><b>Group:</b>
                            <asp:TextBox ID="txt_note_group" runat="server" Width="65px" ></asp:TextBox>			
                	    </td>
           		        <td nowrap> <b>Dept/Spec:</b>
                            <asp:TextBox ID="txt_dept_spec" runat="server" Width="65px" ></asp:TextBox>			
                	    </td>                 
                </tr>
            </table>
                </TD> 
         </TR>
       </table>
       
        <asp:GridView ID="GridView1" CssClass="Grid" runat="server" Width="100%" AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataSourceID="ObjectDataSource1">
        <RowStyle CssClass="shade" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True"  />
            <Columns>
            <asp:CommandField ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText=""
                                ShowSelectButton="True">
                                <ItemStyle Width="10px" />
                            </asp:CommandField>
                <%--<asp:BoundField DataField="vCustDate" HeaderText="Note Date"  SortExpression="vCustDate" />--%>
                <asp:TemplateField HeaderText="Note Date" SortExpression="vCustDate">
                <ItemTemplate>
                <asp:Label ID="vcustdatelabel" runat="server" Text=<%# Bind("vCustDate","{0:MM/dd/yyyy}") %> ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vCustNoteTime" HeaderText="Note Time" SortExpression="vCustNoteTime" />
                <asp:BoundField DataField="vCustNoteTitle" HeaderText="Note Title" SortExpression="vCustNoteTitle" />
                <asp:BoundField DataField="vNoteGroup" HeaderText="Group" SortExpression="vNoteGroup" />               
                <asp:BoundField DataField="vCustCode" HeaderText="Dept" SortExpression="vCustCode" />
                
            </Columns>
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle BackColor="Teal" VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White"></HeaderStyle>
        <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustNotes" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmRecKey" SessionField="ar_inv_cust_rec_key"
                    Type="String" />
                <asp:Parameter Name="prmHeader" Type="String" />
                <asp:Parameter Name="prmNoteDate" Type="String" />
                <asp:Parameter Name="prmNoteTime" Type="String" />
                <asp:Parameter Name="prmUserId" Type="String" />
                <asp:Parameter Name="prmViewed" Type="String" />
                <asp:Parameter Name="prmCode" Type="String" />
                <asp:Parameter Name="prmGroup" Type="String" />
                <asp:Parameter Name="prmNoteTitle" Type="String" />
                <asp:Parameter Name="prmNewNoteTitle" Type="String" />
                <asp:Parameter Name="prmNoteText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </TD>
       </TR>
       </TABLE>
    </div>
    </form>
</body>
</html>
