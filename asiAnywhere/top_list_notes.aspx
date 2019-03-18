<%@ Page Language="C#" AutoEventWireup="true" Inherits="top_list_notes" Codebehind="top_list_notes.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>List Notes</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
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
          <TD  nowrap><font size=+0><b>List Notes &nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
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
                    <td bgcolor="gray"><div  id="navigation" style="width:100%">
		                <ul nowrap> <li class="selected">
                        <asp:LinkButton ID="img_btn_list_notes" runat="server"  OnClick="img_btn_list_notes_click" >List Notes</asp:LinkButton></li>
                        <li><asp:LinkButton ID="img_btn_view_notes" runat="server" OnClick="img_btn_view_notes_click" >View Notes</asp:LinkButton></li></ul></div>
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
                        Note Date:           
			            <asp:TextBox ID="txt_note_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Width="65px" ></asp:TextBox>
			            
			            <a href="#" onClick="showCalendarControl(txt_note_date); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	   
           		        <td nowrap> Dept/Spec:
                            <asp:TextBox ID="txt_dept_spec" runat="server" Width="65px" ></asp:TextBox>			
                	    </td>                 
                </tr>
            </table>
                </TD> 
         </TR>
       </table>
       
        <asp:GridView ID="GridView1" CssClass="Grid" Width="80%" runat="server" AutoGenerateColumns="False" AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataSourceID="ObjectDataSource1">
        <RowStyle CssClass="shade" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True"  />
            <Columns>
                 <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectText="" SelectImageUrl="images\sel.gif" >
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vNoteDate" HeaderText="Note Date" SortExpression="vNoteDate" HtmlEncode="false" DataFormatString="{0:MM/dd/yyyy}" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteTime" HeaderText="Note Time" SortExpression="vNoteTime" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteTitle" HeaderText="Note Title" SortExpression="vNoteTitle" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vUserId" HeaderText="User Id" SortExpression="vUserId" ItemStyle-Wrap="false" />
               <%-- <asp:BoundField DataField="vNoteCode" HeaderText="Dept" SortExpression="vNoteCode" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteFrmNo" HeaderText="Form" SortExpression="vNoteFrmNo" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="vNoteType" HeaderText="Type" SortExpression="vNoteType" ItemStyle-Wrap="false" />--%>
            </Columns>
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White" CssClass="headcolor"></HeaderStyle>
        <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="ListTopOrderNotes" TypeName="orderentry">
            <SelectParameters>
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmRecKey" SessionField="top_list_notes_rec_key"
                    Type="String" />
                <asp:Parameter Name="prmHeader" Type="String" />
                <asp:Parameter Name="prmDept" Type="String" />
                <asp:Parameter Name="prmDate" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </TD>
       </TR>
       </TABLE>
    </div>
    </form>
</body>
</html>
