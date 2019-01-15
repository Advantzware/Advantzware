<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="top_attach_po" Codebehind="top_attach_po.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Attachment</title>
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
    <form id="form1" runat="server" defaultfocus="txt_att_date" defaultbutton="btnSearch">
        <div>
            <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD >&nbsp;</TD>
          <TD  nowrap><font size=+0><b>Attachment &nbsp;<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          
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
		                <ul nowrap> <li  >
                        <asp:LinkButton ID="img_btn_list_attach" runat="server"  OnClick="img_btn_list_attach_click" >Browse Attach</asp:LinkButton></li>
                        <li><asp:LinkButton ID="img_btn_view_attach" runat="server"  OnClick="img_btn_view_attach_click" >View Attach</asp:LinkButton></li></ul></div>
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
                        Created Date:           
			            <asp:TextBox ID="txt_att_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Width="65px" ></asp:TextBox>
			            
			            <a href="#" onClick="showCalendarControl(txt_att_date); return false"><asp:Image ID="OrderLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          	   
           		        <td nowrap> Attach File #:
                            <asp:TextBox ID="txt_att_est" runat="server" Width="65px" ></asp:TextBox>			
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
                <asp:CommandField ShowSelectButton="true" ButtonType="image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                <ItemStyle Width="10px" />
                </asp:CommandField>
                <asp:BoundField DataField="vAttFile" HeaderText="Attached File" SortExpression="vAttFile" />
                <asp:BoundField DataField="vPoNo" HeaderText="PO #" SortExpression="vPoNo" />
                
                <asp:BoundField DataField="vDate" Visible="false" HeaderText="Created Date" SortExpression="vDate" />
                <asp:TemplateField HeaderText="Created Date" SortExpression="vDate">
                <ItemTemplate >
                <asp:Label ID="date_label" runat="server" Text='<%# Bind("vDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField  HeaderText="reckey" Visible="false">
                <ItemTemplate>
                <asp:Label runat="server" ID="rec_key_label" Text= '<%# Bind("vReckey") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
            </Columns>
          <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
        <HeaderStyle  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False" ForeColor="White" CssClass="headcolor"></HeaderStyle>
        <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectAttachpo" TypeName="browspo">
            <SelectParameters>
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter SessionField="order_rec_key" DefaultValue="" Name="prmRecKey" Type="String" />
                <asp:Parameter Name="prmAttFile" Type="String" />
                <asp:SessionParameter Name="prmPoNo" SessionField="pur_ord_po" Type="String" />
                <asp:SessionParameter Name="prmFgitem" SessionField="item" Type="String" />
                <asp:Parameter Name="prmDate" Type="String" />
                <asp:Parameter Name="prmOpenWith" Type="String" />
                <asp:Parameter Name ="prmSerchEst" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </TD>
       </TR>
       </TABLE>
    </div>
    </form>
</body>
</html>
