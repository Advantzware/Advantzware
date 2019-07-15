<%@ Page Language="C#" AutoEventWireup="true"  Inherits="ar_bol_inv" Codebehind="Arbol.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>BOL</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    
    <script language="javascript" type="text/javascript">
    function select_first()
    {
        var table=document.getElementById('<%=GridView1.ClientID %>');
        if (table.rows && table.rows.length>0)
        {       
            //Here you need to use 1 for first row. because 0 means its refers the header row in table.
            var firstrow = table.rows[1];  
            var cell = firstrow.cells[0];  
            var chkChecked = cell.firstChild;

            while(chkChecked) 
            {
                if(chkChecked.tagName=="INPUT" && chkChecked.type=="radio") 
                {
                    chkChecked.checked = 'checked';
                    break;
                }
                chkChecked = chkChecked.nextSibling;
            }
        }
    }
    function select_first2()
    {
        var table=document.getElementById('<%=GridView2.ClientID %>');
        if (table.rows && table.rows.length>0)
        {       
            //Here you need to use 1 for first row. because 0 means its refers the header row in table.
            var firstrow = table.rows[1];  
            var cell = firstrow.cells[0];  
            var chkChecked = cell.firstChild;

            while(chkChecked) 
            {
                if(chkChecked.tagName=="INPUT" && chkChecked.type=="radio") 
                {
                    chkChecked.checked = 'checked';
                    break;
                }
                chkChecked = chkChecked.nextSibling;
            }
        }
    }
    
    function orderhelp()
    {
        var NewWindow = window.open("ar_inv_help.aspx","OrderHelpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function printrep()
    {
        var NewWindow = window.open("topbtnorderreport.aspx","OrderReport","width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function printackrep()
    {
        var NewWindow = window.open("topprintorderack_report.aspx","OrderAcknowledgementReport","width=800,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function ordernotes()
    {
        var NewWindow = window.open("toporder_list_notes.aspx","OrderListNotes","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function printspec()
    {
        var NewWindow = window.open("cust_list_notes.aspx","OrderListNotes","width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function topattach()
    {
        var NewWindow = window.open("top_attach_list.aspx","Attachment","width=650,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");    
    }
    function select_col()
    {
        var NewWindow = window.open("show_avail_order_entry.aspx","SelectColumnWindow","width=300,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
   </script>
    
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <table width="100%"><tr><td>
    <table align="left" border="1" width="75%">
                <tr class="topheadcolor">
                    
                    
                     <td nowrap width="1px";>
                        <a href="#" onClick="select_col(); return false"><asp:Image ID="Image5" Width="35px" runat="server" ImageUrl="~/Images/moveCol.ico" /></a>
                    </td>                  
                   <%--<td nowrap width="25px";>
                        <a href="#" onClick="printackrep(); return false"><asp:Image ID="Image2" Width="35px" runat="server" ImageUrl="~/Images/printAck.bmp" /></a>
                    </td>
                    <td nowrap width="25px";>
                        <a href="#" onClick="printrep(); return false"><asp:Image ID="Image1" Width="35px" runat="server" ImageUrl="~/Images/print-u.bmp" /></a>
                        </td>                    
                   
                      <td nowrap width="25px";>
                        <a href="#" onClick="topattach(); return false"><asp:Image ID="Image4" Width="35px" ToolTip="Attachment" runat="server" ImageUrl="~/Images/clip.ico" /></a>                        
                        </td>
                      <td nowrap width="25px";>
                        <a href="#" onClick="ordernotes(); return false"><asp:Image ID="img_btn_notes" Width="35px" ToolTip="Notes" runat="server" ImageUrl="~/Images/edit.ico" /></a>                        
                        </td>--%>
                         <td nowrap width="25px";>
                        <a href="#" onClick="printspec(); return false"><asp:Image ID="Image6" Width="35px" runat="server" ImageUrl="~/Images/dict.ico" /></a>
                    </td>
                        <td nowrap width="25px";>
                        <a href="#" onClick="orderhelp(); return false"><asp:Image ID="img_help" Width="35px" ToolTip="Help" runat="server" ImageUrl="~/Images/help.ico" /></a>
                        </td>
                      <td nowrap width="25px";>
                        <asp:ImageButton ID="img_btn_exit" runat="server" Width="35px" ImageUrl="~/Images/exit-au.bmp" ToolTip="LogOut" OnClick="img_btn_exit_click" />
                    </td>
                     <td nowrap> &nbsp;</td>
                </tr>
      </table>
     </td>
      </tr>
      <tr>
      <td>
    <div>
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Order Inquiry&nbsp;:<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click">Back to menu</asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            
            &nbsp;&nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
            &nbsp;&nbsp;&nbsp;
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>  
  </div>
    
    <div>
    
    <table>
    <tr style="background-color:Gray">
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li >
    <asp:LinkButton ID="lnkbrowsinvoice" runat="server" OnClick="lnkbrowsinvoice_Click">List Invoices</asp:LinkButton></li>
    <li ><asp:LinkButton ID="lnkviewinvoice" runat="server" OnClick="lnkviewinvoice_Click">View Invoice</asp:LinkButton></li>
    <li id="liCreditStatus" runat="server" > <asp:LinkButton ID="lnkcreditstatus" runat="server" OnClick="lnkcreditstatus_Click">Credit Status</asp:LinkButton></li>
    <li id="liBol" runat="server" class="selected"><asp:LinkButton ID="lnkbol" runat="server">Bol</asp:LinkButton></li>
    <li id="liPrintBol" runat="server"><asp:LinkButton ID="ImageButton1" runat="server" onclick="bolButton">Print Bol</asp:LinkButton>    </li>
    <li id="liPrintSigned" runat="server"><asp:LinkButton ID="ImageButton2" runat="server" onclick="SignbolButton">Signed Bol</asp:LinkButton></li></ul></div>
    </td>
    </tr></table>
    
        
    <table>
        <tr>
            <td style="display:none;">
                <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
            AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1"
            EmptyDataText="No Record Found" Width="100%">
            <EmptyDataRowStyle BorderColor="#404040" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                HorizontalAlign="Center" VerticalAlign="Middle" />
            <Columns>
            <asp:TemplateField HeaderText="Select">
            <ItemTemplate>
            
            <input type="radio" name="selectradio" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.vRowid"))) %>' />
            </ItemTemplate>
            <ControlStyle Width="10px" />
                   <HeaderStyle ForeColor="White" />
                <ItemStyle Width="10px" />
            </asp:TemplateField>
                <asp:BoundField DataField="vBolNo" HeaderText="Bol#" SortExpression="vBolNo" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolIno" HeaderText="Item#" SortExpression="vBolIno" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolTag" HeaderText="Tag" SortExpression="vBolTag" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolJobNo" HeaderText="Job#" SortExpression="vBolJobNo" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:TemplateField SortExpression="vBolinNo2">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("vBolinNo2") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" />
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("vBolinNo2","{0:###,#00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vBolLoc" HeaderText="Whse" SortExpression="vBolLoc" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolLocbin" HeaderText="Location" SortExpression="vBolLocbin" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolCases" HeaderText="Units" SortExpression="vBolCases" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolQtycase" HeaderText="Qty/Unit" SortExpression="vBolQtycase" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="vBolPartial" HeaderText="Partial" SortExpression="vBolPartial" >
                <ItemStyle Wrap="False" />
                
                </asp:BoundField>
                <asp:BoundField DataField="vTotal" HeaderText="Total" SortExpression="vTotal" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                
                <asp:TemplateField HeaderText="Weight" SortExpression="vBolweight">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("vBolweight") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("vBolweight","{0:###,###,##0}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vBolScode" HeaderText="Ship Status" SortExpression="vBolScode" >
                <ItemStyle Wrap="False" />
                </asp:BoundField>
                
                
                
            </Columns>
            <RowStyle CssClass="shade" />
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                VerticalAlign="Middle" Wrap="False" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <script type="text/javascript" language="javascript">select_first();</script>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="BolInv" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />   
                <asp:SessionParameter Name="prmOrderNum" SessionField="brwsinvoice" Type="String" />
                <asp:SessionParameter Name="prmBol" SessionField="vBol" Type="String" />
               
                
            </SelectParameters>
        </asp:ObjectDataSource>
            </td>
        </tr>
        <tr>
            <td>
                <asp:GridView ID="GridView2" runat="server" OnRowCreated="GridView2_RowCreated" AllowPaging="true" 
                        AllowSorting="True" OnSorting="GridView2_Sorting" OnRowDataBound="GridView2_RowDataBound" OnPageIndexChanging="GridView2_PageIndexChanging" 
                        EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" >
                        <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" Wrap="false" />
                        <AlternatingRowStyle CssClass="GridItemOdd" Wrap="False" />            
                        <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
                        <HeaderStyle  ForeColor="White" CssClass="headcolor" Height="40px"  VerticalAlign="Middle"  HorizontalAlign="Center" Wrap="False"></HeaderStyle>
                        <RowStyle CssClass="shade" Wrap="False"  />
                            <Columns>
                                <asp:TemplateField HeaderText="Select">
                                    <ItemTemplate>            
                                        <input type="radio" name="selectradio" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.vRowid"))) %>' />
                                    </ItemTemplate>
                                    <ControlStyle Width="10px" />
                                    <HeaderStyle ForeColor="White" />
                                    <ItemStyle Width="10px" />
                                </asp:TemplateField>            
                            </Columns>
                     </asp:GridView>
                     <script type="text/javascript" language="javascript">select_first2();</script>
            </td>
        </tr>
    </table>
        
        <br />
   
    </div>
    </td></tr></table>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
