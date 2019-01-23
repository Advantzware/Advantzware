<%@ Page Language="C#" AutoEventWireup="True" Inherits="viewinvoice" Codebehind="ViewInv.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">


<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>view invoice</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <style type="text/css">
        .style1
        {
            text-align: right;
            width: 163px;
        }
        .style3
        {
            width: 105px;
        }
        .style5
        {
        }
        .style6
        {
        }
        .style8
        {
            height: 18px;
        }
        .style9
        {
            width: 105px;
            height: 18px;
        }
        .style12
        {
            width: 8px;
        }
        .style13
        {
            height: 18px;
            width: 8px;
        }
        .style14
        {
            width: 163px;
        }
        .style15
        {
            height: 18px;
            width: 163px;
        }
        .style18
        {
            width: 45px;
        }
        .style19
        {
            height: 18px;
            width: 45px;
        }
        .style20
        {
            height: 18px;
            width: 233px;
        }
        .style21
        {
            width: 233px;
        }
    </style>
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    
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
   </script>
   
</head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    
    <div>
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Invoice&nbsp;:<asp:Label ID="lbl_page" runat="server"></asp:Label></b></font></TD>
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
    <tr style="background-color:Gray;">
    <td>
    <div  id="navigation" style="width:100%">
		<ul nowrap><li >
    <asp:LinkButton ID="lnkbrowsinvoice" runat="server" OnClick="lnkbrowsinvoice_Click" >Brws Invoice</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnkviewinvoice" runat="server" OnClick="lnkviewinvoice_Click" >View Invoice</asp:LinkButton></li>
     <li><asp:LinkButton ID="lnkcreditstatus" runat="server" OnClick="lnkcreditstatus_Click" >Credit Status</asp:LinkButton></li>
    <li><asp:LinkButton ID="lnkbol" runat="server" OnClick="lnkbol_Click">Bol</asp:LinkButton></li>
    <li><asp:LinkButton ID="PrintInvoice" runat="server" OnClick="PrintInvoice_Click">Print Invoice</asp:LinkButton></li></ul></div>
    </td></tr>
    </table> 
    
    
    
        
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton4" runat="server">List Order</asp:LinkButton>
    <br />
        <br />
            <asp:Label ID="warning" runat="server" Text="No Record Selected. Please Select a Record" Font-Bold="True" Font-Size="Large" ForeColor="Red"></asp:Label>
        <asp:FormView ID="FormView1" runat="server" CellPadding="4" DataSourceID="ObjectDataSource1"
            ForeColor="#333333" Width="702px">
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            <RowStyle BackColor="#EFF3FB" />
            <ItemTemplate>
            <table style="width: 758px">
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Cust#:</b></td>
            <td class="style3"><b>
                <asp:Label ID="cust_noLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[cust-no]") %>' Width="100px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
            <td class="style5" colspan="3"><b>
                <asp:Label ID="cust_nameLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[cust-name]") %>' Width="190px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
            <td align="Right" style="padding-Right:5px;" class="style14"><b>Invoice#:</b></td>
            <td class="style18"><b>
                <asp:Label ID="inv_noLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[inv-no]") %>' Width="60px" style="text-align: right" 
                    BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;" class="style8"><b>Ship To:</b></td>
            <td class="style9"><b>
                <asp:Label ID="ship_idLabel" runat="server" Text='<%# Bind("[ship-id]") %>' 
                    BackColor="Turquoise" Width="100px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td class="style20"><b></b></td>
                <td class="style8">
                    &nbsp;</td>
            <td class="style8"><b></b></td>
            <td align="Right" style="padding-Right:5px;" class="style15"><b>Invoice Amount:</b></td>
            <td class="style19"><b>
                <asp:Label ID="grossLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("gross" ,"{0:###,###,##0.00##}") %>' Width="60px" 
                    style="text-align: right" BorderColor="Black" BorderStyle="Solid" 
                    BorderWidth="1px"></asp:Label>
                </b></td>
                <td class="style13">
                    &nbsp;</td>
                <td class="style8">
                </td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Carrier:</b></td>
            <td class="style3"><b>
                <asp:Label ID="carrierLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("carrier") %>' Width="80px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
            <td align="Right" style="padding-Right:5px;" class="style21"></td>
                <td align="Right" class="style6" style="padding-Right:5px;">
                    &nbsp;</td>
            <td>&nbsp;</td>
            <td align="Right" style="padding-Right:5px;" class="style14"><b>Order#:</b></td>
            <td class="style18"><b>
                <asp:Label ID="ord_noLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[ord-no]") %>' Width="60px" style="text-align: right" 
                    BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Tax:</b></td>
            <td class="style3"><b>
                <asp:Label ID="tax_codeLabel" runat="server" Text='<%# Bind("[tax-code]") %>' 
                    BackColor="Turquoise" Width="100px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td class="style21"><b></b></td>
                <td class="style6">
                    &nbsp;</td>
            <td><b></b></td>
            <td class="style1"><b>Freight:</b></td>
            <td class="style18"><b>
                <asp:Label ID="freightLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("freight" ,"{0:###,###,##0.00##}") %>' Width="60px" 
                    style="text-align: right" BorderColor="Black" BorderStyle="Solid" 
                    BorderWidth="1px"></asp:Label>
                </b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Terms:</b></td>
            <td class="style3"><b> 
                <asp:Label ID="termsLabel" runat="server" Text='<%# Bind("terms") %>' 
                    BackColor="Turquoise" Width="100px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td class="style6" colspan="3"><b></b><b>
                <asp:Label ID="terms_dLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[terms-d]") %>' BorderColor="Black" BorderStyle="Solid" 
                    BorderWidth="1px" Width="100px"></asp:Label>
                </b></td>
            <td class="style1"><b style="text-align: right">Tax:</b></td>
            <td class="style18"><b>
                <asp:Label ID="tax_amtLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[tax-amt]" ,"{0:###,###,##0.00##}") %>' Width="60px" 
                    style="text-align: right" BorderColor="Black" BorderStyle="Solid" 
                    BorderWidth="1px"></asp:Label>
                </b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Invoice Date:</b></td>
            <td class="style3"><b>
                <asp:Label ID="inv_dateLabel" runat="server" 
                    Text='<%# Bind("[inv-date]","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" 
                    Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td align="Right" style="padding-Right:5px;" class="style21">&nbsp;</td>
                <td align="Right" class="style6" style="padding-Right:5px;">
                    &nbsp;</td>
            <td>&nbsp;</td>
            <td align="Right" style="padding-Right:5px;" class="style14">
                <b style="text-align: right">&nbsp;Discount:</b></td>
            <td class="style18"><b>
                <asp:Label ID="disc_takenLabel" runat="server" 
                    Text='<%# Bind("[disc-taken]" ,"{0:###,###,##0.00##}") %>' 
                    BackColor="Turquoise" Width="60px" style="text-align: right" 
                    BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>Due Date:</b></td>
            <td class="style3"><b>
                <asp:Label ID="due_dateLabel" runat="server" 
                    Text='<%# Bind("[due-date]","{0:MM/dd/yyyy}") %>' BackColor="Turquoise" 
                    Width="100px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td align="Right" style="padding-Right:5px;" class="style21">&nbsp;</td>
                <td align="Right" class="style6" style="padding-Right:5px;">
                    &nbsp;</td>
            <td>&nbsp;</td>
            <td align="Right" style="padding-Right:5px;" class="style14"><b>Amount Paid:</b></td>
            <td class="style18"><b>
                <asp:Label ID="paidLabel" runat="server" 
                    Text='<%# Bind("paid" ,"{0:###,###,##0.00##}") %>' BackColor="Turquoise" 
                    Width="60px" style="text-align: right" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            <tr>
            <td align="Right" style="padding-Right:5px;"><b>PO#:</b></td>
            <td class="style3"><b>
                <asp:Label ID="po_noLabel" runat="server" BackColor="Turquoise" 
                    Text='<%# Bind("[po-no]") %>' Width="80px" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                </b></td>
            <td align="Right" style="padding-Right:5px;" class="style21">&nbsp;</td>
                <td align="Right" class="style6" style="padding-Right:5px;">
                    &nbsp;</td>
            <td>&nbsp;</td>
            <td align="Right" style="padding-Right:5px;" class="style14"><b>Balance Due:</b></td>
            <td class="style18"><b>
                <asp:Label ID="dueLabel" runat="server" 
                    Text='<%# Bind("due" ,"{0:###,###,##0.00##}") %>' BackColor="Turquoise" 
                    Width="60px" style="text-align: right" BorderColor="Black" 
                    BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
                <td class="style12">
                    &nbsp;</td>
                <td>
                    &nbsp;</td>
            </tr>
            </table>
              
                <%--LINE:
                <asp:Label ID="LINELabel" runat="server" Text='<%# Bind("LINE") %>'></asp:Label><br />
                actnum:
                <asp:Label ID="actnumLabel" runat="server" Text='<%# Bind("actnum") %>'></asp:Label><br />
                act-dscr:
                <asp:Label ID="act_dscrLabel" runat="server" Text='<%# Bind("[act-dscr]") %>'></asp:Label><br />
                i-name:
                <asp:Label ID="i_nameLabel" runat="server" Text='<%# Bind("[i-name]") %>'></asp:Label><br />
                i-dscr:
                <asp:Label ID="i_dscrLabel" runat="server" Text='<%# Bind("[i-dscr]") %>'></asp:Label><br />
                inv-qty:
                <asp:Label ID="inv_qtyLabel" runat="server" Text='<%# Bind("[inv-qty]") %>'></asp:Label><br />
                cons-uom:
                <asp:Label ID="cons_uomLabel" runat="server" Text='<%# Bind("[cons-uom]") %>'></asp:Label><br />
                sf-sht:
                <asp:Label ID="sf_shtLabel" runat="server" Text='<%# Bind("[sf-sht]") %>'></asp:Label><br />
                unit-pr:
                <asp:Label ID="unit_prLabel" runat="server" Text='<%# Bind("[unit-pr]") %>'></asp:Label><br />
                pr-qty-uom:
                <asp:Label ID="pr_qty_uomLabel" runat="server" Text='<%# Bind("[pr-qty-uom]") %>'>
                </asp:Label><br />
                amt:
                <asp:Label ID="amtLabel" runat="server" Text='<%# Bind("amt") %>'></asp:Label><br />
                amt-msf:
                <asp:Label ID="amt_msfLabel" runat="server" Text='<%# Bind("[amt-msf]") %>'></asp:Label><br />--%>
            </ItemTemplate>
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        <br />
        <br />
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" 
            OnSelectedIndexChanged="GridView1_SelectedIndexChanged" AllowPaging="True" 
            AllowSorting="True" BorderStyle="Dotted" CssClass="Grid" 
            EmptyDataText="No Record Found" Width="770px">
            <Columns>
            <asp:TemplateField HeaderText="Select">
            <ItemTemplate>
            
            <input type="radio" name="selectradio" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.vRowid"))) %>' />
            </ItemTemplate>
            <ControlStyle Width="10px" />
                   <HeaderStyle ForeColor="White" />
                <ItemStyle Width="10px" />
            </asp:TemplateField>
                <asp:BoundField DataField="LINE" HeaderText="LINE" SortExpression="LINE" />
                <asp:BoundField DataField="actnum" HeaderText="Account Number" SortExpression="actnum" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="act-dscr" HeaderText="Account Description" SortExpression="act-dscr" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="inv-no" HeaderText="Invoice No" SortExpression="inv-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-name" HeaderText="Item Name" SortExpression="i-name" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-dscr" HeaderText="Item Description" SortExpression="i-dscr" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:TemplateField HeaderText="Invoice Qty" SortExpression="inv-qty">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("[inv-qty]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("[inv-qty]" ,"{0:###,###,##0.0}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                
                
                <asp:TemplateField HeaderText="Price" SortExpression="unit-pr">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("[unit-pr]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("[unit-pr]","{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="pr-qty-uom" HeaderText="UOM" SortExpression="pr-qty-uom" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:TemplateField HeaderText="Amount" SortExpression="amt">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("amt") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("amt" ,"{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                
            </Columns>
            <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade" />
            <SelectedRowStyle BackColor="Yellow" />
            <HeaderStyle   ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
                VerticalAlign="Middle" />
            <AlternatingRowStyle CssClass="GdridItemOdd" />
        </asp:GridView>
        <script type="text/javascript" language="javascript">select_first();</script>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="selectViewInv" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="brwsinvoice" Type="String" />
                 <asp:SessionParameter SessionField="browsinvoice_list_invoice"  Name="prmInv" Type="int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
        </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    
        
        
    </form>
</body>
</html>
