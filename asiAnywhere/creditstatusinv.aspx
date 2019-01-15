<%@ Page Language="C#" AutoEventWireup="true" Inherits="creditstatusinv" Codebehind="creditstatusinv.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Brows Invoice</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
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
    <td><div  id="navigation" style="width:100%">
		<ul nowrap><li >
    <asp:LinkButton ID="lnkbrowsinvoice" runat="server" OnClick="lnkbrowsinvoice_Click">Brws Invoice</asp:LinkButton></li>
    <li ><asp:LinkButton ID="lnkviewinvoice" runat="server" OnClick="lnkviewinvoice_Click">View Invoice</asp:LinkButton></li>
    <li class="selected"><asp:LinkButton ID="lnkcreditstatus" runat="server">Credit Status</asp:LinkButton></li>
    <li ><asp:LinkButton ID="lnkbol" runat="server" OnClick="lnkbol_Click">Bol</asp:LinkButton></li></ul></div>
    </td>
    </tr>
    </table>
    
        <br />
        <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton4" runat="server">List Order</asp:LinkButton>
    <br />
        &nbsp;<asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" CellPadding="4" ForeColor="#333333" Width="746px">
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            <EditItemTemplate>
                vCustNo:
                <asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("vCustNo") %>'>
                </asp:TextBox><br />
                vCustName:
                <asp:TextBox ID="vCustNameTextBox" runat="server" Text='<%# Bind("vCustName") %>'>
                </asp:TextBox><br />
                vCustHibal:
                <asp:TextBox ID="vCustHibalTextBox" runat="server" Text='<%# Bind("vCustHibal") %>'>
                </asp:TextBox><br />
                vCustHibalDate:
                <asp:TextBox ID="vCustHibalDateTextBox" runat="server" Text='<%# Bind("vCustHibalDate") %>'>
                </asp:TextBox><br />
                vCustCrHold:
                <asp:CheckBox ID="vCustCrHoldCheckBox" runat="server" Checked='<%# Bind("vCustCrHold") %>' /><br />
                vCustLpay:
                <asp:TextBox ID="vCustLpayTextBox" runat="server" Text='<%# Bind("vCustLpay") %>'>
                </asp:TextBox><br />
                vCustLpayDate:
                <asp:TextBox ID="vCustLpayDateTextBox" runat="server" Text='<%# Bind("vCustLpayDate") %>'>
                </asp:TextBox><br />
                vCustCrLim:
                <asp:TextBox ID="vCustCrLimTextBox" runat="server" Text='<%# Bind("vCustCrLim") %>'>
                </asp:TextBox><br />
                vAccBal:
                <asp:TextBox ID="vAccBalTextBox" runat="server" Text='<%# Bind("vAccBal") %>'>
                </asp:TextBox><br />
                vCustOrdlim:
                <asp:TextBox ID="vCustOrdlimTextBox" runat="server" Text='<%# Bind("vCustOrdlim") %>'>
                </asp:TextBox><br />
                vCustOrdBal:
                <asp:TextBox ID="vCustOrdBalTextBox" runat="server" Text='<%# Bind("vCustOrdBal") %>'>
                </asp:TextBox><br />
                vOnAccount:
                <asp:TextBox ID="vOnAccountTextBox" runat="server" Text='<%# Bind("vOnAccount") %>'>
                </asp:TextBox><br />
                fi_frst-ord-date:
                <asp:TextBox ID="fi_frst_ord_dateTextBox" runat="server" Text='<%# Bind("fifrstorddate") %>'>
                </asp:TextBox><br />
                fi_last-ord-date:
                <asp:TextBox ID="fi_last_ord_dateTextBox" runat="server" Text='<%# Bind("filastorddate") %>'>
                </asp:TextBox><br />
                bInvDate:
                <asp:TextBox ID="bInvDateTextBox" runat="server" Text='<%# Bind("bInvDate") %>'>
                </asp:TextBox><br />
                bInvNo:
                <asp:TextBox ID="bInvNoTextBox" runat="server" Text='<%# Bind("bInvNo") %>'>
                </asp:TextBox><br />
                ld-inv-amt:
                <asp:TextBox ID="ld_inv_amtTextBox" runat="server" Text='<%# Bind("[ld-inv-amt]") %>'>
                </asp:TextBox><br />
                bBalDue:
                <asp:TextBox ID="bBalDueTextBox" runat="server" Text='<%# Bind("bBalDue") %>'>
                </asp:TextBox><br />
                li-days-old:
                <asp:TextBox ID="li_days_oldTextBox" runat="server" Text='<%# Bind("[li-days-old]") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Update">
                </asp:LinkButton>
                <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </EditItemTemplate>
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <InsertItemTemplate>
                vCustNo:
                <asp:TextBox ID="vCustNoTextBox" runat="server" Text='<%# Bind("vCustNo") %>'>
                </asp:TextBox><br />
                vCustName:
                <asp:TextBox ID="vCustNameTextBox" runat="server" Text='<%# Bind("vCustName") %>'>
                </asp:TextBox><br />
                vCustHibal:
                <asp:TextBox ID="vCustHibalTextBox" runat="server" Text='<%# Bind("vCustHibal") %>'>
                </asp:TextBox><br />
                vCustHibalDate:
                <asp:TextBox ID="vCustHibalDateTextBox" runat="server" Text='<%# Bind("vCustHibalDate") %>'>
                </asp:TextBox><br />
                vCustCrHold:
                <asp:CheckBox ID="vCustCrHoldCheckBox" runat="server" Checked='<%# Bind("vCustCrHold") %>' /><br />
                vCustLpay:
                <asp:TextBox ID="vCustLpayTextBox" runat="server" Text='<%# Bind("vCustLpay") %>'>
                </asp:TextBox><br />
                vCustLpayDate:
                <asp:TextBox ID="vCustLpayDateTextBox" runat="server" Text='<%# Bind("vCustLpayDate") %>'>
                </asp:TextBox><br />
                vCustCrLim:
                <asp:TextBox ID="vCustCrLimTextBox" runat="server" Text='<%# Bind("vCustCrLim") %>'>
                </asp:TextBox><br />
                vAccBal:
                <asp:TextBox ID="vAccBalTextBox" runat="server" Text='<%# Bind("vAccBal") %>'>
                </asp:TextBox><br />
                vCustOrdlim:
                <asp:TextBox ID="vCustOrdlimTextBox" runat="server" Text='<%# Bind("vCustOrdlim") %>'>
                </asp:TextBox><br />
                vCustOrdBal:
                <asp:TextBox ID="vCustOrdBalTextBox" runat="server" Text='<%# Bind("vCustOrdBal") %>'>
                </asp:TextBox><br />
                vOnAccount:
                <asp:TextBox ID="vOnAccountTextBox" runat="server" Text='<%# Bind("vOnAccount") %>'>
                </asp:TextBox><br />
                fi_frst-ord-date:
                <asp:TextBox ID="fi_frst_ord_dateTextBox" runat="server" Text='<%# Bind("fifrstorddate") %>'>
                </asp:TextBox><br />
                fi_last-ord-date:
                <asp:TextBox ID="fi_last_ord_dateTextBox" runat="server" Text='<%# Bind("filastorddate") %>'>
                </asp:TextBox><br />
                bInvDate:
                <asp:TextBox ID="bInvDateTextBox" runat="server" Text='<%# Bind("bInvDate") %>'>
                </asp:TextBox><br />
                bInvNo:
                <asp:TextBox ID="bInvNoTextBox" runat="server" Text='<%# Bind("bInvNo") %>'>
                </asp:TextBox><br />
                ld-inv-amt:
                <asp:TextBox ID="ld_inv_amtTextBox" runat="server" Text='<%# Bind("[ld-inv-amt]") %>'>
                </asp:TextBox><br />
                bBalDue:
                <asp:TextBox ID="bBalDueTextBox" runat="server" Text='<%# Bind("bBalDue") %>'>
                </asp:TextBox><br />
                li-days-old:
                <asp:TextBox ID="li_days_oldTextBox" runat="server" Text='<%# Bind("[li-days-old]") %>'>
                </asp:TextBox><br />
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
            </InsertItemTemplate>
            <ItemTemplate>
            <table>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Cust#:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label1" runat="server" Text='<%# Bind("vCustNo") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
                <td colspan="3">
                    <b>
                <asp:Label ID="Label2" runat="server" Text='<%# Bind("vCustName") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="230px"></asp:Label></b><b></b><b></b></td>
            <td style="width: 160px"><b></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>High Balance:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label3" runat="server" Text='<%# Bind("vCustHibal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>ON:</b></td>
            <td style="width: 111px"><b>
                <asp:Label ID="Label4" runat="server" Text='<%# Bind("vCustHibalDate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td><b>
                </b></td>
            <td style="width: 160px"><b><asp:CheckBox ID="CheckBox1" runat="server" Checked='<%# Bind("vCustCrHold") %>'
                    Enabled="false" />Customer On Hold</b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;" width="80"><b>Last Payment:
                </b></td>
            <td style="width: 100px"><b><asp:Label ID="Label5" runat="server" Text='<%# Bind("vCustLpay","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>ON:</b></td>
            <td style="width: 111px"><b>
                <asp:Label ID="Label6" runat="server" Text='<%# Bind("vCustLpayDate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>Credit Limit:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label7" runat="server" Text='<%# Bind("vCustCrLim","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Balance Due:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label8" runat="server" Text='<%# Bind("vAccBal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;"><b>Oredr Limit:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label9" runat="server" Text='<%# Bind("vCustOrdlim","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Open Order:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label10" runat="server" Text='<%# Bind("vCustOrdBal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;"><b>First Order Date:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label11" runat="server" Text='<%# Bind("fifrstorddate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>On Account:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="vOnAccountLabel" runat="server" Text='<%# Bind("vOnAccount","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;" width="90"><b>Last Order Date:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label12" runat="server" Text='<%# Bind("filastorddate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            </table>
            
               <%-- bInvDate:
                <asp:Label ID="Label13" runat="server" Text='<%# Bind("bInvDate") %>'></asp:Label><br />
                bInvNo:
                <asp:Label ID="Label14" runat="server" Text='<%# Bind("bInvNo") %>'></asp:Label><br />
                ld-inv-amt:
                <asp:Label ID="Label15" runat="server" Text='<%# Bind("[ld-inv-amt]") %>'>
                </asp:Label><br />
                bBalDue:
                <asp:Label ID="Label16" runat="server" Text='<%# Bind("bBalDue") %>'></asp:Label><br />
                li-days-old:
                <asp:Label ID="Label17" runat="server" Text='<%# Bind("[li-days-old]") %>'>
                </asp:Label><br />--%>
            
               
            </ItemTemplate>
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        <br />
         <br />
        <table class="shade" align="left" style="width: 751px">
    <tr>
    <td align="center" style="height: 18px; width: 784px;"><b>Open Invoices</b></td>
    </tr>
    </table>
        <br />
    <br />
        <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
            AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1"
            EmptyDataText="No Record Found" Width="750px">
            <EmptyDataRowStyle BorderColor="#404040" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                HorizontalAlign="Center" VerticalAlign="Middle" />
            <Columns>
                <asp:TemplateField HeaderText="Invoice Date" SortExpression="bInvDate">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("bInvDate") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" />
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("bInvDate","{0:d}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="bInvNo" HeaderText="Invoice#" SortExpression="bInvNo" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:TemplateField HeaderText="Invoice Amt" SortExpression="ld-inv-amt">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("[ld-inv-amt]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("[ld-inv-amt]","{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Balance Due" SortExpression="bBalDue">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("bBalDue") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("bBalDue","{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Days Old" SortExpression="li-days-old">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("[li-days-old]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="right" />
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("[li-days-old]","{0:###,###,##0}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                
                
                
            </Columns>
            <RowStyle CssClass="shade" />
            <SelectedRowStyle CssClass="GridSelected" />
            <HeaderStyle BackColor="#404040" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                VerticalAlign="Middle" Wrap="False" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="CreditStatusInv" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter Name="prmCust" SessionField="vCust" Type="String" />
                
                
            </SelectParameters>
        </asp:ObjectDataSource>
        <br />
   
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>
