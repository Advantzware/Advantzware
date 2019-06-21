<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="item_crdit_list" Codebehind="item_stat_crditlist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Customer Crdit History Inquiry</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <script language =javascript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  >   
        
      <div>
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          <TD width=30>&nbsp;</TD>
          <TD align=center nowrap><font size=+0><b>Customer Crdit History Inquiry&nbsp;</b></font></TD>
          <TD >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          
            &nbsp;&nbsp;Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;           
            
            &nbsp;<b>Company:</b>&nbsp;  <asp:Label ID="labelcompany" runat="server" ></asp:Label> &nbsp;&nbsp;
           </TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
            <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" CellPadding="4" ForeColor="#333333" >
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            
            <ItemTemplate>
            <fieldset>
            <table>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Cust#:</b></td>
            <td ><b>
                <asp:Label ID="Label1" runat="server" Text='<%# Bind("vCustNo") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td colspan="3"><b>
                <asp:Label ID="Label2" runat="server" Text='<%# Bind("vCustName") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>High Balance:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label3" runat="server" Text='<%# Bind("vCustHibal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>ON:</b></td>
            <td style="width: 111px"><b>
                <asp:Label ID="Label4" runat="server" Text='<%# Bind("vCustHibalDate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"> 
                </asp:Label></b></td>
            <td><b>
                </b></td>
            <td style="width: 160px"><b><asp:CheckBox ID="CheckBox1" runat="server" Checked='<%# Bind("vCustCrHold") %>'
                    Enabled="false" />Customer On Hold</b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;" width="80"><b>Last Payment:
                </b></td>
            <td style="width: 100px"><b><asp:Label ID="Label5" runat="server" Text='<%# Bind("vCustLpay","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>ON:</b></td>
            <td style="width: 111px"><b>
                <asp:Label ID="Label6" runat="server" Text='<%# Bind("vCustLpayDate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                </asp:Label></b></td>
            <td align="right" style="padding-right:5px;"><b>Credit Limit:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label7" runat="server" Text='<%# Bind("vCustCrLim","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"> 
                </asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Balance Due:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label8" runat="server" Text='<%# Bind("vAccBal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;"><b>Oredr Limit:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label9" runat="server" Text='<%# Bind("vCustOrdlim","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"> 
                </asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>Open Order:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="Label10" runat="server" Text='<%# Bind("vCustOrdBal","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                </asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;"><b>First Order Date:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label11" runat="server" Text='<%# Bind("fifrstorddate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                </asp:Label></b></td>
            </tr>
            <tr>
            <td align="right" style="padding-right:5px;"><b>On Account:</b></td>
            <td style="width: 100px"><b>
                <asp:Label ID="vOnAccountLabel" runat="server" Text='<%# Bind("vOnAccount","{0:###,###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                </asp:Label></b></td>
            <td><b></b></td>
            <td style="width: 111px"><b></b></td>
            <td align="right" style="padding-right:5px;" width="90"><b>Last Order Date:</b></td>
            <td style="width: 160px"><b>
                <asp:Label ID="Label12" runat="server" Text='<%# Bind("filastorddate","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                </asp:Label></b></td>
            </tr>
            </table>
            </fieldset>
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
          
      <asp:Panel ID="grid_panel" ScrollBars="Vertical" runat="server" Height="200px" Width="560px"> 
     
      
        <asp:GridView ID="GridView1" runat="server"  AllowSorting="True" Height="200px" Width="530px"
            AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" DataSourceID="ObjectDataSource1"
            EmptyDataText="No Record Found" >
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
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("[ld-inv-amt]","{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Balance Due" SortExpression="bBalDue">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox3" runat="server" Text='<%# Bind("bBalDue") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label3" runat="server" Text='<%# Bind("bBalDue","{0:###,###,##0.00}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Days Old" SortExpression="li-days-old">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("[li-days-old]") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemStyle Wrap="False" HorizontalAlign="Right" />
                    <ItemTemplate>
                        <asp:Label ID="Label4" runat="server" Text='<%# Bind("[li-days-old]","{0:###,###,##0}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                
                
                
            </Columns>
            <RowStyle CssClass="shade" />
            <SelectedRowStyle CssClass="GridSelected" />
            <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                VerticalAlign="Middle" Wrap="False" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
        </asp:GridView>
       
        </asp:Panel>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="CreditStatusInv" TypeName="browsinvoice">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" /> 
                <asp:SessionParameter Name="prmCust" SessionField="order_entry_cust_no" Type="String" />
                
                
            </SelectParameters>
        </asp:ObjectDataSource>
        <br />  
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

