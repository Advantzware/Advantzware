<%@ Page Language="C#" AutoEventWireup="true" MasterPageFile="~/MasterPage.master" Inherits="vieworder" Title="View Order" Codebehind="~/vieworder.aspx.cs" %>

<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

    
    <table>
        <tr>
            <td style="height: 26px; width: 325px;">
                <asp:FormView ID="FormView1" runat="server" DataKeyNames="company,ord-no" DataSourceID="ObjectDataSource1" Width="943px" Font-Bold="True" CellPadding="4" ForeColor="#333333">
                    
                    <ItemTemplate>
                        
                        Order#:
                        <asp:Label ID="ord_noLabel" runat="server" Text='<%# Eval("[ord-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>
                        &nbsp; &nbsp;&nbsp; &nbsp;
                        Type:
                        <asp:Label ID="typeLabel" runat="server" Text='<%# Bind("type") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>
                        &nbsp; &nbsp; &nbsp;
                        Estimate:
                        <asp:Label ID="est_noLabel" runat="server" Text='<%# Bind("[est-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>
                        &nbsp; &nbsp; &nbsp;&nbsp;
                        Job#2:
                        <asp:Label ID="job_no2Label" runat="server" Text='<%# Bind("[job-no2]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>
                        &nbsp; &nbsp; &nbsp;&nbsp;
                        Status:
                        <asp:Label ID="statLabel" runat="server" Text='<%# Bind("stat") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>
                        &nbsp; &nbsp; &nbsp;&nbsp;
                        Last User:
                        <asp:Label ID="user_idLabel" runat="server" Text='<%# Bind("[user-id]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>&nbsp;
                        
                    </ItemTemplate>
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>
                <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
                    SelectMethod="Selectvieworder" TypeName="Order">
                    <SelectParameters>
                        <asp:Parameter Name="prmUser" Type="String" />
                        <asp:SessionParameter Name="prmOrderNum" SessionField="vieworder" Type="String" />
                    </SelectParameters>
                </asp:ObjectDataSource>
            </td>
        </tr>
    </table>
    <table>
        <tr>
            <td valign=top style="height: 167px; width: 280px;">
                <asp:FormView ID="FormView2" runat="server" CellPadding="4" DataKeyNames="company,ord-no"
                    DataSourceID="ObjectDataSource1" ForeColor="#333333" Height="169px" Width="312px" Font-Bold="True" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    
                    <ItemTemplate>
                        &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; 
                        
                        Bill To:
                        <asp:Label ID="bill_toLabel" runat="server" Text='<%# Bind("[cust-no]") %>' BackColor="Turquoise" BorderColor="White" Width="183px" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Name:
                        <asp:Label ID="cust_nameLabel" runat="server" Text='<%# Bind("[cust-name]") %>' BorderColor="White" BackColor="Turquoise" Width="182px" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp;
                        <asp:Label ID="addr1Label" runat="server" Text='<%# Bind("addr1") %>' BackColor="Turquoise" BorderColor="White" Width="183px" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; <asp:Label ID="addr2Label" runat="server" Text='<%# Bind("addr2") %>' BackColor="Turquoise" BorderColor="White" Width="183px" BorderStyle="Solid" BorderWidth="1px"></asp:Label>
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp;
                        &nbsp; &nbsp;&nbsp; &nbsp;<asp:Label ID="BillCity" runat="server" BackColor="Turquoise" BorderColor="White"
                            BorderWidth="1px" Text="<%# GenerateBillCityLine(Container.DataItem) %>" Width="183px" BorderStyle="Solid"></asp:Label><br />
                        &nbsp;&nbsp; &nbsp; &nbsp;Contact: <asp:Label ID="contactLabel1" runat="server" Text='<%# Bind("[contact]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="183px"></asp:Label><br />
                        &nbsp; &nbsp;<br />
                        &nbsp; &nbsp;&nbsp; 
                        <br />
                        </ItemTemplate>
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                </asp:FormView>
            </td>
            <td valign=top style="height: 167px; width: 280px;"><asp:FormView ID="FormView4" runat="server" CellPadding="4" DataKeyNames="company,ord-no"
                    DataSourceID="ObjectDataSource1" ForeColor="#333333" Height="169px" Width="312px" Font-Bold="True" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                <EditRowStyle BackColor="#2461BF" />
                <RowStyle BackColor="#EFF3FB" />
                <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                <ItemTemplate>
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        Sold To:
                    <asp:Label ID="sold_toLabel" runat="server" BackColor="Turquoise"
                        BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[sold-id]") %>'
                        Width="183px"></asp:Label><br />
                    &nbsp;&nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;Name:
                    <asp:Label ID="sold_nameLabel" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[sold-name]") %>' Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
                    <asp:Label ID="soldaddr1Label" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[sold-addr1]") %>' Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
                    &nbsp; &nbsp;
                    <asp:Label ID="soldaddr2Label" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[sold-addr2]") %>' Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp;
                    &nbsp;
                    <asp:Label ID="SoldCity" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text="<%# GenerateSoldCityLine(Container.DataItem) %>"
                        Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                    &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;<br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;
                    &nbsp;&nbsp;<br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                </ItemTemplate>
                <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            </asp:FormView>
            </td>
            <td valign=top style="height: 167px; width: 280px;"><asp:FormView ID="FormView5" runat="server" CellPadding="4" DataKeyNames="company,ord-no"
                    DataSourceID="ObjectDataSource1" ForeColor="#333333" Height="169px" Width="312px" Font-Bold="True" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                <EditRowStyle BackColor="#2461BF" />
                <RowStyle BackColor="#EFF3FB" />
                <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                <ItemTemplate>
                    &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; Date:
                    <asp:Label ID="ord_dateLabel1" runat="server"
                        BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"
                        Text='<%# Bind("[ord-date]","{0:MM/dd/yyyy}") %>' Width="183px"></asp:Label><br />
                    &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; Due Date: <asp:Label ID="due_dateLabel1" runat="server" BackColor="Turquoise"
                        BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[due-date]","{0:MM/dd/yyyy}") %>'
                        Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        Last Ship:
                    <asp:Label ID="last_dateLabel1" runat="server" BackColor="Turquoise"
                        BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[last-date]","{0:MM/dd/yyyy}") %>'
                        Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
                        
                        Prod Date:
                    <asp:Label ID="prod_dateLabel1" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[prod-date]","{0:MM/dd/yyyy}") %>'
                        Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Cust PO#:
                    <asp:Label ID="po_noLabel1" runat="server" BackColor="Turquoise" BorderColor="White"
                        BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[po-no]") %>' Width="183px"></asp:Label><br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                    &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;<br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp;
                    &nbsp;&nbsp;<br />
                    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                </ItemTemplate>
                <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            </asp:FormView>
            </td>
        </tr>
    </table>
    <table>
        <tr>
            <td valign=top style="height: 109px; width: 465px;">
                <asp:FormView ID="FormView3" runat="server" DataKeyNames="company,ord-no" DataSourceID="ObjectDataSource1" CellPadding="4" Font-Bold="True" ForeColor="#333333" Height="109px" Width="465px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                    
                    <ItemTemplate>
                        &nbsp; &nbsp; &nbsp;&nbsp;
                        Previous Order# :&nbsp;
                        <asp:Label ID="pord_noLabel" runat="server" Text='<%# Bind("[pord-no]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;Overrun%:
                        &nbsp;<asp:Label ID="over_pctLabel" runat="server" Text='<%# Bind("[over-pct]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;Tax Code:&nbsp;
                        <asp:Label ID="tax_grLabel" runat="server" Text='<%# Bind("[tax-gr]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        Underrun%:&nbsp;
                        <asp:Label ID="under_pctLabel" runat="server" Text='<%# Bind("[under-pct]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;Pay Terms:&nbsp;
                        <asp:Label ID="termsLabel" runat="server" Text='<%# Bind("terms") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><asp:Label ID="terms_dLabel" runat="server" Text='<%# Bind("[terms-d]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="179px"></asp:Label><br />
                    </ItemTemplate>
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>                
            </td>
            <td valign=top style="height: 109px; width: 478px;">
                <asp:FormView ID="FormView6" runat="server" DataKeyNames="company,ord-no" DataSourceID="ObjectDataSource1" CellPadding="4" Font-Bold="True" ForeColor="#333333" Height="109px" Width="474px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                    
                    <ItemTemplate>
                        &nbsp;&nbsp;&nbsp; &nbsp; &nbsp; &nbsp;
                        Freight Charge: <asp:Label ID="freightLabel1" runat="server" Text='<%# Bind("[frt-pay]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><asp:Label ID="frt_pay_dscrLabel" runat="server" Text='<%# Bind("[frt-pay-dscr]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label>&nbsp;<%--<asp:RadioButton ID="RadioButton1" runat="server" Text='<%# Bind("[frt-pay]") %>' />&nbsp;
                        <asp:RadioButton ID="RadioButton2" runat="server" Text='<%# Bind("[frt-pay]") %>'/>&nbsp;
                        <asp:RadioButton ID="RadioButton3" runat="server" Text='<%# Bind("[frt-pay]") %>' />&nbsp;
                        <asp:RadioButton ID="RadioButton4" runat="server" Text='<%# Bind("[frt-pay]") %>' />--%><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp; &nbsp;
                        &nbsp; &nbsp; Carrier:
                        <asp:Label ID="carrierLabel" runat="server" Text='<%# Bind("carrier") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><asp:Label ID="CarrdscrLabel" runat="server" Text='<%# Bind("Carrdscr") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label><br />
                        <br />
                        &nbsp; &nbsp; &nbsp;&nbsp;
                         Freight on Board:
                        <asp:Label ID="fob_codeLabel" runat="server" Text='<%# Bind("[fob-code]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="65px"></asp:Label><asp:Label ID="fob_dscrLabel" runat="server" Text='<%# Bind("[fob-dscr]") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label>
                        <br />
                        
                        <br />
                         </ItemTemplate>
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>
            </td>
        </tr>
    </table>
    <table>
        <tr>
            <td style="height: 1px; width: 400px;" width="465">
                <asp:FormView ID="FormView7" runat="server" DataKeyNames="company,ord-no" DataSourceID="ObjectDataSource1" CellPadding="4" Font-Bold="True" ForeColor="#333333" Height="89px" Width="465px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                    
                    <ItemTemplate>
                        &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
                        
                        Sales Rep:
                        <asp:Label ID="sman1Label" runat="server" Text='<%# Bind("sman1") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="24px"></asp:Label><asp:Label ID="sname1Label" runat="server" Text='<%# Bind("sname1") %>' BackColor="Turquoise" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Width="100px"></asp:Label><br />
                        <br />
                        <br />
                        <br />
                         </ItemTemplate>
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>
            </td>
            <td style="height: 1px; width: 400px;">
                <asp:FormView ID="FormView8" runat="server" CellPadding="4" DataKeyNames="company,ord-no"
                    DataSourceID="ObjectDataSource1" Font-Bold="True" ForeColor="#333333" Height="89px"
                    Width="475px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px">
                    <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                    <EditRowStyle BackColor="#2461BF" />
                    <RowStyle BackColor="#EFF3FB" />
                    <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                    <ItemTemplate>
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        Payment Type: <asp:Label ID="cc_typeLabel" runat="server" BackColor="Turquoise" BorderColor="White"
                            BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[cc-type]") %>' Width="121px"></asp:Label>
                        &nbsp; &nbsp; 
                        Expire:
                        <asp:Label ID="cc_expirationLabel" runat="server" BackColor="Turquoise" BorderColor="White"
                            BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[cc-expiration]","{0:MM/dd/yyyy}") %>' Width="121px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; Account #:
                        <asp:Label ID="cc_numLabel" runat="server" BackColor="Turquoise" BorderColor="White"
                            BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[cc-num]") %>' Width="120px"></asp:Label><br />
                        &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                        &nbsp; &nbsp; Ref #:
                        <asp:Label ID="cc_authLabel" runat="server" BackColor="Turquoise" BorderColor="White"
                            BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("[cc-auth]") %>' Width="121px"></asp:Label><br />
                        <br />
                    </ItemTemplate>
                    <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                </asp:FormView>
            </td>
        </tr>
    </table>
    <br />


</asp:Content>