<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" AutoEventWireup="true" Inherits="inventory" Title="Inventory" Codebehind="inventory.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" CellPadding="4" ForeColor="#333333" Width="962px">
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#EFF3FB" />
        <EditItemTemplate>
            <table>
        <tr>
        <td><b>Item:</b></td>
        <td><b><asp:TextBox ID="ItemTextBox" runat="server" Text='<%# Bind("Item") %>'>
            </asp:TextBox></b></td>
        <td><b>Name:</b></td>
        <td><b><asp:TextBox ID="NameTextBox" runat="server" Text='<%# Bind("Name") %>'>
            </asp:TextBox></b></td>
        <td><b>Description:</b></td>
        <td><b><asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'>
            </asp:TextBox></b></td>
        <td style="width: 2px"><b></b></td>
        <td style="width: 5px"><b></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b>Vendor1: </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="Vendor1TextBox" runat="server" Text='<%# Bind("Vendor1") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>VendItem1:
            <br />
            </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="VendItem1TextBox" runat="server" Text='<%# Bind("VendItem1") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>Stocked:</b></td>
        <td style="height: 4px"><b>
            <asp:CheckBox ID="StockedCheckBox" runat="server" Checked='<%# Bind("Stocked") %>' /></b></td>
        <td style="width: 2px; height: 4px">
            pur-uom:
        </td>
        <td style="width: 5px; height: 4px"><b>
            <asp:TextBox ID="pur_uomTextBox" runat="server" Text='<%# Bind("[pur-uom]") %>'>
            </asp:TextBox></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b>Vendor2: </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="Vendor2TextBox" runat="server" Text='<%# Bind("Vendor2") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b><br />
            VendItem2:
            </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="VendItem2TextBox" runat="server" Text='<%# Bind("VendItem2") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>IsSet:</b></td>
        <td style="height: 4px"><b>
            <asp:CheckBox ID="IsSetCheckBox" runat="server" Checked='<%# Bind("IsSet") %>' /></b></td>
        <td style="width: 2px; height: 4px"><b>alloc:</b></td>
        <td style="width: 5px; height: 4px"><b>
            <asp:CheckBox ID="allocCheckBox" runat="server" Checked='<%# Bind("alloc") %>' /></b></td>
        </tr>
        <tr>
        <td><b>OrdPolicy:
            <br />
            </b></td>
        <td><b>
            <asp:CheckBox ID="OrdPolicyCheckBox" runat="server" Checked='<%# Bind("OrdPolicy") %>' /></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td style="width: 2px"><b></b></td>
        <td style="width: 5px"><b></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b>ord-level: </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="ord_levelTextBox" runat="server" Text='<%# Bind("[ord-level]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="width: 2px; height: 4px"><b>Purchased:</b></td>
        <td style="width: 5px; height: 4px"><b>
            <asp:CheckBox ID="PurchasedCheckBox" runat="server" Checked='<%# Bind("Purchased") %>' /></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b>ord-min:
            <br />
            </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="ord_minTextBox" runat="server" Text='<%# Bind("[ord-min]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b>lead-days:</b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="lead_daysTextBox" runat="server" Text='<%# Bind("[lead-days]") %>'>
            </asp:TextBox></b></td>
        <td style="width: 2px; height: 4px"><b></b></td>
        <td style="width: 5px; height: 4px"><b></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b>ord-max:
            <br />
            </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="ord_maxTextBox" runat="server" Text='<%# Bind("[ord-max]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b>beg-date: </b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="beg_dateTextBox" runat="server" Text='<%# Bind("[beg-date]") %>'>
            </asp:TextBox></b></td>
        <td style="width: 2px; height: 4px"><b></b></td>
        <td style="width: 5px; height: 4px"><b></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b>Beg Bal</b></td>
        <td style="height: 4px"><b>On Hand</b></td>
        <td style="height: 4px"><b>On Order</b></td>
        <td style="height: 4px"><b>Allocated</b></td>
        <td style="height: 4px"><b>qBack</b></td>
        <td style="width: 2px; height: 4px"><b>Qavail</b></td>
        <td style="width: 5px; height: 4px"><b></b></td>
        </tr>
        <tr>
        <td style="height: 4px"><b></b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="beg_balTextBox" runat="server" Text='<%# Bind("[beg-bal]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="q_onhTextBox" runat="server" Text='<%# Bind("[q-onh]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="q_onoTextBox" runat="server" Text='<%# Bind("[q-ono]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="q_allocTextBox" runat="server" Text='<%# Bind("[q-alloc]") %>'>
            </asp:TextBox></b></td>
        <td style="height: 4px"><b>
            <asp:TextBox ID="q_backTextBox" runat="server" Text='<%# Bind("[q-back]") %>'>
            </asp:TextBox></b></td>
        <td style="width: 2px; height: 4px"><b>
            <asp:TextBox ID="q_availTextBox" runat="server" Text='<%# Bind("[q-avail]") %>'>
            </asp:TextBox></b></td>
        <td style="width: 5px; height: 4px"><b></b></td>
        </tr>
        </table>
           <br />
            <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                Text="Update">
            </asp:LinkButton>
            <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </EditItemTemplate>
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        <ItemTemplate>
             <table> 
            <tr>
            <td colspan="2"><b> Item: </b><b> <asp:Label ID="ItemLabel" runat="server" Text='<%# Bind("Item") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></b></td>
            <td style="width: 98px"><b> <asp:Label ID="NameLabel" runat="server" Text='<%# Bind("Name") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="150px"></asp:Label></b></td>
            <td colspan="2"><b> </b><b><asp:Label ID="DscrLabel" runat="server" Text='<%# Bind("Dscr") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="150px"></asp:Label></b></td>
            <td style="width: 80px"><b> </b></td>
            <td style="width: 216px"><b> </b></td>
            </tr>
            <tr>
                <%--<td style="width: 56px"><b>Vendor1: </b></td>
            <td style="width: 96px"><b><asp:Label ID="Vendor1Label" runat="server" Text='<%# Bind("Vendor1") %>'></asp:Label> </b></td>
            <td style="width: 98px"><b>Item No: </b></td>
            <td style="width: 102px"><b><asp:Label ID="VendItem1Label" runat="server" Text='<%# Bind("VendItem1") %>'></asp:Label> </b></td>--%>
            <td style="width: 128px"><b><asp:CheckBox ID="StockedCheckBox" runat="server" Checked='<%# Bind("Stocked") %>'
                Enabled="false" />Stocked: </b></td>
            <td style="width: 80px"><b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Item Is:</b></td>
            <td style="width: 216px"><b>
                <asp:RadioButton ID="RD1" Enabled="false" runat="server" />Purchased
                <asp:CheckBox Visible="false" ID="PurchasedCheckBox" runat="server" Checked='<%# Bind("Purchased") %>'
                Enabled="false" />
            
                <asp:RadioButton ID="RD2" Enabled="false" runat="server" />Manufactured</b></td>
            <td style="width: 12px"></td>
            </tr>
            <tr>
            <%--<td style="width: 56px"><b>Vendor2: </b></td>
            <td style="width: 96px"><b><asp:Label ID="Vendor2Label" runat="server" Text='<%# Bind("Vendor2") %>'></asp:Label> </b></td>
            <td style="width: 98px"><b>Item No: </b></td>
            <td style="width: 102px"><b><asp:Label ID="VendItem2Label" runat="server" Text='<%# Bind("VendItem2") %>'></asp:Label> </b></td>--%>
            <td style="width: 128px"><b><asp:CheckBox ID="IsSetCheckBox" runat="server" Checked='<%# Bind("IsSet") %>' Enabled="false" /> &nbsp;Set Header: </b></td>
            <td style="width: 80px"><b>Set Allocation: </b></td>
            <td style="width: 216px">
            <%--<table><tr>
            <td><b>
                <asp:RadioButton ID="RD3" Enabled="false" runat="server" />Assembled</b></td></tr>
            <tr><td><b>
                <asp:RadioButton ID="RD4" Enabled="false" runat="server" />UnAssembled</b></td></tr>
            <tr><td ><b>
                <asp:RadioButton ID="RD5" Enabled="false" runat="server" />Assembled w/Parts Reciept</b></td>
            </tr></table>--%>
            <asp:RadioButtonList ID="allocRadioButtonList"  SelectedValue ='<%# Bind("alloc") %>' Font-Bold="true" Enabled="false"    runat="server">
                                                         <asp:ListItem value="False"  Text="Assembled"   />
                                                         <asp:ListItem value="True" Text="UnAssembled" />
                                                         <asp:ListItem value="?" Text="Assembled w/Parts Reciept" />
                                                        
                                                     </asp:RadioButtonList>
            
            <td style="width: 12px">
            <b>        
            <asp:Label ID="allocLabel" Visible="false" runat="server" Text='<%# Bind("alloc") %>'  ></asp:Label> </b></td>
            </tr>
            </table>
            <table>
            <tr>
            <td style="width: 81px"><b>Order Policy: </b></td>
            <td style="width: 209px"><b><asp:CheckBox Visible="false" ID="OrdPolicyCheckBox" runat="server" Checked='<%# Bind("OrdPolicy") %>'
                Enabled="false" />
                <asp:RadioButton ID="radioreorder" Enabled="false" runat="server" />Reorder
                <asp:RadioButton ID="radiolotcontrolled" Enabled="false" runat="server" />Lot Controlled</b></td>
            <td style="width: 98px"><b> </b></td>
            <td style="width: 120px"><b> </b></td>
            <td style="width: 128px"><b> </b></td>
            <td style="width: 23px"><b> </b></td>
            <td style="width: 86px"><b> </b></td>
            <td><b> </b></td>
            </tr>
            </table>
            
            <fieldset>
            <table>
            <tr>
            <td colspan="2" rowspan="3"><b> </b><b> </b><b> </b><b> </b><b> </b><b> </b></td>
            <td style="width: 126px; padding-right:5px;" align="right"><b>Reorder Level: </b></td>
            <td style="width: 129px"><b><asp:Label ID="ord_levelLabel" runat="server" Text='<%# Bind("[ord-level]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 17px"><b> </b></td>
            <td style="width: 139px; padding-right:5px;" align="right"><b>Purchased Quantity: </b></td>
            <td><b><asp:Label ID="pur_uomLabel" runat="server" Text='<%# Bind("[pur-uom]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label>  </b></td>
            </tr>
            <tr>
            <td style="width: 126px; padding-right:5px;" align="right"><b>Minimum Order: </b></td>
            <td style="width: 129px"><b><asp:Label ID="ord_minLabel" runat="server" Text='<%# Bind("[ord-min]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 17px"><b> </b></td>
            <td style="width: 139px; padding-right:5px;" align="right"><b>Lead Days: </b></td>
            <td><b><asp:Label ID="lead_daysLabel" runat="server" Text='<%# Bind("[lead-days]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            </tr>
            <tr>
            <td style="width: 126px; padding-right:5px;" align="right"><b>Maximum Order: </b></td>
            <td style="width: 129px"><b><asp:Label ID="ord_maxLabel" runat="server" Text='<%# Bind("[ord-max]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 17px"><b> </b></td>
            <td style="width: 139px; padding-right:5px;" align="right"><b>Begining Date: </b></td>
            <td><b><asp:Label ID="beg_dateLabel" runat="server" Text='<%# Bind("[beg-date]","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            </tr>
            </table>
            </fieldset>
            <br />
            <fieldset>
            <table>
            
            <tr>
            <td style="width: 29px"><b> </b></td>
            <td style="width: 101px" align="center"><b>Begin Balance </b></td>
            <td style="width: 110px" align="center"><b><asp:Button ID="Button7" runat="server" Text="On Hand" Font-Bold="True" Width="100px" OnClick="Button7_Click" /> </b></td>
            <td style="width: 116px" align="center"><b><asp:Button ID="Button8" runat="server" Text="On Order" Font-Bold="True" OnClick="Button8_Click" /> 
                                        <asp:Button ID="Button1" runat="server" Text="Brows PO" Font-Bold="True" OnClick="Button1_Click" /></b></td>
            <td style="width: 87px" align="center"><b><asp:Button ID="Button9" runat="server" Text="Alloc To Orders" Font-Bold="True" OnClick="Button9_Click" /> </b></td>
            <td style="width: 113px" align="center"><b>Back ordered </b></td>
            <td style="width: 86px" align="center"><b>Available </b></td>
            <td><b> </b></td>
            </tr>
            <tr>
            <td style="width: 29px"><b>Qty </b></td>
            <td style="width: 101px" align="center"><b><asp:Label ID="beg_balLabel" runat="server" Text='<%# Bind("[beg-bal]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td style="width: 110px" align="center"><b><asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 116px" align="center"><b><asp:Label ID="q_onoLabel" runat="server" Text='<%# Bind("[q-ono]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 87px" align="center"><b><asp:Label ID="q_allocLabel" runat="server" Text='<%# Bind("[q-alloc]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 113px" align="center"><b><asp:Label ID="q_backLabel" runat="server" Text='<%# Bind("[q-back]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td style="width: 86px" align="center"><b><asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]","{0:###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td><b> </b></td>
            </tr>
            
            </table>
            </fieldset>
            <%--<asp:LinkButton ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit"
                Text="Edit">
            </asp:LinkButton>--%>
            
        </ItemTemplate>
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <InsertItemTemplate>
            Item:
            <asp:TextBox ID="ItemTextBox" runat="server" Text='<%# Bind("Item") %>'>
            </asp:TextBox><br />
            Name:
            <asp:TextBox ID="NameTextBox" runat="server" Text='<%# Bind("Name") %>'>
            </asp:TextBox><br />
            Dscr:
            <asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'>
            </asp:TextBox><br />
            Vendor1:
            <asp:TextBox ID="Vendor1TextBox" runat="server" Text='<%# Bind("Vendor1") %>'>
            </asp:TextBox><br />
            VendItem1:
            <asp:TextBox ID="VendItem1TextBox" runat="server" Text='<%# Bind("VendItem1") %>'>
            </asp:TextBox><br />
            Vendor2:
            <asp:TextBox ID="Vendor2TextBox" runat="server" Text='<%# Bind("Vendor2") %>'>
            </asp:TextBox><br />
            VendItem2:
            <asp:TextBox ID="VendItem2TextBox" runat="server" Text='<%# Bind("VendItem2") %>'>
            </asp:TextBox><br />
            OrdPolicy:
            <asp:CheckBox ID="OrdPolicyCheckBox" runat="server" Checked='<%# Bind("OrdPolicy") %>' /><br />
            Stocked:
            <asp:CheckBox ID="StockedCheckBox" runat="server" Checked='<%# Bind("Stocked") %>' /><br />
            Purchased:
            <asp:CheckBox ID="PurchasedCheckBox" runat="server" Checked='<%# Bind("Purchased") %>' /><br />
            IsSet:
            <asp:CheckBox ID="IsSetCheckBox" runat="server" Checked='<%# Bind("IsSet") %>' /><br />
            alloc:
            <asp:CheckBox ID="allocCheckBox" runat="server" Checked='<%# Bind("alloc") %>' /><br />
            ord-level:
            <asp:TextBox ID="ord_levelTextBox" runat="server" Text='<%# Bind("[ord-level]") %>'>
            </asp:TextBox><br />
            ord-min:
            <asp:TextBox ID="ord_minTextBox" runat="server" Text='<%# Bind("[ord-min]") %>'>
            </asp:TextBox><br />
            ord-max:
            <asp:TextBox ID="ord_maxTextBox" runat="server" Text='<%# Bind("[ord-max]") %>'>
            </asp:TextBox><br />
            pur-uom:
            <asp:TextBox ID="pur_uomTextBox" runat="server" Text='<%# Bind("[pur-uom]") %>'>
            </asp:TextBox><br />
            lead-days:
            <asp:TextBox ID="lead_daysTextBox" runat="server" Text='<%# Bind("[lead-days]") %>'>
            </asp:TextBox><br />
            beg-date:
            <asp:TextBox ID="beg_dateTextBox" runat="server" Text='<%# Bind("[beg-date]") %>'>
            </asp:TextBox><br />
            beg-bal:
            <asp:TextBox ID="beg_balTextBox" runat="server" Text='<%# Bind("[beg-bal]") %>'>
            </asp:TextBox><br />
            q-ono:
            <asp:TextBox ID="q_onoTextBox" runat="server" Text='<%# Bind("[q-ono]") %>'>
            </asp:TextBox><br />
            q-onh:
            <asp:TextBox ID="q_onhTextBox" runat="server" Text='<%# Bind("[q-onh]") %>'>
            </asp:TextBox><br />
            q-alloc:
            <asp:TextBox ID="q_allocTextBox" runat="server" Text='<%# Bind("[q-alloc]") %>'>
            </asp:TextBox><br />
            q-avail:
            <asp:TextBox ID="q_availTextBox" runat="server" Text='<%# Bind("[q-avail]") %>'>
            </asp:TextBox><br />
            q-back:
            <asp:TextBox ID="q_backTextBox" runat="server" Text='<%# Bind("[q-back]") %>'>
            </asp:TextBox><br />
            <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                Text="Insert">
            </asp:LinkButton>
            <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:LinkButton>
        </InsertItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectInventoryItem" TypeName="itemhistory" UpdateMethod="UpdateInventoryItem">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
            <asp:Parameter DefaultValue="" Name="prmOrderNum"  Type="String" />               
            <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmVendor1" Type="String" />
            <asp:Parameter Name="prmVendItem1" Type="String" />
            <asp:Parameter Name="prmVendor2" Type="String" />
            <asp:Parameter Name="prmVendItem2" Type="String" />
            <asp:Parameter Name="prmOrdPolicy" Type="String" />
            <asp:Parameter Name="prmStocked" Type="String" />
            <asp:Parameter Name="prmPurchased" Type="String" />
            <asp:Parameter Name="prmIsSet" Type="String" />
            <asp:Parameter Name="prmalloc" Type="String" />
            <asp:Parameter Name="prmordlevel" Type="String" />
            <asp:Parameter Name="prmordmin" Type="String" />
            <asp:Parameter Name="prmordmax" Type="String" />
            <asp:Parameter Name="prmpuruom" Type="String" />
            <asp:Parameter Name="prmleaddays" Type="String" />
            <asp:Parameter Name="prmbegdate" Type="String" />
            <asp:Parameter Name="prmbegbal" Type="String" />
        </SelectParameters>
        <UpdateParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmOrderNum" Type="String" />
            <asp:Parameter Name="prmItemNum" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmVendor1" Type="String" />
            <asp:Parameter Name="prmVendItem1" Type="String" />
            <asp:Parameter Name="prmVendor2" Type="String" />
            <asp:Parameter Name="prmVendItem2" Type="String" />
            <asp:Parameter Name="prmOrdPolicy" Type="String" />
            <asp:Parameter Name="prmStocked" Type="String" />
            <asp:Parameter Name="prmPurchased" Type="String" />
            <asp:Parameter Name="prmIsSet" Type="String" />
            <asp:Parameter Name="prmalloc" Type="String" />
            <asp:Parameter Name="prmordlevel" Type="String" />
            <asp:Parameter Name="prmordmin" Type="String" />
            <asp:Parameter Name="prmordmax" Type="String" />
            <asp:Parameter Name="prmpuruom" Type="String" />
            <asp:Parameter Name="prmleaddays" Type="String" />
            <asp:Parameter Name="prmbegdate" Type="String" />
            <asp:Parameter Name="prmbegbal" Type="String" />
        </UpdateParameters>
    </asp:ObjectDataSource>
</asp:Content>

