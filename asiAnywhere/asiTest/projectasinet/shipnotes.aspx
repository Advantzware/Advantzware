<%@ Page Language="C#" MasterPageFile="~/MasterPage.master" AutoEventWireup="true" Inherits="shipnotesaspx" Title="Ship Notes" Codebehind="shipnotes.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
    <br />
    <%--<a href="order_inquiry.aspx"><span style="color: #0000ff; text-decoration: underline">
        List Orders</span></a>--%>
    
        
        <asp:FormView ID="FormView3" runat="server" CellPadding="4"
            DataSourceID="ObjectDataSource2" ForeColor="#333333" Width="900px">
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            
            <ItemTemplate>
            <table>
            <tr><td><b>Item#:</b></td>
            <td style="width: 169px"><b><asp:Label ID="i_noLabel" runat="server" Text='<%# Bind("[i-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="90px"></asp:Label></b></td>
            <td><b>FG Item:</b></td>
            <td style="width: 149px"><b><asp:Label ID="i_nameLabel" runat="server" Text='<%# Bind("[i-name]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="100px"></asp:Label></b></td>
            <td><b>P.O. Number:</b></td>
            <td style="width: 159px"><b><asp:Label ID="po_noLabel" runat="server" Text='<%# Bind("[po-no]") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            <td><b>Rel Date:</b></td>
            <td style="width: 168px"><b><asp:Label ID="rel_dateLabel" runat="server" Text='<%# Bind("[rel-date]","{0:d}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td></tr>
               
               
                </table>
            </ItemTemplate>
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
        </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" DataObjectTypeName="ShipNote"
        OldValuesParameterFormatString="original_{0}" SelectMethod="SelectShipNotes"
        TypeName="shipnotes" UpdateMethod="Updateshipnotes">
        <SelectParameters>
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmOrderNum" Type="String" />
            <asp:Parameter Name="prmItemNum" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    
   

<fieldset style="width:900px; background-color:#EFF3FB;">
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Width="905px" Height="37px">
        
        <ItemTemplate>
           <b>FG Item Qtys</b> 
            <br />
           <table>
           <tr>
           <td align="center" style="width: 150px"><b> On Hand</b></td>
           <td align="center" style="width: 150px"><b> On Order</b></td>
           <td align="center" style="width: 150px"><b>Allocated</b></td>
           <td align="center" style="width: 150px"><b> Backorder</b></td>
           <td align="center" style="width: 150px"><b>Available</b></td>
           <td align="center" style="width: 150px"><b>Reorder</b></td>
           </tr>
            <tr>
           <td align="center" style="width: 150px"><b><asp:Label ID="q_onhLabel" runat="server" Text='<%# Bind("[q-onh]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           <td align="center" style="width: 150px"><b><asp:Label ID="q_onoLabel" runat="server" Text='<%# Bind("[q-ono]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           <td align="center" style="width: 150px"><b><asp:Label ID="q_allocLabel" runat="server" Text='<%# Bind("[q-alloc]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           <td align="center" style="width: 150px"><b><asp:Label ID="q_backLabel" runat="server" Text='<%# Bind("[q-back]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           <td align="center" style="width: 150px"><b><asp:Label ID="q_availLabel" runat="server" Text='<%# Bind("[q-avail]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           <td align="center" style="width: 150px"><b><asp:Label ID="ord_levelLabel" runat="server" Text='<%# Bind("[ord-level]","{0:###,###,##0}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
           </tr>
           </table>
         
        </ItemTemplate>
    </asp:FormView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectviewSel" TypeName="Order"  >
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
        <asp:SessionParameter Name="prmOrderNum" SessionField="shipnotes" Type="String" />
            <asp:SessionParameter  Name="prmItemNum" SessionField="line" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
        
   
    </fieldset>
    
    <fieldset style="width:892px">
        <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2" CellPadding="4" ForeColor="#333333" Width="896px" 
       OnDataBound="OnDataBound_FormView2"  OnItemUpdated="FormView2_ItemUpdated">
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            <EditItemTemplate>
            <table class="shade" width="922px">
            <tr><td>
            <table class="shade">
            <tr>
           <%-- <td><b> SNote1:</b></td>--%>
            <td style="width: 257px"><b><asp:TextBox ID="SNote1TextBox" runat="server" Text='<%# Bind("SNote1") %>' Width="564px"></asp:TextBox></b></td>
            </tr>
            <tr>
            
            <td style="width: 257px"><b><asp:TextBox ID="SNote2TextBox" runat="server" Text='<%# Bind("SNote2") %>' Width="563px"></asp:TextBox></b></td>
            </tr>
            <tr>
            
            <td style="width: 257px"><b><asp:TextBox ID="SNote3TextBox" runat="server" Text='<%# Bind("SNote3") %>' Width="562px"></asp:TextBox></b></td>
            </tr>
            <tr>
            
            <td style="width: 257px"><b><asp:TextBox ID="SNote4TextBox" runat="server" Text='<%# Bind("SNote4") %>' Width="563px"></asp:TextBox></b></td>
            </tr>
            </table>
            <table class="shade">
            <tr>
            <td style="width: 55px"></td>
            <td><b> <asp:Button ID="UpdateButton" CssClass="button" runat="server" CausesValidation="True" 
                OnClick="UpdateBotton_Click"    Text="Save">
                </asp:Button></b></td>
            
            <td><b><asp:Button ID="UpdateCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:Button></b></td>
            </tr>
            </table>
              </td>
               </tr>
             </table>            
                
            </EditItemTemplate>
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <ItemTemplate>
                <strong>&nbsp;Notes:</strong><br />
            <table style="width: 317px">
            <tr>
            
            <td style="width: 399px"><b><asp:Label ID="SNote1Label" runat="server" Text='<%# Bind("SNote1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="500px"></asp:Label></b></td>
            </tr>
            <tr>
            
            <td style="width: 399px"><b><asp:Label ID="SNote2Label" runat="server" Text='<%# Bind("SNote2") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="500px"></asp:Label></b></td>
            </tr>
            <tr>
            
            <td style="width: 399px"><b><asp:Label ID="SNote3Label" runat="server" Text='<%# Bind("SNote3") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="500px"></asp:Label></b></td>
            </tr>
            <tr>
           
            <td style="width: 399px"><b><asp:Label ID="SNote4Label" runat="server" Text='<%# Bind("SNote4") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="500px"></asp:Label></b></td>
            </tr>
            </table>
            <table>
            <tr>
            
            <td><b>
            <asp:Button ID="UpdateItemButton" runat="server" CssClass="button" CommandName="Edit" Text="Update" />
            </b></td>
            
            </tr>
            </table>
             
                
            </ItemTemplate>
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectShipNotes" TypeName="shipnotes" DataObjectTypeName="ShipNoteData"  >
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="shipnotes"
                    Type="String" />
                    <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="item"
                    Type="String" />
                    <asp:Parameter DefaultValue="Select" Name="prmNote1" Type="String" />
                    <asp:Parameter DefaultValue="Select" Name="prmNote2" Type="String" />
                    <asp:Parameter DefaultValue="Select" Name="prmNote3" Type="String" />
                    <asp:Parameter DefaultValue="Select" Name="prmNote4" Type="String" />
                    
            </SelectParameters>
        </asp:ObjectDataSource>
    </fieldset>


</asp:Content>

