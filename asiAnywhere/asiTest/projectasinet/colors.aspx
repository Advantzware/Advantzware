<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" AutoEventWireup="true" Inherits="colorsitem" Title="Colors" Codebehind="colors.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
<br />
   <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
     <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
    <br />
  
    <table><tr><td style="width: 600px" valign="top" class="shade">
    <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
        AutoGenerateColumns="False" DataSourceID="ObjectDataSource1" CssClass="Grid" EmptyDataText="No  Record Found" Width="100%" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataKeyNames="rm-i-no" >
        <Columns>
            <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" />
            <asp:BoundField DataField="press-type" HeaderText="P" SortExpression="press-type" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="rm-i-no" HeaderText="RM Item" SortExpression="rm-i-no" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Dscr" HeaderText="Description" SortExpression="Dscr" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="pass" HeaderText="pass" SortExpression="pass" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="in-out" HeaderText="In-Out" SortExpression="in-out" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="cover" HeaderText="Coverage" SortExpression="cover" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
            <asp:BoundField DataField="Occurs" HeaderText="Occurs" SortExpression="Occurs" >
                <ItemStyle Wrap="False" />
            </asp:BoundField>
        </Columns>
        <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
            HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        
        <HeaderStyle  ForeColor="White" CssClass="headcolor" HorizontalAlign="Center"
            VerticalAlign="Middle" Wrap="False" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <SelectedRowStyle BackColor="Yellow" />
    </asp:GridView>
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectColor" TypeName="Colors">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter DefaultValue="" Name="prmOrderNum"  Type="String" />
            <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="item_list_item" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>
    </td>
    <td style="width: 148px" valign="top" class="shade">
    <table style="vertical-align:top">
    <tr>
    <td style="height: 87px">
       
        <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource2" CellPadding="4" ForeColor="#333333" Height="38px" Width="434px">
            <EditItemTemplate>
            <table><tr><td><b>rm-no:</b></td>
            <td><b><asp:TextBox ID="rm_noTextBox" runat="server" Text='<%# Bind("[rm-no]") %>'>
                </asp:TextBox><br /></b></td>
            <td><b>press:</b></td>
            <td><b><asp:TextBox ID="pressTextBox" runat="server" Text='<%# Bind("press") %>'>
                </asp:TextBox><br /></b></td></tr>
                <tr><td><b>Dscr:</b></td>
                <td><b><asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'>
                </asp:TextBox><br />
              </b></td>
              </tr>            
                <tr><td><b>  pass:</b></td>
                <td><b></b><asp:TextBox ID="passTextBox" runat="server" Text='<%# Bind("pass") %>'>
                </asp:TextBox><br /></td></tr>
                <tr><td><b>in-out:</b></td>
                <td><b><asp:TextBox ID="in_outTextBox" runat="server" Text='<%# Bind("[in-out]") %>'>
                </asp:TextBox><br /></b></td></tr>
               <tr><td><b>cover:</b></td>
               <td><b><asp:TextBox ID="coverTextBox" runat="server" Text='<%# Bind("cover") %>'>
                </asp:TextBox><br /></b></td>
               <td><b> Occurs:</b></td>
               <td><b><asp:TextBox ID="OccursTextBox" runat="server" Text='<%# Bind("Occurs") %>'>
                </asp:TextBox><br /></b></td></tr> 
                
                <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                    Text="Update">
                </asp:LinkButton>
                <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
                </table>
            </EditItemTemplate>
            <InsertItemTemplate>
            <table>
            <tr><td><b>rm-no:</b></td>
            <td><b><asp:TextBox ID="rm_noTextBox" runat="server" Text='<%# Bind("[rm-no]") %>'>
                </asp:TextBox><br /></b></td>
            <td><b>press:</b></td>
            <td><b><asp:TextBox ID="pressTextBox" runat="server" Text='<%# Bind("press") %>'>
                </asp:TextBox><br /></b></td></tr>
                <tr><td><b> Dscr:</b></td>
                <td><b></b><asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'>
                </asp:TextBox><br /></td></tr>
                
                
                 <tr><td><b>pass:</b></td>
                <td><b> <asp:TextBox ID="passTextBox" runat="server" Text='<%# Bind("pass") %>'>
                </asp:TextBox><br /></b></td></tr>
               
                <tr><td><b> in-out:</b></td>
                <td><b><asp:TextBox ID="in_outTextBox" runat="server" Text='<%# Bind("[in-out]") %>'>
                </asp:TextBox><br /></b></td></tr>
                <tr><td><b>cover:</b></td>
                <td><b> <asp:TextBox ID="coverTextBox" runat="server" Text='<%# Bind("cover") %>'>
                </asp:TextBox><br /></b></td>
                <td><b>   Occurs:</b></td>
                <td><b> <asp:TextBox ID="OccursTextBox" runat="server" Text='<%# Bind("Occurs") %>'>
                </asp:TextBox><br /></b></td></tr>
               
               
                
                
               
             
               
                <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                    Text="Insert">
                </asp:LinkButton>
                <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                    Text="Cancel">
                </asp:LinkButton>
                </table>
            </InsertItemTemplate>
            <ItemTemplate>
            <fieldset>
            <table>
            <tr><td align="right"><b> RM Item#:</b></td>
            <td><b><asp:Label ID="rm_noLabel" runat="server" Text='<%# Bind("[rm-no]") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td>
            <td align="right"><b>Press:</b></td>
            <td><b> <asp:Label ID="pressLabel" runat="server" Text='<%# Bind("press") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td></tr>
             <tr><td align="right"><b> Dscr:</b></td>
             <td><b><asp:Label ID="DscrLabel" runat="server" Text='<%# Bind("Dscr") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td></tr>   
              <tr><td align="right"><b> pass:</b></td>
              <td><b><asp:Label ID="passLabel" runat="server" Text='<%# Bind("pass") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td></tr>
              <tr><td colspan="2"><b>
                  <asp:RadioButtonList ID="RadioButtonList1" SelectedValue='<%# Bind("[in-out]") %>'  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="2" Font-Bold ="true" runat="server">
                  <asp:ListItem Text="Inside" Value="In"></asp:ListItem>
                  <asp:ListItem Text="Outside" Value="Out"></asp:ListItem>
                  </asp:RadioButtonList>
                  </b></td></tr>
             
              <asp:Label ID="in_outLabel" Visible="false"  runat="server" Text='<%# Bind("[in-out]") %>'></asp:Label><tr><td align="right"><b>cover:</b></td>
              <td><b><asp:Label ID="coverLabel" runat="server" Text='<%# Bind("cover") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td>
              <td><b>  Occurs:</b></td>
              <td><b>  <asp:Label ID="OccursLabel" runat="server" Text='<%# Bind("Occurs") %>' BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px"></asp:Label><br /></b></td></tr>  
               
              
              
            </table>
           </fieldset>
            </ItemTemplate>
            <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
            <EditRowStyle BackColor="#2461BF" />
            <RowStyle BackColor="#EFF3FB" />
            <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
            <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
            OnSelecting="ObjectDataSource2_Selecting1" SelectMethod="SelectColSel" TypeName="Colors">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="item_list_item" Type="String" />
                <asp:Parameter Name="prmRawNum" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </td>
    
    </tr>
    <tr>
    <td style="width: 26px; height: 161px;">
        <asp:GridView ID="GridView2" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource3"
            Style="position: static" Width="434px">
            <Columns>
                <asp:BoundField DataField="unit" HeaderText="Unit" SortExpression="unit" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-ps1" HeaderText="PS" SortExpression="i-ps1" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-code1" HeaderText="Code" SortExpression="i-code1" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-dscr1" HeaderText="Description" SortExpression="i-dscr1" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="i-per1" HeaderText="%" SortExpression="i-per1" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectColor" TypeName="Colors">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                <asp:Parameter DefaultValue="" Name="prmOrderNum"  Type="String" />
                <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </td>
    
    </tr>
    <tr>
    <td>
    
        <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource4">
            
            
            <ItemTemplate>
                <table>
                <tr>
                <td><b></b></td>
                <td><b></b></td>
                <td><b></b></td>
                <td><b></b></td>
                </tr>
                </table>
                <b>Inks:</b>
                <asp:Label ID="i_ps1Label" runat="server" Text='<%# Bind("[i-ps1]") %>'></asp:Label>&nbsp;&nbsp;&nbsp;
                <b>Passes:</b>
                <asp:Label ID="passLabel" runat="server" Text='<%# Bind("pass") %>'></asp:Label>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
               
               <b> Coatings:</b>
                <asp:Label ID="i_code1Label" runat="server" Text='<%# Bind("[i-code1]") %>'></asp:Label>&nbsp;&nbsp;&nbsp;
                
                <b>Passes:</b>
                <asp:Label ID="i_dscr1Label" runat="server" Text='<%# Bind("[i-dscr1]") %>'></asp:Label>&nbsp;&nbsp;&nbsp;
                
            </ItemTemplate>
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource4" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectColor" TypeName="Colors">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmOrderNum"  Type="String" />
                <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </td>
    </tr>
    </table>
    </td>
    </tr></table>
</asp:Content>

