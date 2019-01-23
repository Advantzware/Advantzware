<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="fold_estimate_probeit" Codebehind="fold_estimate_probeit.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimate Analysis Per Thousand</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <%--<link href="include/tree.css" rel="stylesheet" type="text/css" />--%>
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
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

    function displayQtyScrDiv() {
        document.getElementById("qtyscr").style.display = "block";
        document.getElementById("btndiv").style.display = "none";
    }
               

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
        <%--<hd:header id="Header1" runat="server"></hd:header>--%>
      <div>               
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
           <asp:HiddenField ID="HiddenField_Vendor" runat="server" />
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
                                                                                                   
                      
    
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server"  DataKeyNames="vPartNo"
              AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static"  EmptyDataText="No Records Found" Width="100%" 
              BorderStyle="Dotted" CssClass="Grid">            
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />        
            
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>
                
                <asp:BoundField DataField="vCustNo" HeaderText="Cust. #" 
                    SortExpression="vCustNo" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vPartNo" HeaderText="Part Number" 
                    SortExpression="vPartNo" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vBlQty" HeaderText="Requested Qty" 
                    SortExpression="vBlQty" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vYldQty" HeaderText="Yield Qty" 
                    SortExpression="vYldQty" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vFactCost" HeaderText="Fact Cost/M" 
                    SortExpression="vFactCost" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vFullCost" HeaderText="Full Cost/M" 
                    SortExpression="vFullCost" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vSellPrice" HeaderText="Sell Price/M" 
                    SortExpression="vSellPrice" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />
                <asp:BoundField DataField="vYRprice" HeaderText="On" 
                    SortExpression="vYRprice" ItemStyle-Wrap="false" HeaderStyle-Wrap="false" />                                
                
            </Columns>
            
            </asp:GridView>
            
            <br /><br />
            
            <table class="shade"><tr><td>
            <asp:FormView ID="FormView1"  runat="server" DataSourceID="ObjectDataSource2" OnDataBound="FormView1_DataBound">            
              <EditItemTemplate>
                <table class="shade">
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Cust. #:</b></td>
                        <td><asp:TextBox ID="vCustNoTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vCustNo") %>'></asp:TextBox></td>
                        <td align="right" style="padding-left:5px"><b>Part Number:</b></td>
                        <td><asp:TextBox ID="vPartNoTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vPartNo") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Requested Qty:</b></td>
                        <td><asp:TextBox ID="vBlQtyTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vBlQty") %>'></asp:TextBox></td>
                        <td align="right" style="padding-left:5px"><b>Yield Qty:</b></td>
                        <td><asp:TextBox ID="vYldQtyTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vYldQty") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Fact Cost/M:</b></td>
                        <td><asp:TextBox ID="vFactCostTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vFactCost") %>'></asp:TextBox></td>
                        <td align="right" style="padding-left:5px"><b>Full Cost/M:</b></td>
                        <td><asp:TextBox ID="vFullCostTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vFullCost") %>'></asp:TextBox></td>
                    </tr>
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Sell Price/M:</b></td>
                        <td><asp:TextBox ID="vSellPriceTextBox" runat="server" Width="100px" Text='<%# Bind("vSellPrice") %>'></asp:TextBox></td>
                        <td align="right" style="padding-left:5px"><b>On:</b></td>
                        <td><asp:TextBox ID="vYRpriceTextBox" Enabled="false" runat="server" Width="100px" Text='<%# Bind("vYRprice") %>'></asp:TextBox></td>
                        
                        <asp:TextBox ID="TextBox1" runat="server" Visible="false" Width="100px" Text='<%# Bind("vSellPrice") %>'></asp:TextBox>
                    </tr>                                                                                           
                    <tr>
                        <td colspan="2">             
                            <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" OnClick="updateItem"  Text="Update" />  
                            &nbsp;<asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel" />             
                        </td>
                    </tr>
                </table>         
                  
                  
              </EditItemTemplate>            
              
              <ItemTemplate>
                <table class="shade">
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Cust. #:</b></td>
                        <td nowrap><asp:Label ID="vCustNoLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vCustNo") %>'></asp:Label></td>
                        <td align="right" style="padding-left:5px"><b>Part Number:</b></td>
                        <td nowrap><asp:Label ID="vPartNoLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vPartNo") %>'></asp:Label></td>                        
                    </tr>
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Requested Qty:</b></td>
                        <td nowrap><asp:Label ID="vBlQtyLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vBlQty") %>'></asp:Label></td>
                        <td align="right" style="padding-left:5px"><b>Yield Qty:</b></td>
                        <td nowrap><asp:Label ID="vYldQtyLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vYldQty") %>'></asp:Label></td>                        
                    </tr>
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Fact Cost/M:</b></td>
                        <td nowrap><asp:Label ID="vFactCostLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vFactCost") %>'></asp:Label></td>
                        <td align="right" style="padding-left:5px"><b>Full Cost/M:</b></td>
                        <td nowrap><asp:Label ID="vFullCostLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vFullCost") %>'></asp:Label></td>                        
                    </tr>
                    
                    <tr>
                        <td align="right" style="padding-left:5px"><b>Sell Price/M:</b></td>
                        <td nowrap><asp:Label ID="vSellPriceLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vSellPrice") %>'></asp:Label></td>
                        <td align="right" style="padding-left:5px"><b>On:</b></td>
                        <td nowrap><asp:Label ID="vYRpriceLabel" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("vYRprice") %>'></asp:Label></td>                        
                    </tr>
                                         
                <tr><td colspan="1">
                <asp:Button ID="updateButton" runat="server" CausesValidation="True" CssClass="button" CommandName="edit" Text="Update" >
                </asp:Button>   
                &nbsp;<asp:Button ID="InsertCancelButton" runat="server" CssClass="button"  CausesValidation="False" OnClientClick="javascript:window.opener.location.href='fold_print.aspx'; self.close()" Text="Close" />             
                </td></tr></table>
                 
              </ItemTemplate>
              
          </asp:FormView>  
          
          </td>
        <td> </td>
        </tr> </table>
                     
        <%--&nbsp;<input type="button" Visible="false" name="Cancel" runat="server" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close()"  /> --%>
        
              
    </div>
    
    
    
    
    
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectFoldEstimateProbeit" 
              TypeName="Corrugated">
              <SelectParameters>
                  <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" DefaultValue="Select" />                  
                  <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
                  <asp:Parameter DefaultValue="1" Name="prmLine" Type="Int32" />
                  <asp:Parameter Name="prmSellPrice" Type="Decimal" />
                  <asp:Parameter Name="prmPartNo" Type="String" />
              </SelectParameters>
    </asp:ObjectDataSource>
    <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectFoldEstimateProbeit" 
              TypeName="Corrugated">
              <SelectParameters>
                  <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" DefaultValue="GetProbeitEst" />                  
                  <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
                  <asp:Parameter DefaultValue="1" Name="prmLine" Type="Int32" />
                  <asp:Parameter DefaultValue="" Name="prmSellPrice" Type="Decimal" />
                  <asp:ControlParameter ControlID="GridView1" DefaultValue="vPartNo" Name="prmPartNo" 
                      PropertyName="SelectedValue" Type="String" />
              </SelectParameters>
    </asp:ObjectDataSource>
          
          
    </form>
</body>
</html>
