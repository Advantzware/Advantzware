<%@ Page Language="C#" AutoEventWireup="true" Inherits="price_lookup" Codebehind="price_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Price Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" AllowSorting="True">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.PriceLookup('<%#DataBinder.Eval(Container,"DataItem.tt-price")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                 <asp:BoundField DataField="tt-selldate" HtmlEncode="false" DataFormatString="{0:MM/dd/yyyy}" HeaderText="Sale Date" SortExpression="tt-selldate" />
                <asp:BoundField DataField="tt-cost" HeaderText="Cost" SortExpression="tt-cost" />
                <asp:BoundField DataField="tt-price" HeaderText="Sell" SortExpression="tt-price" />
                <%--<asp:BoundField DataField="tt-uom-price" HeaderText="tt-uom-price" SortExpression="tt-uom-price" />--%>
                <asp:BoundField DataField="tt-qty" HeaderText="Qty" SortExpression="tt-qty" />
              <asp:BoundField DataField="tt-prof" HeaderText="Profit" SortExpression="tt-prof" />
               
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="PriceLook" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" DefaultValue="select" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:QueryStringParameter DefaultValue="" Name="prmItem" QueryStringField="item" Type="String" />
                <asp:QueryStringParameter DefaultValue="" Name="prmUom" QueryStringField="uom" Type="String" />
                <asp:SessionParameter Name="prmCust" SessionField="order_entry_cust_no" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

