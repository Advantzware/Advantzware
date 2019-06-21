<%@ Page Language="C#" AutoEventWireup="True" Inherits="quantity_look" Codebehind="quantity_look.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Quantity Selected</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
   
    <form id="form1" runat="server" >
    
    <div>
        <b> Quantitiy is missing please select quantity</b><br />
        <br />        
        <asp:GridView ID="GridView1" AllowPaging="true" PageSize="10" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" OnRowDataBound="GridView1_RowDataBound"
             EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  BackColor="Teal" ForeColor="White"  VerticalAlign="Middle"  HorizontalAlign="Center"></HeaderStyle>
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.QuantityLookup('<%#DataBinder.Eval(Container,"DataItem.vQty")%>','<%#DataBinder.Eval(Container,"DataItem.vPrice")%>','<%#DataBinder.Eval(Container,"DataItem.vUom")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            

                 <asp:BoundField DataField="vQty" HeaderText="Quantity" SortExpression="vQty" />
                <asp:BoundField DataField="vPrice" HeaderText="Price" SortExpression="vPrice" />
                <asp:BoundField DataField="vUom" HeaderText="UOM" SortExpression="vUom" />
               
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="ItemQuantityLook" TypeName="LookUp">
            <SelectParameters>               
                                
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:QueryStringParameter Name="prmQuote" QueryStringField="est" Type="String" />
                <asp:QueryStringParameter Name="prmItem" QueryStringField="item" Type="String" />
                <asp:QueryStringParameter Name="prmCustPart" QueryStringField="custpart" Type="String" />
               
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

