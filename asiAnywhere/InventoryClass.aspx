<%@ Page Language="C#" AutoEventWireup="true" Inherits="Inventory_lookup" Codebehind="InventoryClass.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Inventory Class Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  
  
  </tr>
  </table>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowSorting="true" AllowPaging="true" PageSize="10" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static"  EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.InventoryLookup('<%#DataBinder.Eval(Container,"DataItem.vClass")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            

                 <asp:BoundField DataField="vClass" HeaderText="Class" SortExpression="vClass" />
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectInventoryClass" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

