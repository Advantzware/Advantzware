<%@ Page Language="C#" AutoEventWireup="true" Inherits="topformno_lookup" Codebehind="topformno_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Form Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server">
  
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" AllowSorting="True" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		                 <a href="#" onClick="javascript:top.opener.window.TopFormLookUp('<%#DataBinder.Eval(Container,"DataItem.vForm")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                
                <asp:BoundField DataField="vEst" HeaderText="Est#" SortExpression="vEst" />
                <asp:BoundField DataField="vQty" HeaderText="Qty" SortExpression="vQty" />
                <asp:BoundField DataField="vForm" HeaderText="Form" SortExpression="vForm" />
                
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="TopFormNoLook" TypeName="LookUp">
            <SelectParameters>               
                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstNo" SessionField="order_entry_est_no" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

