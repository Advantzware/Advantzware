<%@ Page Language="C#" AutoEventWireup="true" Inherits="usag_look_order" Codebehind="usagorder_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer Order Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                    <asp:ListItem Value="Order">Order</asp:ListItem>  
                                          
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                                       
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AutoGenerateColumns="False" AllowSorting="True" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.UsagOrderLookup('<%#DataBinder.Eval(Container,"DataItem.OrderNo")%>', '<%#DataBinder.Eval(Container,"DataItem.LineNo")%>', '<%#DataBinder.Eval(Container,"DataItem.PartNo")%>', '<%#DataBinder.Eval(Container,"DataItem.ItemNo")%>', '<%#DataBinder.Eval(Container,"DataItem.usagQty")%>', '<%#DataBinder.Eval(Container,"DataItem.Job")%>', '<%#DataBinder.Eval(Container,"DataItem.Job2")%>', '<%#DataBinder.Eval(Container,"DataItem.Price")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="OrderNo" HeaderText="Suppliers Order#" SortExpression="OrderNo" />
                <asp:BoundField DataField="LineNo" HeaderText="Line#" SortExpression="LineNo" />
                <asp:BoundField DataField="PartNo" HeaderText="Customer Part#" SortExpression="PartNo" />
                <asp:BoundField DataField="ItemNo" HeaderText="Supppliers FG Item" SortExpression="ItemNo" />
                <asp:BoundField DataField="usagQty" HeaderText="Quantity" SortExpression="usagQty" />
                <%--<asp:BoundField DataField="Job" HeaderText="Job" SortExpression="Job" />
                <asp:BoundField DataField="Job2" HeaderText="Job2" SortExpression="Job2" />
                <asp:BoundField DataField="Price" HeaderText="Price" SortExpression="Price" />
                              --%>
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="UsagOrderLook" TypeName="LookUp">
            <SelectParameters>               
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="item" Name="prmItem" Type="String" />
                <asp:QueryStringParameter QueryStringField="order" Name="prmOrder" Type="Int32" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

