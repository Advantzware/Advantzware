<%@ Page Language="C#" AutoEventWireup="true" Inherits="po_price_history_lookup" Codebehind="po_price_history_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Price History Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" defaultbutton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="pono">PO #</asp:ListItem>  
                      
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" 
            AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" 
            OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" 
            CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.PoPriceLookup('<%#DataBinder.Eval(Container,"DataItem.pono")%>','<%#DataBinder.Eval(Container,"DataItem.actno")%>','<%#DataBinder.Eval(Container,"DataItem.actname")%>','<%#DataBinder.Eval(Container,"DataItem.tinvqty")%>','<%#DataBinder.Eval(Container,"DataItem.consuom")%>','<%#DataBinder.Eval(Container,"DataItem.unit-pr")%>','<%#DataBinder.Eval(Container,"DataItem.prqtyuom")%>','<%#DataBinder.Eval(Container,"DataItem.srft")%>','<%#DataBinder.Eval(Container,"DataItem.amtmsf")%>','<%#DataBinder.Eval(Container,"DataItem.iname")%>','<%#DataBinder.Eval(Container,"DataItem.job")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                
                 <asp:BoundField DataField="pono" HeaderText="Po Number" SortExpression="pono" />
                 <asp:BoundField DataField="ino" HeaderText="Item" SortExpression="ino" />                
                <asp:BoundField DataField="iname" HeaderText="RM/FG Item name" SortExpression="iname" />
                <asp:BoundField DataField="swid" HeaderText="Width" SortExpression="swid" />
                <asp:BoundField DataField="slen" HeaderText="Length" SortExpression="slen" />
                <asp:BoundField DataField="ordqty" HeaderText="Quantity" SortExpression="ordqty" />
                <asp:BoundField DataField="trecqty" HeaderText="Received Qty" SortExpression="trecqty" />
                <asp:BoundField DataField="tinvqty" HeaderText="Invoiced Qty" SortExpression="tinvqty" />
                <asp:BoundField DataField="podate" HeaderText="PO Date" SortExpression="podate" />
                <asp:BoundField DataField="potype" HeaderText="Type" SortExpression="potype" />
                <asp:BoundField DataField="stat" HeaderText="Status" SortExpression="stat" />
                <asp:BoundField DataField="job" HeaderText="JOB#" SortExpression="job" />
                <asp:BoundField DataField="job2" HeaderText="" SortExpression="job2" />
                
                <asp:BoundField DataField="unit-pr" HeaderText="unit-pr" SortExpression="unit-pr" />
                <asp:BoundField DataField="prqtyuom" HeaderText="prqtyuom" SortExpression="prqtyuom" />
                <asp:BoundField DataField="consuom" HeaderText="consuom" SortExpression="consuom" />
                <asp:BoundField DataField="tax" HeaderText="tax" SortExpression="tax" />
                <asp:BoundField DataField="actno" HeaderText="actno" SortExpression="actno" />
                <asp:BoundField DataField="actname" HeaderText="actname" SortExpression="actname" />
                <asp:BoundField DataField="invqty" HeaderText="invqty" SortExpression="invqty" />
                <asp:BoundField DataField="amt" HeaderText="amt" SortExpression="amt" />
                <asp:BoundField DataField="srft" HeaderText="srft" SortExpression="srft" />
                <asp:BoundField DataField="amtmsf" HeaderText="amtmsf" SortExpression="amtmsf" />
                <asp:BoundField DataField="ext" HeaderText="ext" SortExpression="ext" />
                 
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectPriceHistoryVendor" TypeName="voucherpay">
            <SelectParameters>               
                 <asp:Parameter Name="prmAction" DefaultValue="jgh" Type="String" />
                 <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="pono" Name="prmPono" Type="Int32" 
                     DefaultValue="" />
                
                 <asp:QueryStringParameter Name="prmext" QueryStringField="invno" 
                     Type="String" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


