<%@ Page Language="C#" AutoEventWireup="true" Inherits="custord_rellookup" Codebehind="custord_rellookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Order Items for Customer</title>
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
                      <asp:ListItem Value="order">Order#</asp:ListItem>  
                     
                      
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
        <asp:GridView ID="GridView1" AllowPaging="true" PageSize="10" AllowSorting="true" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CustOrderLookup('<%#DataBinder.Eval(Container,"DataItem.ordno")%>','<%#DataBinder.Eval(Container,"DataItem.ino")%>','<%#DataBinder.Eval(Container,"DataItem.qty")%>','<%#DataBinder.Eval(Container,"DataItem.jobno")%>','<%#DataBinder.Eval(Container,"DataItem.jobno2")%>','<%#DataBinder.Eval(Container,"DataItem.partno")%>','<%#DataBinder.Eval(Container,"DataItem.loc")%>','<%#DataBinder.Eval(Container,"DataItem.pono")%>','<%#DataBinder.Eval(Container,"DataItem.scod")%>','<%#DataBinder.Eval(Container,"DataItem.qtycas")%>','<%#DataBinder.Eval(Container,"DataItem.relno")%>','<%#DataBinder.Eval(Container,"DataItem.cases")%>','<%#DataBinder.Eval(Container,"DataItem.partial")%>','<%#DataBinder.Eval(Container,"DataItem.rno")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            
                
                 <asp:BoundField DataField="ordno" HeaderText="Order#" SortExpression="ordno" />                               
                <asp:BoundField DataField="estno" HeaderText="Estimate#" SortExpression="estno" />
                <asp:BoundField DataField="jobno" HeaderText="Job#" SortExpression="jobno" />
                 <asp:BoundField DataField="jobno2" HeaderText="" SortExpression="jobno2" /> 
                <asp:BoundField DataField="ino" HeaderText="Item#" SortExpression="ino" />
                <asp:BoundField DataField="partno" HeaderText="Cust Part#" SortExpression="partno" />
                <asp:BoundField DataField="qty" HeaderText="Order Quantity " SortExpression="qty" />
                <asp:BoundField DataField="invqty" HeaderText="Invoice Quantity" SortExpression="invqty" /> 
                <asp:BoundField DataField="shpqty" HeaderText="Shipped Quantity" SortExpression="shpqty" />
                <asp:BoundField DataField="proqty" HeaderText="Quantity Produced" SortExpression="proqty" />
                <asp:BoundField DataField="onhbal" HeaderText="Balance On Hand " SortExpression="onhbal" />
                
                
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="OrderCustLookup" TypeName="release">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="custord" Name="prmcust" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

