<%@ Page Language="C#" Inherits="Ordertrans" Title="Order Lookup" Codebehind="order_translookup.aspx.cs" %>
<html>
<head>
<title>Order Lookup</title>
<LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>

<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
<form id="form1" runat="server" defaultfocus="txtSearchValue">
<asp:Panel ID="Panel1" runat="server" DefaultButton="btnSearch">
  <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="btnSearch" runat="server" Width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
  <br />
  <asp:button id="btnShowAll" runat="server" Width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
  </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server" AutoPostBack="true" OnSelectedIndexChanged="searchindexchanged">                    
                      <asp:ListItem Value="ord-no">Order#</asp:ListItem>  
                      <asp:ListItem Value="est-no">Estimate#</asp:ListItem>
                      <asp:ListItem Value="cust-no">Customer No</asp:ListItem>
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                    
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
  </asp:Panel>
  <div>
    <asp:ObjectDataSource ID="ObjectDataSource7" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectOrderLook" TypeName="Order">
        <SelectParameters>
        <asp:SessionParameter SessionField="customer_fglookup_val" Name="prmCust" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" Type="String" />
            <asp:Parameter Name="prmField" Type="String" />
            <asp:Parameter Name="prmCondition" Type="String" />
            <asp:Parameter Name="prmText" Type="String" />
        </SelectParameters>
    </asp:ObjectDataSource>   
    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource7" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />    
		
            <Columns >
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.Order2Lookup('<%#DataBinder.Eval(Container,"DataItem.Order")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="Order" HeaderText="Order#" SortExpression="Order" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="Estimate" HeaderText="Estimate#" SortExpression="Estimate" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="cust-no" HeaderText="Cust#" SortExpression="cust-no" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
            </Columns>
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            
        </asp:GridView>     
  </div>
</form>
</body>
</html>
