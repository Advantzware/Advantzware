<%@ Page Language="C#" AutoEventWireup="true" Inherits="contact_customer_lookup2" Title="Customer Lookup" Codebehind="contact_customer_lookup2.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Customer Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue" DefaultButton="btnSearch">
    
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="550px" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="btnSearch" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="btnShowAll" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="ANY">ANY</asp:ListItem>--%>
                      <asp:ListItem Value="cust-no">CUSTOMER#</asp:ListItem>  
                      <asp:ListItem Value="name">NAME</asp:ListItem>
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <%--<asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem> --%>                  
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    
    <div>
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns > 
              <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.ContactCustomerLookup2('<%#DataBinder.Eval(Container,"DataItem.Customer")%>','<%#DataBinder.Eval(Container,"DataItem.Name")%>','<%#DataBinder.Eval(Container,"DataItem.Address1")%>','<%#DataBinder.Eval(Container,"DataItem.Address2")%>','<%#DataBinder.Eval(Container,"DataItem.city")%>','<%#DataBinder.Eval(Container,"DataItem.state")%>','<%#DataBinder.Eval(Container,"DataItem.zip")%>','<%#DataBinder.Eval(Container,"DataItem.sman")%>','<%#DataBinder.Eval(Container,"DataItem.country")%>','<%#DataBinder.Eval(Container,"DataItem.county")%>','<%#DataBinder.Eval(Container,"DataItem.terr")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="Customer" HeaderText="Cust #" SortExpression="Customer" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="Name" HeaderText="Customer Name" SortExpression="Name" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="city" HeaderText="City" SortExpression="city" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="state" HeaderText="State" SortExpression="state" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="zip" HeaderText="Zip Code" SortExpression="zip" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                 <asp:BoundField DataField="Address1" HeaderText="Address" SortExpression="Address1" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="Address2" HeaderText="Address" SortExpression="Address2" >
                    <ItemStyle Wrap="False" />
                    
                </asp:BoundField>
                <asp:BoundField DataField="Country" HeaderText="country" SortExpression="country" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="county" HeaderText="county" SortExpression="county" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <%--<asp:BoundField DataField="type" HeaderText="Customer Type" SortExpression="type" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>--%>
                <asp:BoundField DataField="sman" HeaderText="Sale Rep" SortExpression="sman" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="terr" HeaderText="Sales Territory" SortExpression="terr" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <%--<asp:BoundField DataField="frtpay" HeaderText="Frieght Pay" SortExpression="frtpay" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>--%>
                <%--<asp:BoundField DataField="sales" HeaderText="Customer Type" SortExpression="sales" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>--%>
                <%--<asp:BoundField DataField="comm" HeaderText="Comm%" SortExpression="comm" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>
                <asp:BoundField DataField="fob-code" HeaderText=" FOB" SortExpression="fob-code" >
                    <ItemStyle Wrap="False" />
                </asp:BoundField>--%>
                </Columns>
            
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustomer" TypeName="Order">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" DefaultValue="" />
                <asp:Parameter DefaultValue="" Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
