<%@ Page Language="C#" AutoEventWireup="true" Inherits="bill_customer_lookupcopy" Codebehind="bill_customer_lookupcopy.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
     <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">                    
                      <asp:ListItem Value="cust-no">CUSTOMER#</asp:ListItem>  
                      <asp:ListItem Value="name">NAME</asp:ListItem> 
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                                        
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table></asp:Panel>
  </div>
    <div>
     <asp:GridView ID="GridView2" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="1800px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.BillCustomerLookupcopy('<%#DataBinder.Eval(Container,"DataItem.Customer")%>', '<%#DataBinder.Eval(Container,"DataItem.Name")%>', '<%#DataBinder.Eval(Container,"DataItem.Address1")%>', '<%#DataBinder.Eval(Container,"DataItem.Address2")%>', '<%#DataBinder.Eval(Container,"DataItem.city")%>', '<%#DataBinder.Eval(Container,"DataItem.state")%>', '<%#DataBinder.Eval(Container,"DataItem.zip")%>', '<%#DataBinder.Eval(Container,"DataItem.type")%>', '<%#DataBinder.Eval(Container,"DataItem.sman")%>', '<%#DataBinder.Eval(Container,"DataItem.sname")%>', '<%#DataBinder.Eval(Container,"DataItem.country")%>', '<%#DataBinder.Eval(Container,"DataItem.county")%>', '<%#DataBinder.Eval(Container,"DataItem.terr")%>', '<%#DataBinder.Eval(Container,"DataItem.frtpay")%>', '<%#DataBinder.Eval(Container,"DataItem.sales")%>', '<%#DataBinder.Eval(Container,"DataItem.comm")%>', '<%#DataBinder.Eval(Container,"DataItem.fob-code")%>', '<%#DataBinder.Eval(Container,"DataItem.carrier")%>', '<%#DataBinder.Eval(Container,"DataItem.contact")%>', '<%#DataBinder.Eval(Container,"DataItem.over")%>','<%#DataBinder.Eval(Container,"DataItem.under")%>','<%#DataBinder.Eval(Container,"DataItem.terms")%>','<%#DataBinder.Eval(Container,"DataItem.Tdscr")%>','<%#DataBinder.Eval(Container,"DataItem.prevOrder")%>','<%#DataBinder.Eval(Container,"DataItem.Taxcode")%>','<%#DataBinder.Eval(Container,"DataItem.ExpDate1")%>','<%#DataBinder.Eval(Container,"DataItem.DueDate")%>' ,'<%#DataBinder.Eval(Container,"DataItem.LastShip")%>' ,'<%#DataBinder.Eval(Container,"DataItem.dueCode")%>' ,'<%#DataBinder.Eval(Container,"DataItem.soldname")%>','<%#DataBinder.Eval(Container,"DataItem.soldadd1")%>','<%#DataBinder.Eval(Container,"DataItem.soldadd2")%>','<%#DataBinder.Eval(Container,"DataItem.soldcity")%>','<%#DataBinder.Eval(Container,"DataItem.soldstate")%>','<%#DataBinder.Eval(Container,"DataItem.soldzip")%>');window.close();">Select</a> </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="Customer" HeaderText="Customer" SortExpression="Customer" />
                <asp:BoundField DataField="Name" HeaderText="Name" SortExpression="Name" />
                <asp:BoundField DataField="Address1" HeaderText="Address1" SortExpression="Address1" />
                <asp:BoundField DataField="Address2" HeaderText="Address2" SortExpression="Address2" />
                <asp:BoundField DataField="city" HeaderText="City" SortExpression="city" />
                <asp:BoundField DataField="state" HeaderText="State" SortExpression="state" />
                <asp:BoundField DataField="zip" HeaderText="Zip" SortExpression="zip" />
                <asp:BoundField DataField="type" HeaderText="Type" SortExpression="type" />
                <asp:BoundField DataField="sman" HeaderText="Sman" SortExpression="sman" />
                <asp:BoundField DataField="sname" HeaderText="Sname" SortExpression="sname" />
                <asp:BoundField DataField="country" HeaderText="Country" SortExpression="country" />
                <asp:BoundField DataField="county" HeaderText="County" SortExpression="county" />
                <asp:BoundField DataField="terr" HeaderText="Terr" SortExpression="terr" />
                <asp:BoundField DataField="frtpay" HeaderText="Frtpay" SortExpression="frtpay" />
                <asp:BoundField DataField="sales" HeaderText="sales" SortExpression="sales" />
                <asp:BoundField DataField="comm" HeaderText="Comm" SortExpression="comm" />
                <asp:BoundField DataField="fob-code" HeaderText="Fob Code" SortExpression="fob-code" />
                <asp:BoundField DataField="carrier" HeaderText="Carrier" SortExpression="carrier" />
                <asp:BoundField DataField="contact" HeaderText="Contact" SortExpression="contact" />
                <asp:BoundField DataField="over" HeaderText="Over" SortExpression="over" />
                <asp:BoundField DataField="under" HeaderText="Under" SortExpression="under" />
                <asp:BoundField DataField="terms" HeaderText="Terms" SortExpression="terms" />
                <asp:BoundField DataField="Tdscr" HeaderText="Tdscr" SortExpression="Tdscr" />
                <asp:BoundField DataField="prevOrder" HeaderText="PreOrder" SortExpression="prevOrder" />
                <asp:BoundField DataField="Taxcode" HeaderText="Taxcode" SortExpression="Taxcode" />
                <asp:TemplateField HeaderText="ExpDate1" SortExpression="ExpDate1">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("ExpDate1") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("ExpDate1","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="DueDate" SortExpression="DueDate">
                    <EditItemTemplate>
                        <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("DueDate") %>'></asp:TextBox>
                    </EditItemTemplate>
                    <ItemTemplate>
                        <asp:Label ID="Label2" runat="server" Text='<%# Bind("DueDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="LastShip" HeaderText="LastShip" SortExpression="LastShip" />
                <asp:BoundField DataField="dueCode" HeaderText="DueCode" SortExpression="dueCode" />

                
                
            </Columns>
        </asp:GridView>
        
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustomer" TypeName="Order">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

