<%@ Page Language="C#" AutoEventWireup="true" Inherits="custpart2_lookup" Codebehind="custpart2_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Custpart Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
    <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="ANY">ANY</asp:ListItem>--%>
                      <asp:ListItem Value="part-no">CUST PART</asp:ListItem>  
                      <%--<asp:ListItem Value="i-name">NAME</asp:ListItem>--%>
                      
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <%--<asp:ListItem Value="More than ...">More than ...</asp:ListItem>
                    <asp:ListItem Value="Less than ...">Less than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or more than ...">Equal or more than ...</asp:ListItem>
                    <asp:ListItem Value="Equal or less than ...">Equal or less than ...</asp:ListItem>
                    <asp:ListItem Value="IsNull">Empty</asp:ListItem> --%>                  
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table></asp:Panel>
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
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CustPartLookup('<%#DataBinder.Eval(Container,"DataItem.Part")%>','<%#DataBinder.Eval(Container,"DataItem.partdscr1")%>','<%#DataBinder.Eval(Container,"DataItem.partdscr2")%>','<%#DataBinder.Eval(Container,"DataItem.ino")%>','<%#DataBinder.Eval(Container,"DataItem.iname")%>','<%#DataBinder.Eval(Container,"DataItem.uom")%>','<%#DataBinder.Eval(Container,"DataItem.price")%>','<%#DataBinder.Eval(Container,"DataItem.qtyhand")%>','<%#DataBinder.Eval(Container,"DataItem.est")%>','<%#DataBinder.Eval(Container,"DataItem.mat")%>','<%#DataBinder.Eval(Container,"DataItem.casecount")%>','<%#DataBinder.Eval(Container,"DataItem.casepall")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="Part" HeaderText="Cust Part#" SortExpression="Part" />
                <asp:BoundField DataField="cust-no" HeaderText="Customer" SortExpression="cust-no" />
                 <asp:BoundField DataField="ino" HeaderText="Item#" SortExpression="ino" />
                <asp:BoundField DataField="iname" HeaderText="Item Dscription" SortExpression="iname" />
                
               <%-- <asp:BoundField DataField="partdscr1" HeaderText="partdscr1" SortExpression="partdscr1" />
                <asp:BoundField DataField="partdscr2" HeaderText="partdscr2" SortExpression="partdscr2" />
                <asp:BoundField DataField="uom" HeaderText="uom" SortExpression="uom" />
                <asp:BoundField DataField="price" HeaderText="price" SortExpression="price" />
                <asp:BoundField DataField="qtyhand" HeaderText="qtyhand" SortExpression="qtyhand" />
                
                <asp:BoundField DataField="est" HeaderText="est" SortExpression="est" />
                <asp:BoundField DataField="mat" HeaderText="mat" SortExpression="mat" />
                <asp:BoundField DataField="casecount" HeaderText="casecount" SortExpression="casecount" />
                <asp:BoundField DataField="casepall" HeaderText="casepall" SortExpression="casepall" />--%>
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectCustPartLook" TypeName="Order">
            <SelectParameters>
            <asp:QueryStringParameter QueryStringField="customer" Name="prmCust" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmItem" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>
