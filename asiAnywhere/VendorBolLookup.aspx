<%@ Page Language="C#" AutoEventWireup="true" Inherits="vendorbol_lookup" Title="Vendor Bol LookUp" Codebehind="VendorBolLookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Vendor Bol LookUp</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server" AutoPostBack="true" OnSelectedIndexChanged="searchindexchanged">
                    <%--<asp:ListItem Value="ANY">ANY</asp:ListItem>--%>
                      <asp:ListItem Value="BolNum">BOLNO#</asp:ListItem>  
                      <asp:ListItem Value="fgitem">ITEM NO</asp:ListItem>                  
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                   
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>                                     
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    
    <div>
        &nbsp;&nbsp;
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
		                <a href="#" onClick="javascript:top.opener.window.VendorBolLookup('<%#DataBinder.Eval(Container,"DataItem.vBolNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vINo")%>','<%#DataBinder.Eval(Container,"DataItem.vCustNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vJobNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vJobNo2")%>', '<%#DataBinder.Eval(Container,"DataItem.vOrdNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vLine")%>', '<%#DataBinder.Eval(Container,"DataItem.vPoNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vQty")%>', '<%#DataBinder.Eval(Container,"DataItem.vVendorCode")%>', '<%#DataBinder.Eval(Container,"DataItem.vVendorDeptCode")%>', '<%#DataBinder.Eval(Container,"DataItem.vVendorPlantCode")%>', '<%#DataBinder.Eval(Container,"DataItem.vCustPartNo")%>', '<%#DataBinder.Eval(Container,"DataItem.vPlantTotOhQty")%>', '<%#DataBinder.Eval(Container,"DataItem.vTransDate","{0:MM/dd/yyyy}")%>', '<%#DataBinder.Eval(Container,"DataItem.vSellPrice")%>');window.close();">Select</a>             	    
                    </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="vBolNo" HeaderText="Bol" SortExpression="vBolNo" />
                <asp:BoundField DataField="vINo" HeaderText="Item No" SortExpression="vINo" />
                <asp:BoundField DataField="vCustNo" HeaderText="Customer" SortExpression="vCustNo" />
                <asp:BoundField DataField="vJobNo" HeaderText="Job Number" SortExpression="vJobNo" />
                <asp:BoundField DataField="vJobNo2" HeaderText="" SortExpression="vJobNo2" />
                <asp:BoundField DataField="vOrdNo" HeaderText="Order#" SortExpression="vOrdNo" />
                <asp:BoundField DataField="vLine" HeaderText="Line#" SortExpression="vLine" />
                <asp:BoundField DataField="vPoNo" HeaderText="Customer Po" SortExpression="vPoNo" />
                <asp:BoundField DataField="vQty" HeaderText="Quantity Shipped" SortExpression="vQty" />
                <%--<asp:BoundField DataField="vVendorCode" HeaderText="vVendorCode" SortExpression="vVendorCode" />
                <asp:BoundField DataField="vVendorDeptCode" HeaderText="vVendorDeptCode" SortExpression="vVendorDeptCode" />
                <asp:BoundField DataField="vVendorPlantCode" HeaderText="vVendorPlantCode" SortExpression="vVendorPlantCode" />
                <asp:BoundField DataField="vCustPartNo" HeaderText="vCustPartNo" SortExpression="vCustPartNo" />
                <asp:BoundField DataField="vPlantTotOhQty" HeaderText="vPlantTotOhQty" SortExpression="vPlantTotOhQty" />
                <asp:BoundField DataField="vTransDate" HeaderText="vTransDate" SortExpression="vTransDate" />
                <asp:BoundField DataField="vSellPrice" HeaderText="vSellPrice" SortExpression="vSellPrice" />
                <asp:CheckBoxField DataField="vSelectedFlag" HeaderText="vSelectedFlag" SortExpression="vSelectedFlag" />--%>
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="VendorBolLook" TypeName="LookUp">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
