<%@ Page Language="C#" AutoEventWireup="true" Inherits="custshiplook" Title="Customer Lookup" Codebehind="custshiplook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Customer Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />

</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <asp:Panel ID="Panel1" runat="server" DefaultButton="btnSearch">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="550px" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="btnSearch" runat="server" width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="btnShowAll" runat="server" width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                   
                      <asp:ListItem Value="cust">CUSTOMER#</asp:ListItem>  
                      <asp:ListItem Value="name">NAME</asp:ListItem>
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                  
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
  </asp:Panel> 
    <div>
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnUnload="grid_unload" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns >  
                  <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.CustShipLook('<%#DataBinder.Eval(Container,"DataItem.Cust-no")%>','<%#DataBinder.Eval(Container,"DataItem.CName")%>','<%#DataBinder.Eval(Container,"DataItem.Addr")%>','<%#DataBinder.Eval(Container,"DataItem.Addr2")%>','<%#DataBinder.Eval(Container,"DataItem.City")%>','<%#DataBinder.Eval(Container,"DataItem.State")%>','<%#DataBinder.Eval(Container,"DataItem.Zip")%>','<%#DataBinder.Eval(Container,"DataItem.carrier")%>','<%#DataBinder.Eval(Container,"DataItem.carrdesc")%>','<%#DataBinder.Eval(Container,"DataItem.Sman")%>','<%#DataBinder.Eval(Container,"DataItem.sname")%>','<%#DataBinder.Eval(Container,"DataItem.terms")%>','<%#DataBinder.Eval(Container,"DataItem.termsdesc")%>','<%#DataBinder.Eval(Container,"DataItem.zone")%>','<%#DataBinder.Eval(Container,"DataItem.zonedesc")%>','<%#DataBinder.Eval(Container,"DataItem.shipid")%>','<%#DataBinder.Eval(Container,"DataItem.shipname")%>','<%#DataBinder.Eval(Container,"DataItem.shipadd1")%>','<%#DataBinder.Eval(Container,"DataItem.shipadd2")%>','<%#DataBinder.Eval(Container,"DataItem.shipcity")%>','<%#DataBinder.Eval(Container,"DataItem.shipstat")%>','<%#DataBinder.Eval(Container,"DataItem.shipzip")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldid")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldname")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldadd1")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldadd2")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldcity")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldstat")%>','<%#DataBinder.Eval(Container,"DataItem.vsoldzip")%>','<%#DataBinder.Eval(Container,"DataItem.vcontact")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>           
              
                <asp:BoundField DataField="Cust-no" HeaderText="Customer" ItemStyle-Wrap="false" SortExpression="Cust-no" >
                </asp:BoundField>
                <asp:BoundField DataField="CName" HeaderText="Name" ItemStyle-Wrap="false" SortExpression="CName" >
                </asp:BoundField>
                
                <asp:BoundField DataField="Addr" HeaderText="Addr" ItemStyle-Wrap="false" SortExpression="Addr" >
                </asp:BoundField>
                <asp:BoundField DataField="Addr2" HeaderText="Addr" ItemStyle-Wrap="false" SortExpression="Addr2" >
                </asp:BoundField>
                <asp:BoundField DataField="City" HeaderText="City" ItemStyle-Wrap="false" SortExpression="City" >
                </asp:BoundField>
                 <asp:BoundField DataField="State" HeaderText="State" ItemStyle-Wrap="false" SortExpression="State" >
                </asp:BoundField>
                <asp:BoundField DataField="Zip" HeaderText="Zip" ItemStyle-Wrap="false" SortExpression="Zip" >
                    
                </asp:BoundField>
               
                
                </Columns>
            
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="selectCustShipLook" TypeName="LookUp">
            <SelectParameters>
                <asp:Parameter DefaultValue="Admin" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmItemNum" Type="String" />
                <asp:Parameter DefaultValue="" Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
    </div>
    </form>
</body>
</html>
