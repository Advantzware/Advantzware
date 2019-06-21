<%@ Page Language="C#" AutoEventWireup="true" Inherits="shipidlook2" Title="ShipIdLook" Codebehind="shipidlook2.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server"  width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server"  width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                                          
                       <asp:ListItem Value="custnum">Cust#</asp:ListItem>
                      <asp:ListItem Value="ship-id">Ship To</asp:ListItem>                                                                                        
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
		    <a href="#" onClick="javascript:top.opener.window.ShipIdLookup2('<%#DataBinder.Eval(Container,"DataItem.Shipid")%>','<%#DataBinder.Eval(Container,"DataItem.carrier")%>' ,'<%#DataBinder.Eval(Container,"DataItem.cas-no")%>','<%#DataBinder.Eval(Container,"DataItem.cas-cnt")%>','<%#DataBinder.Eval(Container,"DataItem.cas-len")%>','<%#DataBinder.Eval(Container,"DataItem.cas-wid")%>','<%#DataBinder.Eval(Container,"DataItem.cas-dep")%>','<%#DataBinder.Eval(Container,"DataItem.cas-pal")%>','<%#DataBinder.Eval(Container,"DataItem.cas-wt")%>','<%#DataBinder.Eval(Container,"DataItem.tr-no")%>','<%#DataBinder.Eval(Container,"DataItem.tr-len")%>','<%#DataBinder.Eval(Container,"DataItem.tr-dep")%>','<%#DataBinder.Eval(Container,"DataItem.tr-wid")%>','<%#DataBinder.Eval(Container,"DataItem.Shipiddscr")%>','<%#DataBinder.Eval(Container,"DataItem.carrierdscr")%>','<%#DataBinder.Eval(Container,"DataItem.adddr1")%>','<%#DataBinder.Eval(Container,"DataItem.addr2")%>','<%#DataBinder.Eval(Container,"DataItem.city")%>','<%#DataBinder.Eval(Container,"DataItem.state")%>','<%#DataBinder.Eval(Container,"DataItem.vzip")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                
                <asp:BoundField DataField="custnum" Visible="false" HeaderText="Cust#" SortExpression="custnum" />
                <asp:BoundField DataField="Shipid" HeaderText="ShipId" SortExpression="Shipid" />
                <asp:BoundField DataField="Shipiddscr" HeaderText="ShipDscr" SortExpression="Shipiddscr" />
                <asp:BoundField DataField="carrier" HeaderText="Carrier" SortExpression="carrier" />
                <asp:BoundField DataField="carrierdscr" HeaderText="CarrierDscr" SortExpression="carrierdscr" />
                <asp:BoundField DataField="cas-no" Visible="false" HeaderText="cas-no" SortExpression="cas-no" />
                <asp:BoundField DataField="cas-cnt" Visible="false" HeaderText="cas-cnt" SortExpression="cas-cnt" />
                <asp:BoundField DataField="cas-len" Visible="false" HeaderText="cas-len" SortExpression="cas-len" />
                <asp:BoundField DataField="cas-wid" Visible="false" HeaderText="cas-wid" SortExpression="cas-wid" />
                <asp:BoundField DataField="cas-dep" Visible="false" HeaderText="cas-dep" SortExpression="cas-dep" />
                <asp:BoundField DataField="cas-pal" Visible="false" HeaderText="cas-pal" SortExpression="cas-pal" />
                <asp:BoundField DataField="cas-wt" Visible="false" HeaderText="cas-wt" SortExpression="cas-wt" />
                <asp:BoundField DataField="tr-no" HeaderText="Pallet" SortExpression="tr-no" />
                <asp:BoundField DataField="cas-paldscr" HeaderText="PalletDscr" SortExpression="cas-paldscr" />
                <asp:BoundField DataField="tr-len" Visible="false" HeaderText="tr-len" SortExpression="tr-len" />
                <asp:BoundField DataField="tr-dep" Visible="false" HeaderText="tr-dep" SortExpression="tr-dep" />
                <asp:BoundField DataField="tr-wid" Visible="false" HeaderText="tr-wid" SortExpression="tr-wid" />
                <asp:BoundField DataField="State1" Visible="false" HeaderText="State1" SortExpression="State1" />
                <asp:BoundField DataField="Zip1" Visible="false" HeaderText="Zip1" SortExpression="Zip1" />
                <asp:BoundField DataField="carrier1" Visible="false" HeaderText="carrier1" SortExpression="carrier1" />
                <asp:BoundField DataField="Loc1" Visible="false" HeaderText="Loc1" SortExpression="Loc1" />
                
                <asp:BoundField DataField="adddr1"  HeaderText="Address" SortExpression="adddr1" />
                <asp:BoundField DataField="addr2"  HeaderText="Address" SortExpression="addr2" />
                <asp:BoundField DataField="city"  HeaderText="City" SortExpression="city" />
                <asp:BoundField DataField="state"  HeaderText="State" SortExpression="state" />
                <asp:BoundField DataField="vZip"  HeaderText="Zip" SortExpression="vZip" />
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="ShipIdLook" TypeName="LookUp">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                 <asp:SessionParameter Name="prmShip" SessionField="custom_shipid_look" Type="String" />
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
