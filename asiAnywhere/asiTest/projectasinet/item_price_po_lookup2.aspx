<%@ Page Language="C#" AutoEventWireup="true" Inherits="item_price_po_lookup2" Codebehind="item_price_po_lookup2.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Select Items for PO:</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
<%--<script language = "JavaScript" src="include/CalendarControl.js"></script>
     <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript" src="include/validate2.js"></script>--%>
    <script language="javascript">   
   
 
  </script>

</head>



<body>
   
    <form id="form1" runat="server" >
    
     <div>
    <table style="display:none;"><tr><td>
        <asp:TextBox runat="server" id="Text1"  />
        <asp:TextBox runat="server" id="Text2" />
        <asp:TextBox runat="server" id="Text3"  />
        <asp:TextBox runat="server" id="Text4" />
        <asp:TextBox runat="server" id="Text5"  />
        <asp:TextBox runat="server" id="Text6" />
        <asp:TextBox runat="server" id="Text7"  />
        <asp:TextBox runat="server" id="Text8" />
        <asp:TextBox runat="server" id="Text9"  />
        <asp:TextBox runat="server" id="Text10" />
        
       
        </td></tr></table>
        
        
    </div>
    
    <div><br /><br />
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server"  OnDataBound="Gridview1_DataBound"
            AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" 
            OnSelectedIndexChanged="GridView1_SelectedIndexChanged" 
            EmptyDataText="All receipts for this PO have been invoiced" Width="100%" BorderStyle="Dotted" 
            CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
            <asp:TemplateField HeaderText="Selected">
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>                        
                       <asp:CheckBox ID="chk1" runat="server" />
                       <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("slct") %>' ></asp:Label>    
                   </ItemTemplate>
                </asp:TemplateField>
                               
                 <asp:TemplateField HeaderText="Qty to Invoice" SortExpression="invqty">
                <ItemTemplate>
                <asp:Label ID="invqtyLabel" runat="server" Text='<%# Bind("invqty") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                              
                <asp:BoundField DataField="qtyinvuom" HeaderText="U/M" SortExpression="qtyinvuom" />
                <asp:BoundField DataField="recqty" HeaderText="Purchased Qty" SortExpression="recqty" />
                <asp:BoundField DataField="qtyrecuom" HeaderText="U/M" SortExpression="qtyrecuom" />
               
                <asp:TemplateField HeaderText="Receipt Date" SortExpression="recdate">
                <ItemTemplate>
                <asp:Label ID="recdateLabel" runat="server" Text='<%# Bind("recdate","{0:MM/dd/yyyy}") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Line" SortExpression="Line">
                <ItemTemplate>
                <asp:Label ID="LineLabel" runat="server" Text='<%# Bind("Line") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
                <asp:BoundField DataField="job" HeaderText="Job Number" SortExpression="job" />
                <asp:TemplateField HeaderText="Sheet#" SortExpression="snum">
                <ItemTemplate>
                <asp:Label ID="snumLabel" runat="server" Text='<%# Bind("snum") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <asp:TemplateField HeaderText="Item#" SortExpression="ino">
                <ItemTemplate>
                <asp:Label ID="inoLabel" runat="server" Text='<%# Bind("ino") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
                <asp:BoundField DataField="slen" HeaderText="Sheet Len" SortExpression="slen" />
                <asp:BoundField DataField="swid" HeaderText="Sheet Wid" SortExpression="swid" />                
                
                <asp:TemplateField HeaderText="P.O. Date" SortExpression="podate">
                <ItemTemplate>
                <asp:Label ID="podateLabel" runat="server" Text='<%# Bind("podate","{0:MM/dd/yyyy}") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="cost" HeaderText="Unit Cost" SortExpression="cost" />
                <asp:BoundField DataField="pruom" HeaderText="Purchased UOM" SortExpression="pruom" />
                <asp:BoundField DataField="tcost" HeaderText="Extended Cost" SortExpression="tcost" />
                <asp:BoundField DataField="iname" HeaderText="Name" SortExpression="iname" />
                
                <asp:TemplateField HeaderText="act" Visible="false" SortExpression="act">
                <ItemTemplate>
                <asp:Label ID="actLabel" runat="server" Text='<%# Bind("act") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
                <asp:TemplateField HeaderText="actname" Visible="false" SortExpression="actname">
                <ItemTemplate>
                <asp:Label ID="actnameLabel" runat="server" Text='<%# Bind("actname") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                 <asp:TemplateField HeaderText="tamt" Visible="false" SortExpression="tamt">
                <ItemTemplate>
                <asp:Label ID="tamtLabel" runat="server" Text='<%# Bind("tamt") %>' ></asp:Label>
                </ItemTemplate>
                </asp:TemplateField>
                
                
                
                               
                 
                
            </Columns>
        </asp:GridView><br />
        <asp:RadioButtonList ID="QtyRadioButtonList1" CssClass="shade"   RepeatLayout="Flow" AutoPostBack="true"  CellSpacing="1" RepeatColumns="2" runat="server">
                                                    <asp:ListItem value="1"  Text="Purchased Qty "   />
                                                    <asp:ListItem  value="2" Text="Receipt Qty" />
                                                </asp:RadioButtonList><br /><br />
         &nbsp;&nbsp;&nbsp;                                       
        <asp:Button ID="Button1" Width="40px" CssClass="button" OnClick="button_Click" runat="server" Text="Ok" />  &nbsp;&nbsp;&nbsp;
         <asp:Button ID="Button2" Width="40px" CssClass="button" OnClick="button_Click" Visible="false" runat="server" Text="Okenter" />                     
        <input type="button" name="close" class="buttonM" id="close" value="Cancel" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectPriceItemVendor2" TypeName="voucherpay">
            <SelectParameters>               
                 <asp:Parameter Name="prmAction" DefaultValue="hj" Type="String" />
                 <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:SessionParameter SessionField="vendor_invoice_reckey_rec" Name="prmText" Type="String" />
                <asp:QueryStringParameter QueryStringField="pono" Name="prmPono" Type="Int32" DefaultValue="" />                
                 <asp:Parameter Name="prmslect" Type="String" />
                 <asp:Parameter Name="prmrdqty" Type="Int32" />
                
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


