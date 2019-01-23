<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="fold_analysis_set" Codebehind="fold_analysis_set.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Estimate Analysis</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <%--<link href="include/tree.css" rel="stylesheet" type="text/css" />--%>
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }

    function displayQtyScrDiv() {
        document.getElementById("qtyscr").style.display = "block";
        document.getElementById("btndiv").style.display = "none";
    }
    function validate(evt) {
        var charcode = (evt.which) ? evt.which : event.keyCode;
        if (charcode > 31 && (charcode < 48 || charcode > 57))
            return false;
            return true;
           
    }
               

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
        <%--<hd:header id="Header1" runat="server"></hd:header>--%>
      <div>
            
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
            <asp:HiddenField ID="HiddenField_Vendor" runat="server" />
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
                             
          <div id="btndiv" runat="server">
            <table id="pugetable" runat="server">
            <tr><td style="width:80px">&nbsp;</td></tr>
          <tr><td align="center">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;    
              <asp:Button ID="Button3" CssClass="buttonM" runat="server" Width="300px" Text="Purge Existing Qty's" OnClick="PurgeQtyClick" /><br /><br /></td></tr>
              <tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Button ID="Button4" CssClass="buttonM" runat="server" Width="300px" Text="Append To Existing Qty's" OnClick="AppendQtyClick" />
          </td></tr></table>
          </div>
                                      
        
        
          <asp:FormView ID="FormView1"  runat="server"  OnDataBound="FormView1_DataBound">
            
              <InsertItemTemplate>
                  <table class="shade">
      <tr><td  nowrap><b>Quantity</b></td>
      <td  nowrap><b>Rels</b></td>
      <td  nowrap><b>Quantity</b></td>
      <td nowrap><b>Rels</b></td>
      <td  nowrap><b>Quantity</b></td>
       <td  nowrap><b>Rels</b></td>
       <td  nowrap><b>Quantity</b></td>
       <td  nowrap><b>Rels</b></td>
       </tr><tr>
         <td> <asp:TextBox ID="TextBox1" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox29" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox2" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox30" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox3" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox31" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>  </td>
            <td> <asp:TextBox ID="TextBox4" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox32" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>   </td> </tr>
           <tr>          
            <td><asp:TextBox ID="TextBox5" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>         
            <td> <asp:TextBox ID="TextBox33" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox6" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox34" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox7" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox35" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
            <td> <asp:TextBox ID="TextBox8" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox36" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
          </tr>
          <tr>
         <td> <asp:TextBox ID="TextBox9" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox37" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox10" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox38" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox11" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox39" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>  </td>
            <td> <asp:TextBox ID="TextBox12" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox40" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>   </td> </tr>
           <tr>          
            <td><asp:TextBox ID="TextBox13" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>         
            <td> <asp:TextBox ID="TextBox41" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox14" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox42" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox15" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox43" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
            <td> <asp:TextBox ID="TextBox16" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox44" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
          </tr>
          <tr>
         <td> <asp:TextBox ID="TextBox17" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox45" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox18" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox46" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox19" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox47" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>  </td>
            <td> <asp:TextBox ID="TextBox20" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox48" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>   </td> </tr>
           <tr>          
            <td><asp:TextBox ID="TextBox21" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>         
            <td> <asp:TextBox ID="TextBox49" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox22" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox50" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox23" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox51" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
            <td> <asp:TextBox ID="TextBox24" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox></td>
            <td><asp:TextBox ID="TextBox52" onkeypress="return validate(event)" Width="35px" runat="server"></asp:TextBox>    </td> 
          </tr>
          <tr>
         <td> <asp:TextBox ID="TextBox25" onkeypress="return validate(event)" Width="70px" runat="server"></asp:TextBox> </td>
            <td> <asp:TextBox ID="TextBox53" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox26" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox54" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox27" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox55" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>  </td>
            <td> <asp:TextBox ID="TextBox28" Width="70px" onkeypress="return validate(event)" runat="server"></asp:TextBox></td>
            <td> <asp:TextBox ID="TextBox56" Width="35px" onkeypress="return validate(event)" runat="server"></asp:TextBox>   </td> </tr>            
                    
          </table>
          <table>
          <tr><td align="center" nowrap> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Button ID="Button1" runat="server" Text="Clear Qtys" OnClick="ClearQtyClick" />
              
          </td></tr></table>
          <table>       
            <tr>
                <td>
                    <b><font size="2"> Selection </font></b>                          
                </td>
                <td align="center">
                    <b><font size="2"> Estimates for Board Cost </font></b>
                </td>
            </tr>
            <tr>
                <td>
                    <b><asp:CheckBox ID="CheckBox1" runat="server" Checked="true" />
                        Recalc Machines' Speed, Spoil%, & Waste?</b>
                </td>
                <td rowspan="4">
                    <asp:TextBox TextMode="MultiLine" Height="115px" Width="150px" ID="TextBox57" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                   <b><asp:CheckBox ID="CheckBox2" runat="server" Checked="true" />
                        Recalc Machines' MR-Hrs?</b>
                </td>
            </tr>
            <tr>
                <td>
                    <b><asp:CheckBox ID="CheckBox3" runat="server" />
                        Override GS&A and/or Warehouse Percentages?</b>
                </td>
            </tr>
            </table>

                  <asp:Button ID="InsertButton" CssClass="buttonM" runat="server" CausesValidation="True" OnClick="SaveBtnClick" Text="Save" />
                  &nbsp;<input type="button" name="Cancel" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close()"  />
              </InsertItemTemplate>
              
          </asp:FormView>  
                                    
    </div>
    <div runat="server" id="vendordiv" align="center" >
          <br />
          <fieldset style="width:350px">
          <table>
          <tr><td align="left"><b>Board Vendor Selection</b></td></tr>
          <tr><td>
              <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_selectedindex_change"
                  DataSourceID="ObjectDataSourceVendor" AllowPaging="True" AllowSorting="True" OnSorting="Gridview1_Sorting" 
            EmptyDataText="No Records Found" Width="300px" BorderStyle="Dotted" CssClass="Grid">
           <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
                  <Columns>
                   <asp:CommandField  ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" >
                    <ItemStyle  Width="10px" />
                    </asp:CommandField>  
                      <asp:BoundField DataField="key-01" HeaderText="Vender" 
                          SortExpression="key-01" />
                   
                      <asp:BoundField DataField="key-02" HeaderText="Vendor Name" 
                          SortExpression="key-02" />
                      <asp:BoundField Visible="false" DataField="rec-id" HeaderText="rec-id" 
                          SortExpression="rec-id" />
                  </Columns>
              </asp:GridView><br /><br /></td></tr>
              <tr><td align="left">
              <asp:Button ID="okButton" CssClass="button" OnClick="ok_button_click" runat="server" Text="Ok" />
              <asp:Button ID="PriceButton" runat="server" CssClass="button" OnClick="Price_Button_Click" Text="Best Price" />
              <asp:Button ID="vandercancelButton" CssClass="button" runat="server" OnClientClick="javascript:window.close();" Text="Cancel" />
              </td></tr>
              </table>
              </fieldset>
              
              <asp:ObjectDataSource ID="ObjectDataSourceVendor" runat="server" 
                  OldValuesParameterFormatString="original_{0}" SelectMethod="SelectVendor" 
                  TypeName="Corrugated">
                  <SelectParameters>
                      <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                      <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                      <asp:SessionParameter Name="prmEstimate" SessionField="order_folding_est" Type="String" />
                  </SelectParameters>
              </asp:ObjectDataSource>
          </div>
    
  
    </form>
</body>
</html>
