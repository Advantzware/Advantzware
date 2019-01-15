<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="print_po_report" Codebehind="print_po_report.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Print Purchase Order</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    
     <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
//    function confirmPost() {
//        var retVal = makeMsgBox("Confirmation", "Are you ready to  Post Invoices?", 48, 4, 256, 4096);
//        if (retVal == 6) {
//            document.forms[0].HiddenFieldPost.value = "Yes";
//        }
//        else {
//            document.forms[0].HiddenFieldPost.value = "No";
//        }
//    }
</script>
        
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

    function preEnter(fieldObj, canEdit) {
        fieldObj.style.backgroundColor = 'blue';
        fieldObj.style.color = 'white';
        if (canEdit == "no") {
            fieldObj.blur();
            leaveField(fieldObj);
        }

        enterField(fieldObj);
        return;
    }
    
    function preLeave( fieldObj, fieldType, fieldFormat ){
    fieldObj.style.backgroundColor='Window';
    fieldObj.style.color='WindowText';
    fieldType = fieldType.toLowerCase();
    if ((fieldType == "") || (fieldType == "text")) {
        leaveField(fieldObj);
    }
}

function focusval(obj) {
    obj.style.backgroundColor = 'blue';
    obj.style.color = 'white';
}
function blurval(obj) {
    obj.style.backgroundColor = 'Window';
    obj.style.color = 'WindowText';
}

var polook = "";
function customerpolook(po1) {
    polook = po1;
    var NewWindow = window.open("poitem_lookup.aspx", "CustomerpoWindow", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ItemPoLookup(ReturnObj1) {
    if (polook == 1) {
        document.forms[0].bepoTextBox.value = ReturnObj1;
    }
    else
        document.forms[0].endpoTextBox.value = ReturnObj1;    
    
}


    function vendtext() {
        var vend = document.getElementById("bevendTextBox");
        vend.focus();
    }
    function periodtext() {
        var vend = document.getElementById("postdateTextBox");
        var pertext = document.getElementById("perTextBox");
        pertext.value = (vend.value).substring(0, 2);

    }
    var vendlook = "";
    function vendorlook(var1) {
        vendlook = var1;
        var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function VendLookup(ReturnObj1) {
        if(vendlook == 1)
            document.forms[0].bevendTextBox.value = ReturnObj1;
            else
                document.forms[0].endvendTextBox.value = ReturnObj1;
        }

        function banklookup() {

            var NewWindow = window.open("bank_lookup.aspx", "banklookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function banklook(ReturnObj1, ReturnObj2) {
            document.forms[0].bnkcodTextBox.value = ReturnObj1;


        }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />
        <asp:HiddenField ID="HiddenField3" runat="server" />
        <asp:HiddenField ID="HiddenField4" runat="server" />
        <asp:HiddenField ID="HiddenField5" runat="server" /> 
        <asp:HiddenField ID="HiddenField6" runat="server" />
        <asp:HiddenField ID="HiddenField7" runat="server" />  
        <asp:HiddenField ID="HiddenField8" runat="server" />
        <asp:HiddenField ID="HiddenField9" runat="server" />
        <asp:HiddenField ID="HiddenField10" runat="server" />
        <asp:HiddenField ID="HiddenField11" runat="server" /> 
        <asp:HiddenField ID="HiddenField12" runat="server" />
         <asp:HiddenField ID="HiddenFieldPost" runat="server" />    
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=left nowrap><font size=+0><b>Print Purchase Order&nbsp;</b></font></TD>
          
          
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       
      <fieldset class="shade">
      <table class="shade" >
      <tr><td><table>
      <tr>
         <td align="right" style="padding-right: 5px"><b>Begining PO#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="bepoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="customerpolook(1); return false"><asp:Image ID="pofglLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending PO#:</b></td>
          <td nowrap><asp:TextBox MaxLength="8" ID="endpoTextBox" Width="100px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="customerpolook(2); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Vendor#:</b></td><td>
          <asp:TextBox ID="bevendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="vendorlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Vendor#:</b></td><td><asp:TextBox ID="endvendTextBox" MaxLength="8" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="vendorlook(2); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
        </td>
      </tr>         
        </table></td></tr>        
        
        <tr></tr><tr></tr>
        <tr><td><table>
            <tr><td><b><asp:CheckBox ID="CheckBox1" Text="Do you want to reprint the PO's? " runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox2" Text="Do you want to reprint closed PO's? " runat="server" /></b>
            </td></tr>                         
            <tr><td><b><asp:CheckBox ID="CheckBox3" Text="Do you want to print deleted line items?" runat="server" /></b>
            </td>
            <td><b><asp:CheckBox ID="CheckBox4" Text="Print Terms and Conditions? " runat="server" /></b>
            </td></tr>            
            <tr><td><b><asp:CheckBox ID="CheckBox5" Text="Print Specification Notes? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox6" Text="Transfer to Corrugator? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox7" Text="Group Notes on Same Page? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox8" Text="Summarize by Item Code/Job? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox9" Text="Print FG Item Description 3 Line? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox10" Text="Print Score Types? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox11" Text="Print Metric? " runat="server" /></b><br />
            </td></tr>
            <tr><td><b><asp:CheckBox ID="CheckBox12" Text="Print Prices? " runat="server" /></b><br />
            </td></tr>
         </table></td></tr>
                 
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       </fieldset>
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">               
             
              <ItemTemplate>
                  prtpo:
                  <asp:Label ID="prtpoLabel" runat="server" 
                      Text='<%# Bind("prtpo") %>'></asp:Label><br />                 
                  
                  
                  
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectPrintPoReport" TypeName="browspo">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmprtpo" Type="String" />
                  <asp:Parameter Name="prmbegpo" Type="Int32" />
                  <asp:Parameter Name="prmendpo" Type="Int32" />
                  <asp:Parameter Name="prmbegvend" Type="String" />
                  <asp:Parameter Name="prmendvend" Type="String" />
                  <asp:Parameter Name="prmreprt" Type="String" />
                  <asp:Parameter Name="prmreprtcl" Type="String" />
                  <asp:Parameter Name="prmdelete" Type="String" />
                  <asp:Parameter Name="prmprttrm" Type="String" />
                  <asp:Parameter Name="prmspec" Type="String" />
                  <asp:Parameter Name="prmcorr" Type="String" />
                  <asp:Parameter Name="prmgrpnts" Type="String" />
                  <asp:Parameter Name="prmsmmritm" Type="String" />
                  <asp:Parameter Name="prmitmdsr" Type="String" />
                  <asp:Parameter Name="prmscrtyp" Type="String" />
                  <asp:Parameter Name="prmmetric" Type="String" />
                  <asp:Parameter Name="prmprtprice" Type="String" />                  
                  <asp:Parameter Name="prmOut" Type="String" />
                  
                  <asp:Parameter Name="prmysno" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    
    </form>
  </body>
</HTML>


