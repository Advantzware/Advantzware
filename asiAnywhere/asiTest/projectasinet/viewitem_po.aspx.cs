using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
//using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class viewitem_po : System.Web.UI.Page
{
    public viewitem_po()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    string checkcurser = "";
        

    protected void Page_Load(object sender, EventArgs e)
    {
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;       
      
        if (!Page.IsPostBack)
        {
            if (Convert.ToString(Session["view_pord_line_add_insert"]) == "Add")
            {
                FormView3.ChangeMode(FormViewMode.Insert);
                Session["view_pord_line_add_insert"] = null;
            }
            
        }
             
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
        
        //ImageButton brwsorder = (ImageButton)Master.FindControl("view_item");
        //brwsorder.ImageUrl = "Images/view item 1.jpg";
        Image movebutton = (Image)Master.FindControl("Image5");
        movebutton.Visible = false;
              
          
        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

           

        }

    }

    
    
    protected void insertadd(object sender, EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox itemtype = (TextBox)FormView3.FindControl("poItemTypeTextBox");
        TextBox item = (TextBox)FormView3.FindControl("poItemNoTextBox");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        Label swidfrac = (Label)FormView3.FindControl("poWidFraclabel");
        Label slenfrac = (Label)FormView3.FindControl("poLenFracLabel");
        Label sdepfrac = (Label)FormView3.FindControl("poDepFracLabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        TextBox fimsf = (TextBox)FormView3.FindControl("poMsflabel");
        TextBox monh = (TextBox)FormView3.FindControl("poMOnhlabel");
        TextBox mono = (TextBox)FormView3.FindControl("poMOnolabel");
        TextBox mcomm = (TextBox)FormView3.FindControl("poMCommlabel");
        TextBox mback = (TextBox)FormView3.FindControl("poMBacklabel");
        TextBox mavail = (TextBox)FormView3.FindControl("poMAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");

        if (job2.Text == "")
            job2.Text = "0";
        if (sno.Text == "")
            sno.Text = "0";
        if (bno.Text == "")
            bno.Text = "0";
        if (ordno.Text == "")
            ordno.Text = "0";
        if (lineno.Text == "")
            lineno.Text = "0";


        browspo poline = new browspo();

        bool check = poline.ValidateViewItemPo(UserLogin.UserName, "validateupdate", Convert.ToInt32(lineno.Text.Trim()), Convert.ToInt32(Session["pur_ord_po"]), "", "", "", 0, "", item.Text.Trim(), itemname.Text.Trim(), job1.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), Convert.ToInt32(sno.Text.Trim()), 0, 0, cust.Text.Trim(), date.Text.Trim(), itemtype.Text.Trim(), Convert.ToInt32(bno.Text.Trim()), prqtyuom.Text.Trim(), 0, "", "", pruom.Text.Trim(), 0, consuom.Text.Trim(), 0, 0, 0, 0, Gl.Text.Trim(), vendINo.Text.Trim(), "", 0, 0, Convert.ToInt32(ordno.Text.Trim()), 0, "", "", 0, "", "", "", "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "Yes", 0, "", "");


        if (check)
        {
            Session["pur_ord_po_line_no"] = lineno.Text.Trim();

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "update";
            ObjectDataSource1.SelectParameters["prmpoItemType"].DefaultValue = itemtype.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoItemNo"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoJobNo"].DefaultValue = job1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoJobNo2"].DefaultValue = job2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoSNum"].DefaultValue = sno.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoBNum"].DefaultValue = bno.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoDate"].DefaultValue = date.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoStat"].DefaultValue = stat.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoItemName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoOrdQty"].DefaultValue = ordqty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoPrQtyUom"].DefaultValue = prqtyuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoConsQty"].DefaultValue = consqty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoScrConsUom"].DefaultValue = consqtyuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoDscr"].DefaultValue = dscr1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoCost"].DefaultValue = cost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoPrUom"].DefaultValue = pruom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoConsUom"].DefaultValue = consuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoConsCost"].DefaultValue = conscost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoDscr2"].DefaultValue = dscr2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoSetup"].DefaultValue = setup.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoTMsf"].DefaultValue = TMsf.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoDisc"].DefaultValue = discount.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoTonnage"].DefaultValue = tonnage.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoSwid"].DefaultValue = swid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoSlen"].DefaultValue = slen.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoSDep"].DefaultValue = sdep.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoWidFrac"].DefaultValue = swidfrac.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoLenFrac"].DefaultValue = slenfrac.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoDepFrac"].DefaultValue = sdepfrac.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoActNum"].DefaultValue = Gl.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoGlDesc"].DefaultValue = GlDscr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoVendINo"].DefaultValue = vendINo.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoTax"].DefaultValue = Convert.ToString(tax.Checked);
            ObjectDataSource1.SelectParameters["prmpoOverPct"].DefaultValue = overpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoUnderPct"].DefaultValue = underpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoCustNo"].DefaultValue = cust.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoOrdNo"].DefaultValue = ordno.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoTCost"].DefaultValue = Tcost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoPbCst"].DefaultValue = nxtcst.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoFiUom"].DefaultValue = fiuom.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoQOnh"].DefaultValue = qonh.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoQOno"].DefaultValue = qono.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoQComm"].DefaultValue = qcomm.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoQBack"].DefaultValue = qback.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoQAvail"].DefaultValue = qavail.Text.Trim();
            ObjectDataSource1.SelectParameters["prmpoMsf"].DefaultValue = "Yes";
            //ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();
            FormView3.ChangeMode(FormViewMode.ReadOnly);
        }
       

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label itemtype = (Label)FormView3.FindControl("poItemTypelabel");
        Label item = (Label)FormView3.FindControl("poItemNolabel");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmpoItemType"].DefaultValue = itemtype.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoItemNo"].DefaultValue = item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoJobNo"].DefaultValue = job1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoJobNo2"].DefaultValue = job2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoSNum"].DefaultValue = sno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoBNum"].DefaultValue = bno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoDate"].DefaultValue = date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoStat"].DefaultValue = stat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoItemName"].DefaultValue = itemname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoOrdQty"].DefaultValue = ordqty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoPrQtyUom"].DefaultValue = prqtyuom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoConsQty"].DefaultValue = consqty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoScrConsUom"].DefaultValue = consqtyuom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoDscr"].DefaultValue = dscr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoCost"].DefaultValue = cost.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoPrUom"].DefaultValue = pruom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoConsUom"].DefaultValue = consuom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoConsCost"].DefaultValue = conscost.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoDscr2"].DefaultValue = dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoSetup"].DefaultValue = setup.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoTMsf"].DefaultValue = TMsf.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoDisc"].DefaultValue = discount.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoTonnage"].DefaultValue = tonnage.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoSwid"].DefaultValue = swid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoSlen"].DefaultValue = slen.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoSDep"].DefaultValue = sdep.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoActNum"].DefaultValue = Gl.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoGlDesc"].DefaultValue = GlDscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoVendINo"].DefaultValue = vendINo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoTax"].DefaultValue = Convert.ToString(tax.Checked);
        ObjectDataSource1.SelectParameters["prmpoOverPct"].DefaultValue = overpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoUnderPct"].DefaultValue = underpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoCustNo"].DefaultValue = cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoOrdNo"].DefaultValue = ordno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoTCost"].DefaultValue = Tcost.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoPbCst"].DefaultValue = nxtcst.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoFiUom"].DefaultValue = fiuom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoQOnh"].DefaultValue = qonh.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoQOno"].DefaultValue = qono.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoQComm"].DefaultValue = qcomm.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoQBack"].DefaultValue = qback.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpoQAvail"].DefaultValue = qavail.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();
        
        FormView3.ChangeMode(FormViewMode.ReadOnly);  
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label itemtype = (Label)FormView3.FindControl("poItemTypelabel");
        Label item = (Label)FormView3.FindControl("poItemNolabel");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        Label lineno = (Label)FormView3.FindControl("LabelpoLine");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "deletepoline";
        ObjectDataSource1.SelectParameters["prmpoRecKey"].DefaultValue = lineno.Text.Trim();

        FormView3.ChangeMode(FormViewMode.ReadOnly);

    }

    protected void InsertCancelButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "deletepoline";
        ObjectDataSource1.SelectParameters["prmpoRecKey"].DefaultValue = lineno.Text.Trim();

        FormView3.ChangeMode(FormViewMode.ReadOnly);
    }


    protected void FormView3_Unload(object sender, EventArgs e)
    {
        try
        {
            Label lineno = (Label)FormView3.FindControl("LabelpoLine");
            Session["pur_ord_po_line_no"] = lineno.Text;  
           
        }
        catch { }
    }

   

    protected void FormView3_DataBound(object sender, EventArgs e)
    {

        if (FormView3.CurrentMode == FormViewMode.ReadOnly)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            

            Label taxlabel = (Label)FormView3.FindControl("poTextCheckBoxLabel");
            CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
            Label com = (Label)FormView3.FindControl("poahdrlabel");
            Label itemType = (Label)FormView3.FindControl("poItemTypelabel");
            TextBox adder = (TextBox)FormView3.FindControl("poaddressTextBox");
            try
            {
                if (itemType.Text == "FG")
                    com.Text = "Allocated";
                else
                    com.Text = "Commaitted";
            }
            catch { }

            try
            {
                if (taxlabel.Text == "True" || taxlabel.Text == "TRUE")
                {
                    tax.Checked = true;
                }
                else tax.Checked = false;
            }
            catch { }
            try
            {
                if (adder.Text == "")
                    adder.Visible = false;
            }
            catch { }
        }
        if (FormView3.CurrentMode == FormViewMode.Edit)
        {
            TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
            Label com = (Label)FormView3.FindControl("poahdrlabel");
            Label itemType = (Label)FormView3.FindControl("poItemTypelabel");
            TextBox adder = (TextBox)FormView3.FindControl("poaddressTextBox");
            try
            {
                if (itemType.Text == "FG")
                    com.Text = "Allocated";
                else
                    com.Text = "Commaitted";
                job1.Focus();
            }
            catch { }
            Label taxlabel = (Label)FormView3.FindControl("poTextCheckBoxLabel");
            CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
            try
            {
                if (taxlabel.Text == "True" || taxlabel.Text == "TRUE")
                {
                    tax.Checked = true;
                }
                else tax.Checked = false;
            }
            catch { }
            try
            {
                if (adder.Text == "")
                    adder.Visible = false;
            }
            catch { }


        }
        if (FormView3.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            TextBox itemtype = (TextBox)FormView3.FindControl("poItemTypeTextBox");
            TextBox item = (TextBox)FormView3.FindControl("poItemNoTextBox");
            TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
            Label stat = (Label)FormView3.FindControl("poStatlabel");
            TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
            TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
            Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
            TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
            TextBox GlDscr = (TextBox)FormView3.FindControl("poGlDescTextBox");
            TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
            TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
            TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
            CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
            Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");
            TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
            TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
            TextBox adder = (TextBox)FormView3.FindControl("poaddressTextBox");
            try
            {
                browspo pobrws = new browspo();
                DataSet dspo = new DataSet();

                dspo = pobrws.SelectViewItemPo(UserLogin.UserName, "Addnewline", 0, Convert.ToInt32(Session["pur_ord_po"]), "", "", "", 0, "", "", "", "", 0, 0, 0, 0, "", "", "", 0, "", 0, "", "", "", 0, "", 0, 0, 0, 0, "", "", "", 0, 0, 0, 0, "", "", 0, "", "", "", "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "", 0, "", "");

                date.Text = dspo.Tables[0].Rows[0][2].ToString();
                lineno.Text = dspo.Tables[0].Rows[0][0].ToString();
                stat.Text = dspo.Tables[0].Rows[0][6].ToString();
                ordqty.Text = dspo.Tables[0].Rows[0][12].ToString();
                consqty.Text = dspo.Tables[0].Rows[0][19].ToString();
                overpct.Text = dspo.Tables[0].Rows[0][32].ToString();
                underpct.Text = dspo.Tables[0].Rows[0][33].ToString();
                Gl.Text = dspo.Tables[0].Rows[0][29].ToString();
                //GlDscr.Text = dspo.Tables[0].Rows[0][42].ToString();
                if (dspo.Tables[0].Rows[0][31].ToString() == "TRUE")
                {
                    tax.Checked = true;

                }
                sno.Text = "1";
                bno.Text = "0";

                itemtype.Text = "RM";
                item.Focus();
            }
            catch { }
            try
            {
                if (adder.Text == "")
                    adder.Visible = false;
            }
            catch { }
                
        }
    }

    protected void poitemno_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox itemtype = (TextBox)FormView3.FindControl("poItemTypeTextBox");
        TextBox item = (TextBox)FormView3.FindControl("poItemNoTextBox");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        Label swidfrac = (Label)FormView3.FindControl("poWidFraclabel");
        Label slenfrac = (Label)FormView3.FindControl("poLenFracLabel");
        Label sdepfrac = (Label)FormView3.FindControl("poDepFracLabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        TextBox fimsf = (TextBox)FormView3.FindControl("poMsflabel");
        TextBox monh = (TextBox)FormView3.FindControl("poMOnhlabel");
        TextBox mono = (TextBox)FormView3.FindControl("poMOnolabel");
        TextBox mcomm = (TextBox)FormView3.FindControl("poMCommlabel");
        TextBox mback = (TextBox)FormView3.FindControl("poMBacklabel");
        TextBox mavail = (TextBox)FormView3.FindControl("poMAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");
        Label nextpageqty = (Label)FormView3.FindControl("poPbQtyLabel");
        Label nextpricebr = (Label)FormView3.FindControl("poPbCstLabel");

        if (item.Text.Trim() == "")
        {
            item.Focus();
            return;
        }
        string typelabel = "";
        if(itemtype.Text.Trim() == "RM")
            typelabel = "RMItem";
        else
            typelabel = "ItemFG";

        try
        {
            browspo polook = new browspo();
            DataSet ds = new DataSet();

            ds = polook.SelectFgAllItemLook("search", UserLogin.UserName, "i-no", "EQUAL", item.Text.Trim(), "", "", Convert.ToString(Session["pur_ord_po"]), job1.Text.Trim(), job2.Text.Trim(), "", typelabel,Convert.ToInt32(lineno.Text.Trim()),"",0,0,0,"",cust.Text.Trim(),0,"","","",0);

            if (ds.Tables[0].Rows.Count == 0)
            {
                item.Focus();
                return;
            }
            else
            {
                itemname.Text = ds.Tables[0].Rows[0][1].ToString();
                consuom.Text = ds.Tables[0].Rows[0][3].ToString();
                prqtyuom.Text = ds.Tables[0].Rows[0][4].ToString();
                conscost.Text = ds.Tables[0].Rows[0][5].ToString();
                dscr1.Text = ds.Tables[0].Rows[0][6].ToString();
                dscr2.Text = ds.Tables[0].Rows[0][7].ToString();                
                itemtype.Text = ds.Tables[0].Rows[0][8].ToString();

                pruom.Text = ds.Tables[0].Rows[0][12].ToString();
                cost.Text = ds.Tables[0].Rows[0][13].ToString();
                slen.Text = ds.Tables[0].Rows[0][15].ToString();
                swid.Text = ds.Tables[0].Rows[0][14].ToString();
                sdep.Text = ds.Tables[0].Rows[0][16].ToString();
                
               
                swidfrac.Text = ds.Tables[0].Rows[0][17].ToString();
                slenfrac.Text = ds.Tables[0].Rows[0][18].ToString();
                sdepfrac.Text = ds.Tables[0].Rows[0][19].ToString();
                Gl.Text = ds.Tables[0].Rows[0][20].ToString();
                GlDscr.Text = ds.Tables[0].Rows[0][21].ToString();
                vendINo.Text = ds.Tables[0].Rows[0][22].ToString();
                cust.Text = ds.Tables[0].Rows[0][24].ToString();

                qonh.Text = ds.Tables[0].Rows[0][29].ToString();
                qono.Text = ds.Tables[0].Rows[0][30].ToString();
                qcomm.Text = ds.Tables[0].Rows[0][31].ToString();
                qback.Text = ds.Tables[0].Rows[0][32].ToString();
                qavail.Text = ds.Tables[0].Rows[0][33].ToString();
                monh.Text = ds.Tables[0].Rows[0][34].ToString();
                mono.Text = ds.Tables[0].Rows[0][35].ToString();
                mcomm.Text = ds.Tables[0].Rows[0][36].ToString();
                mback.Text = ds.Tables[0].Rows[0][37].ToString();
                mavail.Text = ds.Tables[0].Rows[0][38].ToString();
                fiuom.Text = ds.Tables[0].Rows[0][28].ToString();

                fimsf.Text = ds.Tables[0].Rows[0][12].ToString();
                nextpageqty.Text = ds.Tables[0].Rows[0][39].ToString();
                setup.Text = ds.Tables[0].Rows[0][40].ToString();
                nextpricebr.Text = ds.Tables[0].Rows[0][41].ToString();
                checkcurser = "Item";
                poqty_TextChange(sender, e);
                item.Focus();

            }
        }
        catch { }
    }

    protected void poqty_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox itemtype = (TextBox)FormView3.FindControl("poItemTypeTextBox");
        TextBox item = (TextBox)FormView3.FindControl("poItemNoTextBox");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        Label swidfrac = (Label)FormView3.FindControl("poWidFraclabel");
        Label slenfrac = (Label)FormView3.FindControl("poLenFracLabel");
        Label sdepfrac = (Label)FormView3.FindControl("poDepFracLabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        TextBox fimsf = (TextBox)FormView3.FindControl("poMsflabel");
        TextBox monh = (TextBox)FormView3.FindControl("poMOnhlabel");
        TextBox mono = (TextBox)FormView3.FindControl("poMOnolabel");
        TextBox mcomm = (TextBox)FormView3.FindControl("poMCommlabel");
        TextBox mback = (TextBox)FormView3.FindControl("poMBacklabel");
        TextBox mavail = (TextBox)FormView3.FindControl("poMAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");
        Label com = (Label)FormView3.FindControl("poahdrlabel");

        if (ordqty.Text == "")
            ordqty.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        if (setup.Text == "")
            setup.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";
        if (slen.Text == "")
            slen.Text = "0";
        if (swid.Text == "")
            swid.Text = "0";
        if (job2.Text == "")
            job2.Text = "0";
        if (sno.Text == "")
            sno.Text = "0";
        if (bno.Text == "")
            bno.Text = "0";
        if (lineno.Text == "")
        lineno.Text = "0";
        if (itemtype.Text == "FG")
            com.Text = "Allocated";
        else
            com.Text = "Commaitted";
       
        try
        {
            browspo qtylook = new browspo();
            DataSet ds = new DataSet();
           
           ds = qtylook.CostQty(UserLogin.UserName, "calqty", Convert.ToInt32(lineno.Text.Trim()), Convert.ToInt32(Session["pur_ord_po"]), itemtype.Text.Trim(), Convert.ToDecimal(ordqty.Text.Trim()), Convert.ToDecimal(cost.Text.Trim()), Convert.ToDecimal(setup.Text.Trim()), Convert.ToDecimal(discount.Text.Trim()), prqtyuom.Text.Trim(), pruom.Text.Trim(), Convert.ToDecimal(slen.Text.Trim()), Convert.ToDecimal(swid.Text.Trim()), item.Text.Trim(), consuom.Text.Trim(), Convert.ToString(HiddenField1.Value),cust.Text.Trim(),job1.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), Convert.ToInt32(sno.Text.Trim()), Convert.ToInt32(bno.Text.Trim()));
           
            if (ds.Tables[0].Rows.Count == 0)
            {
                return;
            }
            else
            {
                consqtyuom.Text = ds.Tables[0].Rows[0][0].ToString();
                tonnage.Text = ds.Tables[0].Rows[0][1].ToString();
                Tcost.Text = ds.Tables[0].Rows[0][6].ToString();
                consqty.Text = ds.Tables[0].Rows[0][9].ToString();
                conscost.Text = ds.Tables[0].Rows[0][10].ToString();
                TMsf.Text = ds.Tables[0].Rows[0][12].ToString();
                if (ds.Tables[0].Rows[0][3].ToString() != "0" || ds.Tables[0].Rows[0][3].ToString() != "")
                    cost.Text = ds.Tables[0].Rows[0][3].ToString();
                setup.Text = ds.Tables[0].Rows[0][4].ToString();
                HiddenField1.Value = "False";
                
                if (checkcurser == "Item")
                    item.Focus();
                else if (checkcurser == "Job")
                    job1.Focus();
                else
                    prqtyuom.Focus();

            }
        }
        catch { }
    }

    protected void editpoOrdQty_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label itemtype = (Label)FormView3.FindControl("poItemTypelabel");
        Label item = (Label)FormView3.FindControl("poItemNolabel");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineedit");
        Label com = (Label)FormView3.FindControl("poahdrlabel");


        if (ordqty.Text == "")
            ordqty.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        if (setup.Text == "")
            setup.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";
        if (slen.Text == "")
            slen.Text = "0";
        if (swid.Text == "")
            swid.Text = "0";
        if (job2.Text == "")
            job2.Text = "0";
        if (sno.Text == "")
            sno.Text = "0";
        if (bno.Text == "")
            bno.Text = "0";
        if (itemtype.Text == "FG")
            com.Text = "Allocated";
        else
            com.Text = "Commaitted";

        
        try
        {
            browspo qtylook = new browspo();
            DataSet ds = new DataSet();

            ds = qtylook.CostQty(UserLogin.UserName, "calqty", Convert.ToInt32(lineno.Text.Trim()), Convert.ToInt32(Session["pur_ord_po"]), itemtype.Text.Trim(), Convert.ToDecimal(ordqty.Text.Trim()), Convert.ToDecimal(cost.Text.Trim()), Convert.ToDecimal(setup.Text.Trim()), Convert.ToDecimal(discount.Text.Trim()), prqtyuom.Text.Trim(), pruom.Text.Trim(), Convert.ToDecimal(slen.Text.Trim()), Convert.ToDecimal(swid.Text.Trim()), item.Text.Trim(), consuom.Text.Trim(), Convert.ToString(HiddenField1.Value), cust.Text.Trim(), job1.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), Convert.ToInt32(sno.Text.Trim()), Convert.ToInt32(bno.Text.Trim()));

            if (ds.Tables[0].Rows.Count == 0)
            {
                return;
            }
            else
            {
                consqtyuom.Text = ds.Tables[0].Rows[0][0].ToString();
                tonnage.Text = ds.Tables[0].Rows[0][1].ToString();
                Tcost.Text = ds.Tables[0].Rows[0][6].ToString();
                consqty.Text = ds.Tables[0].Rows[0][9].ToString();
                conscost.Text = ds.Tables[0].Rows[0][10].ToString();
                TMsf.Text = ds.Tables[0].Rows[0][12].ToString();
                if (ds.Tables[0].Rows[0][3].ToString() != "0" || ds.Tables[0].Rows[0][3].ToString() != "")
                    cost.Text = ds.Tables[0].Rows[0][3].ToString();
                setup.Text = ds.Tables[0].Rows[0][4].ToString();
                HiddenField1.Value = "False";
                if (checkcurser == "Item")
                    item.Focus();
                else if (checkcurser == "Job")
                    job1.Focus();
                else
                    prqtyuom.Focus();

            }
        }
        catch { }
    }
    protected void pojobno_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox itemtype = (TextBox)FormView3.FindControl("poItemTypeTextBox");
        TextBox item = (TextBox)FormView3.FindControl("poItemNoTextBox");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        Label swidfrac = (Label)FormView3.FindControl("poWidFraclabel");
        Label slenfrac = (Label)FormView3.FindControl("poLenFracLabel");
        Label sdepfrac = (Label)FormView3.FindControl("poDepFracLabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        TextBox fimsf = (TextBox)FormView3.FindControl("poMsflabel");
        TextBox monh = (TextBox)FormView3.FindControl("poMOnhlabel");
        TextBox mono = (TextBox)FormView3.FindControl("poMOnolabel");
        TextBox mcomm = (TextBox)FormView3.FindControl("poMCommlabel");
        TextBox mback = (TextBox)FormView3.FindControl("poMBacklabel");
        TextBox mavail = (TextBox)FormView3.FindControl("poMAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineinsert");
        Label nextpageqty = (Label)FormView3.FindControl("poPbQtyLabel");
        Label nextpricebr = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox poadder = (TextBox)FormView3.FindControl("poaddressTextBox");


        if (job1.Text.Trim() == "")
        {
            job1.Focus();
            return;
        }
        if (job2.Text == "")
            job2.Text = "0";
        if (sno.Text == "")
            sno.Text = "0";
        if (bno.Text == "")
            bno.Text = "0";
        if (slen.Text == "")
            slen.Text = "0";
        if (swid.Text == "")
            swid.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";
        if (setup.Text == "")
            setup.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";
        
        string error = "";
        try
        {
            browspo joblook = new browspo();
            DataSet ds = new DataSet();

            ds = joblook.SelectjobhdrLook("JobData", UserLogin.UserName, "i-no", "EQUAL", "", item.Text.Trim(), cost.Text.Trim(), job1.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), Convert.ToInt32(sno.Text.Trim()), Convert.ToInt32(bno.Text.Trim()), pruom.Text.Trim(), prqtyuom.Text.Trim(), ordqty.Text.Trim(), Convert.ToDecimal(swid.Text.Trim()), Convert.ToDecimal(slen.Text.Trim()), 0, Convert.ToDecimal(discount.Text.Trim()), Convert.ToDecimal(setup.Text.Trim()), consuom.Text.Trim(), cust.Text.Trim(), itemtype.Text.Trim(), Convert.ToInt32(Session["pur_ord_po"]), Convert.ToInt32(lineno.Text.Trim()), ref error);

            if (error != "")
            {
                if (error == "Invalid Job, try help...")
                    HttpContext.Current.Response.Write("<script>alert('" + error + "')</script>");
                if (error == "Update item on Job file?")
                {
                    if (!ClientScript.IsStartupScriptRegistered("alert"))
                    {
                        Page.ClientScript.RegisterStartupScript

                            (this.GetType(), "alert", "confirmAdd(1);", true);

                    }
                }
            }
            if (error == "")
            {
                if (ds.Tables[0].Rows.Count == 0)
                {
                    return;
                }
                else
                {
                    job1.Text = ds.Tables[0].Rows[0][0].ToString();
                    job2.Text = ds.Tables[0].Rows[0][1].ToString();
                    ordno.Text = ds.Tables[0].Rows[0][4].ToString();
                    cust.Text = ds.Tables[0].Rows[0][5].ToString();
                    bno.Text = ds.Tables[0].Rows[0][6].ToString();
                    sno.Text = ds.Tables[0].Rows[0][7].ToString();
                    ordqty.Text = ds.Tables[0].Rows[0][12].ToString();
                    cost.Text = ds.Tables[0].Rows[0][13].ToString();
                    swid.Text = ds.Tables[0].Rows[0][10].ToString();
                    slen.Text = ds.Tables[0].Rows[0][9].ToString();
                    sdep.Text = ds.Tables[0].Rows[0][11].ToString();
                    swidfrac.Text = ds.Tables[0].Rows[0][14].ToString();
                    slenfrac.Text = ds.Tables[0].Rows[0][15].ToString();
                    sdepfrac.Text = ds.Tables[0].Rows[0][16].ToString();
                    conscost.Text = ds.Tables[0].Rows[0][18].ToString();
                    nextpageqty.Text = ds.Tables[0].Rows[0][21].ToString();
                    nextpricebr.Text = ds.Tables[0].Rows[0][22].ToString();
                    if (ds.Tables[0].Rows[0][20].ToString() != "")
                    {
                        poadder.Visible = true;
                        poadder.Text = ds.Tables[0].Rows[0][20].ToString();
                    }
                    checkcurser = "Job";
                    poqty_TextChange(sender, e);
                }
            }
        }
        catch { }

    }


    protected void pojobedit_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label itemtype = (Label)FormView3.FindControl("poItemTypelabel");
        Label item = (Label)FormView3.FindControl("poItemNolabel");
        TextBox job1 = (TextBox)FormView3.FindControl("poJobNoTextBox");
        TextBox job2 = (TextBox)FormView3.FindControl("poJobNo2TextBox");
        TextBox sno = (TextBox)FormView3.FindControl("poSNumTextBox");
        TextBox bno = (TextBox)FormView3.FindControl("poBNumTextBox");
        TextBox date = (TextBox)FormView3.FindControl("poDateTextBox");
        Label stat = (Label)FormView3.FindControl("poStatlabel");
        TextBox itemname = (TextBox)FormView3.FindControl("poItemNameTextBox");
        TextBox ordqty = (TextBox)FormView3.FindControl("poOrdQtyTextBox");
        TextBox prqtyuom = (TextBox)FormView3.FindControl("poPrQtyUomTextBox");
        Label consqty = (Label)FormView3.FindControl("poConsQtylabel");
        Label consqtyuom = (Label)FormView3.FindControl("poScrConsUomlabel");
        TextBox dscr1 = (TextBox)FormView3.FindControl("poDscr1TextBox");
        TextBox cost = (TextBox)FormView3.FindControl("poCostTextBox");
        TextBox pruom = (TextBox)FormView3.FindControl("poPrUomTextBox");
        Label consuom = (Label)FormView3.FindControl("poConsUomlabel");
        Label conscost = (Label)FormView3.FindControl("poConsCostlabel");
        TextBox dscr2 = (TextBox)FormView3.FindControl("poDscr2TextBox");
        TextBox setup = (TextBox)FormView3.FindControl("poSetupTextBox");
        Label TMsf = (Label)FormView3.FindControl("poTMsflabel");
        TextBox discount = (TextBox)FormView3.FindControl("poDiscTextBox");
        Label tonnage = (Label)FormView3.FindControl("poTonnagelabel");
        Label swid = (Label)FormView3.FindControl("poSwidlabel");
        Label slen = (Label)FormView3.FindControl("poSlenlabel");
        Label sdep = (Label)FormView3.FindControl("poSDeplabel");
        TextBox Gl = (TextBox)FormView3.FindControl("poActNumTextBox");
        Label GlDscr = (Label)FormView3.FindControl("poGlDesclabel");
        TextBox vendINo = (TextBox)FormView3.FindControl("poVendINoTextBox");
        CheckBox tax = (CheckBox)FormView3.FindControl("poTaxCheckBox");
        TextBox overpct = (TextBox)FormView3.FindControl("poOverPctTextBox");
        TextBox underpct = (TextBox)FormView3.FindControl("poUnderPctTextBox");
        TextBox cust = (TextBox)FormView3.FindControl("poCustNoTextBox");
        TextBox ordno = (TextBox)FormView3.FindControl("poOrdNoTextBox");
        Label Tcost = (Label)FormView3.FindControl("poTCostLabel");
        Label nxtcst = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox fiuom = (TextBox)FormView3.FindControl("poFiUomlabel");
        TextBox qonh = (TextBox)FormView3.FindControl("poQOnhlabel");
        TextBox qono = (TextBox)FormView3.FindControl("poQOnolabel");
        TextBox qcomm = (TextBox)FormView3.FindControl("poQCommlabel");
        TextBox qback = (TextBox)FormView3.FindControl("poQBacklabel");
        TextBox qavail = (TextBox)FormView3.FindControl("poQAvaillabel");
        Label lineno = (Label)FormView3.FindControl("LabelpoLineedit");
        Label swidfrac = (Label)FormView3.FindControl("poWidFraclabel");
        Label slenfrac = (Label)FormView3.FindControl("poLenFracLabel");
        Label sdepfrac = (Label)FormView3.FindControl("poDepFracLabel");
        Label nextpageqty = (Label)FormView3.FindControl("poPbQtyLabel");
        Label nextpricebr = (Label)FormView3.FindControl("poPbCstLabel");
        TextBox poadder = (TextBox)FormView3.FindControl("poaddressTextBox");

        if (job1.Text.Trim() == "")
        {
            job1.Focus();
            return;
        }
        if (job2.Text == "")
            job2.Text = "0";
        if (sno.Text == "")
            sno.Text = "0";
        if (bno.Text == "")
            bno.Text = "0";
        if (slen.Text == "")
            slen.Text = "0";
        if (swid.Text == "")
            swid.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";
        if (setup.Text == "")
            setup.Text = "0";
        if (cost.Text == "")
            cost.Text = "0";


        string error = "";
        try
        {

            browspo joblook = new browspo();
            DataSet ds = new DataSet();

            ds = joblook.SelectjobhdrLook("JobData", UserLogin.UserName, "i-no", "EQUAL", "", item.Text.Trim(), cost.Text.Trim(), job1.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), Convert.ToInt32(sno.Text.Trim()), Convert.ToInt32(bno.Text.Trim()), pruom.Text.Trim(), prqtyuom.Text.Trim(), ordqty.Text.Trim(), Convert.ToDecimal(swid.Text.Trim()), Convert.ToDecimal(slen.Text.Trim()), 0, Convert.ToDecimal(discount.Text.Trim()), Convert.ToDecimal(setup.Text.Trim()), consuom.Text.Trim(), cust.Text.Trim(), itemtype.Text.Trim(), Convert.ToInt32(Session["pur_ord_po"]), Convert.ToInt32(lineno.Text.Trim()), ref error);


            if (error != "")
            {
                if (error == "Invalid Job, try help...")
                    HttpContext.Current.Response.Write("<script>alert('" + error + "')</script>");
                if (error == "Update item on Job file?")
                {
                    if (!ClientScript.IsStartupScriptRegistered("alert"))
                    {
                        Page.ClientScript.RegisterStartupScript

                            (this.GetType(), "alert", "confirmAdd(2);", true);

                    }
                }
            }
            if (error == "")
            {
                if (ds.Tables[0].Rows.Count == 0)
                {
                    return;
                }
                else
                {
                    job1.Text = ds.Tables[0].Rows[0][0].ToString();
                    job2.Text = ds.Tables[0].Rows[0][1].ToString();
                    ordno.Text = ds.Tables[0].Rows[0][4].ToString();
                    cust.Text = ds.Tables[0].Rows[0][5].ToString();
                    bno.Text = ds.Tables[0].Rows[0][6].ToString();
                    sno.Text = ds.Tables[0].Rows[0][7].ToString();
                    ordqty.Text = ds.Tables[0].Rows[0][12].ToString();
                    cost.Text = ds.Tables[0].Rows[0][13].ToString();
                    swid.Text = ds.Tables[0].Rows[0][10].ToString();
                    slen.Text = ds.Tables[0].Rows[0][9].ToString();
                    sdep.Text = ds.Tables[0].Rows[0][11].ToString();
                    swidfrac.Text = ds.Tables[0].Rows[0][14].ToString();
                    slenfrac.Text = ds.Tables[0].Rows[0][15].ToString();
                    sdepfrac.Text = ds.Tables[0].Rows[0][16].ToString();
                    conscost.Text = ds.Tables[0].Rows[0][18].ToString();

                    nextpageqty.Text = ds.Tables[0].Rows[0][21].ToString();
                    nextpricebr.Text = ds.Tables[0].Rows[0][22].ToString();
                    if (ds.Tables[0].Rows[0][20].ToString() != "")
                    {
                        poadder.Visible = true;
                        poadder.Text = ds.Tables[0].Rows[0][20].ToString();
                    }
                    checkcurser = "Job";
                    editpoOrdQty_TextChange(sender, e);
                }
            }
        }
        catch { }

    }
}
