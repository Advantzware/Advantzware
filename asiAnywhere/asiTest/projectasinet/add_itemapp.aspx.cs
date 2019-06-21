using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for vieworder
/// </summary>
public partial class add_itemapp : System.Web.UI.Page
{
    string mailto = "";
    string user_login = "";
    protected void Page_Load(object sender, EventArgs e)
    {
        // Image move_col = (Image)Master.FindControl("Image5");
        //move_col.Visible = false;

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Convert.ToString(Session["view_item_mode_app"]) == "add")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            
        }
        else if (Convert.ToString(Session["view_item_mode_app"]) == "edit")
        {
            showinfo.Visible = false;
            if (Session["view_line_est_app"] != null)
               
            FormView1.ChangeMode(FormViewMode.Edit);
        }

        if (Request.QueryString["webitem"] == "insert")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
        }
        if (Request.QueryString["webitem"] == "edit")
        {
            showinfo.Visible = false;
            FormView1.ChangeMode(FormViewMode.Edit);
        }
        if (Request.QueryString["webitem"] == "item")
        {
            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        //if (Request.QueryString["webitem"] == "mail")
        //{
        //    try
        //    {
        //        Label ord_label = (Label)FormView1.FindControl("ord_label");
        //        Label item = (Label)FormView1.FindControl("Item1Label");

        //        Order mail = new Order();
        //        mail.OrderMail(Convert.ToString(UserLogin.UserName), "MailOrder", ord_label.Text.Trim(), "", item.Text);
        //        Response.Write("<script> location.href='view_item_estimate.aspx'</script>");

        //    }
        //    catch
        //    { }
        //}


        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_item_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            try
            {

                ord_label.Text = Convert.ToString(Session["order_estweb_app"]);
                cust_label.Text = Convert.ToString(Session["order_entry_cust_no_app"]);
                type_label.Text = Convert.ToString(Session["order_entry_type_app"]);
                last_label.Text = Convert.ToString(Session["Lastuser_id"]);
                type_label.Text = Convert.ToString(Session["order_entry_type_app"]);
                status_label.Text = Convert.ToString(Session["Status_id"]);
            }
            catch { }

            user_login = aUsers;
            lblComp.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        //Label name = (Label)Master.FindControl("lbl_page");
        //name.Text = "View Item";

        //Label userlog = (Label)Master.FindControl("lblUser");
        //ImageButton viewitem = (ImageButton)Master.FindControl("viewitem");
        //viewitem.ImageUrl = "~/img/viewitem1.jpg";

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                if (FormView1.DataItemCount == 0)
                {
                    
                    showinfo.Visible = true;
                }
                else
                {
                  
                    showinfo.Visible = false;
                }
                try
                {
                    Label ordLabel = (Label)FormView1.FindControl("ord_label");
                    Label CustLabel = (Label)FormView1.FindControl("cust_label");
                    ordLabel.Text = Convert.ToString(Session["order_estweb_app"]);
                    CustLabel.Text = Convert.ToString(Session["order_entry_cust_no_app"]);
                }
                catch { }
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_item_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    try
                    {
                        if (aUsers == "internal")
                        {
                            Button del = (Button)FormView1.FindControl("DeleteButton");
                            del.Visible = false;
                        }
                    }
                    catch { };

                    if (aUsers == "external")
                    {
                        try
                        {
                            Label cost1 = (Label)FormView1.FindControl("costLabel");
                            Label cost2 = (Label)FormView1.FindControl("cost2Label");
                            //Label uom1 = (Label)FormView1.FindControl("uomLabel2");
                            //Label uom2 = (Label)FormView1.FindControl("uomLabel");
                            //uom1.Visible = false;
                            //uom2.Visible = false;
                            cost1.Visible = false;
                            cost2.Visible = false;
                        }
                        catch { }
                        try
                        {
                            //Label estimate = (Label)FormView1.FindControl("est_noLabel");
                            Label partlabel = (Label)FormView1.FindControl("partLabel");
                            Label repeat = (Label)FormView1.FindControl("vTypeLabel");
                            Label boardvendor = (Label)FormView1.FindControl("vBoardVenLabel");
                            //Label estlabel = (Label)FormView1.FindControl("Estlabel");
                            Label boardpolabel = (Label)FormView1.FindControl("bpolabel");
                            Label replabel = (Label)FormView1.FindControl("orlabel");
                            Label boardpo = (Label)FormView1.FindControl("vPonoLabel");
                            Label boardvendlabel = (Label)FormView1.FindControl("bvendlabel");
                            Label minvlabel = (Label)FormView1.FindControl("Manlabel");
                            CheckBox minv = (CheckBox)FormView1.FindControl("vManagCheckBox");
                            Label unitlabel = (Label)FormView1.FindControl("qtyunitLabel");
                            Label unitpalletlabel = (Label)FormView1.FindControl("unitpalletLabel");
                            Label qty = (Label)FormView1.FindControl("counterLabel");
                            Label partial = (Label)FormView1.FindControl("vPartialLabel");
                            Label units = (Label)FormView1.FindControl("VcasUnitLabel");

                            //replabel.Visible = false;
                            boardpolabel.Visible = false;
                            boardvendlabel.Visible = false;
                            //estlabel.Visible = false;
                            //estimate.Visible = false;
                            minvlabel.Visible = false;
                            minv.Visible = false;
                            boardpo.Visible = false;
                            boardvendor.Visible = false;
                            //repeat.Enabled = false;
                            partlabel.Visible = false;
                            //unitlabel.Enabled = false;
                            unitpalletlabel.Visible = false;
                            //qty.Enabled = false;
                            partial.Visible = false;
                            units.Visible = false;
                        }
                        catch { }
                    }

                }

            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                
                showinfo.Visible = true;
                TextBox addquote = (TextBox)FormView1.FindControl("vQnoTextBox");
                Label addjob = (Label)FormView1.FindControl("job_noTextBox");
                Label addjob2 = (Label)FormView1.FindControl("job_noTextBox2");
                TextBox vType = (TextBox)FormView1.FindControl("vTypeTextBox");
                vType.Text = "EA";
                if (Convert.ToInt32(Session["order_entry_cust_quote_app"]) != 0)
                {
                    addquote.Text = Convert.ToString(Session["order_entry_quote_app"]);
                    addquote.Enabled = false;

                }
                if (Convert.ToString(Session["order_entry_job_no_1_app"]) != "&nbsp;" || Session["order_entry_job_no_1_app"] != null)
                {
                    if (Convert.ToString(Session["order_entry_job_no_1_app"]) != "&nbsp;")
                        addjob.Text = Convert.ToString(Session["order_entry_job_no_1_app"]);
                }
                addjob2.Text = Convert.ToString(Session["order_entry_job_no_2_app"]);

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_item_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    TextBox sman1 = (TextBox)FormView1.FindControl("vSmanTextBox");
                    TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
                    TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");

                    TextBox sname1 = (TextBox)FormView1.FindControl("vSnameTextBox");
                    TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
                    TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");

                    TextBox overrun = (TextBox)FormView1.FindControl("vOverTextBox");
                    TextBox underrun = (TextBox)FormView1.FindControl("vUnderTextBox");
                    TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
                    TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
                    TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
                    DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
                    DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
                    DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");


                    CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");

                    sman1.Text = Convert.ToString(Session["order_entry_sman1_app"]);
                    sman2.Text = Convert.ToString(Session["order_entry_sman2_app"]);
                    sman3.Text = Convert.ToString(Session["order_entry_sman3_app"]);

                    sname1.Text = Convert.ToString(Session["order_entry_sname1_app"]);
                    sname2.Text = Convert.ToString(Session["order_entry_sname2_app"]);
                    sname3.Text = Convert.ToString(Session["order_entry_sname3_app"]);

                    overrun.Text = Convert.ToString(Session["order_entry_over_app"]);
                    underrun.Text = Convert.ToString(Session["order_entry_under_app"]);
                    duedate.Text = Convert.ToString(Session["order_entry_req_date_app"]);
                    promisedate.Text = Convert.ToString(Session["order_entry_req_date_app"]);
                    custpo.Text = Convert.ToString(Session["order_entry_customer_po_app"]);
                    if (custpo.Text == "&nbsp;")
                    {
                        custpo.Text = "";
                    }


                    type.SelectedValue = Convert.ToString(Session["order_entry_type_app"]);
                    priority.SelectedValue = Convert.ToString(Session["order_entry_req_code_app"]);
                    priority2.SelectedValue = Convert.ToString(Session["order_entry_prom_code_app"]);
                    if (Convert.ToString(Session["order_entry_tax_app"]) == "Y")
                        tax.Checked = true;
                    if (Convert.ToString(Session["order_entry_tax_app"]) == "N")
                        tax.Checked = false;
                    Image imagepro = (Image)FormView1.FindControl("Image1");
                    imagepro.Visible = false;
                    promisedate.Enabled = false;
                    TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
                    units.Text = "1";

                    if (aUsers == "external")
                    {
                        //TextBox estnum = (TextBox)FormView1.FindControl("est_noTextBox");
                        CheckBox minvch = (CheckBox)FormView1.FindControl("vManagCheckBox");
                        DropDownList rep = (DropDownList)FormView1.FindControl("DropDownList1");
                        TextBox bpotext = (TextBox)FormView1.FindControl("vPonoTextBox");
                        TextBox bvendtext = (TextBox)FormView1.FindControl("vBoardVenTextBox");
                        Label boardpolabel = (Label)FormView1.FindControl("bpolabel");
                        Label replabel = (Label)FormView1.FindControl("orlabel");
                        Label boardvendlabel = (Label)FormView1.FindControl("bvendlabel");
                        Label minvlabel = (Label)FormView1.FindControl("Manlabel");

                        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
                        TextBox dscr1 = (TextBox)FormView1.FindControl("DscrTextBox");
                        TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
                        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
                        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
                        //CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
                        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
                        TextBox qty = (TextBox)FormView1.FindControl("counterTextBox");
                        TextBox tprice = (TextBox)FormView1.FindControl("extpriceTextBox");
                        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
                        TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
                        TextBox prodate = (TextBox)FormView1.FindControl("promisdateTextBox");
                        TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
                        Label job = (Label)FormView1.FindControl("job_noTextBox");
                        Label job2 = (Label)FormView1.FindControl("job_noTextBox2");
                        TextBox typek = (TextBox)FormView1.FindControl("vTypeTextBox");
                        TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
                        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");
                        Image imapo = (Image)FormView1.FindControl("Image3");
                        Image image1 = (Image)FormView1.FindControl("Image12");
                        Image image2 = (Image)FormView1.FindControl("Image4");
                        //Image image3 = (Image)FormView1.FindControl("Image5");
                        //Image image4 = (Image)FormView1.FindControl("Image8");
                        Label cost1 = (Label)FormView1.FindControl("cost1Label");
                        TextBox cost2 = (TextBox)FormView1.FindControl("CostTextBox");
                        //Label uom1 = (Label)FormView1.FindControl("uomLabel2");
                        TextBox uom2 = (TextBox)FormView1.FindControl("TextBox1");
                        Label partlabel = (Label)FormView1.FindControl("partLabel");
                        Label unitlabel = (Label)FormView1.FindControl("qtyunitLabel");
                        Label unitpalletlabel = (Label)FormView1.FindControl("unitpalletLabel");

                        //uom1.Visible = false;
                        uom.Enabled = false;
                        uom2.Enabled = false;
                        cost1.Visible = false;
                        cost2.Visible = false;
                        partlabel.Visible = false;
                        unitpalletlabel.Visible = false;
                        //estnum.Enabled = false;
                        itemname.Enabled = false;
                        dscr1.Enabled = false;
                        dscr2.Enabled = false;
                        sman1.Enabled = false;
                        sman2.Enabled = false;
                        sman3.Enabled = false;
                        sname1.Enabled = false;
                        sname2.Enabled = false;
                        sname3.Enabled = false;

                        price.Enabled = false;
                        //uom.Enabled = false;
                        tax.Enabled = false;
                        discount.Enabled = false;
                        qty.Enabled = false;
                        tprice.Enabled = false;
                        partial.Visible = false;
                        units.Visible = false;
                        //priority.Enabled = false;
                        priority2.Enabled = false;
                        prodate.Enabled = false;
                        job.Enabled = false;
                        job2.Enabled = false;


                        minvch.Visible = false;
                        rep.Visible = false;
                        bpotext.Visible = false;
                        bvendtext.Visible = false;
                        boardpolabel.Visible = false;
                        replabel.Visible = false;
                        boardvendlabel.Visible = false;
                        minvlabel.Visible = false;

                        imapo.Visible = false;
                        image1.Visible = false;
                        image2.Visible = false;
                        //image3.Visible = false;
                       // image4.Visible = false;
                    }
                }
            }

            if (FormView1.CurrentMode == FormViewMode.Edit)
            {


                TextBox vType = (TextBox)FormView1.FindControl("vTypeTextBox");
                vType.Text = "EA";

                Label ordLabel = (Label)FormView1.FindControl("ord_label");
                Label CustLabel = (Label)FormView1.FindControl("cust_label");
                Label last_label = (Label)FormView1.FindControl("last_label");
                Label status_label = (Label)FormView1.FindControl("status_label");

                ordLabel.Text = Convert.ToString(Session["order_estweb_app"]);
                CustLabel.Text = Convert.ToString(Session["order_entry_cust_no_app"]);
                last_label.Text = Convert.ToString(Session["Lastuser_id"]);
                
                status_label.Text = Convert.ToString(Session["Status_id"]);
                TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
                custpart.Focus();
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_item_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    TextBox sman1 = (TextBox)FormView1.FindControl("vSmanTextBox");
                    Hiddensman.Value = sman1.Text;
                    TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
                    TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");

                    TextBox sname1 = (TextBox)FormView1.FindControl("vSnameTextBox");
                    Hiddensname.Value = sname1.Text;
                    TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
                    TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");

                    TextBox overrun = (TextBox)FormView1.FindControl("vOverTextBox");
                    TextBox underrun = (TextBox)FormView1.FindControl("vUnderTextBox");
                    TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
                    TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
                    TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
                    DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
                    DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
                    DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");
                    HiddenDropdown1.Value = type.Text;
                    promisedate.Enabled = false;

                    CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
                    Image imagepro = (Image)FormView1.FindControl("Image1");
                    imagepro.Visible = false;

                    if (aUsers == "external")
                    {
                        CheckBox minvch = (CheckBox)FormView1.FindControl("vManagCheckBox");
                        DropDownList rep = (DropDownList)FormView1.FindControl("DropDownList1");
                        TextBox bpotext = (TextBox)FormView1.FindControl("vPonoTextBox");
                        TextBox bvendtext = (TextBox)FormView1.FindControl("vBoardVenTextBox");
                        Label boardpolabel = (Label)FormView1.FindControl("bpolabel");
                        Label replabel = (Label)FormView1.FindControl("orlabel");
                        Label boardvendlabel = (Label)FormView1.FindControl("bvendlabel");
                        Label minvlabel = (Label)FormView1.FindControl("Manlabel");

                        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
                        HiddenFieldname.Value = itemname.Text;
                        TextBox dscr1 = (TextBox)FormView1.FindControl("DscrTextBox");
                        Hiddencustdesc.Value = dscr1.Text;
                        TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
                        Hiddencustdesc2.Value = dscr2.Text;

                        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
                        Hiddenprice.Value = price.Text;
                        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
                        HiddenTextBox1.Value = uom.Text;
                        //CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
                        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
                        Hiddendiscount.Value = discount.Text;
                        TextBox qty = (TextBox)FormView1.FindControl("counterTextBox");
                        Hiddencounter.Value = qty.Text;
                        TextBox tprice = (TextBox)FormView1.FindControl("extpriceTextBox");
                        Hiddentotal.Value = tprice.Text;
                        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
                        TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
                        Hiddencasunit.Value = units.Text;
                        TextBox prodate = (TextBox)FormView1.FindControl("promisdateTextBox");
                        TextBox typek = (TextBox)FormView1.FindControl("vTypeTextBox");
                        TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
                        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");

                        Image imapo = (Image)FormView1.FindControl("Image3");
                        Image image1 = (Image)FormView1.FindControl("Image12");
                        Image image2 = (Image)FormView1.FindControl("Image2");
                                              

                        Label cost1 = (Label)FormView1.FindControl("cost1Label");
                        TextBox cost2 = (TextBox)FormView1.FindControl("CostTextBox");
                        Hiddencost.Value = cost2.Text;
                        //Label uom1 = (Label)FormView1.FindControl("uomLabel2");
                        TextBox uom2 = (TextBox)FormView1.FindControl("TextBox1");
                        Label partlabel = (Label)FormView1.FindControl("partLabel");
                        Label unitlabel = (Label)FormView1.FindControl("qtyunitLabel");
                        Label unitpalletlabel = (Label)FormView1.FindControl("unitpalletLabel");

                        //uom1.Visible = false;
                        uom.Enabled = false;
                        uom2.Enabled = false;
                        cost1.Visible = false;
                        cost2.Visible = false;
                        partlabel.Visible = false;
                        unitpalletlabel.Visible = false;
                        itemname.Enabled = false;
                        dscr1.Enabled = false;
                        dscr2.Enabled = false;
                        sman1.Enabled = false;
                        sman2.Enabled = false;
                        sman3.Enabled = false;
                        sname1.Enabled = false;
                        sname2.Enabled = false;
                        sname3.Enabled = false;

                        price.Enabled = false;
                        //uom.Enabled = false;
                        tax.Enabled = false;
                        discount.Enabled = false;
                        qty.Enabled = false;
                        tprice.Enabled = false;
                        partial.Visible = false;
                        units.Visible = false;
                        //priority.Enabled = false;
                        priority2.Enabled = false;
                        prodate.Enabled = false;

                        //typek.Enabled = false;
                        //cost.Enabled = false;
                        //line.Enabled = false;

                        minvch.Visible = false;
                        rep.Visible = false;
                        bpotext.Visible = false;
                        bvendtext.Visible = false;
                        boardpolabel.Visible = false;
                        replabel.Visible = false;
                        boardvendlabel.Visible = false;
                        minvlabel.Visible = false;
                        //overrun.Enabled = false;
                        //underrun.Enabled = false;

                        imapo.Visible = false;
                        image1.Visible = false;
                        image2.Visible = false;                        
                        
                    }
                    try
                    {
                        Label vlblUser = (Label)FormView1.FindControl("vlblUser");
                        Label vlblComp = (Label)FormView1.FindControl("vlblComp");
                        vlblUser.Text = UserLogin.UserName;
                        vlblComp.Text = PrmComp;
                    }
                    catch { }
                }
            }
        }
        catch
        {
        //    return;
        }

    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
        TextBox quantity = (TextBox)FormView1.FindControl("quantityTextBox");
        CheckBox minv = (CheckBox)FormView1.FindControl("vManagCheckBox");
        //TextBox original = (TextBox)FormView1.FindControl("vTypeTextBox");
        DropDownList original = (DropDownList)FormView1.FindControl("DropDownList1");
        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
        CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
        TextBox totprice = (TextBox)FormView1.FindControl("extpriceTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
        TextBox qtyunit = (TextBox)FormView1.FindControl("counterTextBox");
        TextBox description = (TextBox)FormView1.FindControl("DscrTextBox");
        TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
        TextBox description2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
        TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");
        //TextBox priority = (TextBox)FormView1.FindControl("promisedTextBox");
        //TextBox priority2 = (TextBox)FormView1.FindControl("requestedTextBox");
        DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
        DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
        TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
        TextBox boardpo = (TextBox)FormView1.FindControl("vPonoTextBox");
        TextBox boardvend = (TextBox)FormView1.FindControl("vBoardVenTextBox");

        TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("vSmanTextBox");
        TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
        TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");
        TextBox over = (TextBox)FormView1.FindControl("vOverTextBox");
        TextBox under = (TextBox)FormView1.FindControl("vUnderTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("vSnameTextBox");
        TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
        TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");



        TextBox Cost = (TextBox)FormView1.FindControl("CostTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");

        if (minv.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (tax.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (quantity.Text == "")
            quantity.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (partial.Text == "")
            partial.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";

        if (qtyunit.Text == "")
            qtyunit.Text = "0";
        if (units.Text == "")
            units.Text = "0";

        if (line.Text == "")
            line.Text = "0";
        if (boardpo.Text == "")
            boardpo.Text = "0";
        if (boardvend.Text == "")
            boardvend.Text = "0";
        if (over.Text == "")
            over.Text = "0";

        if (under.Text == "")
            under.Text = "0";
        if (Hiddenspct.Value == "")
            Hiddenspct.Value = "0";
        if (Hiddencomm.Value == "")
            Hiddencomm.Value = "0";

        if (Hiddendiscount.Value == "")
            Hiddendiscount.Value = "0";
        if (Hiddencounter.Value == "")
            Hiddencounter.Value = "0";
        if (Hiddencasunit.Value == "")
            Hiddencasunit.Value = "0";
        if (Hiddencost.Value == "")
            Hiddencost.Value = "0";
        if (Cost.Text == "")
            Cost.Text = "0";
        if (Convert.ToInt32(quantity.Text) <= 0)
        {
            return;
        }

        orderentry ord = new orderentry();
        bool check = ord.AddViewItemEstimate(UserLogin.UserName, "validateUpdate", Convert.ToInt32(Session["order_estweb_app"]), 0, Convert.ToString(HiddenEstmate.Value), fgitem.Text, custpart.Text, Convert.ToDecimal(quantity.Text.Trim()), itemname.Text, description.Text, description2.Text, "", Convert.ToDecimal(price.Text.Trim()), uom.Text, HiddenField2.Value, custpo.Text, "", 0, Convert.ToDecimal(discount.Text), priority2.Text, Convert.ToDateTime(duedate.Text), 0, priority.SelectedValue, Convert.ToDateTime(Session["order_entry_req_date_app"]), 0, Convert.ToInt32(qtyunit.Text), Convert.ToDecimal(partial.Text), Convert.ToInt32(units.Text), Convert.ToInt32(line.Text), Convert.ToInt32(boardpo.Text), sman.Text, sman2.Text, sman3.Text, original.SelectedValue, Convert.ToDecimal(over.Text), Convert.ToDecimal(under.Text), boardvend.Text, HiddenField1.Value, "", sname.Text, sname2.Text, sname3.Text, Convert.ToDecimal(Hiddenspct.Value), 0, 0, Convert.ToDecimal(Hiddencomm.Value), 0, 0, Convert.ToDecimal(Cost.Text), 0, "y");
       string  value = Convert.ToString(check);

       if (value == "True")
       {

           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
           //ObjectDataSource1.SelectParameters["prmQno"].DefaultValue = quote.Text.Trim();
           ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = quantity.Text.Trim();
           ObjectDataSource1.SelectParameters["prmManag"].DefaultValue = HiddenField1.Value;
           ObjectDataSource1.SelectParameters["prmType"].DefaultValue = original.SelectedValue;
           ObjectDataSource1.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
           ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = HiddenTextBox1.Value;
           ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField2.Value;
           ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
           ObjectDataSource1.SelectParameters["prmTPrice"].DefaultValue = Hiddentotal.Value;
           ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
           ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
           ObjectDataSource1.SelectParameters["prmCas"].DefaultValue = qtyunit.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPartdscr"].DefaultValue = description.Text.Trim();
           ObjectDataSource1.SelectParameters["prmUnit"].DefaultValue = units.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPartdscr1"].DefaultValue = description2.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPoNum"].DefaultValue = custpo.Text.Trim();
           ObjectDataSource1.SelectParameters["prmEnum"].DefaultValue = line.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPromCode"].DefaultValue = priority.SelectedValue;
           ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = priority2.SelectedValue;
           ObjectDataSource1.SelectParameters["prmReqDate"].DefaultValue = duedate.Text.Trim();
           //ObjectDataSource1.SelectParameters["prmPromDate"].DefaultValue = promisedate.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPrevOrder"].DefaultValue = boardpo.Text.Trim();
           ObjectDataSource1.SelectParameters["prmVend"].DefaultValue = boardvend.Text.Trim();
           ObjectDataSource1.SelectParameters["prmOver"].DefaultValue = over.Text.Trim();
           ObjectDataSource1.SelectParameters["prmUnder"].DefaultValue = under.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSman2"].DefaultValue = sman2.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSman3"].DefaultValue = sman3.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = sname.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSname2"].DefaultValue = sname2.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSname3"].DefaultValue = sname3.Text.Trim();
           ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = fgitem.Text.Trim();
           ObjectDataSource1.SelectParameters["prmPartNum"].DefaultValue = custpart.Text.Trim();
           ObjectDataSource1.SelectParameters["prmSpct"].DefaultValue = Hiddenspct.Value;
           ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = Hiddencomm.Value;


           ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = Cost.Text.Trim();

           FormView1.ChangeMode(FormViewMode.ReadOnly);

           if (Session["User"] != null)
           {
               string vUserId = UserLogin.UserName;
               string vPage = "view_item_estimate.aspx";
               string aUsers = null;
               string PrmComp = null;
               bool vCanCreate = false;
               bool vCanRun = false;
               bool vCanUpdate = false;
               bool vCanDelete = false;

               func1 f1 = new func1();

               f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
               if (vCanUpdate == false)
               {
                   Response.Write("<script>alert('Sorry! You do not have permission to Update Record');</script>");
                   Response.Write("<script>window.location.href = 'releases.aspx';</script>");

               }
               if (aUsers == "external")
               {
                   ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";

                   //ObjectDataSource1.SelectParameters["prmManag"].DefaultValue = HiddenField1.Value;
                   ObjectDataSource1.SelectParameters["prmType"].DefaultValue = HiddenDropdown1.Value;
                   ObjectDataSource1.SelectParameters["prmPrice"].DefaultValue = Hiddenprice.Value;
                   ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = HiddenTextBox1.Value;
                   //ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField2.Value;
                   ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = HiddenFieldname.Value;
                   ObjectDataSource1.SelectParameters["prmTPrice"].DefaultValue = Hiddentotal.Value;
                   //ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                   ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = Hiddendiscount.Value;
                   ObjectDataSource1.SelectParameters["prmCas"].DefaultValue = Hiddencounter.Value;
                   ObjectDataSource1.SelectParameters["prmPartdscr"].DefaultValue = Hiddencustdesc.Value;
                   ObjectDataSource1.SelectParameters["prmUnit"].DefaultValue = Hiddencasunit.Value;
                   ObjectDataSource1.SelectParameters["prmPartdscr1"].DefaultValue = Hiddencustdesc2.Value;
                   ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = Hiddencost.Value;


                   ObjectDataSource1.SelectParameters["prmReqDate"].DefaultValue = duedate.Text.Trim();
                   ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = Hiddensman.Value;
                   ObjectDataSource1.SelectParameters["prmSman2"].DefaultValue = Hiddensman2.Value;
                   ObjectDataSource1.SelectParameters["prmSman3"].DefaultValue = Hiddensman3.Value;
                   ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = Hiddensname.Value;
                   ObjectDataSource1.SelectParameters["prmSname2"].DefaultValue = Hiddensname2.Value;
                   ObjectDataSource1.SelectParameters["prmSname3"].DefaultValue = Hiddensname3.Value;
               }
           }
           Session["view_item_mode_app"] = null;
           Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");
       }
       custpart.Focus();
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_item_estimate.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            if (vCanCreate == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission to Add a Record');</script>");
                Response.Write("<script>window.location.href = 'releases.aspx';</script>");

            }
            user_login = aUsers;
        }
        TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
        TextBox estno = (TextBox)FormView1.FindControl("est_noTextBox");
        Label job = (Label)FormView1.FindControl("job_noTextBox");
        Label job2 = (Label)FormView1.FindControl("job_noTextBox2");
        TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
        TextBox quantity = (TextBox)FormView1.FindControl("quantityTextBox");
        CheckBox minv = (CheckBox)FormView1.FindControl("vManagCheckBox");
        //TextBox original = (TextBox)FormView1.FindControl("vTypeTextBox");
        DropDownList original = (DropDownList)FormView1.FindControl("DropDownList1");
        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
        CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
        TextBox totprice = (TextBox)FormView1.FindControl("extpriceTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
        TextBox qtyunit = (TextBox)FormView1.FindControl("counterTextBox");
        TextBox description = (TextBox)FormView1.FindControl("DscrTextBox");
        TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
        TextBox description2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
        TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");
        //TextBox priority = (TextBox)FormView1.FindControl("promisedTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
        TextBox boardpo = (TextBox)FormView1.FindControl("vPonoTextBox");
        TextBox boardvend = (TextBox)FormView1.FindControl("vBoardVenTextBox");
        //TextBox priority2 = (TextBox)FormView1.FindControl("requestedTextBox");
        DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
        DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
        TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("vSmanTextBox");
        TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
        TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");
        TextBox over = (TextBox)FormView1.FindControl("vOverTextBox");
        TextBox under = (TextBox)FormView1.FindControl("vUnderTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("vSnameTextBox");
        TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
        TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");



        TextBox Cost = (TextBox)FormView1.FindControl("CostTextBox");

        if (minv.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (tax.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (quote.Text.Trim() == "")
        {
            quote.Text = "0";
        }
        if (Convert.ToString(job.Text.Trim()) == "")
        {
            if (Convert.ToInt32(quote.Text.Trim()) > 0)
            {
                job.Text = Convert.ToString(Session["order_estweb_app"]);
            }
        }

        if (quantity.Text == "")
            quantity.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (partial.Text == "")
            partial.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";

        if (qtyunit.Text == "")
            qtyunit.Text = "0";
        if (units.Text == "")
            units.Text = "0";

        if (line.Text == "")
            line.Text = "0";
        if (boardpo.Text == "")
            boardpo.Text = "0";
        if (boardvend.Text == "")
            boardvend.Text = "0";
        if (over.Text == "")
            over.Text = "0";

        if (under.Text == "")
            under.Text = "0";
        if (Hiddenspct.Value == "")
            Hiddenspct.Value = "0";
        if (Hiddencomm.Value == "")
            Hiddencomm.Value = "0";

        if (Hiddendiscount.Value == "")
            Hiddendiscount.Value = "0";
        if (Hiddencounter.Value == "")
            Hiddencounter.Value = "0";
        if (Hiddencasunit.Value == "")
            Hiddencasunit.Value = "0";
        if (Hiddencost.Value == "")
            Hiddencost.Value = "0";
        if (Hiddenprice.Value == "")
            Hiddenprice.Value = "0";
        if (Cost.Text == "")
            Cost.Text = "0";
        if (duedate.Text == "")
            duedate.Text = "01/01/2014";
        string cError = "";

        try
        {
            string value = "";
            if (user_login == "internal")
            {
                orderentry ord = new orderentry();
                bool check = ord.AddViewItemEstimateApp(UserLogin.UserName, "AddValidate", Convert.ToInt32(Session["order_estweb_app"]), 0, Convert.ToString(HiddenEstmate.Value), fgitem.Text, custpart.Text, Convert.ToDecimal(quantity.Text.Trim()), itemname.Text, description.Text, description2.Text, "", Convert.ToDecimal(price.Text.Trim()), uom.Text, HiddenField2.Value, custpo.Text, "", 0, Convert.ToDecimal(discount.Text), priority2.Text, Convert.ToDateTime(duedate.Text), 0, priority.SelectedValue, Convert.ToDateTime(Session["order_entry_req_date_app"]), 0, Convert.ToInt32(qtyunit.Text), Convert.ToDecimal(partial.Text), Convert.ToInt32(units.Text), Convert.ToInt32(line.Text), Convert.ToInt32(boardpo.Text), sman.Text, sman2.Text, sman3.Text, original.SelectedValue, Convert.ToDecimal(over.Text), Convert.ToDecimal(under.Text), boardvend.Text, HiddenField1.Value, "", sname.Text, sname2.Text, sname3.Text, Convert.ToDecimal(Hiddenspct.Value), 0, 0, Convert.ToDecimal(Hiddencomm.Value), 0, 0, Convert.ToDecimal(Cost.Text), Convert.ToInt32(quote.Text.Trim()), "y",ref cError);
                value = Convert.ToString(check);

            }
            if (user_login == "external")
            {
                if (Hiddencasunit.Value == null)
                {
                    Hiddencasunit.Value = "1";
                }
                
                orderentry ord = new orderentry();
                bool check = ord.AddViewItemEstimateApp(UserLogin.UserName, "AddValidate", Convert.ToInt32(Session["order_estweb_app"]), 0, Convert.ToString(HiddenEstmate.Value), fgitem.Text, custpart.Text, Convert.ToDecimal(quantity.Text.Trim()), Convert.ToString(HiddenFieldname.Value), Convert.ToString(Hiddencustdesc.Value), Convert.ToString(Hiddencustdesc2.Value), "", Convert.ToDecimal(Hiddenprice.Value), Convert.ToString(HiddenTextBox1.Value), HiddenField2.Value, custpo.Text, "", 0, Convert.ToDecimal(Hiddendiscount.Value), "ASAP", Convert.ToDateTime(duedate.Text), 0, priority.SelectedValue, Convert.ToDateTime(Session["order_entry_req_date_app"]), 0, Convert.ToInt32(Hiddencounter.Value), Convert.ToDecimal(partial.Text), Convert.ToInt32(Hiddencasunit.Value), Convert.ToInt32(line.Text), Convert.ToInt32(boardpo.Text), Convert.ToString(Session["order_entry_sman1_app"]), Convert.ToString(Session["order_entry_sman2_app"]), Convert.ToString(Session["order_entry_sman3_app"]), "N", Convert.ToDecimal(over.Text), Convert.ToDecimal(under.Text), boardvend.Text, HiddenField1.Value, "", Convert.ToString(Session["order_entry_sname1_app"]), Convert.ToString(Session["order_entry_sname2_app"]), Convert.ToString(Session["order_entry_sname3_app"]), Convert.ToDecimal(Hiddenspct.Value), 0, 0, Convert.ToDecimal(Hiddencomm.Value), 0, 0, Convert.ToDecimal(Hiddencost.Value), Convert.ToInt32(quote.Text.Trim()), "y",ref cError);
                value = Convert.ToString(check);                              
            }
            if (cError != "")
            {
                HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
                if (cError == "PO# is mandatory for this Customer...")
                    custpo.Focus();
                else if (cError == "Date lies between 01/01/2009 & 12/12/2099")
                    duedate.Focus();
                else if (cError == "Invalid Cust Part#. Try help. ")
                    custpart.Focus();
                else if (cError == "Invalid Quote try help...")
                    quote.Focus();
                else
                    fgitem.Focus();

            }

            if (value == "True")
            {

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = HiddenEstmate.Value;
                ObjectDataSource1.SelectParameters["prmJob"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQno"].DefaultValue = quote.Text.Trim();
                ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = fgitem.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartNum"].DefaultValue = custpart.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = quantity.Text.Trim();
                ObjectDataSource1.SelectParameters["prmManag"].DefaultValue = HiddenField1.Value;
                ObjectDataSource1.SelectParameters["prmType"].DefaultValue = original.SelectedValue;
                ObjectDataSource1.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = HiddenTextBox1.Value;
                ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField2.Value;
                ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTPrice"].DefaultValue = Hiddentotal.Value;
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCas"].DefaultValue = qtyunit.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartdscr"].DefaultValue = description.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUnit"].DefaultValue = units.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartdscr2"].DefaultValue = description2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPoNum"].DefaultValue = custpo.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEnum"].DefaultValue = line.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPromCode"].DefaultValue = priority.SelectedValue;
                ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = priority2.SelectedValue;
                ObjectDataSource1.SelectParameters["prmReqDate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPromDate"].DefaultValue = Convert.ToString(Session["order_entry_req_date_app"]);
                ObjectDataSource1.SelectParameters["prmPrevOrder"].DefaultValue = boardpo.Text.Trim();
                ObjectDataSource1.SelectParameters["prmVend"].DefaultValue = boardvend.Text.Trim();
                ObjectDataSource1.SelectParameters["prmOver"].DefaultValue = over.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUnder"].DefaultValue = under.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman2"].DefaultValue = sman2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman3"].DefaultValue = sman3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSname2"].DefaultValue = sname2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSname3"].DefaultValue = sname3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSpct"].DefaultValue = Hiddenspct.Value;
                ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = Hiddencomm.Value;
                ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = Cost.Text.Trim();

                ObjectDataSource1.SelectParameters["prmMultiItem"].DefaultValue = HiddenFielditemcount.Value;



                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_item_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    if (aUsers == "external")
                    {
                        if (Hiddencasunit.Value == null)
                        {
                            Hiddencasunit.Value = "1";
                        }

                        ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField2.Value;
                        ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = "ASAP";
                        ObjectDataSource1.SelectParameters["prmType"].DefaultValue = "N";
                        ObjectDataSource1.SelectParameters["prmPrice"].DefaultValue = Hiddenprice.Value;
                        ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = HiddenTextBox1.Value;
                        ObjectDataSource1.SelectParameters["prmItemName"].DefaultValue = HiddenFieldname.Value;
                        ObjectDataSource1.SelectParameters["prmTPrice"].DefaultValue = Hiddentotal.Value;
                        ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = Hiddendiscount.Value;
                        ObjectDataSource1.SelectParameters["prmCas"].DefaultValue = Hiddencounter.Value;
                        ObjectDataSource1.SelectParameters["prmPartdscr"].DefaultValue = Hiddencustdesc.Value;
                        ObjectDataSource1.SelectParameters["prmUnit"].DefaultValue = Hiddencasunit.Value;
                        ObjectDataSource1.SelectParameters["prmPartdscr1"].DefaultValue = Hiddencustdesc2.Value;
                        ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = Hiddencost.Value;
                        ObjectDataSource1.SelectParameters["prmReqDate"].DefaultValue = duedate.Text.Trim();
                        ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = Convert.ToString(Session["order_entry_sman1_app"]);
                        ObjectDataSource1.SelectParameters["prmSman2"].DefaultValue = Convert.ToString(Session["order_entry_sman2_app"]);
                        ObjectDataSource1.SelectParameters["prmSman3"].DefaultValue = Convert.ToString(Session["order_entry_sman3_app"]);
                        ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = Convert.ToString(Session["order_entry_sname1_app"]);
                        ObjectDataSource1.SelectParameters["prmSname2"].DefaultValue = Convert.ToString(Session["order_entry_sname2_app"]);
                        ObjectDataSource1.SelectParameters["prmSname3"].DefaultValue = Convert.ToString(Session["order_entry_sname3_app"]);


                    }
                }


                Session["order_est_value_check_app"] = 1;
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Session["view_item_mode_app"] = null;
                HiddenQuoteNum.Value = "";
                HiddenFielditemcount.Value = "";

                //if (!ClientScript.IsStartupScriptRegistered("alert"))
                //{
                //    Page.ClientScript.RegisterStartupScript

                //        (this.GetType(), "alert", "confirmAdd();", true);
                //}
                Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");

            }
        }
        catch { }


    }

    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_item_estimate.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();

                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (vCanDelete == false)
                {
                    Response.Write("<script>alert('Sorry! You do not have permission to Delete Record');</script>");
                    Response.Write("<script>window.location.href = 'releases.aspx';</script>");

                }
            }
            Label vline = (Label)FormView1.FindControl("vlineLabe");
            Session["view_line_est_app"] = vline.Text;

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["view_line_est_app"]);
            FormView1.ChangeMode(FormViewMode.ReadOnly);
            Session["view_item_mode_app"] = null;
        }
        catch { }
    }

    //protected void newAddButton_Click(object sender, EventArgs e)
    //{
    //    FormView1.ChangeMode(FormViewMode.Insert);
    //    NewAddButton.Visible = false;
    //}
    protected void FormView1_Unload(object sender, EventArgs e)
    {

        try
        {
            Label quote = (Label)FormView1.FindControl("vQnoLabel");
            Label job = (Label)FormView1.FindControl("job_noLabel");
            Label job2 = (Label)FormView1.FindControl("Jobno2Label");
            Label entryline = (Label)FormView1.FindControl("vlineLabe");
            Label fgitem = (Label)FormView1.FindControl("Item1Label");
            Label estnote = (Label)FormView1.FindControl("est_Label_est");
            Label reckey = (Label)FormView1.FindControl("Label_rec_key");

            Session["view_line_est_app"] = entryline.Text;
            Session["line_app"] = entryline.Text;
            Session["item_app"] = fgitem.Text;
            Session["order_rec_key_app"] = reckey.Text;
            Session["order_entry_est_no_app"] = estnote.Text;

        }
        catch { }
    }

    protected void fgitem_value_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
            TextBox iname = (TextBox)FormView1.FindControl("Name1TextBox");
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox dscr = (TextBox)FormView1.FindControl("DscrTextBox");
            TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
            TextBox cascount = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("VcasUnitTextBox");
            TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
            DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");
            TextBox total = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            
            if (qty.Text == "")
                qty.Text = "0";
            if (quote.Text.Trim() == "")
                quote.Text = "0";
            if (quote.Text.Trim() == "" || quote.Text.Trim() == "0")
            {
                LookUp look = new LookUp();
                DataSet ds = new DataSet();
                ds = look.SelectfgLookup("search", UserLogin.UserName, "viewitemfg", "", fgitem.Text, Convert.ToString(Session["order_entry_cust_no_app"]), Convert.ToInt32(quote.Text.Trim()), Convert.ToInt32(qty.Text));
                //Response.Write(ds.Tables[0].Rows[0][16].ToString());
                HiddenDropdown1.Value = ds.Tables[0].Rows[0][16].ToString();
                //Response.Write(HiddenDropdown1.Value);
                fgitem.Text = ds.Tables[0].Rows[0][0].ToString().Replace(":", "\"");
                iname.Text = ds.Tables[0].Rows[0][1].ToString().Replace(":", "\"");
                HiddenFieldname.Value = ds.Tables[0].Rows[0][1].ToString().Replace(":", "\"");
                custpart.Text = ds.Tables[0].Rows[0][4].ToString().Replace(":", "\"");
                dscr.Text = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
                Hiddencustdesc.Value = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
                dscr2.Text = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
                Hiddencustdesc2.Value = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
                price.Text = ds.Tables[0].Rows[0][10].ToString();
                Hiddenprice.Value = ds.Tables[0].Rows[0][10].ToString();
                uom.Text = ds.Tables[0].Rows[0][11].ToString();
                HiddenTextBox1.Value = ds.Tables[0].Rows[0][11].ToString();
                cascount.Text = ds.Tables[0].Rows[0][12].ToString();
                Hiddencounter.Value = ds.Tables[0].Rows[0][12].ToString();
                casunit.Text = ds.Tables[0].Rows[0][13].ToString();
                Hiddencasunit.Value = ds.Tables[0].Rows[0][13].ToString();
                cost.Text = ds.Tables[0].Rows[0][15].ToString();
                Hiddencost.Value = ds.Tables[0].Rows[0][15].ToString();
                disc.Text = ds.Tables[0].Rows[0][17].ToString();
                Hiddendiscount.Value = ds.Tables[0].Rows[0][17].ToString();
                decimal pprice = Convert.ToDecimal(ds.Tables[0].Rows[0][10].ToString());
                int qqty = Convert.ToInt32(qty.Text);
                int ccascount = Convert.ToInt32(ds.Tables[0].Rows[0][12].ToString());
                decimal discount = Convert.ToInt32(ds.Tables[0].Rows[0][17].ToString());

                if (ds.Tables[0].Rows[0][11].ToString() == "M")
                {
                    total.Text = Convert.ToString((pprice * qqty) / 1000);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else if (ds.Tables[0].Rows[0][11].ToString() == "C")
                {
                    total.Text = Convert.ToString(pprice * qqty / 100);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else if (ds.Tables[0].Rows[0][11].ToString() == "CS" && ccascount != 0)
                {
                    total.Text = Convert.ToString(pprice * qqty / ccascount);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else
                {
                    total.Text = Convert.ToString(pprice * qqty);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }

                //Response.Write(HiddenDropdown1.Value);
                //Response.Write(ds.Tables[0].Rows[0][16].ToString());
                if (ds.Tables[0].Rows[0][16].ToString() == "O")
                    type.SelectedIndex = 0;
                if (ds.Tables[0].Rows[0][16].ToString() == "C")
                    type.SelectedIndex = 1;
                if (ds.Tables[0].Rows[0][16].ToString() == "N")
                    type.SelectedIndex = 2;
                if (ds.Tables[0].Rows[0][16].ToString() == "Q")
                    type.SelectedIndex = 3;
                if (ds.Tables[0].Rows[0][16].ToString() == "R")
                    type.SelectedIndex = 4;
                if (ds.Tables[0].Rows[0][16].ToString() == "T")
                    type.SelectedIndex = 5;
                if (ds.Tables[0].Rows[0][16].ToString() == "X")
                    type.SelectedIndex = 6;
                qty.Focus();
            }
        }
        catch { }
    }

    protected void custpart_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox dscr = (TextBox)FormView1.FindControl("DscrTextBox");

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.ItemPartLookup(Convert.ToString(Session["order_entry_cust_no_app"]), "search", UserLogin.UserName, custpart.Text);
            custpart.Text = ds.Tables[0].Rows[0][0].ToString();
        }
        catch
        {
            //TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            //custpart.Text = "";
        }

    }

    protected void Quantity_Change_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");

            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
            TextBox cascount = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("VcasUnitTextBox");
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");

            TextBox total = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");
            TextBox vType = (TextBox)FormView1.FindControl("vTypeTextBox");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            price.Text = Convert.ToString(Hiddenprice.Value);
            uom.Text = Convert.ToString(HiddenTextBox1.Value);

            if (price.Text == "")
                price.Text = "0";
            if (qty.Text == "")
                qty.Text = "0";
            if (cascount.Text == "")
                cascount.Text = "0";
            if (disc.Text == "")
                disc.Text = "0";
           
            if (quote.Text.Trim() == "")
                quote.Text = "0";
            decimal pprice = 0;
            if (quote.Text != "0")
            {
                try
                {
                    LookUp look = new LookUp();
                    DataSet ds = new DataSet();
                    ds = look.ItemQuantityLook(UserLogin.UserName, Convert.ToInt32(quote.Text.Trim()), fgitem.Text, custpart.Text);

                    for (int j = 0; j <= ds.Tables[0].Rows.Count - 1; j++)
                    {
                        if (ds.Tables[0].Rows[j][0].ToString().Trim() == qty.Text.Trim())
                        {
                            price.Text = ds.Tables[0].Rows[j][1].ToString();
                            Hiddenprice.Value = ds.Tables[0].Rows[j][1].ToString();
                            pprice = Convert.ToDecimal(ds.Tables[0].Rows[j][1].ToString());
                        }
                    }

                }
                catch
                {
                    
                }
            }
            else
            {
                try
                {
                    LookUp look = new LookUp();
                    DataSet ds = new DataSet();
                    ds = look.SelectfgLookup("search", UserLogin.UserName, "viewitemfg", "", fgitem.Text, Convert.ToString(Session["order_entry_cust_no_app"]), Convert.ToInt32(quote.Text.Trim()), Convert.ToInt32(qty.Text));



                    price.Text = ds.Tables[0].Rows[0][10].ToString();
                    Hiddenprice.Value = ds.Tables[0].Rows[0][10].ToString();
                    pprice = Convert.ToDecimal(ds.Tables[0].Rows[0][10].ToString());
                }
                catch {
                    
                }
            }
            pprice = Convert.ToDecimal(Hiddenprice.Value);
            
            int qqty = Convert.ToInt32(qty.Text);
            int ccascount = Convert.ToInt32(cascount.Text);
            decimal discount = Convert.ToInt32(disc.Text);
            decimal caldis = 0;

            if (uom.Text == "M")
            {
                total.Text = Convert.ToString((pprice * qqty) / 1000);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else if (uom.Text == "C")
            {
                total.Text = Convert.ToString(pprice * qqty / 100);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else if (uom.Text == "CS" && ccascount != 0)
            {
                total.Text = Convert.ToString(pprice * qqty / ccascount);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else
            {
                total.Text = Convert.ToString(pprice * qqty);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            if (HiddenFieldLookuval.Value != "lookup")
            {
                vType.Focus();
                HiddenFieldLookuval.Value = "";
            }
  

           

        }
        catch { }
    }
    protected void UpdateCancelButton_Click(object sender, EventArgs e)
    {
        Session["view_item_mode_app"] = null;
        Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");
    }
    protected void InsertCancelButton_Click(object sender, EventArgs e)
    {
        Session["view_item_mode_app"] = null;
        HiddenQuoteNum.Value = "";
        Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");
    }
    protected void quote_text_changed(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox quote1 = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
            TextBox dscr1 = (TextBox)FormView1.FindControl("DscrTextBox");
            TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox cascount = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");
            TextBox tprice = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
            TextBox casunit = (TextBox)FormView1.FindControl("VcasUnitTextBox");
            TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");          
           
            TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
            DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");
            TextBox total = (TextBox)FormView1.FindControl("extpriceTextBox");
           
            if (quote1.Text == "")
                quote1.Text = "0";
            if (HiddenQuoteNum.Value == "")
            {
                HiddenQuoteNum.Value = "0";
            }

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.ItemQuoteLookup("sea", UserLogin.UserName, "Quote", "EQUAL", quote1.Text, Convert.ToInt32(quote1.Text), Convert.ToString(Session["order_entry_cust_no_app"]));
            
            if (ds.Tables[0].Rows.Count > 1)
            {
                HiddenFieldmultirec.Value = "multirec";
                if (Convert.ToInt32(quote1.Text.Trim()) > 0 && Convert.ToInt32(HiddenQuoteNum.Value) <= 0)  
                {
                    Response.Write("<script>window.open('item_qut_look_app.aspx?quote=" + quote1.Text.Trim() + "&cust=" + cust_label.Text.Trim() + "','typoeordLookupWindow','width=720,height=450, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");

                }
                if (quote1.Text != Convert.ToString(HiddenQuoteNum.Value) && Convert.ToInt32(quote1.Text.Trim()) > 0)
                {
                    Response.Write("<script>window.open('item_qut_look_app.aspx?quote=" + quote1.Text.Trim() + "&cust=" + cust_label.Text.Trim() + "','typoeordLookupWindow','width=720,height=450, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
                }
            }
            if (ds.Tables[0].Rows.Count == 1)
            {
                
                HiddenQuoteNum.Value = ds.Tables[0].Rows[0][0].ToString();
                fgitem.Text = ds.Tables[0].Rows[0][1].ToString().Replace(":", "\"");
                itemname.Text = ds.Tables[0].Rows[0][6].ToString().Replace(":", "\"");
                HiddenFieldname.Value = ds.Tables[0].Rows[0][6].ToString().Replace(":", "\"");
                custpart.Text = ds.Tables[0].Rows[0][5].ToString().Replace(":", "\"");
                dscr1.Text = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
                Hiddencustdesc.Value = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
                dscr2.Text = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
                Hiddencustdesc2.Value = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
                price.Text = ds.Tables[0].Rows[0][10].ToString();
                Hiddenprice.Value = ds.Tables[0].Rows[0][10].ToString();
                uom.Text = ds.Tables[0].Rows[0][11].ToString();
                HiddenTextBox1.Value = ds.Tables[0].Rows[0][11].ToString();
                cascount.Text = ds.Tables[0].Rows[0][21].ToString();
                Hiddencounter.Value = ds.Tables[0].Rows[0][21].ToString();
                casunit.Text = ds.Tables[0].Rows[0][22].ToString();
                Hiddencasunit.Value = ds.Tables[0].Rows[0][22].ToString();
               // cost.Text = ds.Tables[0].Rows[0][15].ToString();
                //Hiddencost.Value = ds.Tables[0].Rows[0][15].ToString();
                disc.Text = ds.Tables[0].Rows[0][13].ToString();
                Hiddendiscount.Value = ds.Tables[0].Rows[0][13].ToString();
                qty.Text = ds.Tables[0].Rows[0][14].ToString();
                
                decimal pprice = Convert.ToDecimal(ds.Tables[0].Rows[0][10].ToString());
                int qqty = Convert.ToInt32(qty.Text);
                int ccascount = Convert.ToInt32(ds.Tables[0].Rows[0][21].ToString());
                decimal discount = Convert.ToInt32(ds.Tables[0].Rows[0][13].ToString());

                if (ds.Tables[0].Rows[0][11].ToString() == "M")
                {
                    total.Text = Convert.ToString((pprice * qqty) / 1000);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else if (ds.Tables[0].Rows[0][11].ToString() == "C")
                {
                    total.Text = Convert.ToString(pprice * qqty / 100);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else if (ds.Tables[0].Rows[0][11].ToString() == "CS" && ccascount != 0)
                {
                    total.Text = Convert.ToString(pprice * qqty / ccascount);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                else
                {
                    total.Text = Convert.ToString(pprice * qqty);
                    if (discount >= 1)
                        total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                    Hiddentotal.Value = total.Text;
                }
                                

                LookUp lookqty = new LookUp();
                DataSet dsqty = new DataSet();
                dsqty = look.ItemQuantityLook(UserLogin.UserName, Convert.ToInt32(quote1.Text.Trim()), fgitem.Text, custpart.Text);
                if (dsqty.Tables[0].Rows.Count > 1)
                {
                    Response.Write("<script>window.open('quantity_look.aspx?est=" + quote1.Text.Trim() + "&item=" + fgitem.Text.Trim() + "&custpart=" + custpart.Text.Trim() + "','quantityLookupWindow','width=500,height=200, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
                }
                else
                {
                    qty.Focus();
                }
                
            }




        }
        catch
        {
            TextBox quote1 = (TextBox)FormView1.FindControl("vQnoTextBox");

            quote1.Focus();

        }
    }
    protected void Page_Unload(object sender, EventArgs e)
    {

    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    protected void backbutton_Click(object sender, EventArgs e)
    {
        Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");
    }

}



