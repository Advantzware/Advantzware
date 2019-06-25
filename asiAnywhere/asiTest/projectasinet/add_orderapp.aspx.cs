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
public partial class add_orderapp : System.Web.UI.Page
{

    string vcompany;
    string vuser;
    private Int32 line_total;
    private string mode = "";
    private string stritem = "";

    protected void Page_Load(object sender, EventArgs e)
    {
        //FormView1.ChangeMode(FormViewMode.ReadOnly);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";


        try
        {
            Label orderno = (Label)FormView1.FindControl("VOrderNumLabel11");
            Session["order_estweb_app"] = orderno.Text;
            Label quote = (Label)FormView1.FindControl("vRfqLabel");
            Session["order_entry_quote_app"] = quote.Text;
            Label job = (Label)FormView1.FindControl("Label3");
            Session["order_entry_job_no_1_app"] = job.Text;
            Label job2 = (Label)FormView1.FindControl("Label4");
            Session["order_entry_job_no_2_app"] = job2.Text;
            

        }
        catch { }

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_order_estimate.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
                
                if (Request.QueryString["webadd"] == "ADD")
                {
                   
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["order_estweb_app"] = null;
                    //Session["order_estweb_app"] = "202139";   
                }
                if (Convert.ToString(Session["order_estweb_app"]) == null || Convert.ToString(Session["order_estweb_app"]) == "" )
                {
                    itembuttonform.Visible = false;
                }
                lblComp.Text = PrmComp;
                lblUser.Text = UserLogin.UserName;
                vcompany = PrmComp;
                vuser = UserLogin.UserName;
            }

           
            try
            {
                Label fob = (Label)FormView1.FindControl("Label5");
                RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
                RadioButton origin = (RadioButton)FormView1.FindControl("RD6");

                if (fob.Text == "Destination")
                {
                    destination.Checked = true;
                }
                else
                {
                    origin.Checked = true;
                }
                //if (fob.Text == "ORIG")
                //{
                //    origin.Checked = true;
                //}
                //else
                //{
                //    destination.Checked = true;
                //}

                Label frieght = (Label)FormView1.FindControl("Label1");
                RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD1");
                RadioButton Bill = (RadioButton)FormView1.FindControl("RD2");
                RadioButton Collect = (RadioButton)FormView1.FindControl("RD3");
                RadioButton party = (RadioButton)FormView1.FindControl("RD4");

                if (frieght.Text == "P")
                {
                    Prepaid.Checked = true;
                }
                if (frieght.Text == "B")
                {
                    Bill.Checked = true;
                }
                if (frieght.Text == "C")
                {
                    Collect.Checked = true;
                }
                if (frieght.Text == "T")
                {
                    party.Checked = true;
                }
            }
            catch
            {
                return;
            }
        }
        try
        {
            if (Session["view_item_index_val_app"] != null)
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["view_item_index_val_app"]);
                Session["order_rec_key_app"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
                Session["order_entry_est_no_app"] = GridView1.SelectedRow.Cells[8].Text;
                Label itemname = (Label)GridView1.SelectedRow.FindControl("Label2");
                //Response.Write(itemname.Text);
                Session["item_app"] = itemname.Text;
            }

            if (Session["view_item_index_val_app"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["line_app"] = GridView1.SelectedRow.Cells[1].Text;

                Session["order_rec_key_app"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
                Session["order_entry_est_no_App"] = GridView1.SelectedRow.Cells[8].Text;

                Session["order_app"] = Session["order_app"];
                Session["order_est_app"] = Session["order_app"];
                Session["view_line_est_app"] = Session["line_app"];

            }
        }
        catch { }

        if (Request.QueryString["webadd"] == "additemweb")
        {
            Response.Write("<script> location.href='add_itemapp.aspx?webitem=insert'</script>");
        }
               

    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                GridView1.Visible = false;
                FormView2.Visible = false;
                itembuttonform.Visible = false;
                Label vlblUser = (Label)FormView1.FindControl("lblUser");
                Label vlblComp = (Label)FormView1.FindControl("lblComp");
                
                orderfielset.Attributes.Add("style", "display: none; width: 400px;");
                RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
                origin.Checked = true;
                RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD2");
                Prepaid.Checked = true;
                Label date = (Label)FormView1.FindControl("VOrdateLabel");
                date.Text = DateTime.Now.ToShortDateString();
                TextBox qote = (TextBox)FormView1.FindControl("RfqTextBox");
                TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
                if (qote.Text.Trim() == "")
                {
                    duedate.Text = DateTime.Now.AddDays(1).ToShortDateString();
                }

                DropDownList duecode = (DropDownList)FormView1.FindControl("DropDownList1");
                duecode.SelectedIndex = 9;
                TextBox bill = (TextBox)FormView1.FindControl("VCustomerTextBox");                
                bill.Focus();

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_order_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    vlblUser.Text = UserLogin.UserName;
                    vlblComp.Text = PrmComp;
                    if (aUsers == "external")
                    {

                        TextBox taxc = (TextBox)FormView1.FindControl("VTaxgrTextBox");
                        Image tax_img = (Image)FormView1.FindControl("Image7");
                        TextBox payterm = (TextBox)FormView1.FindControl("VTermsTextBox");
                        TextBox paydesc = (TextBox)FormView1.FindControl("VTermdscrTextBox");
                        CheckBox rd1 = (CheckBox)FormView1.FindControl("RD1");
                        CheckBox rd2 = (CheckBox)FormView1.FindControl("RD2");
                        CheckBox rd3 = (CheckBox)FormView1.FindControl("RD3");
                        CheckBox rd4 = (CheckBox)FormView1.FindControl("RD4");
                        TextBox preorder = (TextBox)FormView1.FindControl("VProdTextBox");
                        TextBox sales = (TextBox)FormView1.FindControl("VSmanTextBox");
                        TextBox salesman = (TextBox)FormView1.FindControl("VSnameTextBox");
                        Image img_sales = (Image)FormView1.FindControl("Image2");
                        Image img_pay = (Image)FormView1.FindControl("Image5");
                        TextBox proddate = (TextBox)FormView1.FindControl("VProdDateTextBox");
                        Label prddate = (Label)FormView1.FindControl("prd_label");
                        //TextBox comm3 = (TextBox)FormView1.FindControl("vScomm3TextBox");
                        //Response.Write(aUsers);
                        TextBox est = (TextBox)FormView1.FindControl("estimateTextBox");
                        Image estimg = (Image)FormView1.FindControl("Image8");
                        Label estlabel = (Label)FormView1.FindControl("est_Label");
                        estlabel.Visible = false;
                        estimg.Visible = false;
                        est.Visible = false;
                        //comm1.Visible = false;
                        //comm2.Visible = false;
                        //comm3.Visible = false;
                        // commlab.Visible = false;
                        payterm.Enabled = false;
                        paydesc.Enabled = false;
                        rd1.Enabled = false;
                        rd2.Enabled = false;
                        rd3.Enabled = false;
                        rd4.Enabled = false;
                        preorder.Enabled = false;
                        sales.Enabled = false;
                        salesman.Enabled = false;
                        img_sales.Visible = false;
                        img_pay.Visible = false;
                        proddate.Visible = false;
                        prddate.Visible = false;
                        tax_img.Visible = false;
                        taxc.Enabled = false;

                        // when  user  is external 
                        try
                        {
                            TextBox est2 = (TextBox)FormView1.FindControl("estimateTextBox");
                            TextBox cust = (TextBox)FormView1.FindControl("VCustomerTextBox");
                            //TextBox cname = (TextBox)FormView1.FindControl("VCustNameTextBox");
                            //TextBox cadd1 = (TextBox)FormView1.FindControl("VCustAddrTextBox");
                            //TextBox cadd2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
                            //TextBox ccity = (TextBox)FormView1.FindControl("VCityTextBox");
                            //TextBox cstate = (TextBox)FormView1.FindControl("VStateTextBox");
                            //TextBox czip = (TextBox)FormView1.FindControl("VZipTextBox");
                            TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
                            TextBox soldto2 = (TextBox)FormView1.FindControl("VSoldTextBox");
                            //TextBox sname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
                            //TextBox sadd1 = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
                            //TextBox sadd2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
                            //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
                            //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
                            //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");
                            TextBox overrun = (TextBox)FormView1.FindControl("VOverpctTextBox");
                            TextBox taxcode = (TextBox)FormView1.FindControl("VTaxgrTextBox");
                            TextBox underrun = (TextBox)FormView1.FindControl("VUnderpctTextBox");
                            TextBox pterms = (TextBox)FormView1.FindControl("VTermsTextBox");
                            TextBox ptdesc = (TextBox)FormView1.FindControl("VTermdscrTextBox");
                            TextBox carrier = (TextBox)FormView1.FindControl("VCarrierTextBox");
                            TextBox sales2 = (TextBox)FormView1.FindControl("VSmanTextBox");
                            TextBox salesman2 = (TextBox)FormView1.FindControl("VSnameTextBox");
                            TextBox lastship = (TextBox)FormView1.FindControl("VLastDateTextBox");
                            TextBox duedate2 = (TextBox)FormView1.FindControl("VDueDateTextBox");
                            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

                            RadioButton rd11 = (RadioButton)FormView1.FindControl("RD1");
                            RadioButton rd12 = (RadioButton)FormView1.FindControl("RD2");
                            RadioButton rd13 = (RadioButton)FormView1.FindControl("RD3");
                            RadioButton rd14 = (RadioButton)FormView1.FindControl("RD4");
                            RadioButton rd15 = (RadioButton)FormView1.FindControl("RD5");
                            RadioButton rd16 = (RadioButton)FormView1.FindControl("RD6");

                            Order ord = new Order();
                            DataSet ds = new DataSet();
                            ds = ord.SelectOrderCust("ViewCust", UserLogin.UserName, "", "", cust.Text);

                            
                            if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "prepaid")
                            {
                                rd11.Checked = true;
                                rd12.Checked = false;
                                rd13.Checked = false;
                                rd14.Checked = false;
                            }
                            if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "bill")
                            {
                                rd11.Checked = false;
                                rd12.Checked = true;
                                rd13.Checked = false;
                                rd14.Checked = false;
                            }
                            if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "collect")
                            {
                                rd11.Checked = false;
                                rd12.Checked = false;
                                rd13.Checked = true;
                                rd14.Checked = false;
                            }
                            if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "3rd party")
                            {
                                rd11.Checked = false;
                                rd12.Checked = false;
                                rd13.Checked = false;
                                rd14.Checked = true;
                            }
                            if (ds.Tables[0].Rows[0][16].ToString().ToLower() == "dest")
                            {
                                rd15.Checked = true;
                                rd16.Checked = false;
                            }
                            if (ds.Tables[0].Rows[0][16].ToString().ToLower() == "orig")
                            {
                                rd15.Checked = false;
                                rd16.Checked = true;
                            }
                            cust.Text = ds.Tables[0].Rows[0][0].ToString();
                            soldto2.Text = cust.Text;

                           
                            sales2.Text = ds.Tables[0].Rows[0][8].ToString();
                            salesman2.Text = ds.Tables[0].Rows[0][9].ToString();
                            HiddenCarr.Value = ds.Tables[0].Rows[0][17].ToString();
                            carrier.Text = ds.Tables[0].Rows[0][17].ToString();

                            contact.Text = ds.Tables[0].Rows[0][18].ToString();
                            overrun.Text = ds.Tables[0].Rows[0][19].ToString();
                            underrun.Text = ds.Tables[0].Rows[0][20].ToString();
                            HiddenTerm.Value = ds.Tables[0].Rows[0][21].ToString();
                            HiddenTermdesc.Value = ds.Tables[0].Rows[0][22].ToString();

                            pterms.Text = ds.Tables[0].Rows[0][21].ToString();
                            ptdesc.Text = ds.Tables[0].Rows[0][22].ToString();

                            taxcode.Text = ds.Tables[0].Rows[0][24].ToString();
                            HiddenField10.Value = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][36].ToString()).ToShortDateString());
                            lastship.Text = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][36].ToString()).ToShortDateString());

                            
                            duedate2.Text = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][35].ToString()).ToShortDateString());
                            contact.Focus();
                            cust.Enabled = false;
                        }
                        catch { }


                    }
                }
            }
        }
        catch { }

        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                GridView1.Visible = true;
                FormView2.Visible = true;
                itembuttonform.Visible = true;

                Label vlblUser = (Label)FormView1.FindControl("lblUser");
                Label vlblComp = (Label)FormView1.FindControl("lblComp");
                

                orderfielset.Attributes.Add("style", "display: inline; width: 400px;");
                if (Session["order_estweb_app"] == null || Convert.ToString(Session["order_estweb_app"]) == "")
                {
                    GridView1.Visible = false;
                    FormView2.Visible = false;
                    
                    orderfielset.Attributes.Add("style", "display: none; width: 400px;");
                }
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_order_estimate.aspx";
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
                        vlblUser.Text = UserLogin.UserName;
                        vlblComp.Text = PrmComp;
                    }
                    catch { }

                    //if (aUsers == "external")
                    //{
                    //    string quotemail = "0";
                    //    Label prddate = (Label)FormView1.FindControl("prd_label");
                    //    Label prodate = (Label)FormView1.FindControl("VProdDateLabel");
                    //    prddate.Visible = false;
                    //    prodate.Visible = false;
                    //    Label estlabel = (Label)FormView1.FindControl("est_label");
                    //    Label estimate = (Label)FormView1.FindControl("EstimateTextBox");
                    //    Label orderno = (Label)FormView1.FindControl("VOrderNumLabel11");
                    //    Label quote = (Label)FormView1.FindControl("vRfqLabel");
                    //    if (quote.Text == "0" && estimate.Text == "")
                    //        quotemail = "0";
                    //    else
                    //        quotemail = "2";

                    //    estlabel.Visible = false;
                    //    estimate.Visible = false;

                    //    if (this.mode != "")
                    //    {
                    //        try
                    //        {
                    //            if (Convert.ToInt32(quotemail) > 0)
                    //            {
                    //                Order mail = new Order();
                    //                mail.OrderMail(UserLogin.UserName, "MailOrder", orderno.Text.Trim(), "", "");
                    //                Response.Write("<script>window.location.href='view_item_estimate.aspx?mode=item';</script>");
                    //            }
                    //            else
                    //            {
                    //                Response.Write("<script>window.location.href='view_item_estimate.aspx?mode=insert';</script>");
                    //            }
                    //        }
                    //        catch { }


                    //    }
                    //}
                    //if (aUsers == "internal")
                   // {
                        Button del = (Button)FormView1.FindControl("deleteButton");
                        //del.Visible = false;

                        Label quoteno = (Label)FormView1.FindControl("vRfqLabel");
                        if (quoteno.Text == "")
                            quoteno.Text = "0";
                        Int32 qtnum = Convert.ToInt32(quoteno.Text.Trim());

                        Label estimate = (Label)FormView1.FindControl("EstimateTextBox");

                        //if (qtnum == 0 && Convert.ToString(estimate.Text) != "" && this.mode == "add")
                        //if (Convert.ToString(estimate.Text) != "" && this.mode == "add")
                        //{
                        //    this.line_total = Convert.ToInt32(((Label)FormView1.FindControl("LineTotalLabel")).Text.Trim());
                        //    open_popup();
                        //    this.mode = "";
                        //}
                        //else if (this.mode == "add")
                        //{
                        //    Response.Write("<script>window.location.href='view_item_estimate.aspx?mode=insert';</script>");
                        //}
                   // }

                }

                Label user = (Label)FormView1.FindControl("VUseridLabel");
                Label Status = (Label)FormView1.FindControl("VStatLabel");

                Session["Lastuser_id"] = user.Text;
                Session["Status_id"] = Status.Text;

            }
            //if (FormView1.CurrentMode == FormViewMode.Insert)
            //{
            //    Label user = (Label)FormView1.FindControl("VUseridLabel");
            //    Label Status = (Label)FormView1.FindControl("VStatLabel");
            //    user.Text = Convert.ToString(Session["Lastuser_id"]);
            //    Status.Text = Convert.ToString(Session["Status_id"]);

            //}
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                GridView1.Visible = false;
                FormView2.Visible = false;
                itembuttonform.Visible = false;

                Label vlblUser = (Label)FormView1.FindControl("lblUser");
                Label vlblComp = (Label)FormView1.FindControl("lblComp");
                vlblUser.Text = vuser;
                vlblComp.Text = vcompany;
                
                orderfielset.Attributes.Add("style", "display: none; width: 400px;");
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_order_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    vlblUser.Text = UserLogin.UserName;
                    vlblComp.Text = PrmComp;
                    TextBox custpo = (TextBox)FormView1.FindControl("VContactTextBox");
                    custpo.Focus();
                    if (aUsers == "external")
                    {
                        // Response.Write(aUsers);
                        TextBox taxc = (TextBox)FormView1.FindControl("VTaxgrTextBox");
                        Image tax_img = (Image)FormView1.FindControl("Image7");
                        //TextBox type = (TextBox)FormView1.FindControl("ordtypeTextBox");
                        DropDownList type = (DropDownList)FormView1.FindControl("DropDownList2");
                        //TextBox custname = (TextBox)FormView1.FindControl("VCustNameTextBox");
                        //TextBox soldname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
                        //TextBox custaddr1 = (TextBox)FormView1.FindControl("VCustAddrTextBox");
                        //TextBox soldaddr1 = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
                        TextBox lastship = (TextBox)FormView1.FindControl("VLastDateTextBox");
                        // TextBox custaddr2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
                        //TextBox soldaddr2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
                        Label prddate = (Label)FormView1.FindControl("prd_label");
                        TextBox proddate = (TextBox)FormView1.FindControl("VProdDateTextBox");
                        //TextBox ccity = (TextBox)FormView1.FindControl("VCityTextBox");
                        //TextBox cstate = (TextBox)FormView1.FindControl("VStateTextBox");
                        //TextBox czip = (TextBox)FormView1.FindControl("VZipTextBox");
                        //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
                        TextBox reff = (TextBox)FormView1.FindControl("VCauthTextBox");
                        TextBox paymenttype = (TextBox)FormView1.FindControl("VCtypeTextBox");
                        TextBox preorder = (TextBox)FormView1.FindControl("VProdTextBox");
                        TextBox pterms = (TextBox)FormView1.FindControl("VTermsTextBox");
                        TextBox ptermsdscr = (TextBox)FormView1.FindControl("VTermdscrTextBox");
                        TextBox sman = (TextBox)FormView1.FindControl("VSmanTextBox");
                        TextBox sname = (TextBox)FormView1.FindControl("VSnameTextBox");
                        TextBox sman2 = (TextBox)FormView1.FindControl("VSman2TextBox");
                        TextBox sname2 = (TextBox)FormView1.FindControl("VSname2TextBox");
                        TextBox sman3 = (TextBox)FormView1.FindControl("VSman3TextBox");
                        TextBox sname3 = (TextBox)FormView1.FindControl("VSname3TextBox");

                        Label est = (Label)FormView1.FindControl("EstimateTextBox");
                        //Image estimg = (Image)FormView1.FindControl("Image8");
                        Label estlabel = (Label)FormView1.FindControl("est_Label");
                        estlabel.Visible = false;
                        //estimg.Visible = false;
                        est.Visible = false;

                        RadioButton rpre = (RadioButton)FormView1.FindControl("RD1");
                        RadioButton rbill = (RadioButton)FormView1.FindControl("RD2");
                        RadioButton rcoll = (RadioButton)FormView1.FindControl("RD3");
                        RadioButton rtp = (RadioButton)FormView1.FindControl("RD4");
                        RadioButton rdest = (RadioButton)FormView1.FindControl("RD5");
                        RadioButton rorg = (RadioButton)FormView1.FindControl("RD6");
                        //CheckBox minv = (CheckBox)FormView1.FindControl("VWhisCheckBox");

                        //comm1.Visible = false;
                        //comm2.Visible = false;
                        //comm3.Visible = false;
                        //commlab.Visible = false;
                        tax_img.Visible = false;
                        taxc.Enabled = false;
                        type.Enabled = false;
                        lastship.Enabled = false;
                        proddate.Visible = false;
                        prddate.Visible = false;
                        preorder.Enabled = false;
                        pterms.Enabled = false;
                        ptermsdscr.Enabled = false;
                        sman.Enabled = false;
                        sname.Enabled = false;
                        //sman2.Enabled = false;
                        //sname2.Enabled = false;
                        //sman3.Enabled = false;
                        //sname3.Enabled = false;
                        rpre.Enabled = false;
                        rbill.Enabled = false;
                        rcoll.Enabled = false;
                        rtp.Enabled = false;
                        rdest.Enabled = false;
                        rorg.Enabled = false;
                        // minv.Enabled = false;
                        //reff.Enabled = false;
                        paymenttype.Enabled = false;
                        Image seles1 = (Image)FormView1.FindControl("Image2");
                        Image seles2 = (Image)FormView1.FindControl("Image3");
                        Image seles3 = (Image)FormView1.FindControl("Image4");
                        Image payterms = (Image)FormView1.FindControl("Image5");
                        Image tax = (Image)FormView1.FindControl("Image7");
                        //Image lship = (Image)FormView1.FindControl("Image9");
                        //Image prodate = (Image)FormView1.FindControl("Image11");
                        //seles1.Visible = false;
                        //seles2.Visible = false;
                        //seles3.Visible = false;
                        //payterms.Visible = false;
                        tax.Visible = false;
                        //lship.Visible = false;
                        //prodate.Visible = false;
                    }

                }

                TextBox frieght = (TextBox)FormView1.FindControl("FRTextBox");
                RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD1");
                RadioButton Bill = (RadioButton)FormView1.FindControl("RD2");
                RadioButton Collect = (RadioButton)FormView1.FindControl("RD3");
                RadioButton party = (RadioButton)FormView1.FindControl("RD4");

                if (frieght.Text == "P")
                {
                    Prepaid.Checked = true;
                }
                if (frieght.Text == "B")
                {
                    Bill.Checked = true;
                }
                if (frieght.Text == "C")
                {
                    Collect.Checked = true;
                }
                if (frieght.Text == "T")
                {
                    party.Checked = true;
                }

                TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
                RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
                RadioButton origin = (RadioButton)FormView1.FindControl("RD6");

                if (fob.Text == "Destination")
                {
                    destination.Checked = true;
                }
                else
                {
                    origin.Checked = true;
                }

            }
        }

        catch {  }
    }

    protected string GenerateBillCityLine(object dataItem)
    {
        return (string)DataBinder.Eval(dataItem, "city")
            + ", " + (string)DataBinder.Eval(dataItem, "state")
            + " " + (string)DataBinder.Eval(dataItem, "zip");
    }

    protected string GenerateSoldCityLine(object dataItem)
    {
        return (string)DataBinder.Eval(dataItem, "[sold-city]")
            + ", " + (string)DataBinder.Eval(dataItem, "[sold-state]")
            + " " + (string)DataBinder.Eval(dataItem, "[sold-zip]");
    }

    protected void UpdateButon_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_order_estimate.aspx";
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
        }
        //TextBox Custname = (TextBox)FormView1.FindControl("VCustNameTextBox");
        //TextBox Soldname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
        //TextBox custaddr = (TextBox)FormView1.FindControl("VCustAddrTextBox");
        //TextBox soldaddr = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
        //TextBox caddr2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
        //TextBox saddr2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
        //TextBox ccity = (TextBox)FormView1.FindControl("VCityTextBox");
        //TextBox cstate = (TextBox)FormView1.FindControl("VStateTextBox");
        //TextBox czip = (TextBox)FormView1.FindControl("VZipTextBox");
        //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
        //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
        //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");


        Label orderno = (Label)FormView1.FindControl("VOrderNumLabel");
        Session["order_estweb_app"] = orderno.Text;
        Session["order_app"] = orderno.Text;
        //TextBox type = (TextBox)FormView1.FindControl("ordtypeTextBox");
        DropDownList type = (DropDownList)FormView1.FindControl("DropDownList2");
        Label lastuser = (Label)FormView1.FindControl("VUseridLabel");
        Label status = (Label)FormView1.FindControl("VStatLabel");
        TextBox cust = (TextBox)FormView1.FindControl("VCustomerTextBox2");

        TextBox sold = (TextBox)FormView1.FindControl("VSoldTextBox");
        TextBox orddate = (TextBox)FormView1.FindControl("VOrdateLabel");
        
        //TextBox code = (TextBox)FormView1.FindControl("VDueCodeTextBox");
        DropDownList code = (DropDownList)FormView1.FindControl("DropDownList1");
        TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
        
        TextBox lastdate = (TextBox)FormView1.FindControl("VLastDateTextBox");
        HiddenField10.Value = lastdate.Text;
        
        //TextBox prodate = (TextBox)FormView1.FindControl("VProdDateTextBox");
       
        TextBox ponum = (TextBox)FormView1.FindControl("VPonumTextBox");
        TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
        TextBox prod = (TextBox)FormView1.FindControl("VProdTextBox");
        TextBox overpct = (TextBox)FormView1.FindControl("VOverpctTextBox");
        TextBox taxgr = (TextBox)FormView1.FindControl("VTaxgrTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("VUnderpctTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("VTermsTextBox");
        TextBox termdscr = (TextBox)FormView1.FindControl("VTermdscrTextBox");
        // TextBox freight = (TextBox)FormView1.FindControl("RadioButtonList1");
        TextBox carr = (TextBox)FormView1.FindControl("VCarrierTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
        RadioButton origin = (RadioButton)FormView1.FindControl("RD6");

        if (destination.Checked == true)
        {
            fob.Text = "Dest";
        }
        else
        {
            fob.Text = "Orig";
        }

        TextBox sman = (TextBox)FormView1.FindControl("VSmanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("VSnameTextBox");
        //TextBox sman2 = (TextBox)FormView1.FindControl("VSman2TextBox");
        //TextBox sname2 = (TextBox)FormView1.FindControl("VSname2TextBox");
        //TextBox sman3 = (TextBox)FormView1.FindControl("VSman3TextBox");
        //TextBox sname3 = (TextBox)FormView1.FindControl("VSname3TextBox");
        TextBox ctype = (TextBox)FormView1.FindControl("VCtypeTextBox");
        TextBox cExp = (TextBox)FormView1.FindControl("VcExpTextBox");
        TextBox cnum = (TextBox)FormView1.FindControl("VCnumTextBox");
        TextBox cauth = (TextBox)FormView1.FindControl("VCauthTextBox");
        //TextBox spct1 = (TextBox)FormView1.FindControl("vSpctTextBox");
        //TextBox spct2 = (TextBox)FormView1.FindControl("vSpct2TextBox");
        //TextBox spct3 = (TextBox)FormView1.FindControl("vSpct3TextBox");
        //TextBox comm1 = (TextBox)FormView1.FindControl("vScommTextBox");
        //TextBox comm2 = (TextBox)FormView1.FindControl("vScomm2TextBox");
        //TextBox comm3 = (TextBox)FormView1.FindControl("vScomm3TextBox");
        //CheckBox maninvent = (CheckBox)FormView1.FindControl("VWhisCheckBox");

        //if (maninvent.Checked)
        //{
        //    HiddenField1.Value = "Yes";
        //}
        //else
        //{
        //    HiddenField1.Value = "No";
        //}
        TextBox freight = (TextBox)FormView1.FindControl("FRTextBox");
        RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD1");
        RadioButton Bill = (RadioButton)FormView1.FindControl("RD2");
        RadioButton Collect = (RadioButton)FormView1.FindControl("RD3");
        RadioButton party = (RadioButton)FormView1.FindControl("RD4");
        if (Prepaid.Checked == true)
        {
            freight.Text = "P";
        }
        if (Bill.Checked == true)
        {
            freight.Text = "B";
        }
        if (Collect.Checked == true)
        {
            freight.Text = "C";
        }
        if (party.Checked == true)
        {
            freight.Text = "T";
        }
        if (cExp.Text.Trim() == "")
        {
            cExp.Text = DateTime.Now.ToShortDateString();
        }

        string cError = "";
        orderentry ord = new orderentry();

        bool check = ord.ValidateEstEntryApp(UserLogin.UserName, "Update", "", Convert.ToInt32(orderno.Text.Trim()), cust.Text.Trim(), "", status.Text.Trim(), sold.Text.Trim(), Convert.ToDateTime(orddate.Text.Trim()), "", "", Convert.ToDateTime(duedate.Text.Trim()), "", "", Convert.ToDateTime("01/01/2009"), "", "", Convert.ToDateTime("01/01/2009"), "", "", "", "", "", "", ponum.Text.Trim(), contact.Text.Trim(), 0, 0, Convert.ToString(HiddenTerm.Value), "", Convert.ToInt32("0"), taxgr.Text.Trim(), freight.Text.Trim(), HiddenCarr.Value, fob.Text.Trim(), sman.Text.Trim(), sname.Text.Trim(), "", "", "", "", ctype.Text.Trim(), Convert.ToDateTime(cExp.Text.Trim()), cnum.Text.Trim(), cauth.Text.Trim(), "", type.SelectedValue.Trim(), Convert.ToInt32("0"), "", Convert.ToInt64("0"), "", 0, "", Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), 0, 0,ref cError);

        string value = Convert.ToString(check);
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            if (cError == "Due date can not be less than order date")
                duedate.Focus();
            else if (cError == "PO# is mandatory for this Customer...")
                ponum.Focus();
            else if (cError == "Customer PO already exists for Order/Item - ")
                ponum.Focus();
            else
                contact.Focus();
        }

        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmOrderNum"].DefaultValue = orderno.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = cust.Text.Trim();
            ObjectDataSource1.SelectParameters["prmUserid"].DefaultValue = lastuser.Text.Trim();
            ObjectDataSource1.SelectParameters["prmType"].DefaultValue = type.SelectedValue.Trim();
            ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = status.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSold"].DefaultValue = sold.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOrdate"].DefaultValue = orddate.Text.Trim();
            
            ObjectDataSource1.SelectParameters["prmDueCode"].DefaultValue = code.SelectedValue.Trim();
            ObjectDataSource1.SelectParameters["prmDueDate"].DefaultValue = duedate.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = Custname.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldName"].DefaultValue = Soldname.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCustAddr"].DefaultValue = custaddr.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldAddr"].DefaultValue = soldaddr.Text.Trim();            
            //ObjectDataSource1.SelectParameters["prmcustAddr2"].DefaultValue = caddr2.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldAddr2"].DefaultValue = saddr2.Text.Trim();
            ////ObjectDataSource1.SelectParameters["prmProdDate"].DefaultValue = prodate.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmCity"].DefaultValue = ccity.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmState"].DefaultValue = cstate.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmZip"].DefaultValue = czip.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldCity"].DefaultValue = scity.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldState"].DefaultValue = sstate.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmSoldZip"].DefaultValue = szip.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLastDate"].DefaultValue = HiddenField10.Value;
            ObjectDataSource1.SelectParameters["prmPonum"].DefaultValue = ponum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = contact.Text.Trim();
            ObjectDataSource1.SelectParameters["prmProd"].DefaultValue = prod.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOverpct"].DefaultValue = overpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTaxgr"].DefaultValue = taxgr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmUnderpct"].DefaultValue = underpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTermdscr"].DefaultValue = termdscr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = freight.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = HiddenCarr.Value;
            ObjectDataSource1.SelectParameters["prmFob"].DefaultValue = fob.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = sname.Text.Trim();

            ObjectDataSource1.SelectParameters["prmCtype"].DefaultValue = ctype.Text.Trim();
            ObjectDataSource1.SelectParameters["prmcExp"].DefaultValue = cExp.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCnum"].DefaultValue = cnum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCauth"].DefaultValue = cauth.Text.Trim();

            Session["order_entry_req_date_app"] = duedate.Text.Trim();
            Session["customer_fglookup_val_app"] = cust.Text.Trim();
            Session["order_entry_cust_no_app"] = cust.Text.Trim();
            Session["order_entry_cust_no"] = cust.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        
    }

    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Label ord11 = (Label)FormView1.FindControl("VOrderNumLabel11");
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmOrderNum"].DefaultValue = ord11.Text.Trim();

        Response.Write("<script>window.location.href='menu.aspx?webadd=additemweb';</script>");
    }
    protected void InsertButon_click(object sender, EventArgs e)
    {

        try
       {
            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_order_estimate.aspx";
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
                    Response.Write("<script>window.location.href = 'view_order_estimate.aspx';</script>");

                }
                //if (aUsers == "external")
                //{
                //    if (quote.Text == "")
                //    {
                //        Response.Write("<script>alert('Please enter quote no');</script>");
                //        Response.Write("<script>history.back();</script>");
                //    }
                //}
            }



            DropDownList type = (DropDownList)FormView1.FindControl("DropDownList2");
            Label lastuser = (Label)FormView1.FindControl("VUseridLabel");
            Label status = (Label)FormView1.FindControl("VStatLabel");
            TextBox cust = (TextBox)FormView1.FindControl("VCustomerTextBox");
            TextBox est = (TextBox)FormView1.FindControl("EstimateTextBox");
            Label job = (Label)FormView1.FindControl("VJobTextBox");
            Label job2 = (Label)FormView1.FindControl("VJob2TextBox");
            TextBox sold = (TextBox)FormView1.FindControl("VSoldTextBox");
            Label orddate = (Label)FormView1.FindControl("VOrdateLabel");
            //TextBox Custname = (TextBox)FormView1.FindControl("VCustNameTextBox");
            //TextBox Soldname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
            //TextBox code = (TextBox)FormView1.FindControl("VDueCodeTextBox");
            DropDownList code = (DropDownList)FormView1.FindControl("DropDownList1");
            TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
            //TextBox custaddr = (TextBox)FormView1.FindControl("VCustAddrTextBox");
            //TextBox soldaddr = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
            TextBox lastdate = (TextBox)FormView1.FindControl("VLastDateTextBox");
            //TextBox caddr2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
            //TextBox saddr2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
            //TextBox prodate = (TextBox)FormView1.FindControl("VProdDateTextBox");
            //TextBox ccity = (TextBox)FormView1.FindControl("VCityTextBox");
            //TextBox cstate = (TextBox)FormView1.FindControl("VStateTextBox");
            //TextBox czip = (TextBox)FormView1.FindControl("VZipTextBox");
            //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
            //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
            //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");
            TextBox ponum = (TextBox)FormView1.FindControl("VPonumTextBox");
            TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
            TextBox prod = (TextBox)FormView1.FindControl("VProdTextBox");
            TextBox overpct = (TextBox)FormView1.FindControl("VOverpctTextBox");
            TextBox taxgr = (TextBox)FormView1.FindControl("VTaxgrTextBox");
            TextBox underpct = (TextBox)FormView1.FindControl("VUnderpctTextBox");
            TextBox terms = (TextBox)FormView1.FindControl("VTermsTextBox");
            TextBox termdscr = (TextBox)FormView1.FindControl("VTermdscrTextBox");
            // TextBox freight = (TextBox)FormView1.FindControl("RadioButtonList1");
            TextBox carr = (TextBox)FormView1.FindControl("VCarrierTextBox");
            TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");



            Session["order_entry_job_no_1_app"] = job.Text;
            Session["order_entry_job_no_2_app"] = job2.Text;

            RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
            RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
            if (destination.Checked == true)
            {
                fob.Text = "Dest";
            }
            else
            {
                fob.Text = "Orig";
            }

            TextBox sman = (TextBox)FormView1.FindControl("VSmanTextBox");
            TextBox sname = (TextBox)FormView1.FindControl("VSnameTextBox");

            TextBox ctype = (TextBox)FormView1.FindControl("VCtypeTextBox");
            TextBox cExp = (TextBox)FormView1.FindControl("VcExpTextBox");
            TextBox cnum = (TextBox)FormView1.FindControl("VCnumTextBox");
            TextBox cauth = (TextBox)FormView1.FindControl("VCauthTextBox");

            TextBox freight = (TextBox)FormView1.FindControl("FRTextBox");
            RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD1");
            RadioButton Bill = (RadioButton)FormView1.FindControl("RD2");
            RadioButton Collect = (RadioButton)FormView1.FindControl("RD3");
            RadioButton party = (RadioButton)FormView1.FindControl("RD4");
            if (Prepaid.Checked == true)
            {
                freight.Text = "P";
            }
            if (Bill.Checked == true)
            {
                freight.Text = "B";
            }
            if (Collect.Checked == true)
            {
                freight.Text = "C";
            }
            if (party.Checked == true)
            {
                freight.Text = "T";
            }
            Session["order_est_value_check_app"] = null;

            if (cExp.Text.Trim() == "")
            {
                cExp.Text = DateTime.Now.ToShortDateString();
            }
            if (HiddenField9.Value == "")
            {
                HiddenField9.Value = "0";
            }
            if (quote.Text.Trim() == "")
            {
                quote.Text = "0";
            }
            Session["order_entry_quote_app"] = quote.Text;
            string cError = "";
            orderentry ord = new orderentry();

            bool check = ord.ValidateEstEntryApp(UserLogin.UserName, "Validate", "", Convert.ToInt32("0"), cust.Text.Trim(), lastuser.Text.Trim(), status.Text.Trim(), sold.Text.Trim(), Convert.ToDateTime(orddate.Text.Trim()), "", "", Convert.ToDateTime(duedate.Text.Trim()), "", "", Convert.ToDateTime(HiddenField10.Value), "", "", Convert.ToDateTime("01/01/2009"), "", "", "", "", "", "", ponum.Text.Trim(), contact.Text.Trim(), Convert.ToDecimal(overpct.Text.Trim()), Convert.ToDecimal(underpct.Text.Trim()), Convert.ToString(HiddenTerm.Value), Convert.ToString(HiddenTermdesc), Convert.ToInt32("0"), taxgr.Text.Trim(), freight.Text.Trim(), HiddenCarr.Value, fob.Text.Trim(), sman.Text.Trim(), sname.Text.Trim(), "", "", "", "", ctype.Text.Trim(), Convert.ToDateTime(cExp.Text.Trim()), cnum.Text.Trim(), cauth.Text.Trim(), "", type.SelectedValue.Trim(), Convert.ToInt32("0"), "", Convert.ToInt64("0"), HiddenField8.Value, Convert.ToInt32(HiddenField9.Value), "", Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToDecimal("0"), Convert.ToInt32(quote.Text.Trim()),ref cError);

            if (Hiddensold.Value != "")
            {
                sold.Text = Hiddensold.Value;
            }
            string value = Convert.ToString(check);

            if (cError != "")
            {
                HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
                if (cError == "Due date can not be less than order date")
                    duedate.Focus();
                else if (cError == "PO# is mandatory for this Customer...")
                    ponum.Focus();
                else if (cError == "Customer PO already exists for Order/Item - ")
                    ponum.Focus();
                else if (cError == "Invalid Salesman")
                    sman.Focus();
                else if (cError == "Invalid Customer. Try Help..")
                    cust.Focus();
                else if (cError == "Invalid Sold To. Try Help..")
                    sold.Focus();
                else
                    contact.Focus();
            }
                      
            
            if (value == "True")
            {
                if (Convert.ToInt32(quote.Text.Trim()) == 0)
                {
                    Session["order_entry_cust_quote_app"] = null;
                }

                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";

                ObjectDataSource1.SelectParameters["prmQuote"].DefaultValue = quote.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUserid"].DefaultValue = lastuser.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = est.Text;
                ObjectDataSource1.SelectParameters["prmJob"].DefaultValue = job.Text;
                ObjectDataSource1.SelectParameters["prmJob2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmType"].DefaultValue = type.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = status.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSold"].DefaultValue = sold.Text.Trim();
                ObjectDataSource1.SelectParameters["prmOrdate"].DefaultValue = orddate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDueCode"].DefaultValue = code.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmDueDate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLastDate"].DefaultValue = HiddenField10.Value;
                               
                ObjectDataSource1.SelectParameters["prmPonum"].DefaultValue = ponum.Text.Trim();
                ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = contact.Text.Trim();
                ObjectDataSource1.SelectParameters["prmProd"].DefaultValue = prod.Text.Trim();
                ObjectDataSource1.SelectParameters["prmOverpct"].DefaultValue = overpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTaxgr"].DefaultValue = taxgr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUnderpct"].DefaultValue = underpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTermdscr"].DefaultValue = termdscr.Text.Trim();

                ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = freight.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = HiddenCarr.Value;
                ObjectDataSource1.SelectParameters["prmFob"].DefaultValue = fob.Text.Trim();

                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSname"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCtype"].DefaultValue = ctype.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcExp"].DefaultValue = cExp.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCnum"].DefaultValue = cnum.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCauth"].DefaultValue = cauth.Text.Trim();

                ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = HiddenQuoteQty.Value;
                ObjectDataSource1.SelectParameters["prmPrice"].DefaultValue = HiddenQuotePrice.Value;
                ObjectDataSource1.SelectParameters["prmUom"].DefaultValue = HiddenQuoteUom.Value;

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
                        ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = HiddenTerm.Value;
                        ObjectDataSource1.SelectParameters["prmTermdscr"].DefaultValue = HiddenTermdesc.Value;

                    }
                }


                Session["order_entry_customer_po_app"] = ponum.Text.Trim();
                Session["order_entry_req_date_app"] = duedate.Text.Trim();
                Session["order_entry_req_code_app"] = code.SelectedValue.Trim();
                Session["order_entry_prom_code_app"] = code.SelectedValue.Trim(); ;
                Session["order_entry_prom_date_app"] = duedate.Text.Trim();
                Session["order_entry_over_app"] = overpct.Text.Trim();
                Session["order_entry_under_app"] = underpct.Text.Trim();
                Session["order_entry_sman1_app"] = sman.Text.Trim();
                Session["order_entry_luser_app"] = lastuser.Text.Trim(); ;
                Session["order_entry_type_app"] = type.Text.Trim();
                Session["order_entry_sname1_app"] = sname.Text.Trim();
                Session["order_entry_type_app"] = type.SelectedValue.Trim();
                Session["customer_fglookup_val_app"] = cust.Text.Trim();
                Session["order_entry_cust_no_app"] = cust.Text.Trim();
                Session["order_entry_cust_no"] = cust.Text.Trim();
                if (quote.Text != "")
                {
                    Session["view_line_est_app"] = 1;
                }

                mode = "add";
                FormView1.ChangeMode(FormViewMode.ReadOnly);

                HiddenQuoteNum.Value = "";
                //Response.Write("<script> location.href='add_itemapp.aspx?webitem=insert'</script>");
                Response.Write("<script>window.location.href='add_orderapp.aspx?webadd=additemweb';</script>");
            }
            else
            {
                             
            }
        }
        catch { }
        finally
        {

        }

        
    }


    //protected void open_popup()
    //{
    //    Session["itm_visibility"] = "emode";
    //    RegisterStartupScript("str1", "<script>window.open('view_item_popup.aspx?line=1&totline=" + this.line_total + "','ItemWindow','width=860,height=520, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
    //}

    protected void Btn_Insert_cancel(object sender, EventArgs e)
    {
        HiddenQuoteNum.Value = "";
        
        Session["order_estweb_app"] = null;
        Response.Write("<script>window.location.href='menu.aspx?webadd=additemweb';</script>");
    }

    protected void UpdateButton_Cancel_Click(object sender, EventArgs e)
    {
        Label ord = (Label)FormView1.FindControl("VOrderNumLabel");
        Session["order_estweb_app"] = ord.Text;
    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        if (Session["get_new_order_est_2_app"] != null)
        {
            Session["order_estweb_app"] = Session["get_new_order_est_2_app"];
        }

        try
        {

            Label fob = (Label)FormView1.FindControl("Label5");
            RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
            RadioButton origin = (RadioButton)FormView1.FindControl("RD6");

            if (fob.Text == "Destination")
            {
                destination.Checked = true;
            }
            else
            {
                origin.Checked = true;
            }

            Label frieght = (Label)FormView1.FindControl("Label1");
            RadioButton Prepaid = (RadioButton)FormView1.FindControl("RD1");
            RadioButton Bill = (RadioButton)FormView1.FindControl("RD2");
            RadioButton Collect = (RadioButton)FormView1.FindControl("RD3");
            RadioButton party = (RadioButton)FormView1.FindControl("RD4");

            if (frieght.Text == "P")
            {
                Prepaid.Checked = true;
            }
            if (frieght.Text == "B")
            {
                Bill.Checked = true;
            }
            if (frieght.Text == "C")
            {
                Collect.Checked = true;
            }
            if (frieght.Text == "T")
            {
                party.Checked = true;
            }

        }
        catch
        {
            //return;
        }
    }

    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label orderno = (Label)FormView1.FindControl("VOrderNumLabel11");
            Label rec_key = (Label)FormView1.FindControl("lbl_rec_key");
            Label stat = (Label)FormView1.FindControl("VStatLabel");
            Session["order_rec_key_app"] = rec_key.Text;
            Session["my_new_order_est_app"] = orderno.Text;
            if (Session["order_estweb_app"] == Session["my_new_order_est_app"])
            {
                Session["my_new_order_est_app"] = null;
            }
            else
            {
                Session["order_estweb_app"] = Session["my_new_order_est_app"];
                Session["order_app"] = Session["my_new_order_est_app"];
            }
            Session["order_entry_status_check_app"] = stat.Text.Trim();

        }
        catch { }
    }
    protected void newAddButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
       
    }
    protected void BillTo_TextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            TextBox est = (TextBox)FormView1.FindControl("estimateTextBox");
            TextBox cust = (TextBox)FormView1.FindControl("VCustomerTextBox");
            //TextBox cname = (TextBox)FormView1.FindControl("VCustNameTextBox");
            //TextBox cadd1 = (TextBox)FormView1.FindControl("VCustAddrTextBox");
            //TextBox cadd2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
            //TextBox ccity = (TextBox)FormView1.FindControl("VCityTextBox");
            //TextBox cstate = (TextBox)FormView1.FindControl("VStateTextBox");
            //TextBox czip = (TextBox)FormView1.FindControl("VZipTextBox");
            TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
            TextBox sman = (TextBox)FormView1.FindControl("VSoldTextBox");
            //TextBox sname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
            //TextBox sadd1 = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
            //TextBox sadd2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
            //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
            //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
            //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");
            TextBox overrun = (TextBox)FormView1.FindControl("VOverpctTextBox");
            TextBox taxcode = (TextBox)FormView1.FindControl("VTaxgrTextBox");
            TextBox underrun = (TextBox)FormView1.FindControl("VUnderpctTextBox");
            TextBox pterms = (TextBox)FormView1.FindControl("VTermsTextBox");
            TextBox ptdesc = (TextBox)FormView1.FindControl("VTermdscrTextBox");
            TextBox carrier = (TextBox)FormView1.FindControl("VCarrierTextBox");
            TextBox sales = (TextBox)FormView1.FindControl("VSmanTextBox");
            TextBox salesman = (TextBox)FormView1.FindControl("VSnameTextBox");
            TextBox lastship = (TextBox)FormView1.FindControl("VLastDateTextBox");
            TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

            RadioButton rd1 = (RadioButton)FormView1.FindControl("RD1");
            RadioButton rd2 = (RadioButton)FormView1.FindControl("RD2");
            RadioButton rd3 = (RadioButton)FormView1.FindControl("RD3");
            RadioButton rd4 = (RadioButton)FormView1.FindControl("RD4");
            RadioButton rd5 = (RadioButton)FormView1.FindControl("RD5");
            RadioButton rd6 = (RadioButton)FormView1.FindControl("RD6");


            //ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "OrderCust";
            //ObjectDataSource2.SelectParameters["prmText"].DefaultValue = cust.Text;
            if (quote.Text == "" && est.Text == "")
            {
                Order ord = new Order();
                DataSet ds = new DataSet();
                ds = ord.SelectOrderCust("OrderCust", UserLogin.UserName, "", "", cust.Text);

                //Response.Write(ds.Tables[0].Rows[0][13].ToString());
                //Response.Write(ds.Tables[0].Rows[0][16].ToString());

                if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "prepaid")
                {
                    rd1.Checked = true;
                    rd2.Checked = false;
                    rd3.Checked = false;
                    rd4.Checked = false;
                }
                if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "bill")
                {
                    rd1.Checked = false;
                    rd2.Checked = true;
                    rd3.Checked = false;
                    rd4.Checked = false;
                }
                if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "collect")
                {
                    rd1.Checked = false;
                    rd2.Checked = false;
                    rd3.Checked = true;
                    rd4.Checked = false;
                }
                if (ds.Tables[0].Rows[0][13].ToString().ToLower() == "3rd party")
                {
                    rd1.Checked = false;
                    rd2.Checked = false;
                    rd3.Checked = false;
                    rd4.Checked = true;
                }
                if (ds.Tables[0].Rows[0][16].ToString().ToLower() == "dest")
                {
                    rd5.Checked = true;
                    rd6.Checked = false;
                }
                if (ds.Tables[0].Rows[0][16].ToString().ToLower() == "orig")
                {
                    rd5.Checked = false;
                    rd6.Checked = true;
                }

                sman.Text = cust.Text;

                //cname.Text = ds.Tables[0].Rows[0][1].ToString();
                //cadd1.Text = ds.Tables[0].Rows[0][2].ToString();
                //cadd2.Text = ds.Tables[0].Rows[0][3].ToString();
                //ccity.Text = ds.Tables[0].Rows[0][4].ToString();
                //cstate.Text = ds.Tables[0].Rows[0][5].ToString();
                //czip.Text = ds.Tables[0].Rows[0][6].ToString();
                sales.Text = ds.Tables[0].Rows[0][8].ToString();
                salesman.Text = ds.Tables[0].Rows[0][9].ToString();
                HiddenCarr.Value = ds.Tables[0].Rows[0][17].ToString();
                carrier.Text = ds.Tables[0].Rows[0][17].ToString();

                contact.Text = ds.Tables[0].Rows[0][18].ToString();
                overrun.Text = ds.Tables[0].Rows[0][19].ToString();
                underrun.Text = ds.Tables[0].Rows[0][20].ToString();
                HiddenTerm.Value = ds.Tables[0].Rows[0][21].ToString();
                HiddenTermdesc.Value = ds.Tables[0].Rows[0][22].ToString();

                pterms.Text = ds.Tables[0].Rows[0][21].ToString();
                ptdesc.Text = ds.Tables[0].Rows[0][22].ToString();

                taxcode.Text = ds.Tables[0].Rows[0][24].ToString();
                HiddenField10.Value = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][36].ToString()).ToShortDateString());
                lastship.Text = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][36].ToString()).ToShortDateString());

                //sname.Text = ds.Tables[0].Rows[0][38].ToString();
                //sadd1.Text = ds.Tables[0].Rows[0][39].ToString();
                //sadd2.Text = ds.Tables[0].Rows[0][40].ToString();
                //scity.Text = ds.Tables[0].Rows[0][41].ToString();
                //sstate.Text = ds.Tables[0].Rows[0][42].ToString();
                //szip.Text = ds.Tables[0].Rows[0][43].ToString();
                duedate.Text = Convert.ToString(Convert.ToDateTime(ds.Tables[0].Rows[0][35].ToString()).ToShortDateString());
                contact.Focus();
            }
        }
        catch { }
    }
    protected void Quote_TextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

            TextBox estimate = (TextBox)FormView1.FindControl("estimateTextBox");
            Label job1 = (Label)FormView1.FindControl("VJobTextBox");
            Label job2 = (Label)FormView1.FindControl("VJob2TextBox");
            TextBox billto = (TextBox)FormView1.FindControl("VCustomerTextBox");
            //TextBox bname = (TextBox)FormView1.FindControl("VCustNameTextBox");
            //TextBox badd1 = (TextBox)FormView1.FindControl("VCustAddrTextBox");
            //TextBox badd2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
            //TextBox bcity = (TextBox)FormView1.FindControl("VCityTextBox");
            //TextBox bstate = (TextBox)FormView1.FindControl("VStateTextBox");
            //TextBox bzip = (TextBox)FormView1.FindControl("VZipTextBox");
            TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
            TextBox soldto = (TextBox)FormView1.FindControl("VSoldTextBox");
            //TextBox sname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
            //TextBox sadd1 = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
            //TextBox sadd2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
            //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
            //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
            //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");
            TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
            TextBox lastship = (TextBox)FormView1.FindControl("VLastDateTextBox");
            TextBox preorder = (TextBox)FormView1.FindControl("VProdTextBox");
            TextBox overrun = (TextBox)FormView1.FindControl("VOverpctTextBox");
            TextBox underrun = (TextBox)FormView1.FindControl("VUnderpctTextBox");
            TextBox taxcode = (TextBox)FormView1.FindControl("VTaxgrTextBox");
            TextBox payterms = (TextBox)FormView1.FindControl("VTermsTextBox");
            TextBox paydesc = (TextBox)FormView1.FindControl("VTermdscrTextBox");
            TextBox carrier = (TextBox)FormView1.FindControl("VCarrierTextBox");
            TextBox salesrep = (TextBox)FormView1.FindControl("VSmanTextBox");
            TextBox salesname = (TextBox)FormView1.FindControl("VSnameTextBox");
            RadioButton frerd1 = (RadioButton)FormView1.FindControl("RD1");
            RadioButton frerd2 = (RadioButton)FormView1.FindControl("RD2");
            RadioButton frerd3 = (RadioButton)FormView1.FindControl("RD3");
            RadioButton frerd4 = (RadioButton)FormView1.FindControl("RD4");
            RadioButton fobrd1 = (RadioButton)FormView1.FindControl("RD5");
            RadioButton fobrd2 = (RadioButton)FormView1.FindControl("RD6");

            if (quote.Text != "")
            {
                orderentry ordentry = new orderentry();
                DataSet dsquote = new DataSet();
                dsquote = ordentry.SeOrderQuoteup("OrderQuote", UserLogin.UserName, "", Convert.ToInt32(quote.Text));

                estimate.Text = dsquote.Tables[0].Rows[0][0].ToString();
                billto.Text = dsquote.Tables[0].Rows[0][1].ToString();
                soldto.Text = dsquote.Tables[0].Rows[0][1].ToString();
                salesrep.Text = dsquote.Tables[0].Rows[0][8].ToString();
                job1.Text = dsquote.Tables[0].Rows[0][14].ToString();
                job2.Text = dsquote.Tables[0].Rows[0][15].ToString();
                carrier.Text = dsquote.Tables[0].Rows[0][10].ToString();
                //bname.Text = dsquote.Tables[0].Rows[0][16].ToString();
                //badd1.Text = dsquote.Tables[0].Rows[0][17].ToString();
                //badd2.Text = dsquote.Tables[0].Rows[0][18].ToString();
                //bcity.Text = dsquote.Tables[0].Rows[0][19].ToString();
                //bstate.Text = dsquote.Tables[0].Rows[0][20].ToString();
                //bzip.Text = dsquote.Tables[0].Rows[0][21].ToString();
                contact.Text = dsquote.Tables[0].Rows[0][22].ToString();
                lastship.Text = Convert.ToString(Convert.ToDateTime(dsquote.Tables[0].Rows[0][23].ToString()).ToShortDateString());

                HiddenField10.Value = Convert.ToString(Convert.ToDateTime(dsquote.Tables[0].Rows[0][23].ToString()).ToShortDateString());
                HiddenCarr.Value = dsquote.Tables[0].Rows[0][10].ToString();
                HiddenTerm.Value = dsquote.Tables[0].Rows[0][26].ToString();
                HiddenTermdesc.Value = dsquote.Tables[0].Rows[0][27].ToString();
                HiddenField8.Value = dsquote.Tables[0].Rows[0][9].ToString();


                duedate.Text = Convert.ToString(Convert.ToDateTime(dsquote.Tables[0].Rows[0][25].ToString()).ToShortDateString());
                payterms.Text = dsquote.Tables[0].Rows[0][26].ToString();
                paydesc.Text = dsquote.Tables[0].Rows[0][27].ToString();
                overrun.Text = dsquote.Tables[0].Rows[0][28].ToString();
                underrun.Text = dsquote.Tables[0].Rows[0][29].ToString();
                if (dsquote.Tables[0].Rows[0][11].ToString().ToLower() == "p")
                {
                    frerd1.Checked = true;
                    frerd2.Checked = false;
                    frerd3.Checked = false;
                    frerd4.Checked = false;
                }

                if (dsquote.Tables[0].Rows[0][11].ToString().ToLower() == "b")
                {
                    frerd1.Checked = false;
                    frerd2.Checked = true;
                    frerd3.Checked = false;
                    frerd4.Checked = false;
                }
                if (dsquote.Tables[0].Rows[0][11].ToString().ToLower() == "c")
                {
                    frerd1.Checked = false;
                    frerd2.Checked = false;
                    frerd3.Checked = true;
                    frerd4.Checked = false;
                }
                if (dsquote.Tables[0].Rows[0][11].ToString().ToLower() == "t")
                {
                    frerd1.Checked = false;
                    frerd2.Checked = false;
                    frerd3.Checked = false;
                    frerd4.Checked = true;
                }

                if (dsquote.Tables[0].Rows[0][30].ToString().ToLower() == "dest")
                {
                    fobrd1.Checked = true;
                    fobrd2.Checked = false;
                }
                else
                {
                    fobrd2.Checked = true;
                    fobrd1.Checked = false;
                }

                taxcode.Text = dsquote.Tables[0].Rows[0][31].ToString();
                //sname.Text = dsquote.Tables[0].Rows[0][35].ToString();
                //sadd1.Text = dsquote.Tables[0].Rows[0][36].ToString();
                //sadd2.Text = dsquote.Tables[0].Rows[0][37].ToString();
                //scity.Text = dsquote.Tables[0].Rows[0][38].ToString();
                //sstate.Text = dsquote.Tables[0].Rows[0][39].ToString();
                //szip.Text = dsquote.Tables[0].Rows[0][40].ToString();
                salesname.Text = dsquote.Tables[0].Rows[0][42].ToString();
                job1.Text = dsquote.Tables[0].Rows[0][43].ToString();
                preorder.Text = dsquote.Tables[0].Rows[0][44].ToString();


                TextBox custpo = (TextBox)FormView1.FindControl("VPonumTextBox");
                if (HiddenQuoteNum.Value == "")
                {
                    HiddenQuoteNum.Value = "0";
                }

                if (Convert.ToInt32(quote.Text.Trim()) > 0 && Convert.ToInt32(HiddenQuoteNum.Value) <= 0)
                {

                    // Response.Write("<script>window.open('quantity_lookup.aspx?est= " + quote.Text.Trim() + "','typoeordLookupWindow','width=500,height=200, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");
                    RegisterStartupScript("str", "<script>window.open('quantity_lookup.aspx?est= " + quote.Text.Trim() + "','typoeordLookupWindow','width=500,height=200, scrollbars=1, toolbars=1,statusbar=1,resizeable=1');</script>");

                }
            }

        }
        catch
        {
            HttpContext.Current.Response.Write("<script>alert('Sorry quote does not match with user')</script>");
            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");
            quote.Focus();
        }
    }

    protected void Estimate_TextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

            TextBox estimate = (TextBox)FormView1.FindControl("estimateTextBox");
            Label job1 = (Label)FormView1.FindControl("VJobTextBox");
            Label job2 = (Label)FormView1.FindControl("VJob2TextBox");
            TextBox billto = (TextBox)FormView1.FindControl("VCustomerTextBox");
            //TextBox bname = (TextBox)FormView1.FindControl("VCustNameTextBox");
            //TextBox badd1 = (TextBox)FormView1.FindControl("VCustAddrTextBox");
            //TextBox badd2 = (TextBox)FormView1.FindControl("VcustAddr2TextBox");
            //TextBox bcity = (TextBox)FormView1.FindControl("VCityTextBox");
            //TextBox bstate = (TextBox)FormView1.FindControl("VStateTextBox");
            //TextBox bzip = (TextBox)FormView1.FindControl("VZipTextBox");
            TextBox contact = (TextBox)FormView1.FindControl("VContactTextBox");
            TextBox soldto = (TextBox)FormView1.FindControl("VSoldTextBox");
            //TextBox sname = (TextBox)FormView1.FindControl("VSoldNameTextBox");
            //TextBox sadd1 = (TextBox)FormView1.FindControl("VSoldAddrTextBox");
            //TextBox sadd2 = (TextBox)FormView1.FindControl("VSoldAddr2TextBox");
            //TextBox scity = (TextBox)FormView1.FindControl("VSoldCityTextBox");
            //TextBox sstate = (TextBox)FormView1.FindControl("VSoldStateTextBox");
            //TextBox szip = (TextBox)FormView1.FindControl("VSoldZipTextBox");
            TextBox duedate = (TextBox)FormView1.FindControl("VDueDateTextBox");
            TextBox lastship = (TextBox)FormView1.FindControl("VLastDateTextBox");
            TextBox preorder = (TextBox)FormView1.FindControl("VProdTextBox");
            TextBox overrun = (TextBox)FormView1.FindControl("VOverpctTextBox");
            TextBox underrun = (TextBox)FormView1.FindControl("VUnderpctTextBox");
            TextBox taxcode = (TextBox)FormView1.FindControl("VTaxgrTextBox");
            TextBox payterms = (TextBox)FormView1.FindControl("VTermsTextBox");
            TextBox paydesc = (TextBox)FormView1.FindControl("VTermdscrTextBox");
            TextBox carrier = (TextBox)FormView1.FindControl("VCarrierTextBox");
            TextBox salesrep = (TextBox)FormView1.FindControl("VSmanTextBox");
            TextBox salesname = (TextBox)FormView1.FindControl("VSnameTextBox");
            RadioButton frerd1 = (RadioButton)FormView1.FindControl("RD1");
            RadioButton frerd2 = (RadioButton)FormView1.FindControl("RD2");
            RadioButton frerd3 = (RadioButton)FormView1.FindControl("RD3");
            RadioButton frerd4 = (RadioButton)FormView1.FindControl("RD4");
            RadioButton fobrd1 = (RadioButton)FormView1.FindControl("RD5");
            RadioButton fobrd2 = (RadioButton)FormView1.FindControl("RD6");

            LookUp lookup = new LookUp();
            DataSet dsestimate = new DataSet();
            dsestimate = lookup.SeEstimateLookup("search", UserLogin.UserName, "Estimate", "EQUAL", estimate.Text, "");

            string efreight = dsestimate.Tables[0].Rows[0][11].ToString();

            if (efreight.ToLower() == "p")
            {
                frerd1.Checked = true;
                frerd2.Checked = false;
                frerd3.Checked = false;
                frerd4.Checked = false;
            }
            if (efreight.ToLower() == "b")
            {
                frerd1.Checked = false;
                frerd2.Checked = true;
                frerd3.Checked = false;
                frerd4.Checked = false;
            }
            if (efreight.ToLower() == "c")
            {
                frerd1.Checked = false;
                frerd2.Checked = false;
                frerd3.Checked = true;
                frerd4.Checked = false;
            }
            if (efreight.ToLower() == "t")
            {
                frerd1.Checked = false;
                frerd2.Checked = false;
                frerd3.Checked = false;
                frerd4.Checked = true;
            }


            estimate.Text = dsestimate.Tables[0].Rows[0][0].ToString();
            billto.Text = dsestimate.Tables[0].Rows[0][1].ToString();
            soldto.Text = dsestimate.Tables[0].Rows[0][1].ToString();
            salesrep.Text = dsestimate.Tables[0].Rows[0][8].ToString();
            job1.Text = dsestimate.Tables[0].Rows[0][14].ToString();
            job2.Text = dsestimate.Tables[0].Rows[0][15].ToString();
            carrier.Text = dsestimate.Tables[0].Rows[0][10].ToString();
            //bname.Text = dsestimate.Tables[0].Rows[0][16].ToString();
            //badd1.Text = dsestimate.Tables[0].Rows[0][17].ToString();
            //badd2.Text = dsestimate.Tables[0].Rows[0][18].ToString();
            //bcity.Text = dsestimate.Tables[0].Rows[0][19].ToString();
            //bstate.Text = dsestimate.Tables[0].Rows[0][20].ToString();
            //bzip.Text = dsestimate.Tables[0].Rows[0][21].ToString();
            contact.Text = dsestimate.Tables[0].Rows[0][22].ToString();
            lastship.Text = Convert.ToString(Convert.ToDateTime(dsestimate.Tables[0].Rows[0][23].ToString()).ToShortDateString());

            HiddenField10.Value = Convert.ToString(Convert.ToDateTime(dsestimate.Tables[0].Rows[0][23].ToString()).ToShortDateString());
            HiddenCarr.Value = dsestimate.Tables[0].Rows[0][10].ToString();
            HiddenTerm.Value = dsestimate.Tables[0].Rows[0][26].ToString();
            HiddenTermdesc.Value = dsestimate.Tables[0].Rows[0][27].ToString();
            HiddenField8.Value = dsestimate.Tables[0].Rows[0][9].ToString();


            duedate.Text = Convert.ToString(Convert.ToDateTime(dsestimate.Tables[0].Rows[0][25].ToString()).ToShortDateString());
            payterms.Text = dsestimate.Tables[0].Rows[0][26].ToString();
            paydesc.Text = dsestimate.Tables[0].Rows[0][27].ToString();
            overrun.Text = dsestimate.Tables[0].Rows[0][28].ToString();
            underrun.Text = dsestimate.Tables[0].Rows[0][29].ToString();

            if (dsestimate.Tables[0].Rows[0][30].ToString().ToLower() == "dest")
            {
                fobrd1.Checked = true;
                fobrd2.Checked = false;
            }
            else
            {
                fobrd2.Checked = true;
                fobrd1.Checked = false;
            }

            taxcode.Text = dsestimate.Tables[0].Rows[0][31].ToString();
            //sname.Text = dsestimate.Tables[0].Rows[0][35].ToString();
            //sadd1.Text = dsestimate.Tables[0].Rows[0][36].ToString();
            //sadd2.Text = dsestimate.Tables[0].Rows[0][37].ToString();
            //scity.Text = dsestimate.Tables[0].Rows[0][38].ToString();
            //sstate.Text = dsestimate.Tables[0].Rows[0][39].ToString();
            //szip.Text = dsestimate.Tables[0].Rows[0][40].ToString();
            salesname.Text = dsestimate.Tables[0].Rows[0][42].ToString();
            job1.Text = dsestimate.Tables[0].Rows[0][43].ToString();
            preorder.Text = dsestimate.Tables[0].Rows[0][44].ToString();

            TextBox custpo = (TextBox)FormView1.FindControl("VPonumTextBox");
            if (HiddenQuoteNum.Value == "")
            {
                HiddenQuoteNum.Value = "0";
            }


        }
        catch
        {
            //HttpContext.Current.Response.Write("<script>alert('Sorry Estimate does not match with user')</script>");
            //TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");
            //quote.Focus();
        }
    }


    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

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

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["view_item_index_val_app"] = GridView1.SelectedIndex;
        Session["order_est_value_check_app"] = 1;
        Session["line_app"] = GridView1.SelectedRow.Cells[1].Text;
        Session["order_app"] = Session["order_app"];
        Session["order_est_app"] = Session["order_APP"];
        Session["view_line_est_app"] = Session["line_app"];
        Session["orderestimate121_app"] = GridView1.SelectedRow.Cells[8].Text;
        Session["order_entry_est_no_app"] = GridView1.SelectedRow.Cells[8].Text;
        Session["order_rec_key_app"] = ((Label)GridView1.SelectedRow.FindControl("Label_vReckey")).Text;
        foreach (GridViewRow gvr in GridView1.Rows)
        {

            Label itemname = (Label)GridView1.SelectedRow.FindControl("Label2");
            //Response.Write(itemname.Text);
            Session["item_app"] = itemname.Text;
        }
        Session["val_app"] = Session["line_app"];
        Session["index2_app"] = Convert.ToInt32(Session["val_app"]) - 1;
        Page_Load(sender, e);
    }

    //protected void itemButon_click(object sender, EventArgs e)
    //{
    //    TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");
    //    UserClass.CheckLogin(Page);
    //    UserClass UserLogin = (UserClass)Session["User"];
    //    if (quote.Text.Trim() == "")
    //        quote.Text = "0";

    //    if (((quote.Text.Trim() == "") || (quote.Text.Trim() == "0")) && Session["order_estweb_app"] == null)
    //    {            
    //        HttpContext.Current.Response.Write("<script>alert('Please Enter Quote for create Item')</script>");
    //        return;
    //    }

    //    orderentry ord = new orderentry();
    //    DataSet ds = new DataSet();
    //    ds = ord.SelectViewItemEstimate(UserLogin.UserName, "AddWebItem", Convert.ToInt32(Session["order_estweb_app"]), 0, "", Convert.ToString(HiddenFieldFgItem.Value), "", 0, "", "", "", "", 0, "", "", "", "", 0, 0, "", Convert.ToDateTime("01/01/2014"), 0, "", Convert.ToDateTime("01/01/2014"), 0, 0, 0, 0, 0, 0, "", "", "", "", 0, 0, "", "", "", "", "", "", 0, 0, 0, 0, 0, 0, 0, Convert.ToInt32(quote.Text), "y");


    //    try
    //    {
    //        Order mail = new Order();
    //        mail.OrderMail(Convert.ToString(UserLogin.UserName), "MailOrder", Convert.ToString(Session["order_estweb_app"]), "", Convert.ToString(HiddenFieldFgItem.Value));
    //        Response.Write("<script> location.href='add_orderapp.aspx'</script>");

    //    }
    //    catch
    //    { }   
        

    //}


    protected void Freight_click(Object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "CalcFreight";
    }

    protected void FormView2_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                Button update = (Button)FormView2.FindControl("UpdateButton");
                Button fri = (Button)FormView2.FindControl("CalFrightButton");

                Label tax = (Label)FormView2.FindControl("Tax1Label");
                Label tot = (Label)FormView2.FindControl("TotalLabel");
                Label cos = (Label)FormView2.FindControl("costLabel");

                TaxesHiddenField.Value = tax.Text;
                TotHiddenField.Value = tot.Text;
                CosHiddenField.Value = cos.Text;
                                
            }
        }
        catch { }
    }
    protected void Update_click(Object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        UserClass.CheckLogin(Page);

        TextBox Weight = (TextBox)FormView2.FindControl("Weight1TextBox");
        TextBox Tax = (TextBox)FormView2.FindControl("TaxesTextBox");
        TextBox Freight = (TextBox)FormView2.FindControl("FreigTextBox");
        CheckBox BillF = (CheckBox)FormView2.FindControl("BillFCheckBox");
        TextBox Tot = (TextBox)FormView2.FindControl("TotTextBox");
        TextBox Com = (TextBox)FormView2.FindControl("ComTextBox");
        TextBox Cos = (TextBox)FormView2.FindControl("CosTextBox");
               
        try
        {

            ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "update";
            ObjectDataSource3.SelectParameters["prmWeight"].DefaultValue = Weight.Text.Trim();
            ObjectDataSource3.SelectParameters["prmTax"].DefaultValue = TaxesHiddenField.Value;
            ObjectDataSource3.SelectParameters["prmFreight"].DefaultValue = Freight.Text.Trim();
            ObjectDataSource3.SelectParameters["prmFbill"].DefaultValue = BillF.Checked.ToString();
            ObjectDataSource3.SelectParameters["prmRevenue"].DefaultValue = TotHiddenField.Value;
            ObjectDataSource3.SelectParameters["prmCost"].DefaultValue = CosHiddenField.Value;
            ObjectDataSource3.SelectParameters["prmComm"].DefaultValue = Com.Text.Trim();
        }
        catch { }
        finally
        {
            FormView2.ChangeMode(FormViewMode.ReadOnly);
        }
    }


    //protected void quote_text_changed(object sender, EventArgs e)
    //{
    //    try
    //    {
    //        UserClass.CheckLogin(Page);
    //        UserClass UserLogin = (UserClass)Session["User"];
    //        Label cust = (Label)FormView1.FindControl("VCustomerLabel");
    //        TextBox quote = (TextBox)FormView1.FindControl("RfqTextBox");

    //        if (quote.Text == "")
    //            quote.Text = "0";

    //        LookUp lookup = new LookUp();
    //        DataSet dsestimate = new DataSet();
    //        dsestimate = lookup.ItemQuoteLookup("search", UserLogin.UserName, "Quote", "EQUAL", quote.Text, 0, cust.Text);

    //        HiddenFieldFgItem.Value = dsestimate.Tables[0].Rows[0][1].ToString();          

    //    }
    //    catch
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('Invalid Quote, Try Help')</script>");
    //        return;
    //    }
    //}

    
    protected void addlink_button_Click(object sender, EventArgs e)
    {
        Response.Write("<script> location.href='add_itemapp.aspx?webitem=insert'</script>");
    }
    protected void updatelink_button_Click(object sender, EventArgs e)
    {
        if (GridView1.Rows.Count > 0)
        {
            Response.Write("<script> location.href='add_itemapp.aspx?webitem=edit'</script>");
        }
    }
    protected void viewdelete_button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        orderentry ord = new orderentry();
        DataSet ds = new DataSet();
        ds = ord.SelectViewItemEstimate(UserLogin.UserName, "Delete", Convert.ToInt32(Session["order_estweb_app"]), Convert.ToInt32(Session["view_line_est_app"]) , "","","",0, "", "","","",0 ,"","","","",0,0,"",Convert.ToDateTime("01/01/2012"),0,"",Convert.ToDateTime("01/01/2012"),0,0,0,0,0,0,"","","","",0,0,"","","","","","",0,0,0,0,0,0,0,0,"");
        Response.Write("<script> location.href='add_orderapp.aspx?webadd=edit'</script>");
       
    }
    protected void ordUpdatButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Edit);
    }


}

