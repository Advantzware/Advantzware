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
public partial class add_rfqapp : System.Web.UI.Page
{

    string userid = "";
    public add_rfqapp()
    {

    }
    protected void Page_Load(object sender, EventArgs e)
    {       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";
         
        if (!Page.IsPostBack)
        {
            
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_rfqs.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;
                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                userid = aUsers;
                if (aUsers == "external")
                {                    
                    try
                    {
                        Label commlabel = (Label)FormView1.FindControl("commlabel");
                        Label comm = (Label)FormView1.FindControl("Label15");
                        comm.Visible = false;
                        commlabel.Visible = false;                        
                    }
                    catch
                    {
                    }
                 
                }
                if (vCanRun == true)
                {

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }
                lblComp.Text = PrmComp;
                lblUser.Text = UserLogin.UserName;
                
                if (Convert.ToString(Request.QueryString["add_rfq_app"]) == "add")
                {
                    
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["Rfqview_app"] = "";
                    //Session["Rfqview_app"] = "923";
                    
                }
                
            }                        
        }
    }
    
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
       {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            FormView2.Visible = true;
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_rfqs.aspx";
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
                    Label commlabel = (Label)FormView1.FindControl("commlabel");
                    Label comm = (Label)FormView1.FindControl("Label15");
                    comm.Visible = false;
                    commlabel.Visible = false;
                }

            }
        }

            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                try
                {
                    FormView2.Visible = false;
                    Image image4 = (Image)FormView1.FindControl("Image4");
                    image4.Visible = false;
                    RadioButtonList rdo1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
                    rdo1.SelectedIndex = 0;
                    Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
                    DropDownList chg = (DropDownList)FormView1.FindControl("chg_methodTextBox");
                    chg.SelectedIndex = 2;

                    UserClass.CheckLogin(Page);
                    UserClass UserLogin = (UserClass)Session["User"];
                    try
                    {
                        rfqs rfqclass = new rfqs();
                        DataSet dsrfqs = new DataSet();
                        dsrfqs = rfqclass.ViewRfq(UserLogin.UserName, "getRfqNo", "", 0, Convert.ToDateTime("12/12/2011"), Convert.ToDateTime("12/12/2011"), "", "", "", "", "", "", "", "", "", 0, "", "", 0, "", 0);

                        rfq.Text = dsrfqs.Tables[0].Rows[0][0].ToString();                       

                    }
                    catch { }

                    TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
                    TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");

                    if (Session["User"] != null)
                    {
                        string vUserId = UserLogin.UserName;
                        string vPage = "view_rfqs.aspx";
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
                            Label commlabel = (Label)FormView1.FindControl("commlabel");
                            TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                            Image imagecustlook = (Image)FormView1.FindControl("CustomerLook");

                            TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
                            TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
                            TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
                            TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
                            TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
                            TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
                            TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
                            TextBox salesman = (TextBox)FormView1.FindControl("smanTextBox");
                            TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");


                            comm.Visible = false;
                            commlabel.Visible = false;
                            image4.Visible = true;
                            imagecustlook.Visible = false;

                            TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
                            TextBox fobcod = (TextBox)FormView1.FindControl("fob_codeTextBox");

                            try
                            {

                                Order ord = new Order();
                                DataSet ds = new DataSet();

                                ds = ord.SelectCustomer("select", UserLogin.UserName, "cust-no", "EQUAL", "");

                                cust.Text = ds.Tables[0].Rows[0][0].ToString();
                                custname.Text = ds.Tables[0].Rows[0][1].ToString();
                                addr1.Text = ds.Tables[0].Rows[0][2].ToString();
                                addr2.Text = ds.Tables[0].Rows[0][3].ToString();
                                city.Text = ds.Tables[0].Rows[0][4].ToString();
                                state.Text = ds.Tables[0].Rows[0][5].ToString();
                                zip.Text = ds.Tables[0].Rows[0][6].ToString();

                                salesman.Text = ds.Tables[0].Rows[0][8].ToString();
                                sname.Text = ds.Tables[0].Rows[0][9].ToString();
                                fobcod.Text = ds.Tables[0].Rows[0][16].ToString();
                            }
                            catch
                            {
                                HttpContext.Current.Response.Write("<script>alert('Invalid Customer Name')</script>");
                            }




                        }
                        if (aUsers == "internal")
                        {
                            TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                            comm.Enabled = false;
                        }

                        reqdate.Text = DateTime.Now.ToShortDateString();
                        duedate.Text = DateTime.Now.ToShortDateString();
                        reqdate.Focus();

                    }
                }
                catch { }
            }


        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            FormView2.Visible = false;
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_rfqs.aspx";
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
                    TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
                    TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
                    TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
                    TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
                    TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
                    TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
                    TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
                    TextBox salesman = (TextBox)FormView1.FindControl("smanTextBox");
                    TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
                    Label commlabel = (Label)FormView1.FindControl("commlabel");
                    TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                    comm.Visible = false;
                    commlabel.Visible = false;

                    RadioButton fob1 = (RadioButton)FormView1.FindControl("RD5");
                    RadioButton fob2 = (RadioButton)FormView1.FindControl("RD6");
                    DropDownList fcharge = (DropDownList)FormView1.FindControl("chg_methodTextBox");
                    Image salesrep = (Image)FormView1.FindControl("SalesRep");
                    salesrep.Visible = false;
                    Image custlook = (Image)FormView1.FindControl("CustomerLook");
                    custlook.Visible = false;

                    cust.Enabled = false;
                    custname.Enabled = false;
                    addr1.Enabled = false;
                    addr2.Enabled = false;
                    city.Enabled = false;
                    state.Enabled = false;
                    sname.Enabled = false;
                    zip.Enabled = false;
                    salesman.Enabled = false;
                    fob1.Enabled = false;
                    fob2.Enabled = false;
                    fcharge.Enabled = false;
                }
                if (aUsers == "internal")
                {
                    TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                    HiddenFieldComm.Value = comm.Text;
                    comm.Enabled = false;

                }
            }
        }

        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
        RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
        TextBox reqdate2 = (TextBox)FormView1.FindControl("req_dateTextBox");
        reqdate2.Focus();
        if (fob.Text == "D" || fob.Text == "DEST")
        {
            destination.Checked = true;
        }
        else
        {
            origin.Checked = true;
        }
        if (fob.Text == "O")
        {
            origin.Checked = true;
        }
        else
        {
            destination.Checked = true;
        }
        }
        catch {  }

    }

    protected void UpdateButon_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_rfqs.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

        }

        Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        //Response.Write(rfq.Text);
        Session["Rfqview_app"] = rfq.Text;
        TextBox requesteddate = (TextBox)FormView1.FindControl("req_dateTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");

        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        DropDownList freightcharge = (DropDownList)FormView1.FindControl("chg_methodTextBox");
        TextBox address1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox address2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        DropDownList warehouse = (DropDownList)FormView1.FindControl("wh_monthTextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox commision = (TextBox)FormView1.FindControl("commTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox sno = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
        RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
        if (destination.Checked == true)
        {
            fob.Text = "D";
        }
        else
        {
            fob.Text = "O";
        }

        Session["rfqcust_app"] = cust.Text;

        try
        {

            rfqs rfqs = new rfqs();
            bool check = rfqs.ViewRfqValidate(UserLogin.UserName, "ValidateRfq", "", Convert.ToInt32(rfq.Text.Trim()), Convert.ToDateTime("01/01/0001"), Convert.ToDateTime("01/01/0001"), cust.Text.Trim(), "", "", "", "", "", "", sno.Text.Trim(), sname.Text.Trim(), 0, "", "", 0, "", 0);

            if (check)
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRfqView";
                ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmReqdate"].DefaultValue = requesteddate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr"].DefaultValue = address1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr2"].DefaultValue = address2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSmanName"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = HiddenFieldComm.Value;
                ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.Text.Trim();
                ObjectDataSource1.SelectParameters["prmChgmethod"].DefaultValue = freightcharge.Text.Trim();
                ObjectDataSource1.SelectParameters["prmWhmonth"].DefaultValue = warehouse.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmInst"].DefaultValue = sinst.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
        }
        catch { }
    }
   

    protected void Delete_Rfqview(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
                
        //ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = swhere;
        Label rfq = (Label)FormView1.FindControl("Label1");
               
        ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DeleteView";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        //Session["delete"] = "Delete";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        

    }
    

    protected void InsertButton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
        DropDownList fchrg = (DropDownList)FormView1.FindControl("chg_methodTextBox");
        DropDownList whmonth = (DropDownList)FormView1.FindControl("wh_monthTextBox");        
        RadioButtonList rdo1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        if (rdo1.SelectedIndex == 0)
        {
            fob.Text = "D";
        }
        else
        {
            fob.Text = "O";
        }

        Session["rfqcust_app"] = cust.Text;
        Session["Rfqview_app"] = rfq.Text.Trim();

        try
        {
            rfqs rfqs = new rfqs();
            bool check = rfqs.ViewRfqValidate(UserLogin.UserName, "ValidateRfq", "", Convert.ToInt32(Session["Rfqview_app"]), Convert.ToDateTime("01/01/0001"), Convert.ToDateTime("01/01/0001"), cust.Text.Trim(), "", "", "", "", "", "", sman.Text.Trim(), sname.Text.Trim(), 0, "", "", 0, "", 0);
            if (check)
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddRfqView";
                ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmReqdate"].DefaultValue = reqdate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr"].DefaultValue = addr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr2"].DefaultValue = addr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSmanName"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmInst"].DefaultValue = sinst.Text.Trim();
                ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = HiddenFieldComm.Value;
                ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.Text.Trim();
                ObjectDataSource1.SelectParameters["prmChgmethod"].DefaultValue = fchrg.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmWhmonth"].DefaultValue = whmonth.SelectedValue.Trim();

                Session["Rfqview_app"] = rfq.Text.Trim();
                Session["Rfqseq_app"] = Session["Rfqview_app"];
                Session["rfqsnos_app"] = Session["Rfqview_app"];
                //Response.Write("<script>window.location.href='list_rfqs.aspx'</script>");
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                //Response.Write("<script>javascript:window.location.href='rfqitem.aspx?rfqitmmod=insert'</script>");
                FormView2.ChangeMode(FormViewMode.Insert);
            }
        }
        catch { }
    }
   
    protected void FormView1_Unload(object sender, EventArgs e)
    {
       //try
        //{
            //Label rfqno = (Label)FormView1.FindControl("Label1");
            //Session["my_new_rfq_app"] = rfqno.Text;
            //if (Session["Rfqview_app"] == Session["my_new_rfq_app"])
            //{
            //    Session["my_new_rfq_app"] = null;
            //}
            //else
            //{
            //    Session["Rfqseq_app"] = Session["my_new_rfq_app"];
            //    Session["Rfqview_app"] = Session["my_new_rfq_app"];

            //    Session["rfqsnos_app"] = Session["my_new_rfq_app"];
            //    //Session["list_rfq_rfq_nos"] = Session["my_new_rfq"];
            //}
        //}
        //catch {  }
    }

    

   
    protected void Update2cancle_click(object sender, EventArgs e)
    {
        //FormView2.Visible = false;
        FormView1.Visible = true;
    }
    
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        //if (Session["get_new_rfq_no_2_app"] != null)
        //{
        //    Session["Rfqview_app"] = Session["get_new_rfq_no_2_app"];
        //}
        try
        {
            Label ldest = (Label)FormView1.FindControl("Label5");
            RadioButton dest = (RadioButton)FormView1.FindControl("RD5");
            RadioButton orig = (RadioButton)FormView1.FindControl("RD6");
            if (ldest.Text == "D" || ldest.Text == "DEST")
            {
                dest.Checked = true;
            }
            if (ldest.Text == "O")
            {
                orig.Checked = true;
            }
            // Response.Write(ldest.Text);
        }
        catch { }
    }
    protected void F1UpdateButton_Click(object sender, EventArgs e)
    {
        //FormView2.ChangeMode(FormViewMode.Insert);
    }
    protected void UpdateButton_Cancel_Click(object sender, EventArgs e)
    {
        //Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        //Session["Rfqview_app"] = rfq.Text;
    }    

    protected void cust_change_textbox(object sender, EventArgs e)
    {        
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        

        try
        {
            Order ord = new Order();            
            DataSet ds = new DataSet();

            ds = ord.SelectCustomer("search", UserLogin.UserName, "cust-no", "EQUAL", cust.Text.Trim());         

            custname.Text = ds.Tables[0].Rows[0][1].ToString();
            addr1.Text = ds.Tables[0].Rows[0][2].ToString();
            addr2.Text = ds.Tables[0].Rows[0][3].ToString();
            city.Text = ds.Tables[0].Rows[0][4].ToString();
            state.Text = ds.Tables[0].Rows[0][5].ToString();
            zip.Text = ds.Tables[0].Rows[0][6].ToString();

            sman.Text = ds.Tables[0].Rows[0][8].ToString();
            sname.Text = ds.Tables[0].Rows[0][9].ToString();
            fob.Text = ds.Tables[0].Rows[0][16].ToString();
            comm.Text = ds.Tables[0].Rows[0][15].ToString();
            HiddenFieldComm.Value = ds.Tables[0].Rows[0][15].ToString();
            cust.Focus();
        }
        catch
        {
            HttpContext.Current.Response.Write("<script>alert('Invalid Customer Name')</script>");
            cust.Focus();
        }
        
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
        //Session["list_rfq_grid_seqno"] = GridView1.SelectedIndex + 1;
        //Session["rfqcustpart"] = GridView1.SelectedRow.Cells[1].Text;
        //Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
        //Session["list_rfq_cust_style"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
        //foreach (GridViewRow gv in GridView1.Rows)
        //{
        //    Session["rfqsequenceno"] = ((Label)GridView1.SelectedRow.FindControl("rfqrowid")).Text;
        //    //Response.Write(Session["rfqsequenceno"]);
        //}
    }

    protected void Delete_RfqItem(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfqitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanDelete == false)
            {
                Response.Write("<script>alert('Sorry! You do not have permission Delete Record');</script>");
                Response.Write("<script>window.location.href = 'rfqitem.aspx';</script>");

            }
        }


        Label seqno = (Label)FormView2.FindControl("RfqSeqNoLabel");
        Label rfqno = (Label)FormView1.FindControl("Label1");

        ObjectDataSource4.SelectParameters["prmAction"].DefaultValue = "delete";
        ObjectDataSource4.SelectParameters["RfqSeqNo"].DefaultValue = seqno.Text.Trim();
        ObjectDataSource4.SelectParameters["prmRfqNo"].DefaultValue = rfqno.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }



    protected void updateRfqitem(object sender, EventArgs e)
    {

        ObjectDataSource4.SelectParameters["prmAction"].DefaultValue = "UpdateRfqItem";
        Session["rfqseq_app"] = Session["rfqseq_app"];

        TextBox RfqQty = (TextBox)FormView2.FindControl("RfqQtyTextBox");
        TextBox RfqStock = (TextBox)FormView2.FindControl("RfqStockTextBox");
        TextBox RfqName = (TextBox)FormView2.FindControl("RfqNameTextBox");
        TextBox RfqPartno = (TextBox)FormView2.FindControl("RfqPartnoTextBox");
        TextBox Rfqstyle = (TextBox)FormView2.FindControl("RfqstyleTextBox");
        TextBox RfqProcat = (TextBox)FormView2.FindControl("RfqProcatTextBox");
        TextBox RfqCol = (TextBox)FormView2.FindControl("RfqColTextBox");
        TextBox RfqCoat = (TextBox)FormView2.FindControl("RfqCoatTextBox");
        TextBox RfqLength = (TextBox)FormView2.FindControl("RfqLengthTextBox");
        TextBox RfqWidth = (TextBox)FormView2.FindControl("RfqWidthTextBox");
        TextBox RfqDepth = (TextBox)FormView2.FindControl("RfqDepthTextBox");
        TextBox RfqBoard = (TextBox)FormView2.FindControl("RfqBoardTextBox");
        TextBox RfqCal = (TextBox)FormView2.FindControl("RfqCalTextBox");

        TextBox lv_qty_1 = (TextBox)FormView2.FindControl("lv_qty_1");
        TextBox lv_qty_2 = (TextBox)FormView2.FindControl("lv_qty_2");
        TextBox lv_qty_3 = (TextBox)FormView2.FindControl("lv_qty_3");
        TextBox lv_qty_4 = (TextBox)FormView2.FindControl("lv_qty_4");
        TextBox lv_qty_5 = (TextBox)FormView2.FindControl("lv_qty_5");
        TextBox lv_qty_6 = (TextBox)FormView2.FindControl("lv_qty_6");
        TextBox lv_qty_7 = (TextBox)FormView2.FindControl("lv_qty_7");
        TextBox lv_qty_8 = (TextBox)FormView2.FindControl("lv_qty_8");
        TextBox lv_qty_9 = (TextBox)FormView2.FindControl("lv_qty_9");
        TextBox lv_qty_10 = (TextBox)FormView2.FindControl("lv_qty_10");

        TextBox lv_delivery_1 = (TextBox)FormView2.FindControl("lv_delivery_1");
        TextBox lv_delivery_2 = (TextBox)FormView2.FindControl("lv_delivery_2");
        TextBox lv_delivery_3 = (TextBox)FormView2.FindControl("lv_delivery_3");
        TextBox lv_delivery_4 = (TextBox)FormView2.FindControl("lv_delivery_4");
        TextBox lv_delivery_5 = (TextBox)FormView2.FindControl("lv_delivery_5");
        TextBox lv_delivery_6 = (TextBox)FormView2.FindControl("lv_delivery_6");
        TextBox lv_delivery_7 = (TextBox)FormView2.FindControl("lv_delivery_7");
        TextBox lv_delivery_8 = (TextBox)FormView2.FindControl("lv_delivery_8");
        TextBox lv_delivery_9 = (TextBox)FormView2.FindControl("lv_delivery_9");
        TextBox lv_delivery_10 = (TextBox)FormView2.FindControl("lv_delivery_10");

        TextBox lv_price_1 = (TextBox)FormView2.FindControl("lv_price_1");
        TextBox lv_price_2 = (TextBox)FormView2.FindControl("lv_price_2");
        TextBox lv_price_3 = (TextBox)FormView2.FindControl("lv_price_3");
        TextBox lv_price_4 = (TextBox)FormView2.FindControl("lv_price_4");
        TextBox lv_price_5 = (TextBox)FormView2.FindControl("lv_price_5");
        TextBox lv_price_6 = (TextBox)FormView2.FindControl("lv_price_6");
        TextBox lv_price_7 = (TextBox)FormView2.FindControl("lv_price_7");
        TextBox lv_price_8 = (TextBox)FormView2.FindControl("lv_price_8");
        TextBox lv_price_9 = (TextBox)FormView2.FindControl("lv_price_9");
        TextBox lv_price_10 = (TextBox)FormView2.FindControl("lv_price_10");

        TextBox lv_uom_1 = (TextBox)FormView2.FindControl("lv_uom_1");
        TextBox lv_uom_2 = (TextBox)FormView2.FindControl("lv_uom_2");
        TextBox lv_uom_3 = (TextBox)FormView2.FindControl("lv_uom_3");
        TextBox lv_uom_4 = (TextBox)FormView2.FindControl("lv_uom_4");
        TextBox lv_uom_5 = (TextBox)FormView2.FindControl("lv_uom_5");
        TextBox lv_uom_6 = (TextBox)FormView2.FindControl("lv_uom_6");
        TextBox lv_uom_7 = (TextBox)FormView2.FindControl("lv_uom_7");
        TextBox lv_uom_8 = (TextBox)FormView2.FindControl("lv_uom_8");
        TextBox lv_uom_9 = (TextBox)FormView2.FindControl("lv_uom_9");
        TextBox lv_uom_10 = (TextBox)FormView2.FindControl("lv_uom_10");

        TextBox lv_date_1 = (TextBox)FormView2.FindControl("lv_date_1");
        TextBox lv_date_2 = (TextBox)FormView2.FindControl("lv_date_2");
        TextBox lv_date_3 = (TextBox)FormView2.FindControl("lv_date_3");
        TextBox lv_date_4 = (TextBox)FormView2.FindControl("lv_date_4");
        TextBox lv_date_5 = (TextBox)FormView2.FindControl("lv_date_5");
        TextBox lv_date_6 = (TextBox)FormView2.FindControl("lv_date_6");
        TextBox lv_date_7 = (TextBox)FormView2.FindControl("lv_date_7");
        TextBox lv_date_8 = (TextBox)FormView2.FindControl("lv_date_8");
        TextBox lv_date_9 = (TextBox)FormView2.FindControl("lv_date_9");
        TextBox lv_date_10 = (TextBox)FormView2.FindControl("lv_date_10");

        ObjectDataSource4.SelectParameters["RfqQty"].DefaultValue = RfqQty.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqName"].DefaultValue = RfqName.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqPartno"].DefaultValue = RfqPartno.Text.Trim();
        ObjectDataSource4.SelectParameters["Rfqstyle"].DefaultValue = Rfqstyle.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqProcat"].DefaultValue = RfqProcat.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCol"].DefaultValue = RfqCol.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCoat"].DefaultValue = RfqCoat.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqLength"].DefaultValue = RfqLength.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqWidth"].DefaultValue = RfqWidth.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqBoard"].DefaultValue = RfqBoard.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqDepth"].DefaultValue = RfqDepth.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCal"].DefaultValue = RfqCal.Text.Trim();

        ObjectDataSource4.SelectParameters["lv_qty2"].DefaultValue = lv_qty_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty3"].DefaultValue = lv_qty_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty4"].DefaultValue = lv_qty_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty5"].DefaultValue = lv_qty_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty6"].DefaultValue = lv_qty_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty7"].DefaultValue = lv_qty_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty8"].DefaultValue = lv_qty_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty9"].DefaultValue = lv_qty_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_1"].DefaultValue = lv_delivery_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_2"].DefaultValue = lv_delivery_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_3"].DefaultValue = lv_delivery_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_4"].DefaultValue = lv_delivery_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_5"].DefaultValue = lv_delivery_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_6"].DefaultValue = lv_delivery_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_7"].DefaultValue = lv_delivery_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_8"].DefaultValue = lv_delivery_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_9"].DefaultValue = lv_delivery_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_10"].DefaultValue = lv_delivery_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_1"].DefaultValue = lv_price_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_2"].DefaultValue = lv_price_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_3"].DefaultValue = lv_price_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_4"].DefaultValue = lv_price_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_5"].DefaultValue = lv_price_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_6"].DefaultValue = lv_price_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_7"].DefaultValue = lv_price_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_8"].DefaultValue = lv_price_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_9"].DefaultValue = lv_price_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_10"].DefaultValue = lv_price_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_1"].DefaultValue = lv_uom_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_2"].DefaultValue = lv_uom_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_3"].DefaultValue = lv_uom_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_4"].DefaultValue = lv_uom_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_5"].DefaultValue = lv_uom_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_6"].DefaultValue = lv_uom_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_7"].DefaultValue = lv_uom_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_8"].DefaultValue = lv_uom_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_9"].DefaultValue = lv_uom_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_10"].DefaultValue = lv_uom_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_1"].DefaultValue = lv_date_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_2"].DefaultValue = lv_date_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_3"].DefaultValue = lv_date_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_4"].DefaultValue = lv_date_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_5"].DefaultValue = lv_date_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_6"].DefaultValue = lv_date_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_7"].DefaultValue = lv_date_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_8"].DefaultValue = lv_date_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_9"].DefaultValue = lv_date_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_10"].DefaultValue = lv_date_10.Text.Trim();

        FormView2.ChangeMode(FormViewMode.ReadOnly);

    }


    protected void rfq_estimate(object sender, EventArgs e)
    {

        Session["rfqestno_app"] = Session["rfqsnos_app"];
        Response.Redirect("rfq_estimate.aspx");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "rfqitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;
            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
        }
    }

    protected void Insertbutton_Click(object sender, EventArgs e)
    {
        TextBox RfqQty = (TextBox)FormView2.FindControl("RfqQtyTextBox");
        TextBox RfqStock = (TextBox)FormView2.FindControl("RfqStockTextBox");
        TextBox RfqName = (TextBox)FormView2.FindControl("RfqNameTextBox");
        TextBox RfqPartno = (TextBox)FormView2.FindControl("RfqPartnoTextBox");
        TextBox Rfqstyle = (TextBox)FormView2.FindControl("RfqstyleTextBox");
        TextBox RfqProcat = (TextBox)FormView2.FindControl("RfqProcatTextBox");
        TextBox RfqCol = (TextBox)FormView2.FindControl("RfqColTextBox");
        TextBox RfqCoat = (TextBox)FormView2.FindControl("RfqCoatTextBox");
        TextBox RfqLength = (TextBox)FormView2.FindControl("RfqLengthTextBox");
        TextBox RfqWidth = (TextBox)FormView2.FindControl("RfqWidthTextBox");
        TextBox RfqDepth = (TextBox)FormView2.FindControl("RfqDepthTextBox");
        TextBox RfqBoard = (TextBox)FormView2.FindControl("RfqBoardTextBox");
        TextBox RfqCal = (TextBox)FormView2.FindControl("RfqCalTextBox");

        TextBox lv_qty_1 = (TextBox)FormView2.FindControl("lv_qty_1");
        TextBox lv_qty_2 = (TextBox)FormView2.FindControl("lv_qty_2");
        TextBox lv_qty_3 = (TextBox)FormView2.FindControl("lv_qty_3");
        TextBox lv_qty_4 = (TextBox)FormView2.FindControl("lv_qty_4");
        TextBox lv_qty_5 = (TextBox)FormView2.FindControl("lv_qty_5");
        TextBox lv_qty_6 = (TextBox)FormView2.FindControl("lv_qty_6");
        TextBox lv_qty_7 = (TextBox)FormView2.FindControl("lv_qty_7");
        TextBox lv_qty_8 = (TextBox)FormView2.FindControl("lv_qty_8");
        TextBox lv_qty_9 = (TextBox)FormView2.FindControl("lv_qty_9");
        TextBox lv_qty_10 = (TextBox)FormView2.FindControl("lv_qty_10");

        TextBox lv_delivery_1 = (TextBox)FormView2.FindControl("lv_delivery_1");
        TextBox lv_delivery_2 = (TextBox)FormView2.FindControl("lv_delivery_2");
        TextBox lv_delivery_3 = (TextBox)FormView2.FindControl("lv_delivery_3");
        TextBox lv_delivery_4 = (TextBox)FormView2.FindControl("lv_delivery_4");
        TextBox lv_delivery_5 = (TextBox)FormView2.FindControl("lv_delivery_5");
        TextBox lv_delivery_6 = (TextBox)FormView2.FindControl("lv_delivery_6");
        TextBox lv_delivery_7 = (TextBox)FormView2.FindControl("lv_delivery_7");
        TextBox lv_delivery_8 = (TextBox)FormView2.FindControl("lv_delivery_8");
        TextBox lv_delivery_9 = (TextBox)FormView2.FindControl("lv_delivery_9");
        TextBox lv_delivery_10 = (TextBox)FormView2.FindControl("lv_delivery_10");

        TextBox lv_price_1 = (TextBox)FormView2.FindControl("lv_price_1");
        TextBox lv_price_2 = (TextBox)FormView2.FindControl("lv_price_2");
        TextBox lv_price_3 = (TextBox)FormView2.FindControl("lv_price_3");
        TextBox lv_price_4 = (TextBox)FormView2.FindControl("lv_price_4");
        TextBox lv_price_5 = (TextBox)FormView2.FindControl("lv_price_5");
        TextBox lv_price_6 = (TextBox)FormView2.FindControl("lv_price_6");
        TextBox lv_price_7 = (TextBox)FormView2.FindControl("lv_price_7");
        TextBox lv_price_8 = (TextBox)FormView2.FindControl("lv_price_8");
        TextBox lv_price_9 = (TextBox)FormView2.FindControl("lv_price_9");
        TextBox lv_price_10 = (TextBox)FormView2.FindControl("lv_price_10");

        TextBox lv_uom_1 = (TextBox)FormView2.FindControl("lv_uom_1");
        TextBox lv_uom_2 = (TextBox)FormView2.FindControl("lv_uom_2");
        TextBox lv_uom_3 = (TextBox)FormView2.FindControl("lv_uom_3");
        TextBox lv_uom_4 = (TextBox)FormView2.FindControl("lv_uom_4");
        TextBox lv_uom_5 = (TextBox)FormView2.FindControl("lv_uom_5");
        TextBox lv_uom_6 = (TextBox)FormView2.FindControl("lv_uom_6");
        TextBox lv_uom_7 = (TextBox)FormView2.FindControl("lv_uom_7");
        TextBox lv_uom_8 = (TextBox)FormView2.FindControl("lv_uom_8");
        TextBox lv_uom_9 = (TextBox)FormView2.FindControl("lv_uom_9");
        TextBox lv_uom_10 = (TextBox)FormView2.FindControl("lv_uom_10");

        TextBox lv_date_1 = (TextBox)FormView2.FindControl("lv_date_1");
        TextBox lv_date_2 = (TextBox)FormView2.FindControl("lv_date_2");
        TextBox lv_date_3 = (TextBox)FormView2.FindControl("lv_date_3");
        TextBox lv_date_4 = (TextBox)FormView2.FindControl("lv_date_4");
        TextBox lv_date_5 = (TextBox)FormView2.FindControl("lv_date_5");
        TextBox lv_date_6 = (TextBox)FormView2.FindControl("lv_date_6");
        TextBox lv_date_7 = (TextBox)FormView2.FindControl("lv_date_7");
        TextBox lv_date_8 = (TextBox)FormView2.FindControl("lv_date_8");
        TextBox lv_date_9 = (TextBox)FormView2.FindControl("lv_date_9");
        TextBox lv_date_10 = (TextBox)FormView2.FindControl("lv_date_10");

        HiddenField hf1 = (HiddenField)FormView2.FindControl("HiddenField1");
        if (hf1.Value != "" && hf1.Value != "&nbsp;")
        {
            Session["my_new_est_no_app"] = "1";
        }
        else
        {
            Session["my_new_est_no_app"] = null;
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];


        ObjectDataSource4.SelectParameters["prmAction"].DefaultValue = "AddRfqItem";
        ObjectDataSource4.SelectParameters["RfqQty"].DefaultValue = RfqQty.Text.Trim();
        ObjectDataSource4.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource4.SelectParameters["RfqName"].DefaultValue = RfqName.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqPartno"].DefaultValue = RfqPartno.Text.Trim();
        ObjectDataSource4.SelectParameters["Rfqstyle"].DefaultValue = Rfqstyle.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqProcat"].DefaultValue = RfqProcat.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCol"].DefaultValue = RfqCol.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCoat"].DefaultValue = RfqCoat.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqLength"].DefaultValue = RfqLength.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqWidth"].DefaultValue = RfqWidth.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqBoard"].DefaultValue = RfqBoard.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqDepth"].DefaultValue = RfqDepth.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqCal"].DefaultValue = RfqCal.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqStock"].DefaultValue = RfqStock.Text.Trim();

        ObjectDataSource4.SelectParameters["lv_qty2"].DefaultValue = lv_qty_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty3"].DefaultValue = lv_qty_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty4"].DefaultValue = lv_qty_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty5"].DefaultValue = lv_qty_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty6"].DefaultValue = lv_qty_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty7"].DefaultValue = lv_qty_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty8"].DefaultValue = lv_qty_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_qty9"].DefaultValue = lv_qty_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_1"].DefaultValue = lv_delivery_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_2"].DefaultValue = lv_delivery_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_3"].DefaultValue = lv_delivery_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_4"].DefaultValue = lv_delivery_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_5"].DefaultValue = lv_delivery_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_6"].DefaultValue = lv_delivery_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_7"].DefaultValue = lv_delivery_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_8"].DefaultValue = lv_delivery_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_9"].DefaultValue = lv_delivery_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_delivery_10"].DefaultValue = lv_delivery_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_1"].DefaultValue = lv_price_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_2"].DefaultValue = lv_price_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_3"].DefaultValue = lv_price_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_4"].DefaultValue = lv_price_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_5"].DefaultValue = lv_price_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_6"].DefaultValue = lv_price_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_7"].DefaultValue = lv_price_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_8"].DefaultValue = lv_price_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_9"].DefaultValue = lv_price_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_price_10"].DefaultValue = lv_price_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_1"].DefaultValue = lv_uom_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_2"].DefaultValue = lv_uom_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_3"].DefaultValue = lv_uom_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_4"].DefaultValue = lv_uom_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_5"].DefaultValue = lv_uom_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_6"].DefaultValue = lv_uom_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_7"].DefaultValue = lv_uom_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_8"].DefaultValue = lv_uom_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_9"].DefaultValue = lv_uom_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_uom_10"].DefaultValue = lv_uom_10.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_1"].DefaultValue = lv_date_1.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_2"].DefaultValue = lv_date_2.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_3"].DefaultValue = lv_date_3.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_4"].DefaultValue = lv_date_4.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_5"].DefaultValue = lv_date_5.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_6"].DefaultValue = lv_date_6.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_7"].DefaultValue = lv_date_7.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_8"].DefaultValue = lv_date_8.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_9"].DefaultValue = lv_date_9.Text.Trim();
        ObjectDataSource4.SelectParameters["lv_date_10"].DefaultValue = lv_date_10.Text.Trim();
        ObjectDataSource4.SelectParameters["RfqEstNo"].DefaultValue = hf1.Value;

        FormView2.ChangeMode(FormViewMode.ReadOnly);

        Response.Write("<script>window.location.href='add_rfqapp.aspx?add_rfq_app=edit';</script>");


    }
    protected void FormView2_Unload(object sender, EventArgs e)
    {
        try
        {
            Label seqno = (Label)FormView2.FindControl("RfqSeqNoLabel");
            Session["my_new_seq_no_gen_app"] = seqno.Text;

        }
        catch
        {
        }

    }
    protected void FormView2_PreRender(object sender, EventArgs e)
    {
        //Label seqno = (Label)FormView2.FindControl("RfqSeqNoLabel");
        //try
        //{
        //    Session["my_new_seq_no_gen_app"] = seqno.Text;
            
        //}
        //catch { }
    }

    //protected void AddNewButton_Click(object sender, EventArgs e)
    //{
    //    FormView2.Visible = true;
    //    FormView2.ChangeMode(FormViewMode.Insert);
    //    AddNewButton.Visible = false;
    //}

    protected void FormView2_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView2.CurrentMode == FormViewMode.ReadOnly)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                //ObjectDataSource4.SelectParameters["prmAction"].DefaultValue = "Select";
            }

            if (FormView2.CurrentMode == FormViewMode.Insert)
            {
                Image image4 = (Image)FormView2.FindControl("Image4");
                // image4.Visible = false;

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];

                TextBox reqdate = (TextBox)FormView2.FindControl("req_dateTextBox");
                TextBox duedate = (TextBox)FormView2.FindControl("due_dateTextBox");
                TextBox color = (TextBox)FormView2.FindControl("RfqColTextBox");
                color.Text = "1";

                //AddNewButton.Visible = false;

                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "rfqitem.aspx";
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
                        for (int i = 1; i <= 10; i++)
                        {
                            int dateimgval = i + 15;
                            TextBox price = (TextBox)FormView2.FindControl("lv_price_" + i);
                            TextBox date = (TextBox)FormView2.FindControl("lv_date_" + i);
                            Image dateimg = (Image)FormView2.FindControl("Image" + dateimgval);

                            price.Enabled = false;
                            date.Enabled = false;
                            dateimg.Visible = false;
                        }
                    }
                }
            }

            if (FormView2.CurrentMode == FormViewMode.Edit)
            {
                TextBox RfqQty = (TextBox)FormView2.FindControl("RfqQtyTextBox");
                RfqQty.Focus();
                Image image4 = (Image)FormView2.FindControl("Image4");
                //image4.Visible = false;

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];

                TextBox reqdate = (TextBox)FormView2.FindControl("req_dateTextBox");
                TextBox duedate = (TextBox)FormView2.FindControl("due_dateTextBox");

                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "rfqitem.aspx";
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
                        for (int i = 1; i <= 10; i++)
                        {
                            int dateimgval = i + 15;
                            TextBox price = (TextBox)FormView2.FindControl("lv_price_" + i);
                            TextBox date = (TextBox)FormView2.FindControl("lv_date_" + i);
                            Image dateimg = (Image)FormView2.FindControl("Image" + dateimgval);

                            price.Enabled = false;
                            date.Enabled = false;
                            dateimg.Visible = false;
                        }
                    }
                }
            }
            if (FormView2.CurrentMode == FormViewMode.Insert)
            {
                TextBox RfqQty = (TextBox)FormView2.FindControl("RfqQtyTextBox");
                RfqQty.Focus();
            }


        }
        catch { }
    }


    protected void StyleTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Rfqstyle = (TextBox)FormView2.FindControl("RfqstyleTextBox");
        TextBox RfqLength = (TextBox)FormView2.FindControl("RfqLengthTextBox");

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", Rfqstyle.Text.Trim(), "", "");

        if (cerror != "")
        {
            Rfqstyle.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");

            Rfqstyle.Focus();
        }
        else
        {
            RfqLength.Focus();
        }
    }

    protected void CategoryTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Rfqprocat = (TextBox)FormView2.FindControl("RfqProcatTextBox");
        TextBox Rfqcolor = (TextBox)FormView2.FindControl("RfqColTextBox");
        Table QHelp = (Table)FormView2.FindControl("Qtyhelp");


        /*HiddenField procathidden = (HiddenField)FormView2.FindControl("HiddenField2");
        string procatval = procathidden.Value; */

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", "", Rfqprocat.Text.Trim(), "");

        if (cerror != "")
        {
            Rfqprocat.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");

            //Page.ClientScript.RegisterStartupScript
            //(this.GetType(), "showqty", "showQty();", true);
            Rfqprocat.Focus();
        }
        else
        {
            //Page.ClientScript.RegisterStartupScript
            // (this.GetType(), "showqty", "showQty();", true);
            Rfqcolor.Focus();
        }
    }
    protected void BoardTextChanged(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox Rfqboard = (TextBox)FormView2.FindControl("RfqBoardTextBox");
        TextBox Qtytxt = (TextBox)FormView2.FindControl("TextBox1");
        TextBox Calipar = (TextBox)FormView2.FindControl("RfqCalTextBox");

        rfqs rfq = new rfqs();
        string cerror = rfq.ValidateRfq("", UserLogin.UserName, "ValidateRfq", "", "", "", Rfqboard.Text.Trim());

        if (cerror != "")
        {
            Rfqboard.Text = "";
            HttpContext.Current.Response.Write("<script>alert('" + cerror + "')</script>");

            //Page.ClientScript.RegisterStartupScript
            //            (this.GetType(), "showqty", "showQty();", true);
            Rfqboard.Focus();
        }
        else
        {
            LookUp lookup = new LookUp();
            DataSet ds = new DataSet();

            ds = lookup.BoardLook("search", UserLogin.UserName, "", "i-no", "EQUAL", Rfqboard.Text);
            Calipar.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

            //Page.ClientScript.RegisterStartupScript
            //            (this.GetType(), "showqty", "showQty();", true);
            Qtytxt.Focus();
        }
    }

    protected void View_Rfqview(object sender, EventArgs e)
    {
        Response.Redirect("list_rfqs.aspx");
    }
    
}
